/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.openda.blackbox.config;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.interfaces.IConfigurable;
import org.openda.interfaces.IDataObject;
import org.openda.utils.ObjectSupport;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.channels.FileChannel;
import java.util.*;

/**
 * TODO: description
 */
public class BBUtils {

    public static final boolean RUNNING_ON_WINDOWS = System.getProperty("os.name").startsWith("Windows");
    public static final boolean RUNNING_ON_LINUX = System.getProperty("os.name").startsWith("Linux");
    public static final boolean RUNNING_ON_64bit = System.getProperty("sun.arch.data.model").equals("64");
	public static final boolean RUNNING_ON_MAC = System.getProperty("os.name").startsWith("Mac") || System.getProperty("os.name").startsWith("Darwin");
    private Map<File, ClassLoader> classLoaders = new HashMap<File, ClassLoader>();
    private static long runNanos = 0L;

    /**
     * Copy file contents from source to destination
     * using a loop with chunks smaller than 64Mb - 32Kb,
     * otherwise on Windows, especially when copying to UNC paths,
     * with files bigger than 64MB, a "java.io.IOException:
     * Insufficient system resources exist to complete the requested service" could be thrown.
     * see http://forums.oracle.com/forums/thread.jspa?messageID=8622867
     * Also, without using small chunks, on some windows systems
     * for files larger than about 260 MB could get the error
     * "java.io.IOException: Map failed".
     * See http://www.coderanch.com/t/279109/Streams/java/Java-nio
     */
    private static final int MAX_COUNT = (64 * 1024 * 1024) - (32 * 1024);

    public static void makeDirectoryClone(File source, File target) {
        checkSourceAndTarget(source, target, true);
        if (target.exists()) {
            deleteDirectory(target);
        }
        copyDirectory(source, target, false);
    }

	public static void makeDirectoryCloneForTestRun(File source, File target) {
		checkSourceAndTarget(source, target, true);
		if (target.exists()) {
			deleteDirectory(target);
		}
		copyDirectory(source, target, true);
	}

	public static void makeFileClone(File source, File target) {
        checkSourceAndTarget(source, target, false);
        if (target.exists()) {
            boolean success = target.delete();
            if (!success) {
                throw new RuntimeException("BBUtils.makeClone: could not delete file " + target.getAbsolutePath());
            }
        }
        try {
            copyFile(source, target);
        } catch (IOException e) {
            throw new RuntimeException("BBUtils.makeClone: could not copy file " +
                    source.getAbsolutePath() + " to " + target.getAbsolutePath(), e);
        }
    }

    public static IoObjectInterface createIoObjectInstance(File workingDir, String className, String fileName, String[] arguments) {
        Object object = createIoOrDataObject(className);
        if (object instanceof IoObjectInterface) {
            IoObjectInterface ioObject = (IoObjectInterface) createIoOrDataObject(className);
            if (ioObject != null) {
                ioObject.initialize(workingDir, fileName, arguments);
            }
            return ioObject;
        } else {
            return null;
        }
    }

    public static IDataObject createDataObject(File workingDir, String className, String fileName, String[] arguments) {
        IDataObject dataObject = (IDataObject) createIoOrDataObject(className);
        if (dataObject != null) {
            String[] dataObjectArguments = new String[arguments.length + 1];
            dataObjectArguments[0] = fileName;
            System.arraycopy(arguments, 0, dataObjectArguments, 1, arguments.length);
            dataObject.initialize(workingDir, dataObjectArguments);
        }
        return dataObject;
    }

    public static Object createIoOrDataObject(String className) {
        // Create instance of class
        final Class javaClass;
        Object object;
        try {
            javaClass = Class.forName(className);
            object = javaClass.newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage(), e);
        }
        if (className.equals("org.openda.interfaces.IDataObject") &&
                !IDataObject.class.isInstance(object)) {
            return null;
        }
        if (className.equals("org.openda.blackbox.interfaces.IoObjectInterface") &&
                !IoObjectInterface.class.isInstance(object)) {
            return null;
        }
        return object;
    }

    public static SelectorInterface createSelectorInstance(File workingDir, String className, String[] arguments) {
        SelectorInterface selector = (SelectorInterface) ObjectSupport.createNewInstance(className, SelectorInterface.class);
        selector.initialize(workingDir, arguments);
        return selector;
    }

    public static int runExecutable(String exePath, File fileOrDir, String[] arguments) throws IOException {

		int exitValue;
        Process process = null;
        
        try {
            ArrayList<String> commandArgList = new ArrayList<String>();
            
        	File exeFile = new File(exePath); // remove things like  dir/./file and dir/../file from path
        	exePath = exeFile.getCanonicalPath(); // this seems important for the runtime below.
        	
            commandArgList.add(exePath);
            if (RUNNING_ON_WINDOWS) {
                final String[] knownScriptingExtensions = {".pl", ".tcl"};
                final String[] knownScriptingCommands = {"perl", "tclsh"};
                boolean isScript = false;
                for (int i = 0; !isScript && i < knownScriptingExtensions.length; i++) {
                    if (exePath.toLowerCase().endsWith(knownScriptingExtensions[i])) {
                        commandArgList.set(0,knownScriptingCommands[i]);
                        commandArgList.add(exePath);
                        isScript = true;
                    }
                }
            }else{
            	new File(exePath).setExecutable(true);
            }

            commandArgList.addAll(Arrays.asList(arguments));
            String[] commandArgs = commandArgList.toArray(new String[commandArgList.size()]);

            File workingDir = fileOrDir.isDirectory() ? fileOrDir : fileOrDir.getParentFile();

            // Write to log what we are running
            String message="Start executable:";
            for(String arg : commandArgList){
            	message+=arg+" ";
            }
			System.out.println("command="+message);
			System.out.println("workingDir="+workingDir);
            process = Runtime.getRuntime().exec(commandArgs, null, workingDir);

            BufferedReader stdInput = new BufferedReader(new
                    InputStreamReader(process.getInputStream()));

            //Tunnel screen output of executable's stdout
            String line;
            System.out.println("<OUTPUT>");
            while ((line = stdInput.readLine()) != null)
                System.out.println(line);
            System.out.println("</OUTPUT>");

            //wait until executable is finished
            exitValue = process.waitFor();

            System.out.println("\nExit value: " + exitValue);
        } catch (InterruptedException e) {
            process.destroy();
            throw new RuntimeException("Executable " + exePath +
                    " on " + fileOrDir.getAbsolutePath() + "has been interrupted", e);
        }
		return exitValue;
    }

    public static Object runJavaClass(String className, File fileOrDir, String[] arguments) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
		Object retClass = null;
        // Create instance of class and run it
        final Class javaClass;
        try {
            javaClass = Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new ClassNotFoundException("Class " + className + " does not exist.", e);
        }

        if (IConfigurable.class.isAssignableFrom(javaClass)) {
            IConfigurable configurable = (IConfigurable) javaClass.newInstance();
            configurable.initialize(fileOrDir, arguments);

        } else {
            ClassLoader classLoader = BBUtils.class.getClassLoader();

            Class<?> clz = classLoader.loadClass(className);
            try {
                final Method mainMethod = getMainMethod(clz);
                final String[] args = new String[arguments.length];
                for (int i = 0; i < arguments.length; i++) {
                    String argument = arguments[i];
                    //Note:
                    //The following code changes the pathnames in the arguments that are relative to the instanceDir to absolute pathnames,
                    //before passing them to the main method, because the instanceDir cannot be passed as workingDir to the main method in any way.
                    //This may cause problems when the mainMethod expects arguments with relative pathnames instead of absolute pathnames.
                    //Furthermore, there is no way to know which arguments are pathnames. This code will not change pathnames of files/directories
                    //that do not exist yet. This will cause problems for e.g. pathnames of output files.
                    //This code cannot be repaired without losing backwards compatibility. Therefore make sure that each configured BBAction implements
                    //the IConfigurable interface, so that this code is not used.
                    if (((new File(fileOrDir, argument)).isDirectory()) || (new File(fileOrDir, argument)).isFile()) {
                         args[i] = (new File(fileOrDir, arguments[i])).getAbsolutePath() ;
                    }else {
                        args[i] = arguments[i];
                    }
                }
                retClass = startAndWait(mainMethod, args);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException("Class " + className + " should implement org.openda.interfaces.IConfigurable or should have a main() method.", e);
            }
        }
		return retClass;
    }

    private static void checkSourceAndTarget(File source, File target, boolean sourceIsdirectory) {
        if (!source.exists())
            throw new RuntimeException("BBUtils.makeClone: source does not exist: " + source.getAbsolutePath());
        if (!source.canRead())
            throw new RuntimeException("BBUtils.makeClone: source is unreadable: " + source.getAbsolutePath());
        if (target.exists()) {
            if (sourceIsdirectory) {
                if (!target.isDirectory()) {
                    throw new RuntimeException("BBUtils.makeClone: target dir. " + target.getPath()
                            + " already exists as a file.");
                }
            } else {
                if (target.isDirectory()) {
                    throw new RuntimeException("BBUtils.makeFileClone: target file " + target.getPath()
                            + " already exists as a directory.");
                }
            }
        }
    }

    public static File getCurrentDir() {
        return new File("x").getAbsoluteFile().getParentFile();
    }

    public static void copyFile(File source, File target) throws IOException {
        FileChannel inChannel = null;
        FileChannel outChannel = null;
        try {
            inChannel = new FileInputStream(source).getChannel();
            outChannel = new FileOutputStream(target).getChannel();

            //Copy file contents from source to destination
            //Use a loop with chunks smaller than 64Mb - 32Kb,
            //otherwise on Windows, especially when copying to UNC paths,
            //with files bigger than 64MB, a "java.io.IOException: 
            //Insufficient system resources exist to complete the requested service" could be thrown.
            //see http://forums.oracle.com/forums/thread.jspa?messageID=8622867
            //Also, without using small chunks, on some windows systems
            //for files larger than about 260 MB could get the error
            //"java.io.IOException: Map failed".
            //See http://www.coderanch.com/t/279109/Streams/java/Java-nio
            long fileSize = inChannel.size();
            long position = 0;
            while (position < fileSize) {
                position += inChannel.transferTo(position, MAX_COUNT, outChannel);
            }
        } finally {
            if (inChannel != null) inChannel.close();
            if (outChannel != null) outChannel.close();
        }

        copyFilePermissions(source, target);
    }

    public static void copyDirectory(File source, File target, boolean acceptFileInUseError) {
        if (source.getName().equalsIgnoreCase(".svn")) {
            return;
        }
        if (!target.exists()) {
            boolean success = target.mkdirs();
            if (!success) {
                throw new RuntimeException("BBUtils.copyDirectory: could not create directory " + target.getAbsolutePath());
            }
        }
        String[] files = source.list();

        for (String file : files) {
            File src = new File(source, file);
            File tgt = new File(target, file);

            if (src.isDirectory()) {
                copyDirectory(src, tgt, acceptFileInUseError);
            } else {
                try {
                    copyFile(src, tgt);
                } catch (IOException e) {
					if (acceptFileInUseError && e.getMessage().contains(
							"The process cannot access the file because it is being used by another process"))
					{
						// no action
					} else {
						throw new RuntimeException("BBUtils.copyDirectory: could not copy file " +
								src.getAbsolutePath() + " to " + tgt.getAbsolutePath() + ":" + e.getMessage(), e);
					}
                }
            }
        }

        copyFilePermissions(source, target);
    }

    /**
     * Sets the file permissions of the given source file
     * on the given target file.
     *
     * @param source
     * @param target
     */
    private static void copyFilePermissions(File source, File target) {
        if (source.canRead()) {
            target.setReadable(true, false);
        } else {
            target.setReadable(false, false);
        }

        if (source.canWrite()) {
            target.setWritable(true, false);
        } else {
            target.setWritable(false, false);
        }

        if (source.canExecute()) {
            target.setExecutable(true, false);
        } else {
            target.setExecutable(false, false);
        }
    }

    public static void deleteDirectory(File directory) {

        if (directory.getPath().length() <= 4) {
            throw new IllegalArgumentException("Path length is too short");
        }

        for (File file : directory.listFiles()) {
            if (file.isDirectory()) {
                deleteDirectory(file);
            } else {
                boolean success = file.delete();
//                if (!success) {
//                    throw new RuntimeException("Could not delete file " + file.getAbsolutePath());
//                }
            }
        }
        boolean success = directory.delete();
//        if (!success) {
//            throw new RuntimeException("Could not delete directory " + directory.getAbsolutePath());
//        }
    }

	/**
	 * Removes all model instance directories from previous runs,
	 * i.e. the directories that have the given path postfixed by the instanceNumber.
	 */
	public static void removeExistingModelInstanceDirectories(File instanceDirectoryWithoutPostfix) {
		int tries = 0;
		for (int n = 0; n < 1000; n++) { // fixed number at present
			File instanceDir = new File(instanceDirectoryWithoutPostfix.getAbsolutePath() + n);
			if (tries > 10) break; // just to avoid going through the whole loop
			// tries is the counter to check how many times I cannot find the directory
			if (!instanceDir.exists()) {
				tries++;
				continue;
			}
			try {
				BBUtils.deleteDirectory(instanceDir);
			} catch (Exception e) {
				throw new RuntimeException("Error deleting old directory " + instanceDir.getAbsolutePath(), e);
			}
		}
	}

    public static String determineExe(File configDir, String executableName) {
        String fullExecutablePath = checkExe(configDir, executableName);
        if (fullExecutablePath == null) {
            fullExecutablePath = checkExe(executableName);
        }
        if (fullExecutablePath == null) {
            //fullExecutablePath can be in instance directory. If it is in the instance directory,
            //then it may not be present at this moment, because instance directory has not
            //been created yet. Therefore no error.
            //if fullExecutablePath contains an absolute or a relative path as well as a filename
            //(as opposed to only a filename), then the fullExecutablePath
            //can still be in the instance directory, so no error.
        }
        return fullExecutablePath;
    }

    public static String checkExe(File directory, String exeName) {
        String fullExecutablePath = null;
        File exe = (directory != null) ? new File(directory, exeName) : new File(exeName);
        if (exe.exists()) {
        	exe.setExecutable(true);
            try {
                fullExecutablePath = exe.getCanonicalPath();
                fullExecutablePath = exe.getAbsolutePath();
            } catch (IOException e) {
                throw new RuntimeException("could not create canonical path for " + exe.getAbsolutePath(), e);
            }
            if (!exe.canExecute())  {
                throw new RuntimeException(fullExecutablePath + " is not an executable");
            }
        }
        return fullExecutablePath;
    }

    public static String checkExe(String exeName) {
        return checkExe(null, exeName);
    }

    public static String getFileNameWithOtherExtension(String fileName, String otherExtension) {
        return getFileNameWithoutExtension(fileName) + "." + otherExtension;
    }

    public static String getFileNameWithoutExtension(String fileName) {
        int pointPos = fileName.lastIndexOf('.');
        return (pointPos < 0) ? fileName : fileName.substring(0, pointPos);
    }

    public static File getFileOnOpenDaTempDir(String userSettingsFileName) {

        // determine directory where user settings file is stored
        File userSettingsFile = null;
        if (BBUtils.RUNNING_ON_LINUX || BBUtils.RUNNING_ON_MAC) {
            String homeDirPath = System.getenv("HOME");
            if (homeDirPath != null) {
                File homeDir = new File(homeDirPath);
                if (homeDir.exists() && homeDir.isDirectory()) {
                    File opendaUserConfigDir = new File(homeDir, ".openda");
                    if (opendaUserConfigDir.exists() || opendaUserConfigDir.mkdir()) {
                        userSettingsFile = new File(opendaUserConfigDir, userSettingsFileName);
                    }
                }
            }
        } else if (BBUtils.RUNNING_ON_WINDOWS) {
            String tempDirPath = System.getenv("temp");
            if (tempDirPath == null) tempDirPath = System.getenv("TEMP");
            if (tempDirPath == null) tempDirPath = System.getenv("TMP");
            if (tempDirPath == null) tempDirPath = System.getenv("tmp");
            if (tempDirPath != null) {
                File tempDir = new File(tempDirPath);
                if (tempDir.exists()) {
                    userSettingsFile = new File(tempDir, userSettingsFileName);
                }
            }
        }
        return userSettingsFile;
    }

    public static void deleteFileOrDir(File instanceFileOrDir) {
        if (instanceFileOrDir.isDirectory()) {
            deleteDirectory(instanceFileOrDir);
        } else {
            if (!instanceFileOrDir.delete()) {
                 throw new RuntimeException("BB Stoch model factory: Could not delete instance file "
                         + instanceFileOrDir.getAbsolutePath());
            }
        }
    }

    public static void validateClass(String className) {
        //validate class by creating an instance or by finding the main method.
        final Class javaClass;
        try {
            javaClass = Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Class " + className + " does not exist.", e);
        }

        if (IConfigurable.class.isAssignableFrom(javaClass)) {
            try {
                javaClass.newInstance();
            } catch (Exception e) {
                throw new RuntimeException("Error creating Class Instance " + className + ": " + e.getMessage(), e);
            }

        } else {
            try {
                ClassLoader classLoader = BBUtils.class.getClassLoader();
                Class<?> clz = classLoader.loadClass(className);
                Method mainMethod = getMainMethod(clz);
            } catch (Exception e) {
                throw new RuntimeException("Class " + className + " should implement org.openda.interfaces.IConfigurable or should have a main() method.", e);
            }
        }
    }


    public static Method getMainMethod(Class targetClass) throws NoSuchMethodException {
        Class[] argTypes = {String[].class};
        Method res;
        try {
            res = targetClass.getMethod("main", argTypes);
        } catch (NoSuchMethodException e) {
            throw new NoSuchMethodException("Cannot find method public static void main(String[]) in class " + targetClass.getName());
        }

        if (res.getReturnType() != Void.TYPE) {
            throw new NoSuchMethodException("main method should return void\n" + targetClass.getName());
        }

        if (!Modifier.isPublic(res.getModifiers())) {
            throw new NoSuchMethodException("main method should be public\n" + targetClass.getName());
        }

        if (!Modifier.isStatic(res.getModifiers())) {
            throw new NoSuchMethodException("main method should be static\n" + targetClass.getName());
        }
        return res;
    }

    private static Object startAndWait(final Method mainMethod, final String[] args)  {
        Exception exception = null;
		Object retMethod = null;
        try {
            long startNanoTime = System.nanoTime();
            try {
                retMethod = mainMethod.invoke(null, new Object[]{args});
            } finally {
                runNanos = System.nanoTime() - startNanoTime;
            }
        } catch (InvocationTargetException e) {
            Throwable targetException = e.getTargetException();
            if (targetException instanceof NoClassDefFoundError) {
                exception = new ClassNotFoundException(targetException.getMessage(), targetException);
            } else if (targetException instanceof Exception) {
                exception = (Exception) targetException;
            } else {
                exception = new Exception(targetException.getMessage(), targetException);
            }
        } catch (Exception e) {
            exception = e;
        } catch (NoClassDefFoundError e) {
            if (e.getCause() instanceof ClassNotFoundException) {
                exception = (Exception) e.getCause();
            } else {
                exception = new ClassNotFoundException(e.getMessage(), e);
            }
        } catch (Throwable e) {
            exception = new Exception(e.getMessage(), e);
        }

        if (exception != null) {
            throw new RuntimeException(exception);
        }

		return retMethod;
    }

    public static String getIdForGrid(String parameterId) {
        if (parameterId == null) {
            throw new IllegalArgumentException("parameterId == null");
        }

        return "Grid." + parameterId;
    }

    public static String getLocationFromId(String id) {
        if (id == null) {
            throw new IllegalArgumentException("id == null");
        }

        int periodIndex = id.indexOf('.');
        if (periodIndex <= 0) {
            throw new RuntimeException("Trouble parsing exchangeItem id: " + id);
        }
        return id.substring(0, periodIndex);
    }

    public static String getParameterFromId(String id) {
        if (id == null) {
            throw new IllegalArgumentException("id == null");
        }

        int periodIndex = id.indexOf('.');
        if (periodIndex <= 0) {
            throw new RuntimeException("Trouble parsing exchangeItem id: " + id);
        }
        return id.substring(periodIndex + 1, id.length());
    }

    public static double[] toDoubleArray(float[] array) {
        double[] res = new double[array.length];

        for (int i = 0; i < res.length; i++) {
            res[i] = array[i];
        }

        return res;
    }

    public static float[] toFloatArray(double[] array) {
        float[] res = new float[array.length];

        for (int i = 0; i < res.length; i++) {
            res[i] = (float) array[i];
        }

        return res;
    }

    public static float[][] toFloatArrays(double[][] arrays) {
        float[][] res = new float[arrays.length][];
        for (int i = 0; i < arrays.length; i++) {
            res[i] = toFloatArray(arrays[i]);
        }

        return res;
    }

    public static int[] toIntArray(double[] array) {
        int[] res = new int[array.length];

        for (int i = 0; i < res.length; i++) {
            res[i] = (int) array[i];
        }

        return res;
    }

    public static Integer[] box(int[] array) {
        if (array == null) throw new IllegalArgumentException("array is null.");

        Integer[] res = new Integer[array.length];
        for (int i = 0; i < array.length; i++) {
            res[i] = array[i];
        }

        return res;
    }

    public static int[] unbox(Integer[] array) {
        if (array == null) throw new IllegalArgumentException("array is null.");

        int[] res = new int[array.length];
        for (int i = 0; i < array.length; i++) {
            res[i] = array[i];
        }

        return res;
    }

	public static double[] unbox(Double[] array) {
		if (array == null) throw new IllegalArgumentException("array is null.");

		double[] res = new double[array.length];
		for (int i = 0; i < array.length; i++) {
			res[i] = array[i];
		}

		return res;
	}

    public static int count(int[] array, int value) {
        int res = 0;
        for (int i = 0; i < array.length; i++) {
            if (array[i] == value) res++;
        }
        return res;
    }

	public static boolean containsNaN(double[] array) {
		return indexOfNaN(array) != -1;
	}

	public static int indexOfNaN(double[] array) {
		return indexOfNaN(array, 0, array.length);
	}

	public static int indexOfNaN(double[] array, int pos, int length) {
		checkArg("array", array, pos, length);

		for (int i = pos, n = pos + length; i < n; i++) {
			if (Double.isNaN(array[i])) return i;
		}

		return -1;
	}

	public static void checkArg(String arrayName, double[] array, int pos, int length) {
		if (array == null)
			throw new IllegalArgumentException(arrayName + " == null");

		if (pos < 0)
			throw new IllegalArgumentException(arrayName + " pos < 0 " + pos + " < 0");

		if (length < 0)
			throw new IllegalArgumentException(arrayName + " length < 0 " + length + " < 0" );

		if (pos + length > array.length)
			throw new IllegalArgumentException(arrayName + " pos + " + arrayName + " length > " + arrayName + ".length "
					+ pos + " + " + length + " > " + array.length);
	}

	public static boolean isColdStart(String string) {
		return string != null
				&& (string.toUpperCase().equals("COLD_START") || string.toUpperCase().equals("COLDSTART") || string.toUpperCase().equals("COLD START"));
	}

	/**
	 * A path string in Python only works if there is no trailing path separator sign at the end of the path string.
	 * A path string in Python only works with / signs as path separators, not with \ signs,
	 * since the \ sign is an escape character in Python.
	 *
	 * @param file full path to a file or directory.
	 * @return file path string that can be passed to Python.
	 */
	public static String getFilePathStringForPython(File file) {
		String path;
		try {
			path = file.getCanonicalPath();
		} catch (IOException e) {
			throw new RuntimeException(BBUtils.class.getSimpleName() + ": cannot get canonical path of file '" + file.getAbsolutePath() + "'. Message was: " + e.getMessage(), e);
		}

		//remove any trailing \ or / sign.
		if (path.endsWith("\\") || path.endsWith("/")) {
			path = path.substring(0, path.length() - 1);
		}

		//replace all \ signs with / signs.
		path = path.replaceAll("\\\\", "/");

		return path;
	}
}
