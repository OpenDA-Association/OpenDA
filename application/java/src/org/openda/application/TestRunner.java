/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.application;
import org.openda.costa.CtaInitialize;
import org.openda.utils.ConfigTree;
import org.openda.utils.StochVector;
import org.openda.utils.io.FileSupport;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Supports running of stand-alone cases for OpenDA, ie based on complete configurations
 * with *.oda files. Tests are defined in *.tst files and configure a run and the required output.
 * 
 * The syntax for .tst files looks like this:
 * <?xml version="1.0" encoding="UTF-8"?>
 *<testConfig>
 *        <id>Enkf for simple_oscillator</id>
 *        <odaFile>../Enkf.oda</odaFile>
 *        <checks>
 *                <check>
 *                        <file>../Enkf.log</file>
 *                        <grep>===DONE===</grep>
 *                </check>
 *                <check>
 *                        <file>../enkf_results.m</file>
 *                </check>
 *                <check>
 *                        <file>../error_file_does_not_exist.m</file>
 *                </check>
 *        </checks>
 *</testConfig>
 *
 * @author verlaanm
 *
 */
public class TestRunner {

	/**
	 * Run all tests for given input.  
	 * @param args directory ot file with the test(s)
	 */
	public static void main(String[] args) {
		if(args.length==0){
			throw new RuntimeException("No argument was give. Please add a directory or tst-file name as argument.\n" +
					helpText());
		}
		int i=0;
		boolean configMode=false;
		while((i<args.length) && (args[i].startsWith("-"))){
			// option
			if(args[i].startsWith("-c")){
				// generate configuration
				configMode=true;
				i++;
			}

		}
		if(i>=args.length){
			throw new RuntimeException("Not enough arguments were given.");
		}


		File root = new File(args[i]);
		if(! root.exists()){
			throw new RuntimeException("File does not exist: "+root.getAbsolutePath());
		}
		if(configMode==true){
			List<File> files=findFiles(root,"oda");
			createTestFiles(files);
		}else{
			List<File> files=findFiles(root,"tst");
			int errors=0;
			int count=1;
			int thisErrors=0;
			for(File file : files){
				thisErrors=runOneTest(file,count);
				count++;
				errors+=thisErrors;
				System.out.println("   errors: "+thisErrors);
			}

			//Final message
			System.out.println("");
			System.out.println("Summary:");
			System.out.println("");		
			System.out.println("Number of tests="+(count-1));
			System.out.println("Number of errors="+errors);

			if(errors>0){
				throw new RuntimeException("Errors detected in one or more tests.");
			}
		}
	}

	private static String helpText(){
		String result;
		result ="Usage:\n";
		result+="oda_test [options] foo\n";
		result+="-c writes a new default configuation instead of running the tests.\n";
		result+="   The configuartion is generated based on the oda-files that are found.\n";
		result+="foo can be a file or directory. Test files have extension tst.";
		return result;
	}

	public static int runOneTest(File tstFile, int count){
		File workingDir=tstFile.getParentFile();
		ConfigTree conf = new ConfigTree(workingDir,tstFile.getName(),false);
		//System.out.println("config:"+conf.toString());
		String id=tstFile.getName();
		id=conf.getAsString("/id", id);
		String isString="====================================";
		System.out.println(isString+isString+isString);
		System.out.println(" test : " +id);
		System.out.println(isString+isString+isString);
		System.out.println("   number : " +count);
		// make list of actions
		String odaFileNames[] = null;
		ConfigTree[] actions=conf.getSubTrees("/actions/odaFile");
		if(actions!=null){
			odaFileNames = new String[actions.length];
			int i=0;
			for(ConfigTree action: actions){
				odaFileNames[i]=action.getAsString("/", "");
				if(odaFileNames[i].length()==0){
					throw new RuntimeException("No oda file found in test:"+tstFile.getAbsolutePath());
				}
				i++;
			}
		}else{
			odaFileNames = new String[1];
			odaFileNames[0]=tstFile.getAbsolutePath();
			odaFileNames[0]=odaFileNames[0].replace(".tst",".oda");
			odaFileNames[0]=conf.getAsString("/odaFile", odaFileNames[0]);
		}
		//checks
		ConfigTree[] checks=conf.getSubTrees("/checks/check");
		deleteOutput(workingDir,checks);

		// try run
		for(String odaFileName : odaFileNames){
			// odaFile
			File odaFile=new File(odaFileName);
			if(!odaFile.isAbsolute()){
				odaFile=new File(tstFile.getParentFile(),odaFileName);
			}
			System.out.println("   oda-file: "+odaFileName);
			if(! odaFile.exists()){
				System.out.println("   ERROR: oda-file does not exist: "+odaFile.getAbsolutePath());
				return 1;
			}else{
				//logfile
				String logFileName=odaFile.getAbsolutePath();
				logFileName=logFileName.replace(".oda",".log");
				File logFile=new File(logFileName);
				if(!logFile.isAbsolute()){
					logFile=new File(odaFile.getParentFile(),logFileName);
				}
				System.out.println("   log-file: "+logFileName);
				if(logFile.exists()){ //remove log-file before run
					logFile.delete();
				}
				System.out.println("   starting run.");
				PrintStream console=null;
				try {
					System.out.println("Set initial seed for StochVector:" +20100816);
					System.out.println("Set initial seed for native part:" +2101975);
					StochVector.setSeed(20100816); //make sure results stay the same
					CtaInitialize.setRandomSeed(2101975);
					ApplicationRunnerSingleThreaded runner= new ApplicationRunnerSingleThreaded();
					System.out.flush();
					console = System.out;
					System.setOut(new PrintStream(new FileOutputStream(logFile)));
					runner.initialize(odaFile.getParentFile(), odaFile.getName());
					runner.runSingleThreaded();
					System.out.println("===DONE===");
					System.out.flush();
					System.setOut(console);
					System.out.println("   run finished");
				} catch (Exception e) {
					System.out.println("===ERROR===");
					System.out.flush();
					System.setOut(console);
					System.out.println("   ERROR: "+e.getMessage());
					return 1;
				}
			}
		}
		return doChecks(workingDir,checks);
	}

	/**
	 * Perform a number of checks on the output of a run
	 * @return
	 */
	public static int doChecks(File workingDir, ConfigTree[] checks){
		int errors=0;
		if(checks!=null){
			int count=1;
			for(ConfigTree check: checks){
				System.out.println("   Check:"+count);
				//System.out.println("check="+check.toString());
				String fileName=check.getContentString("/file");
				if(fileName.length()==0){
					System.out.println("   ERROR: Filename was empty");
					errors++;
				}
				System.out.println("   Check for file: "+fileName);
				File file=new File(workingDir,fileName);
				if(! file.exists()){
					System.out.println("   ERROR: File does not exist:"+file.getAbsolutePath());
					errors++;				
				}
				// check contents
				String toBeFound=check.getContentString("/find");
				if((toBeFound!=null) && (toBeFound.length()>0)){
					System.out.println("   Check for substring in content: "+toBeFound);
					if(!FileSupport.FileContains(file,toBeFound)){
						System.out.println("   ERROR: Text not found:"+toBeFound);
						errors++;
					}
				}
				toBeFound=check.getContentString("/regex");
				if((toBeFound!=null) && (toBeFound.length()>0)){
					System.out.println("   Check for regular-expression in content: "+toBeFound);
					if(!FileSupport.FileContains(file,toBeFound,true)){
						System.out.println("   ERROR: Regex not found:"+toBeFound);
						errors++;
					}
				}
				count++;
			}
		}
		return errors;
	}

	/**
	 * Delete output files to be used for tests, or tests may look at old output.
	 * @return
	 */
	public static void deleteOutput(File workingDir, ConfigTree[] checks){
		if(checks!=null){
			int count=1;
			for(ConfigTree check: checks){
				//System.out.println("   Check:"+count);
				String fileName=check.getContentString("/file");
				File file=new File(workingDir,fileName);
				boolean removeBeforeTest = check.getAsBoolean("/file@removeBeforeTest", true);
				if(file.exists()){
					if(removeBeforeTest){
						System.out.println("   Removing existing file:"+file.getAbsolutePath());
						file.delete();
					}else{
						//System.out.println("   NOT removing existing file:"+file.getAbsolutePath());						
					}
				}else{
					//System.out.println("   File does not exist yet:"+file.getAbsolutePath());
				}
				count++;
			}
		}
	}

	/**
	 * Find all files in a directory and its subdirectories that end with a fixed suffix.
	 * @param root
	 * @param suffix
	 * @return List of Files that match
	 */
	public static List<File> findFiles(File root, String suffix){
		ArrayList<File> result = new ArrayList<File>();
		if(root.isFile()){
			if(root.getName().endsWith(suffix)){
				//System.out.println("+ "+root.getAbsolutePath());
				result.add(root);
			}else{
				//System.out.println("- "+root.getAbsolutePath());
			}
		}else if(root.isDirectory()){
			if(root.getAbsolutePath().indexOf(".svn")<0){ //skip svn files.
				File fileList[] = root.listFiles();
				Arrays.sort(fileList);
				for(File fileOrDir : fileList){
					if(fileOrDir.isFile()){
						if(fileOrDir.getName().endsWith(suffix)){
							//System.out.println("+ "+fileOrDir.getAbsolutePath());
							result.add(fileOrDir);
						}else{
							//System.out.println("- "+fileOrDir.getAbsolutePath());
						}
					}else if(fileOrDir.isDirectory()){
						List<File> subList = findFiles(fileOrDir,suffix);
						for(File file: subList){
							result.add(file);
						}
					}
				}

			}else{
				//System.out.println("- "+root.getAbsolutePath());
			}
		}
		return result;
	}

	public static void createTestFiles(List<File> files){
		for(File odaFile : files){
			System.out.println("Create default configuration for: "+odaFile.getAbsolutePath());
			File baseDir = odaFile.getParentFile();
			File testDir = new File(baseDir,"tests");
			if(! testDir.exists()){
				testDir.mkdir();
				System.out.println("   Creating dir: "+testDir.getAbsolutePath());
			}
			// tst-file
			String testFileName = odaFile.getName();
			testFileName=testFileName.replaceAll(".oda", ".tst");
			File testFile=new File(new File(baseDir,"tests"),testFileName);
			//log-file
			String logFileName=odaFile.getName();
			logFileName=".."+File.separatorChar+logFileName.replace(".oda",".log");
			// oda-file
			String odaFileName = ".."+File.separatorChar+odaFile.getName();

			if(testFile.exists()){
				System.out.println("   tst-file already exists: "+testFile.getName());
			}else{
				System.out.println("   create tst-file: "+testFile.getName());
				writeTstFile(testFile, odaFile.getAbsolutePath(), odaFileName, logFileName);
			}
		}
	}

	private static void writeTstFile(File file, String id, String odaFileName, String logFileName){
		try {
			BufferedWriter output =  new BufferedWriter(new FileWriter(file));
			output.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
			output.write("<testConfig>\n");
			output.write("        <id>"+id+"</id>\n");
			output.write("        <odaFile>"+odaFileName+"</odaFile>\n");
			output.write("        <checks>\n");
			output.write("                <check>\n");
			output.write("                        <file removeBeforeTest=\"yes\" >"+logFileName+"</file>\n");
			output.write("                        <find>===DONE===</find>\n");
			output.write("                </check>\n");
			output.write("        </checks>\n");
			output.write("</testConfig>");
			output.close();
		} catch (Exception e) {
			// TODO: handle exception
		}

	}

}
