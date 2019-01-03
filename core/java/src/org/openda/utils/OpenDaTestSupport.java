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
package org.openda.utils;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.utils.generalJavaUtils.StringUtilities;
import org.openda.utils.io.AsciiFileUtils;
import org.openda.utils.io.FileSupport;

import java.io.*;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;

/**
 * Support routines for junit tests. The main idea is:
 * - to split sources from tests, with sources in src/... and tests under test/...
 * - input for tests is put in directories with name testData with the corresponding package
 * - before each junit-test is started the input is copied to a separate direcory called opendaTestRuns
 * ? answers can be compared to expected output
 * 
 * You can get started by including the following code in your junit-test classes:
 *
 *   private File testRunDataDir;
 *   OpendaTestSupport testData;
 *
 *   protected void setUp() throws IOException {
 *       testData = new OpenDaTestSupport(SimonaNetcdfFileTest.class,"moduleName"); //creates a rundir
 *       testRunDataDir = testData.getTestRunDataDir();
 *   }
 *   
 *   other useful methods
 *   
 *   boolean checkIt    = testData.FilesAreIdentical(file1,file2);
 *   boolean checkAgain = testData.FileContains(file1,".*answer=1.0.*") // the regex needs to match the entire line!
 *   
 *   
 *   The following names are used throughout:
 *   projectRootDir = base dir containing all sources and tests
 *   testDataDir
 *   testRunDataDir
 *   JavaTestDir
 *   
 *   Example for module 'core':
 *   trunk=public <-- projectRootDir
 *         unit_test_info.txt with UnitTestsRootDir=. ModuleName=project
 *         core <-- projectRootDir if only part of openda is available
 *         	    <-- moduleRootDir
 *              unit_test_info.txt with UnitTestsRootDir=. ModuleName=core
 *              native
 *                  src
 *                  include
 *                  external
 *              java
 *                  unit_test_info.txt with UnitTestsRootDir=..
 *                  src
 *                     org
 *                        openda
 *                            utils
 *                                example.java (not a real example)
 *                  test <-- javaTestDir
 *                     org
 *                        openda
 *                            utils
 *                                testExample.java
 *                                testData <-- testDataDir
 *                                      input.txt
 *                  external
 *         opendaTestRuns <-- temporary copy of data directories for tests. Created automatically at test setup
 *            core
 *               org
 *                  openda
 *                     utils <-- testRunDataDir
 *                        input.txt
 *                 ... etc
 *         tests <-- testsDir
 *            test1
 *               file1_in_test1
 *               file2_in_test1
 *            test2
 *
 */
public class OpenDaTestSupport {

	// fixed names, prefixes, etc
    static private String testDataDirPostfix   = "testData";
    static private String testRunDirPrefix     = "opendaTestRuns";
    static private String javaTestDirPrefix    = "java"+File.separator+"test";

	//global attribute names.
	private static final String FILE_LOCATION = "netcdf";
	private static final String HISTORY_ATTRIBUTE = "history";
	private static final String DATE_CREATED_ATTRIBUTE = "date_created";
	private static final String CASE_NAME_ATTRIBUTE = "caseName";

	private static final String FILE_LOCATION_REGULAR_EXPRESSION = FILE_LOCATION + ".*$";
	private static final String FILE_LOCATION_REPLACEMENT = FILE_LOCATION + " replaced by dummy text in unit test";
	private static final Pattern FILE_LOCATION_PATTERN = Pattern.compile(FILE_LOCATION_REGULAR_EXPRESSION);

	private static final String HISTORY_ATTRIBUTE_REGULAR_EXPRESSION = HISTORY_ATTRIBUTE + " = \".*\"";
	private static final String HISTORY_ATTRIBUTE_REPLACEMENT = HISTORY_ATTRIBUTE + " = \"actual " + HISTORY_ATTRIBUTE + " attribute text replaced by dummy text in unit test\"";
	private static final Pattern HISTORY_ATTRIBUTE_PATTERN = Pattern.compile(HISTORY_ATTRIBUTE_REGULAR_EXPRESSION);

	private static final String DATE_CREATED_ATTRIBUTE_REGULAR_EXPRESSION = DATE_CREATED_ATTRIBUTE + " = \".*\"";
	private static final String DATE_CREATED_ATTRIBUTE_REPLACEMENT = DATE_CREATED_ATTRIBUTE + " = \"actual " + DATE_CREATED_ATTRIBUTE + " attribute text replaced by dummy text in unit test\"";
	private static final Pattern DATE_CREATED_ATTRIBUTE_PATTERN = Pattern.compile(DATE_CREATED_ATTRIBUTE_REGULAR_EXPRESSION);

	private static final String CASE_NAME_ATTRIBUTE_REGULAR_EXPRESSION = CASE_NAME_ATTRIBUTE + " = \".*\"";
	private static final String CASE_NAME_ATTRIBUTE_REPLACEMENT = CASE_NAME_ATTRIBUTE + " = \"actual " + CASE_NAME_ATTRIBUTE + " attribute text replaced by dummy text in unit test\"";
	private static final Pattern CASE_NAME_ATTRIBUTE_PATTERN = Pattern.compile(CASE_NAME_ATTRIBUTE_REGULAR_EXPRESSION);

	// paths and files for this test
	private File projectRoot    = null;
    private File javaTestDir    = null;
    private File testDataDir    = null;
	private File testRunRoot    = null;
	private File testRunDataDir = null;
	private File moduleRootDir  = null; 

    public OpenDaTestSupport(Class testClass,String moduleName){

		// find proper projectRoot
		File currentDir = new File("").getAbsoluteFile();

        File unitTestInfoFile = new File(currentDir, "unit_test_info.txt");
		boolean workInModule;
        if (unitTestInfoFile.exists()) {
            UnitTestInfo unitTestInfo = new UnitTestInfo(unitTestInfoFile);
            String moduleHintName = unitTestInfo.getModuleName();
            if(moduleHintName.equalsIgnoreCase(moduleName)){ // we are inside the proper module
            	// try to work one level up
            	File moduleRoot = unitTestInfo.getUnitTestsRootDir();
            	File topDir = new File(moduleRoot,"..");
            	File mainUnitTestInfoFile = new File(topDir, "unit_test_info.txt");
                if (mainUnitTestInfoFile.exists()) {
                	this.projectRoot = topDir.getAbsoluteFile();
                	workInModule =false;
                }else{
                	this.projectRoot = moduleRoot.getAbsoluteFile();
                	workInModule = true;
                }
            }else if(moduleHintName.equalsIgnoreCase("project")){ // main rootDir
            	this.projectRoot = currentDir;
            	workInModule = false;
            }else{
            	throw new RuntimeException("Requested module is "+moduleName
            			+" but the test started in module "+moduleHintName);
            }
        }else{
			throw new RuntimeException("No file with name unit_test_info.txt found"
        			+" to indicate project root in directory:"+currentDir.getAbsolutePath());
        }

        // look for directory test with the java unit tests and its input files 
        File actualProjectRoot = this.projectRoot;
        if (workInModule){
        	this.moduleRootDir = this.projectRoot;
        	this.javaTestDir = new File(this.projectRoot,javaTestDirPrefix);
        }else{
        	File moduleRoot = new File(this.projectRoot,moduleName);
        	this.moduleRootDir = moduleRoot;
        	if(! (moduleRoot.exists() && (moduleRoot.isDirectory())) ){
                throw new RuntimeException("Did not find module "+moduleName+". Was looking in "+ moduleRoot);
        	}
        	this.javaTestDir = new File(new File(actualProjectRoot, moduleName),javaTestDirPrefix);
        }
    	if(! (this.javaTestDir.exists() && (this.javaTestDir.isDirectory())) ){
    		throw new RuntimeException("Did not find test data. Was looking in "+this.javaTestDir);
    	}

    	// look for directory with test data
        String testPackage = getPackageDirectoryPath(testClass);
		File testDir    = new File(this.javaTestDir, testPackage);
		if(! (testDir.exists() && (testDir.isDirectory())) ){
    		throw new RuntimeException("Did not find directory with test class. Was looking in "+testDir);
    	}
		this.testDataDir    = new File(testDir,testDataDirPostfix);
		if(! (this.testDataDir.exists() && (this.testDataDir.isDirectory())) ){
    		//throw new RuntimeException("Did not find directory with test data. Was looking in "+testDataDir);
			// We accept if this directory does not exist, then no data is copied
			this.testDataDir = null;
    	}

		// find where for perform tests and copy input data
        this.testRunRoot = new File(actualProjectRoot,testRunDirPrefix);
		if(! this.testRunRoot.isDirectory()){
            //noinspection ResultOfMethodCallIgnored
            this.testRunRoot.mkdirs();
		}
		
		this.testRunDataDir = new File(testRunRoot,moduleName
				+File.separator+ testPackage);
		if(this.testRunDataDir.exists()){ //delete old version
			BBUtils.deleteFileOrDir(this.testRunDataDir);
		}
		
		// create directory with test data
		BBUtils.makeDirectoryCloneForTestRun(this.testDataDir, this.testRunDataDir);
	}

    public File getTestRunDataDir(){
        return this.testRunDataDir;
    } 

    /**
     * Check if two files are equal. Returns false if there is a problem.
     * @param file1
     * @param file2
     * @return same?
     */
	public boolean FilesAreIdentical(File file1,File file2){
		int skipLines=0;
		return FilesAreIdentical(file1,file2,skipLines);
	}
    
	/**
	 * Check if two files are equal. Returns false if there is a problem.
	 * Has option to skip first few headerlines.
	 * @param file1
	 * @param file2
	 * @param skipLines
	 * @return
	 */
	public boolean FilesAreIdentical(File file1,File file2, int skipLines){
		return FilesAreIdentical(file1,file2, skipLines, 0.0);
	}

	/**
	 * Read a text file remove all whitespace/tabs/line enindgs and return content as a single string
	 * Usefull for making regular expression matches with parts of a file.
	 * @param file1
	 * @return string with file content excluding spaces/spacial characters
	 */
	public String returnFileAsSingleString(File file1){
		try {
			FileReader fileReader1 = new FileReader(file1);
			BufferedReader buff1 = new BufferedReader(fileReader1);
			String line = null;
			StringBuilder stringBuilder = new StringBuilder();

			while ((line = buff1.readLine()) != null) {
				line=line.replaceAll("\\s+","");
				stringBuilder.append(line);
			}
			String fileContent=stringBuilder.toString();
			return fileContent;

		} catch (IOException e) {
			System.out.println("Error -- " + e.toString());
			throw new RuntimeException("Cannot read from file "+file1
					+ "\n");
		}
	}



	/**
	 * Check if two files are equal. Returns false if there is a problem.
	 * Has option to skip first few headerlines.
	 * @param file1
	 * @param file2
	 * @param skipLines
	 * @param eps       absolute torreance on numbers in lines
	 * @return
	 */
	public boolean FilesAreIdentical(File file1,File file2, int skipLines, double eps){
		boolean identical = true;
		try {
			// open file
			if (!file1.exists()) {
				//throw new FileNotFoundException ("File does not exist: " + file1);
				System.out.println("File does not exist: " + file1);
				return false;
			}
			if (!file1.isFile()) {
				//throw new IllegalArgumentException("Should be a file: " + file1);
				return false;
			}
			FileReader fileReader1 = new FileReader(file1);
			BufferedReader buff1 = new BufferedReader(fileReader1);
			// open file 2
			if (!file2.exists()) {
				//throw new FileNotFoundException ("File does not exist: " + file2);
				System.out.println("File does not exist: " + file2);
				return false;
			}
			if (!file2.isFile()) {
				//throw new IllegalArgumentException("Should be a file: " + file2);
				return false;
			}
			FileReader fileReader2 = new FileReader(file2);
			BufferedReader buff2 = new BufferedReader(fileReader2);
			// skip header
			String line1;
			String line2;
			for(int line=0;line<skipLines;line++){
				line1 = buff1.readLine();
				line2 = buff2.readLine();
				System.out.println("Skipping line "+line);
				if (line1 == null){
					System.out.println("Problem skipping header for "
							+file1.getAbsolutePath());
					return false;
				}
				if (line2 == null){
					System.out.println("Problem skipping header for "
							+file2.getAbsolutePath());
					return false;
				}
			}
			// read line by line
			boolean done = false;
			int lineno=1;
			while (!done) {
				line1 = buff1.readLine();
				line2 = buff2.readLine();
				//System.out.println("file1: "+ line1);
			    //System.out.println("file2: "+ line2);
				if ( (line1 == null) & (line2 == null) ){
					done = true; // identical until end of file
				}else if (line1==null){
					identical = false;
					done = true;
					System.out.println("line "+lineno+": line1 is empty");
				    System.out.println("file2: "+ line2);
				}else if (line2==null){
					identical = false;
					done = true;					
					System.out.println("line "+lineno+": line2 is empty");
					System.out.println("file1: "+ line1);
				}else if (!line1.equals(line2)){
					if (eps>0){
						identical=compareStringsWithRealTollerance(line1, line2, 1.0e-6, lineno);
						done = ! identical;
					}
					else {
						identical=false;
						done=true;
						System.out.println("line "+lineno+": line1 != line2");
						System.out.println("file1: "+ line1);
					    System.out.println("file2: "+ line2);
					}
				}else{
					//System.out.print("=");
				}
				lineno++;
			}
			buff1.close();
			buff2.close();
			System.out.println("");
		} catch (IOException e) {
			System.out.println("Error -- " + e.toString());
			throw new RuntimeException("Cannot read from file "+file1 
					+ "\n or file "+file2);
		}
		if(identical){
			System.out.println("Files are identical :\n"
					+"file1="+file1.getAbsolutePath()+"\n"
					+"file2="+file2.getAbsolutePath());
		}
		return identical;
	}
	

	private boolean compareStringsWithRealTollerance(String line1, String line2, double eps, int lineno){
	//Extract all numbers
		ArrayList<String> numbers1= parseIntsAndFloats(line1);
		ArrayList<String> numbers2= parseIntsAndFloats(line2);
	   	String remaining1 = removeIntsAndFloats(line1);
	   	String remaining2 = removeIntsAndFloats(line2);
	    if (remaining1.equals(remaining2) && numbers1.size()== numbers2.size()){
	    	//Compare all numbers:
	    	for (int iNumber=0;iNumber<numbers1.size();iNumber++){
	    		double val1 = Double.parseDouble(numbers1.get(iNumber));
	    		double val2 = Double.parseDouble(numbers2.get(iNumber));
	    		if (Math.abs((val1-val2)) > eps) {
	    			System.out.println("line "+lineno+": line1 != line2");
					System.out.println("file1: "+ line1);
				    System.out.println("file2: "+ line2);
	    			System.out.println("abs("+val1+"-"+val2+") > "+eps);
	    			return false;
	    		}
	    	}
	    	System.out.println("line "+lineno+": line1 != line2 (eps="+eps+")");
			System.out.println("file1: "+ line1);
		    System.out.println("file2: "+ line2);
	    	return true;
	    }
	    System.out.println("line "+lineno+": line1 != line2");
		System.out.println("file1: "+ line1);
	    System.out.println("file2: "+ line2);
	    return false;
	}
	
	
	
	private static String removeIntsAndFloats(String line){
		line=line.replaceAll("[0-9]*\\.?[0-9]+", "");
		line=line.replaceAll(" ", "");
		return line;
	}
	
	private ArrayList<String> parseIntsAndFloats(String raw) {

	    ArrayList<String> listBuffer = new ArrayList<String>();

	    Pattern p = Pattern.compile("[0-9]*\\.?[0-9]+");

	    Matcher m = p.matcher(raw);

	    while (m.find()) {
	        listBuffer.add(m.group());
	    }

	    return listBuffer;
	}
	
	
	
	
	private boolean compareLinesWithRealTollerance(String line1, String line2, int ndigits){
		if (line1.equals(line2)){
			return true;
		}
		if (ndigits==0) return false; 
		//We only consider reals/floats. The format is always x.xxxxxxxx with x in 0..9
		if (line1.length()!=line2.length()) return false;
		int     nsame=0;
		int     npoint=0;
		boolean inVal=false;
		
		
		
		
		
		
		for (int i=0; i<line1.length(); i++){
			if (!inVal){
				if (line1.substring(i,1).matches("[0-9]")) inVal=true;
			}
			
		}
		
		
		
		return false;
	}
	
	
	
	/**
	 * Does this file contain a certain string, eg. "Ended OK."
	 * Throws exceptions on IO troubles
	 * @param file File to be checkec
	 * @param toBeFound regex specification of the string to be found
	 * @return True if string was found in file, false otherwise
	 */
	public boolean FileContains(File file,String toBeFound){
		return FileSupport.FileContains(file, toBeFound);
	}

	/**
	 * Compares the two given texts after removing the given line from both texts.
	 *
	 * @param expectedText
	 * @param actualText
	 * @param lineNumber to remove from both texts before comparing them. The first line is 1.
	 * @throws IOException
	 */
	public static void compareSkipLine(String expectedText, String actualText, int lineNumber) throws IOException {
		//remove line.
		actualText = removeLine(actualText, lineNumber);
		if (actualText.isEmpty()) {
			Assert.fail("Actual text is empty after removing line " + lineNumber + ". Nothing to compare.");
		}

		expectedText = removeLine(expectedText, lineNumber);
		if (expectedText.isEmpty()) {
			Assert.fail("Expected text is empty after removing line " + lineNumber + ". Nothing to compare.");
		}

		Assert.assertEquals("Actual text does not equal expected text after removing line " + lineNumber + " from both texts.",
				expectedText, actualText);
	}

	/**
	 * Removes the first numberOfLines lines from the given text.
	 *
	 * @param text
	 * @param lineNumber to remove from given text. The first line is 1.
	 * @return resulting text.
	 * @throws IOException
	 */
	private static String removeLine(String text, int lineNumber) throws IOException {
		StringBuffer content = new StringBuffer();
		BufferedReader reader = new BufferedReader(new StringReader(text));

		try {
			String line = reader.readLine();
			int number = 1;
			while (line != null) {
				if (number != lineNumber) {
					content.append(line).append("\n");
				}
				line = reader.readLine();
				number++;
			}
		} finally {
			reader.close();
		}

		return content.toString();
	}

	/**
	 * Truncates the given string to the given number of characters.
	 *
	 * @param string
	 * @return truncated string.
	 */
	public static String truncate(String string, int maxCharacterCount) {
		if (string != null && string.length() > maxCharacterCount) {
			return string.substring(0, maxCharacterCount);
		}
		return string;
	}

	/*
	 * 
	 * Internal and testing this class
	 * 
	 * SHOULD NOT BE NEEDED FOR NORMAL USE
	 * 
	 */
	
	/**
	 * Return root directory for this project
	 * SHOULD NOT BE NEEDED FOR NORMAL USE
	 * @return moduleRootDir
	 */
	public File getModuleRootDir(){
        return this.moduleRootDir;
    }
	
	/**
	 * Return root directory for this project
	 * SHOULD NOT BE NEEDED FOR NORMAL USE
	 * @return projectRootDir
	 */
	public File getProjectRootDir(){
        return this.projectRoot;
    } 

	/**
	 * Return directory with java unit tests for this module
	 * SHOULD NOT BE NEEDED FOR NORMAL USE
	 * @return projectRootDir
	 */
	public File getJavaTestDir(){
        return this.javaTestDir;
    } 

	/**
	 * Return directory with data for unit test for this test class
	 * SHOULD NOT BE NEEDED FOR NORMAL USE
	 * @return projectRootDir
	 */
	public File getTestDataDir(){
        return this.testDataDir;
    } 
	
	/**
	 * Return directory where all input files for tests will be copied
	 * SHOULD NOT BE NEEDED FOR NORMAL USE
	 * @return projectRootDir
	 */
	@SuppressWarnings({"UnusedDeclaration"})
    public File getRunRootDir(){
        return this.testRunRoot;
    }

    /**
    * Gets directory path from class package name.
    * Replaces all package delimiters by directory delimiters:
    *
    * "package1.package2.class1" => "package1/package2/";
    *
    * @param cls Class to obtain directory for.
    * @return path to the class data directory.
    */
   public static String getPackageDirectoryPath(Class cls) {
       String packageName = cls.getName();
       packageName = packageName.substring(0, packageName.lastIndexOf('.'));

       return packageName.replace('.', File.separatorChar);
   }

    private boolean moduleIsPartOfPublicProject(File publicProjectRoot, String moduleName) {

		if (publicProjectRoot == null) {
			throw new RuntimeException("Input file object called publicProjectRoot is a null object.");
		}
		ArrayList<File> moduleDirs = new ArrayList<File>();
		File[] projectSubDirs = publicProjectRoot.listFiles();
		if (projectSubDirs == null) {
			throw new RuntimeException("Could not list files in " + publicProjectRoot.getAbsolutePath());
		}
		for (File projectSubDir : projectSubDirs) {
			File javaDir = new File(projectSubDir, "java");
			if (javaDir.exists() && javaDir.isDirectory()) {
				moduleDirs.add(projectSubDir);
			}
		}

		for (File moduleDir : moduleDirs) {
			if (moduleDir.getName().equalsIgnoreCase(moduleName)) {
				return true;
			}
		}
		return false;
	}

    /**
     * Reads the file and replaces each 'separator-key-separator' combination with replaceString
     * This can be used for example, to replace the working dir $WORKDIR$ of a particular file with current working directory
     * @param filePath
     * @param replaceString
     * @param key
     * @param separator
     * @throws IOException
     */
    public void replaceKeyStringInFile(String filePath, String replaceString, String key, String separator) throws IOException {
        File file = new File(filePath);
        if (!file.exists()) throw new IOException("File does not exist " + filePath);

        String[] sContent = FileSupport.readContentOfFile(file);
        /* Iterate over all lines in the file */
        for (int iLine=0; iLine < sContent.length; iLine++){
            if (sContent[iLine].contains(separator + key + separator)) {
                sContent[iLine] = sContent[iLine].replace(separator+key+separator,replaceString);
            }
        }

        FileSupport.writeContentOfFile(file, sContent);
    }

	/**
	 * Replace lines with netcdf history and date_created attributes with a dummy text before comparison,
	 * otherwise comparison will fail, because these attributes contain the current system time.
	 *
	 * @param text netcdf file in text format.
	 */
	private static String replaceNetcdfAttributeLinesWithTimeStamps(String text) {
		String[] lines = text.split("\n");

		//replace per line to avoid out of memory errors for large files.
		for (int n = 0; n < lines.length; n++) {
			lines[n] = FILE_LOCATION_PATTERN.matcher(lines[n]).replaceFirst(FILE_LOCATION_REPLACEMENT);
			lines[n] = HISTORY_ATTRIBUTE_PATTERN.matcher(lines[n]).replaceFirst(HISTORY_ATTRIBUTE_REPLACEMENT);
			lines[n] = DATE_CREATED_ATTRIBUTE_PATTERN.matcher(lines[n]).replaceFirst(DATE_CREATED_ATTRIBUTE_REPLACEMENT);
			lines[n] = CASE_NAME_ATTRIBUTE_PATTERN.matcher(lines[n]).replaceFirst(CASE_NAME_ATTRIBUTE_REPLACEMENT);
		}

		return StringUtilities.joinStringArrayUsingSeparator(lines, "\n");
	}

	/**
	 * This can be used e.g. in unit tests to compare netcdf data in human readable text format instead of in binary format.
	 *
	 * @param textFileWithExpectedOutput
	 * @param netcdfFileWithActualOutput
	 * @param variableNames semicolon delimited list of variables of which the data should be used in the comparison.
	 *                      If this is null or empty, then all variables are used.
	 * @throws IOException
	 *
	 * @deprecated The ascii output from NetcdfUtils.netcdfFileToString changes with new version of the netCDF library.
	 *             Use compareNetcdfFilesInTextFormat instead of this method and provide a netcdf file as reference.
	 */
	@Deprecated
	public static void compareNetcdfFileInTextFormat(File textFileWithExpectedOutput, File netcdfFileWithActualOutput, String variableNames) throws IOException {
		String expectedOutputString = replaceNetcdfAttributeLinesWithTimeStamps(AsciiFileUtils.readText(textFileWithExpectedOutput));
		//convert netcdf data to text for text comparison.
		String actualOutputString = replaceNetcdfAttributeLinesWithTimeStamps(NetcdfUtils.netcdfFileToString(netcdfFileWithActualOutput, variableNames));
		Assert.assertEquals("Actual output file '" + netcdfFileWithActualOutput + "' does not equal expected output file '" + textFileWithExpectedOutput + "'.",
				expectedOutputString, actualOutputString);
	}

	/**
	 * This can be used e.g. in unit tests to compare netcdf data in human readable text format instead of in binary format.
	 *
	 * @param expectedFile
	 * @param actualFile
	 * @param variableNames semicolon delimited list of variables of which the data should be used in the comparison.
	 *                      If this is null or empty, then all variables are used.
	 * @throws IOException
	 */
	public static void compareNetcdfFiles(File expectedFile, File actualFile, String variableNames) throws IOException {

		String expectedOutputString = replaceNetcdfAttributeLinesWithTimeStamps(NetcdfUtils.netcdfFileToString(expectedFile, variableNames));
		//convert netcdf data to text for text comparison.
		String actualOutputString = replaceNetcdfAttributeLinesWithTimeStamps(NetcdfUtils.netcdfFileToString(actualFile, variableNames));
		Assert.assertEquals("Actual output file '" + actualFile + "' does not equal expected output file '" + expectedFile
				+ "'.",
			expectedOutputString, actualOutputString);
	}

	/**
	 * This can be used e.g. in unit tests to compare netcdf data in human readable text format instead of in binary format.
	 *
	 * @param expectedFile
	 * @param actualFile
	 * @throws IOException
	 */
	public static void compareNetcdfFiles(File expectedFile, File actualFile) throws IOException {
		compareNetcdfFiles(expectedFile, actualFile, null);
	}

}
