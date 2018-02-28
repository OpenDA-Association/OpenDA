/* OpenDA v2.4.3 
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

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Tests for configured OpenDa applications
 */
public class OpenDaApplicationTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(OpenDaApplicationTest.class,"application");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testEnsrSimpleObsSimpleOscill() throws IOException {
    	String inputFile  = ( new File(testRunDataDir,"oscillatorEnkfOpenDaConfig.xml") ).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[1];
    	args[0] = inputFile;
        OpenDaApplication.main(args);
    }

	public void testFixedVersusRandomNoise() throws IOException {
		OpenDaApplication.main(new String[] {
				(new File(testRunDataDir,"oscillatorEnkfOpenDaConfig.xml") ).getAbsolutePath()} );
		File resultFile = new File(testRunDataDir, "enkf_results.m");
		File resultFile_1 = new File(testRunDataDir, "enkf_results_1.m");
		File resultFile_random = new File(testRunDataDir, "enkf_results_random.m");
		BBUtils.copyFile(resultFile, resultFile_1);
		OpenDaApplication.main(new String[] {
				(new File(testRunDataDir,"oscillatorEnkfOpenDaConfig.xml") ).getAbsolutePath()} );
		if(!testData.FilesAreIdentical(resultFile_1, resultFile)) {
			// files differ for unclear reasons. compare line by line
			List<String> linesInFile1 = FileComparer.ReadLines(resultFile_1);
			List<String> linesInFile2 = FileComparer.ReadLines(resultFile);
			int numLinesInFile1 = linesInFile1.size();
			int numLinesInFile2 = linesInFile2.size();
			System.out.println("numLinesInFile1="+numLinesInFile1+", numLinesInFile2" + numLinesInFile2);
			for (int i = 0; i < Math.min(numLinesInFile1, numLinesInFile2); i++) {
				String lineInFile1 = linesInFile1.get(i);
				String lineInFile2 = linesInFile2.get(i);
				if(!lineInFile1.equals(lineInFile2)) {
					System.out.println("Difference on line " + i + ":");
					System.out.println("1: " + lineInFile1);
					System.out.println("2: " + lineInFile2);
				}
			}
			throw new RuntimeException("Files differ!");
		}
		OpenDaApplication.main(new String[] {
				(new File(testRunDataDir,"oscillatorEnkfOpenDaConfigRandomNoise.xml") ).getAbsolutePath()} );
		assertTrue(!testData.FilesAreIdentical(resultFile_1, resultFile_random));
	}

	public void testEnsrOscillMultiResultWriters() throws IOException {
    	String inputFile  = ( new File(testRunDataDir,"oscillEnsrMultiRes_1.xml") ).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[1];
    	args[0] = inputFile;
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(args);
    }

    public void testEnsrOscillGui() throws IOException {
    	String inputFile  = ( new File(testRunDataDir,"oscillEnsrMultiRes_1.xml") ).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[2];
    	args[0] = "-gui";
    	args[1] = inputFile;
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(args);
        try{Thread.sleep(4000);}catch(InterruptedException e){/*do nothing*/}
    }

    public void testEmptyGui() throws IOException {
    	String args[] = {};
        OpenDaApplication.main(args);
        try{Thread.sleep(4000);}catch(InterruptedException e){/*do nothing*/}
    }
}
