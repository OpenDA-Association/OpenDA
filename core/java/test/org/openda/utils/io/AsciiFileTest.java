package org.openda.utils.io;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class AsciiFileTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(AsciiFileTest.class, "core");
	}

	public void testAsciiFile(){
		File outFile1 = new File(testData.getTestRunDataDir(),"lines_out1.txt"); 
		String original[] = new String[]{"line 1","line 2","line 3"};
		
		// write to file
		AsciiFileUtils.writeLines(outFile1, Arrays.asList(original));

		File refFile = new File(testData.getTestRunDataDir(),"lines_ref.txt"); 
		assertTrue(testData.FilesAreIdentical(outFile1,refFile));


		// read from file
		List<String> lines = AsciiFileUtils.readLines(outFile1);
		
		int i=0;
		for(String line : lines){
			assertEquals("compare contents line by line",original[i], line);
			i++;
		}
		
		File outFile2 = new File(testData.getTestRunDataDir(),"lines_out2.txt"); 

		// write to file
		AsciiFileUtils.writeLines(outFile2, lines);

		assertTrue(testData.FilesAreIdentical(outFile2,refFile));

	}
	
}
