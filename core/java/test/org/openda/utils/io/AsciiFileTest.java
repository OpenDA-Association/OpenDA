package org.openda.utils.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

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
		try {
			AsciiFileUtils.writeLines(outFile1, original);
		} catch (IOException e) {
			throw new RuntimeException("problems writing to file"+outFile1.getAbsolutePath()
					+e.getMessage());
		}

		File refFile = new File(testData.getTestRunDataDir(),"lines_ref.txt"); 
		assertTrue(testData.FilesAreIdentical(outFile1,refFile));


		// read from file
		ArrayList<String> lines;
		try {
			lines = AsciiFileUtils.readLines(outFile1);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("problems opening file"+outFile1.getAbsolutePath()
					+e.getMessage());
		} catch (IOException e) {
			throw new RuntimeException("problems reading from file"+outFile1.getAbsolutePath()
					+e.getMessage());
		}
		
		int i=0;
		for(String line : lines){
			assertEquals("compare contents line by line",original[i], line);
			i++;
		}
		
		File outFile2 = new File(testData.getTestRunDataDir(),"lines_out2.txt"); 

		// write to file
		try {
			AsciiFileUtils.writeLines(outFile2, lines);
		} catch (IOException e) {
			throw new RuntimeException("problems writing to file"+outFile2.getAbsolutePath()
					+e.getMessage());
		}

		assertTrue(testData.FilesAreIdentical(outFile2,refFile));

	}
	
}
