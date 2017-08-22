/* OpenDA v2.4.1 
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
