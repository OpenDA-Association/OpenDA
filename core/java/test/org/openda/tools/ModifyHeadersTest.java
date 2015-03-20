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
package org.openda.tools;
import java.io.File;
import java.io.IOException;

import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class ModifyHeadersTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(ModifyHeadersTest.class,"core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testModifyOneFile() {
		System.out.println("==============================================================================");
		System.out.println("Modify header of individual java files");
		System.out.println("==============================================================================");

		HeaderModifier mod = new HeaderModifier("jav");

		File source1 = new File(testRunDataDir,"GroupObservationDesrciptions.jav");
		File source1_mod = new File(testRunDataDir,"GroupObservationDesrciptions.jav_new");
		File source1_ref = new File(testRunDataDir,"GroupObservationDesrciptions.jav_ref");
		mod.modifyOneHeader(source1,source1_mod);
		boolean filesEqual = testData.FilesAreIdentical(source1_mod, source1_ref);
		assertTrue("modified IoObjectInterface.jav equals reference", filesEqual);
		
		source1 = new File(testRunDataDir,"IoObjectInterface.jav");
		source1_mod = new File(testRunDataDir,"IoObjectInterface.jav_new");
		source1_ref = new File(testRunDataDir,"IoObjectInterface.jav_ref");
		mod.modifyOneHeader(source1,source1_mod);
		filesEqual = testData.FilesAreIdentical(source1_mod, source1_ref);
		assertTrue("modified IoObjectInterface.jav equals reference", filesEqual);

		source1 = new File(testRunDataDir,"NetcdfResultWriter.jav");
		source1_mod = new File(testRunDataDir,"NetcdfResultWriter.jav_new");
		source1_ref = new File(testRunDataDir,"NetcdfResultWriter.jav_ref");
		mod.modifyOneHeader(source1,source1_mod);
		filesEqual = testData.FilesAreIdentical(source1_mod, source1_ref);
		assertTrue("modified NetcdfResultWriter.jav equals reference", filesEqual);

		source1 = new File(testRunDataDir,"NoosTimeSeriesFormatter.jav");
		source1_mod = new File(testRunDataDir,"NoosTimeSeriesFormatter.jav_new");
		source1_ref = new File(testRunDataDir,"NoosTimeSeriesFormatter.jav_ref");
		mod.modifyOneHeader(source1,source1_mod);
		filesEqual = testData.FilesAreIdentical(source1_mod, source1_ref);
		assertTrue("modified NoosTimeSeriesFormatter.jav equals reference", filesEqual);

		source1 = new File(testRunDataDir,"ThisFileIsAlreadyModified.jav");
		source1_mod = new File(testRunDataDir,"ThisFileIsAlreadyModified.jav_new");
		mod.modifyOneHeader(source1,source1_mod);
		boolean modExists = source1_mod.exists();
		assertTrue("modified ThisFileIsAlreadyModified.jav should not exist", !modExists);
}

	public void testModifyAllFilesInTree() {
		System.out.println("==============================================================================");
		System.out.println("Modify header of all java files in a tree (directory and subdirs");
		System.out.println("==============================================================================");

		HeaderModifier mod = new HeaderModifier("jav");

		File tree = testRunDataDir;
		mod.modifyAllHeaders(tree,false); // do not replace files but create .jav.mod files instead
		File source1_mod = new File(testRunDataDir,"IoObjectInterface.jav.mod");
		boolean textFound = testData.FileContains(source1_mod, "GNU Lesser General Public License");
		assertTrue("looking for text in modified file", textFound);
		textFound = testData.FileContains(source1_mod, "/* MOD_V2.0");
		assertTrue("looking for text in modified file", textFound);

		mod.modifyAllHeaders(tree,true); // now replace originals
		source1_mod = new File(testRunDataDir,"IoObjectInterface.jav");
		textFound = testData.FileContains(source1_mod, "GNU Lesser General Public License");
		assertTrue("looking for text in modified file", textFound);
		textFound = testData.FileContains(source1_mod, "/* MOD_V2.0");
		assertTrue("looking for text in modified file", textFound);
		
		File source1 = new File(testRunDataDir,"ThisFileIsAlreadyModified.jav");
		source1_mod = new File(testRunDataDir,"ThisFileIsAlreadyModified.jav_new");
		mod.modifyOneHeader(source1,source1_mod);
		boolean modExists = source1_mod.exists();
		assertTrue("modified ThisFileIsAlreadyModified.jav should not exist", !modExists);
	}

	public void testModifyAllWithMain() {
		System.out.println("==============================================================================");
		System.out.println("Modify headers with main()");
		System.out.println("==============================================================================");

		File tree = testRunDataDir;
		String args[] = new String[2];
		args[0]= tree.getAbsolutePath();
		args[1]= "jav";
		HeaderModifier.main(args);

		File source1_mod = new File(testRunDataDir,"IoObjectInterface.jav");
		boolean textFound = testData.FileContains(source1_mod, "GNU Lesser General Public License");
		assertTrue("looking for text in modified file", textFound);
		textFound = testData.FileContains(source1_mod, "/* MOD_V2.0");
		assertTrue("looking for text in modified file", textFound);
	}
}
