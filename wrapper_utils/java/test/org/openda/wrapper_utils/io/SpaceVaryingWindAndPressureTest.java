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

package org.openda.wrapper_utils.io;
import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import org.junit.Test;
import org.junit.Rule;
import org.junit.rules.ExpectedException;

import java.io.File;
import java.io.IOException;

public class SpaceVaryingWindAndPressureTest extends TestCase{

	private OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(SpaceVaryingWindAndPressureTest.class, "public", "wrapper_utils");
	}

	@Rule
	public ExpectedException thrown= ExpectedException.none();

	@Test
	public void testFileNotFoundException() {
		thrown.expect(RuntimeException.class);
		thrown.expectMessage("Input file does not exist: ");
		File nonExistingFile = new File("c:/", "idontexist.txt");
		SpaceVaryingWindAndPressureFile svwpFile = new SpaceVaryingWindAndPressureFile(nonExistingFile);
	}

	public void testAmuFile() {
		File amuFile = new File(testData.getTestRunDataDir(), "windx.amu");
		SpaceVaryingWindAndPressureFile svwpFile = new SpaceVaryingWindAndPressureFile(amuFile);
		assertEquals(svwpFile.getN_cols(), 26);
	}
}
