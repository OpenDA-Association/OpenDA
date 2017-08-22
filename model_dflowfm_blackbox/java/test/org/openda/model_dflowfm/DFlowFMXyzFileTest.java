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
package org.openda.model_dflowfm;

import junit.framework.TestCase;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.model_dflowfm.DflowfmFrictionCoefficientFile;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 */
public class DFlowFMXyzFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMXyzFile.class, "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "Forcingfile");
	}

	public void testExistingfile() {

		// First write a test file
		File dataDir = testRunDataDir;

		DFlowFMXyzFile frictionCoefficientFile = new DFlowFMXyzFile();
		String arg[] = new String[3];
		arg[0] = "frcfact.xyz";
		arg[1] = "idsFromValuesInFile";
		arg[2] = "idPrefix=friction_";

		frictionCoefficientFile.initialize(testRunDataDir, arg);

		String ids[] = frictionCoefficientFile.getExchangeItemIDs();

		int n = ids.length;
		assertEquals(3, n);
		IExchangeItem[] items = new IExchangeItem[n];

		for (int i = 0; i < n; i++) {
			String id = ids[i];
			items[i] = frictionCoefficientFile.getDataObjectExchangeItem(id);
			assertEquals(items[i].getValuesType(), IExchangeItem.ValueType.IVectorType);
		}

		// change parameters
		IVector vector = (IVector) items[0].getValues();
		vector.setConstant(1.1);
		items[0].setValues(vector);

		vector = (IVector) items[1].getValues();
		vector.setConstant(0.9);
		items[1].setValues(vector);

		vector = (IVector) items[2].getValues();
		vector.setConstant(1.05);
		items[2].setValues(vector);

		// flush to file
		frictionCoefficientFile.finish();

		// check output
		File outputFile = new File(testRunDataDir, "frcfact.xyz");
		File referenceFile = new File(dataDir, "frcfact.xyz.ref");
		assertTrue(outputFile.exists());
		assertTrue(referenceFile.exists());
		assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));
	}
}
