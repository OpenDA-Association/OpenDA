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
package org.openda.exchange.iotools;

import junit.framework.TestCase;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.TestDataObject;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.io.IOException;

public class DataObjectDiffTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DataObjectDiffTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testDiffNetcdfObject(){
		//read input.
//		NetcdfDataObject referenceDataObject = new NetcdfDataObject();
//        referenceDataObject.initialize(this.testRunDataDir, new String[]{"diff/model_output_ref.nc", "false", "false"});
//        NetcdfDataObject testDataObject = new NetcdfDataObject();
//        testDataObject.initialize(this.testRunDataDir, new String[]{"diff/model_output_test.nc", "false", "false"});

		//test data.
		DataObjectDiff differ = new DataObjectDiff();
        File referenceFile = new File(testRunDataDir, "diff/model_output_ref.nc");
        File testFile = new File(testRunDataDir, "diff/model_output_test.nc");

        differ.initialize(this.testRunDataDir, new String[]{referenceFile.getAbsolutePath(), "-a", "false false",  testFile.getAbsolutePath(), "-a", "false false" });
        boolean result = differ.compare();
        assertFalse(result);
	}

}
