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
import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

public class DFlowFMExtInputFileTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMExtInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Forcingfile");
	}

	public void testDflowfmParseExtInputFile() {

		File dataDir=testRunDataDir;
		File testfile=new File(dataDir,"simple_waal.ext");
		DFlowFMExtInputFile extOptions = new DFlowFMExtInputFile(testfile);
		String[] ref_quantities = new String[4];
		ref_quantities[0] = "dischargebnd";
		ref_quantities[1] = "waterlevelbnd";
		ref_quantities[2] = "frictioncoefficient";
		ref_quantities[3] = "frictioncoefficient";
		for (int i=0; i < extOptions.count(); i++ ) {
			assertEquals(extOptions.get("QUANTITY", i), ref_quantities[i]);
		}
	}
}
