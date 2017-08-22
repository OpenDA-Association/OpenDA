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

public class DFlowFMMduInputFileTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMMduInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Timeseries");
	}

	public void testDflowfmParseExtInputFile() {

		File dataDir=testRunDataDir;

		DFlowFMMduInputFile mduOptions = new DFlowFMMduInputFile(dataDir, "estuary.mdu");
		
		String forcingFilePath = mduOptions.get("external forcing","ExtForceFile");
		assertEquals( "estuary.ext" , forcingFilePath);
		
		Double refDate = mduOptions.getReferenceDateInMjd();
		assertEquals( "48257.0", refDate.toString() );
		
		Double factor = mduOptions.getTimeToMjdFactor();
		assertEquals( 24.0 * 60.0, 1.0/factor);
	}
}
