/* OpenDA v2.4 
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

import org.openda.interfaces.IDataObject;
import org.openda.blackbox.config.BBUtils;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;

/**
 * Tests reading from and writing to the D-Flow FM restart file dflowfm_map.nc.
 */
public class DFlowFMRestartTest extends TestCase {

	OpenDaTestSupport testData = null;
    private File testRunDataRestartFileDir;
	private File testCopyDir;


	protected void setUp() throws IOException {
	    testData = new OpenDaTestSupport(DFlowFMRestartTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "Restartfile");
		testCopyDir = new File(testRunDataRestartFileDir,"copy");
	}


	public void testReadInput() {

		IDataObject RestartFile = new DFlowFMRestartFileWrapper();
		String[] args = new String[]{"dflowfm_map.nc"};

		RestartFile.initialize(testRunDataRestartFileDir, args);

		String[] exchangeItemIDs= RestartFile.getExchangeItemIDs();

		// loop over all exchangeItems and check some for Quantity and Unit
		for (String id : exchangeItemIDs) {
			if (id.matches("time")) {
			    DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assert(ex.getUnitId().startsWith("seconds since"));
			}
			if (id.matches("timestep")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()",ex.getQuantityId(),"timestep");
				assert(ex.getUnitId().matches("seconds"));
			}

			if (id.matches("s1")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_surface_height");
                assert(ex.getUnitId().matches("m"));
			}

			if (id.matches("s0")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_surface_height");
				assert(ex.getUnitId().matches("m"));
			}

			if (id.matches("taus")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"taucurrent");
				assert(ex.getUnitId().matches("N/m2"));
			}

			if (id.matches("unorm")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_water_speed");
				assert(ex.getUnitId().matches("m s-1"));

			}

			if (id.matches("u0")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_water_speed_old");
				assert(ex.getUnitId().matches("m s-1"));
			}

		}
	}

	public void testWriteInput() {

		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "Restartfile");
		IDataObject RestartFile = new DFlowFMRestartFileWrapper();

		// Copy the original file to a "copy" directory to prevent overwriting
		File original = new File(testRunDataRestartFileDir,"dflowfm_map.nc");
		File copy = new File(testCopyDir,"dflowfm_map.nc");
		try {
			BBUtils.copyFile(original, copy);
			} catch (IOException e) {
				throw new RuntimeException("Could not copy files");
		}
		String[] args = new String[]{"dflowfm_map.nc","1200"};
	    RestartFile.initialize(testRunDataRestartFileDir, args);

		String[] exchangeItemIDs= RestartFile.getExchangeItemIDs();

		//change a value
		for (String id : exchangeItemIDs) {
//			System.out.println("id = "+id);
			if (id.matches("s1")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				double values[] = ex.getValuesAsDoubles();
				for (int i=0; i < values.length ; i++) {
					values[i] = 100.0;
				}
				RestartFile.getDataObjectExchangeItem(id).setValuesAsDoubles(values);
			}
		}
		// write file
		RestartFile.finish();

		// Reread file and compare with copy
		RestartFile.initialize(testRunDataRestartFileDir, args);

		IDataObject RestartCopy = new DFlowFMRestartFileWrapper();
		RestartCopy.initialize(testCopyDir, args);

		double orig[] = RestartFile.getDataObjectExchangeItem("s1").getValuesAsDoubles();
		double[] newval = RestartCopy.getDataObjectExchangeItem("s1").getValuesAsDoubles();
		assertTrue(Math.abs(orig[0]-newval[0])>1.e-20);
		RestartFile.finish();
		RestartCopy.finish();
	}
}
