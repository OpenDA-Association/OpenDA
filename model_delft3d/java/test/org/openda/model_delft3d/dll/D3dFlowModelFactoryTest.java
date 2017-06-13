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
package org.openda.model_delft3d.dll;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Delft3D flow DLL
 */
public class D3dFlowModelFactoryTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dFlowModelFactoryTest.class, "public", "model_delft3d");
	}

	public void testDummy(){
		//nothing here on purpose
	}

//	public void testTwoTimesD3dFlowModelFactoryOnFilterTest2DModel() {
//		tstD3dFlowModelFactoryOnFilterTest2DModel();
//		tstD3dFlowModelFactoryOnFilterTest2DModel();
//	}
//
	public void testD3dFlowModelFactoryOnFilterTest2DModel() {

		//TODO linux
		//currently only dll file for windows is available (not for linux), so only run this on windows.
		if (!BBUtils.RUNNING_ON_WINDOWS) {
			return;
		}

		D3dFlowModelFactory d3dFlowModelFactory = new D3dFlowModelFactory();
		File d3dModelFactoryConfigDir = testData.getTestRunDataDir();
		d3dFlowModelFactory.initialize(d3dModelFactoryConfigDir,
				new String[]{"ft2d_d3dModelFactoryConfig_win32_ifort.xml"});

		int instanceCount = 2;
		IModelInstance[] modelInstances = new IModelInstance[instanceCount];

		for (int i = 0; i < instanceCount ; i++) {
			modelInstances[i] = d3dFlowModelFactory.getInstance(new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);
		}

		for (int i = 0; i < instanceCount ; i++) {
			modelInstances[i].getCurrentTime();

			boolean hBnd2Found = false;
			boolean hBnd2AFound = false;
			String[] EiIds = modelInstances[i].getExchangeItemIDs();
			assertEquals("#Exchange items:",8, EiIds.length);
			for (String id : EiIds) {
				if (id.equals("H_bnd2")) {
					IPrevExchangeItem hBnd2 = modelInstances[i].getExchangeItem(id);
					assertEquals("hBnd2.getRole:", IPrevExchangeItem.Role.Input, hBnd2.getRole());
					hBnd2Found = true;
				} else if (id.equals("H_bnd2_A.waterlevel")) {
					IPrevExchangeItem hBnd2A = modelInstances[i].getExchangeItem(id);
					assertEquals("hBnd2A.getRole:", IPrevExchangeItem.Role.Output, hBnd2A.getRole());
					hBnd2AFound = true;
				}
			}
			assertTrue("hBnd2Found", hBnd2Found);
			assertTrue("hBnd2AFound", hBnd2AFound);
		}

		for (int i = 0; i < instanceCount; i++) {
			modelInstances[i].finish();
		}
		d3dFlowModelFactory.finish();

	}
}
