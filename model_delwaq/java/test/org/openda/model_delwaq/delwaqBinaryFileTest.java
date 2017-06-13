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
package org.openda.model_delwaq;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by sumihar on 1/8/15.
 */
public class delwaqBinaryFileTest extends TestCase {
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(delwaqBinaryFileTest.class, "model_delwaq");
	}

	public void testDelwaqBinaryFile(){
		delwaqBinaryFile hisFile = new delwaqBinaryFile();
		String fileName = "ch_a.his";
		hisFile.initialize(testData.getTestRunDataDir(),new String[]{fileName});

		// Test getIds:
		String[] exchangeIDs = hisFile.getExchangeItemIDs();
		assertEquals("ExchangeID 1: ", "IM1_cell8", exchangeIDs[0]);
		assertEquals("ExchangeID 10: ", "IM2_cell59", exchangeIDs[9]);

		// Test getValues:
		IExchangeItem exchangeItem = hisFile.getDataObjectExchangeItem("IM1_cell8");
		double[] values = (double[]) exchangeItem.getValues();
		assertEquals("IM1_cell8[93]",0.2529326379299164,values[93]);
		assertEquals("IM1_cell8[100]",0.25410667061805725,values[100]);
		assertEquals("IM1_cell8[558]",0.25555458664894104,values[558]);

		exchangeItem = hisFile.getDataObjectExchangeItem("IM1_cell22");
		values = (double[]) exchangeItem.getValues();
		assertEquals("IM1_cell22[93]",0.5860220789909363,values[93]);
		assertEquals("IM1_cell22[100]",0.5875983834266663,values[100]);
		assertEquals("IM1_cell22[558]",0.5895381569862366,values[558]);

		exchangeItem = hisFile.getDataObjectExchangeItem("SS_cell79");
		values = (double[]) exchangeItem.getValues();
		assertEquals("SS_cell79[93]",17.716453552246094,values[93]);
		assertEquals("SS_cell79[100]",17.716459274291992,values[100]);
		assertEquals("SS_cell79[558]",17.716463088989258,values[558]);

		exchangeItem = hisFile.getDataObjectExchangeItem("SURF_cell39");
		values = (double[]) exchangeItem.getValues();
		assertEquals("SURF_cell39[93]",5000.0,values[93]);
		assertEquals("SURF_cell39[100]",5000.0,values[100]);
		assertEquals("SURF_cell39[558]",5000.0,values[558]);

		exchangeItem = hisFile.getDataObjectExchangeItem("LocalDepth_cell59");
		values = (double[]) exchangeItem.getValues();
		assertEquals("LocalDepth_cell59[93]",1.1876778602600098,values[93]);
		assertEquals("LocalDepth_cell59[100]",1.1880366802215576,values[100]);
		assertEquals("LocalDepth_cell59[558]",1.1884762048721313,values[558]);

		exchangeItem = hisFile.getDataObjectExchangeItem("IM2_cell71");
		values = (double[]) exchangeItem.getValues();
		assertEquals("IM2_cell71[93]",0.0,values[93]);
		assertEquals("IM2_cell71[100]",0.0,values[100]);
		assertEquals("IM2_cell71[558]",0.0,values[558]);

	}
}
