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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by Theo on 20.04.2016.
 */
public class NetcdfD3dMapExchangeItemTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfD3dMapExchangeItemTest.class,"model_delft3d");
	}

	public void testGetValuesAsDoubles() throws Exception {

		NetcdfD3dMapDataObject netcdfFile = new NetcdfD3dMapDataObject();
		netcdfFile.initialize(testData.getTestRunDataDir(), new String[] {"trim-cadagno_netcdf.nc"});
		String[] exchangeItemIDs =  netcdfFile.getExchangeItemIDs();
		assertEquals("#exchange items", 4, exchangeItemIDs.length);

		IExchangeItem exchangeItem = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[3]);
		double[] times = exchangeItem.getTimes();
		// for memory reasons, on the last time step is stored in the exchange item
		assertEquals("#times", 1, times.length);
		double[] vEchangeItem = exchangeItem.getValuesAsDoubles();
		// V1, check for KMAXOUT_RESTR * M * NC values
		assertEquals("#values", 14850, vEchangeItem.length);
	}

}
