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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by Theo on 13.04.2016.
 */
public class NetcdfD3dHisExchangeItemTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfD3dHisExchangeItemTest.class,"public","model_delft3d");
	}

	public void testGetValuesAsDoubles() throws Exception {

		NetcdfD3dHisDataObject netcdfFile = new NetcdfD3dHisDataObject();
		netcdfFile.initialize(testData.getTestRunDataDir(), new String[] {"trih-cadagno_netcdf.nc"});
		String[] exchangeItemIDs =  netcdfFile.getExchangeItemIDs();

		assertEquals("#exchange items", 100, exchangeItemIDs.length);

		IExchangeItem exchangeItem = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[27]);

		double[] dataTimeSeries = exchangeItem.getValuesAsDoubles();

		assertEquals("#time steps", 17, dataTimeSeries.length);

	}

}
