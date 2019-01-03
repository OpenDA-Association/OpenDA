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
 * Created by Theo on 13.04.2016.
 */
public class NetcdfD3dHisExchangeItemTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfD3dHisExchangeItemTest.class,"model_delft3d");
	}

	public void testGetValuesAsDoubles() throws Exception {

		NetcdfD3dHisDataObject netcdfFile = new NetcdfD3dHisDataObject();
		netcdfFile.initialize(testData.getTestRunDataDir(), new String[] {"trih-cadagno_netcdf.nc"});
		String[] exchangeItemIDs =  netcdfFile.getExchangeItemIDs();

		assertEquals("#exchange items", 600, exchangeItemIDs.length);

		IExchangeItem exchangeItemGRO = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[350]);
		IExchangeItem exchangeItemVel1 = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[15]);
		IExchangeItem exchangeItemVel2 = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[115]);
		IExchangeItem exchangeItemEN = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[415]);

		double[] dataTimeSeriesGRO = exchangeItemGRO.getValuesAsDoubles();
		double[] dataTimeSeriesVel1 = exchangeItemVel1.getValuesAsDoubles();
		double[] dataTimeSeriesVel2 = exchangeItemVel2.getValuesAsDoubles();
		double[] dataTimeSeriesEN = exchangeItemEN.getValuesAsDoubles();

		assertEquals("#time steps", 17, dataTimeSeriesGRO.length);
		assertEquals("#squared velocities vs energy", 1000*0.5*(dataTimeSeriesVel1[2]*dataTimeSeriesVel1[2] + dataTimeSeriesVel2[2]*dataTimeSeriesVel2[2]), dataTimeSeriesEN[2], 1.E-10);

	}

}
