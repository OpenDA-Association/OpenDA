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
package org.openda.exchange.dataobjects;
import junit.framework.TestCase;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.Arrays;

/**
 * Tests for NetcdfDataObject
 */
public class NetcdfDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(NetcdfDataObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	public void testReadTimeSeriesEnsemble() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"netcdf_timeseries_ensemble.nc", "true", "false"});
		int[] ensembleIndices = dataObject.getEnsembleMemberIndices();
		assertEquals(3, ensembleIndices.length);
		String[] ensembleIds = dataObject.getEnsembleExchangeItemIds();
		assertEquals(4, ensembleIds.length);
		IExchangeItem item = dataObject.getDataObjectExchangeItem("27.waterlevel", 1);
		assertFalse(item == null);
		double[] itemValues = item.getValuesAsDoubles();
		assertEquals(3, itemValues.length);
	}

	public void testObserver1Location() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"dcsmv5_airpressure_noise_dim-46.nc", "true", "false"});
		String[] ensembleExchangeItemIds = dataObject.getExchangeItemIDs();
		System.out.println(ensembleExchangeItemIds);
		int[] ensembleIndices = dataObject.getEnsembleMemberIndices();
		assertEquals(3, ensembleIndices.length);
		String[] ensembleIds = dataObject.getEnsembleExchangeItemIds();
		assertEquals(4, ensembleIds.length);
		IExchangeItem item = dataObject.getDataObjectExchangeItem("27.waterlevel", 1);
		assertFalse(item == null);
		double[] itemValues = item.getValuesAsDoubles();
		assertEquals(3, itemValues.length);
	}

	public void testReadGridEnsemble() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"netcdf_grid_with_ensemble.nc", "true", "false"});
		int[] ensembleIndices = dataObject.getEnsembleMemberIndices();
		assertEquals(3, ensembleIndices.length);
		String[] ensembleIds = dataObject.getEnsembleExchangeItemIds();
		assertEquals(2, ensembleIds.length);
		NetcdfGridTimeSeriesExchangeItem item = (NetcdfGridTimeSeriesExchangeItem)dataObject.getDataObjectExchangeItem("pressure", 1);
		assertFalse(item == null);
		double[] itemValues = item.getValuesAsDoublesForSingleTimeIndex(0);
		assertEquals(2500, itemValues.length);
	}

	public void testPressureNoiseDFlowFM() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"dcsmv5_airpressure_noise.nc", "true", "false"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertTrue(exchangeItemIDs.length > 0);
		NetcdfGridTimeSeriesExchangeItem airPressure = (NetcdfGridTimeSeriesExchangeItem) dataObject.getDataObjectExchangeItem("air_pressure");
		double[] valuesAsDoubles = airPressure.getValuesAsDoublesForSingleTimeIndex(0);
		Arrays.sort(valuesAsDoubles);
		assertEquals(-10.0387, valuesAsDoubles[0], 0.0001);
		assertEquals(-10.0387, valuesAsDoubles[valuesAsDoubles.length - 1], 0.0001);
		double[] valuesAsDoubles1 = airPressure.getValuesAsDoublesForSingleTimeIndex(1);
		Arrays.sort(valuesAsDoubles1);
		assertEquals(-10.0387, valuesAsDoubles1[0], 0.0001);
		assertEquals(-10.0387, valuesAsDoubles1[valuesAsDoubles.length - 1], 0.0001);
		double[] axpyValues = new double[valuesAsDoubles.length];
		Arrays.fill(axpyValues, 1);
		airPressure.axpyOnValuesForSingleTimeIndex(1, 1.1, axpyValues);
		double[] valuesAsDoubles1Axpy = airPressure.getValuesAsDoublesForSingleTimeIndex(1);
		Arrays.sort(valuesAsDoubles1Axpy);
		assertEquals(-10.0387, valuesAsDoubles1Axpy[0], 0.0001);
		assertEquals(-10.0387, valuesAsDoubles1Axpy[valuesAsDoubles.length - 1], 0.0001);
	}

	public void testRequiredExchangeItemIds() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"requiredExchangeItemIds.nc", "true", "false", "requiredExchangeItemId=24.waterlevel", "requiredExchangeItemId=26.waterlevel", "requiredExchangeItemId=27.waterlevel"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(3, exchangeItemIDs.length);
		String[] expectedExchangeItemIds = {"24.waterlevel", "26.waterlevel", "27.waterlevel"};
		for (String expectedExchangeItemId : expectedExchangeItemIds) {
			assertNotNull(dataObject.getDataObjectExchangeItem(expectedExchangeItemId));
		}
	}
}
