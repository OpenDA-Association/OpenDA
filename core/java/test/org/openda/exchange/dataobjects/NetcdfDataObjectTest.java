/*
* Copyright (c) 2023 OpenDA Association 
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
import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.exchange.NetcdfScalarTimeSeriesExchangeItem;
import org.openda.exchange.PointGeometryInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

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

	public void testRequiredExchangeItemIds() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"requiredExchangeItemIds.nc", "true", "false", "requiredExchangeItemId=24.waterlevel", "requiredExchangeItemId=26.waterlevel", "requiredExchangeItemId=27.waterlevel"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(3, exchangeItemIDs.length);
		String[] expectedExchangeItemIds = {"24.waterlevel", "26.waterlevel", "27.waterlevel"};
		for (String expectedExchangeItemId : expectedExchangeItemIds) {
			IExchangeItem dataObjectExchangeItem = dataObject.getDataObjectExchangeItem(expectedExchangeItemId);
			assertNotNull(dataObjectExchangeItem);
			assertTrue(dataObjectExchangeItem instanceof NetcdfScalarTimeSeriesExchangeItem);
			IGeometryInfo geometryInfo = dataObjectExchangeItem.getGeometryInfo();
			assertNotNull(geometryInfo);
			assertTrue(geometryInfo instanceof PointGeometryInfo);
		}
	}

	public void testMultipleLayers() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		File testRunDataDir = new File(this.testRunDataDir, "multipleLayers");
		dataObject.initialize(testRunDataDir, new String[]{"multipleLayers.nc", "true", "false", "layerDimensionName=laydim"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(442, exchangeItemIDs.length);
		IExchangeItem temperatureEI0 = dataObject.getDataObjectExchangeItem("station01.temperature.layer0");
		double[] valuesEI0 = temperatureEI0.getValuesAsDoubles();
		IExchangeItem temperatureEI13 = dataObject.getDataObjectExchangeItem("station01.temperature.layer13");
		double[] valuesEI13 = temperatureEI13.getValuesAsDoubles();
		IExchangeItem temperatureEI19 = dataObject.getDataObjectExchangeItem("station01.temperature.layer19");
		double[] valuesEI19 = temperatureEI19.getValuesAsDoubles();
		assertEquals(49, valuesEI0.length);
		assertEquals(49, valuesEI13.length);
		assertEquals(49, valuesEI19.length);
		assertTrue( valuesEI13[48] - valuesEI0[48] != 0.0d);
	}

	public void testNoSpecifyGridDimensionOrder() {
		NetcdfDataObject dataObject32 = new NetcdfDataObject();
		dataObject32.initialize(this.testRunDataDir, new String[]{"drymask_3_2.nc"});
		String[] exchangeItemIds = dataObject32.getExchangeItemIDs();
		assertEquals(1, exchangeItemIds.length);
		ArrayExchangeItem item = (ArrayExchangeItem) dataObject32.getDataObjectExchangeItem("DRYMASK");
		assertNotNull(item);
		double[] valuesAsDoubles32 = item.getValuesAsDoubles();
		assertEquals(6, valuesAsDoubles32.length);


		NetcdfDataObject dataObject23 = new NetcdfDataObject();
		dataObject23.initialize(this.testRunDataDir, new String[]{"drymask_2_3.nc"});
		String[] exchangeItemIds23 = dataObject23.getExchangeItemIDs();
		assertEquals(1, exchangeItemIds23.length);
		ArrayExchangeItem item23 = (ArrayExchangeItem) dataObject23.getDataObjectExchangeItem("DRYMASK");
		assertNotNull(item23);
		double[] valuesAsDoubles23 = item23.getValuesAsDoubles();
		assertEquals(6, valuesAsDoubles23.length);

		item23.multiplyValues(valuesAsDoubles32);

		double[] valuesAsDoublesMultiplied = item23.getValuesAsDoubles();
		assertEquals(1, valuesAsDoublesMultiplied[0], 0.01);
		assertEquals(8, valuesAsDoublesMultiplied[1], 0.01);
		assertEquals(6, valuesAsDoublesMultiplied[2], 0.01);
		assertEquals(20, valuesAsDoublesMultiplied[3], 0.01);
		assertEquals(15, valuesAsDoublesMultiplied[4], 0.01);
		assertEquals(36, valuesAsDoublesMultiplied[5], 0.01);
	}

	public void testSpecifyGridDimensionOrder() {
		NetcdfDataObject dataObject32 = new NetcdfDataObject();
		dataObject32.initialize(this.testRunDataDir, new String[]{"drymask_3_2.nc", "timeDimensionName=TIME", "gridDimensionName1=N", "gridDimensionName2=M"});
		String[] exchangeItemIds = dataObject32.getExchangeItemIDs();
		assertEquals(1, exchangeItemIds.length);
		ArrayExchangeItem item = (ArrayExchangeItem) dataObject32.getDataObjectExchangeItem("DRYMASK");
		assertNotNull(item);
		double[] valuesAsDoubles32 = item.getValuesAsDoubles();
		assertEquals(6, valuesAsDoubles32.length);


		NetcdfDataObject dataObject23 = new NetcdfDataObject();
		dataObject23.initialize(this.testRunDataDir, new String[]{"drymask_2_3.nc"});
		String[] exchangeItemIds23 = dataObject23.getExchangeItemIDs();
		assertEquals(1, exchangeItemIds23.length);
		ArrayExchangeItem item23 = (ArrayExchangeItem) dataObject23.getDataObjectExchangeItem("DRYMASK");
		assertNotNull(item23);
		double[] valuesAsDoubles23 = item23.getValuesAsDoubles();
		assertEquals(6, valuesAsDoubles23.length);

		item23.multiplyValues(valuesAsDoubles32);

		double[] valuesAsDoublesMultiplied = item23.getValuesAsDoubles();
		assertEquals(1, valuesAsDoublesMultiplied[0], 0.01);
		assertEquals(4, valuesAsDoublesMultiplied[1], 0.01);
		assertEquals(9, valuesAsDoublesMultiplied[2], 0.01);
		assertEquals(16, valuesAsDoublesMultiplied[3], 0.01);
		assertEquals(25, valuesAsDoublesMultiplied[4], 0.01);
		assertEquals(36, valuesAsDoublesMultiplied[5], 0.01);
	}

	public void testSpecifyGridDimensionOrderViceVersa() {
		NetcdfDataObject dataObject32 = new NetcdfDataObject();
		dataObject32.initialize(this.testRunDataDir, new String[]{"drymask_3_2.nc"});
		String[] exchangeItemIds = dataObject32.getExchangeItemIDs();
		assertEquals(1, exchangeItemIds.length);
		ArrayExchangeItem item = (ArrayExchangeItem) dataObject32.getDataObjectExchangeItem("DRYMASK");
		assertNotNull(item);
		double[] valuesAsDoubles32 = item.getValuesAsDoubles();
		assertEquals(6, valuesAsDoubles32.length);


		NetcdfDataObject dataObject23 = new NetcdfDataObject();
		dataObject23.initialize(this.testRunDataDir, new String[]{"drymask_2_3.nc", "timeDimensionName=TIME", "gridDimensionName1=N", "gridDimensionName2=M"});
		String[] exchangeItemIds23 = dataObject23.getExchangeItemIDs();
		assertEquals(1, exchangeItemIds23.length);
		ArrayExchangeItem item23 = (ArrayExchangeItem) dataObject23.getDataObjectExchangeItem("DRYMASK");
		assertNotNull(item23);
		double[] valuesAsDoubles23 = item23.getValuesAsDoubles();
		assertEquals(6, valuesAsDoubles23.length);

		item23.multiplyValues(valuesAsDoubles32);

		double[] valuesAsDoublesMultiplied = item23.getValuesAsDoubles();
		assertEquals(1, valuesAsDoublesMultiplied[0], 0.01);
		assertEquals(16, valuesAsDoublesMultiplied[1], 0.01);
		assertEquals(4, valuesAsDoublesMultiplied[2], 0.01);
		assertEquals(25, valuesAsDoublesMultiplied[3], 0.01);
		assertEquals(9, valuesAsDoublesMultiplied[4], 0.01);
		assertEquals(36, valuesAsDoublesMultiplied[5], 0.01);
	}
}
