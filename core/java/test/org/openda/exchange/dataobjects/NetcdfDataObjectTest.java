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
import org.openda.blackbox.wrapper.BBGridExchangeItem;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGridTimeSeriesExchangeItem;
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
			assertNotNull(dataObject.getDataObjectExchangeItem(expectedExchangeItemId));
		}
	}

	public void testWFlowJuliaState() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		File testRunDataDir = new File(this.testRunDataDir, "WFlowJuliaState");
		dataObject.initialize(testRunDataDir, new String[]{"output_scalar.nc", "true", "false"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(12, exchangeItemIDs.length);
	}

	public void testNetcdfGridBBExchangeItem() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		File testRunDataDir = new File(this.testRunDataDir, "netcdfGrid");
		dataObject.initialize(testRunDataDir, new String[]{"netcdfGrid.nc", "true", "false"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(1, exchangeItemIDs.length);
		IExchangeItem precipitationEI = dataObject.getDataObjectExchangeItem("precipitation");
		assertTrue(precipitationEI instanceof IGridTimeSeriesExchangeItem);
		BBGridExchangeItem bbGridEI = new BBGridExchangeItem("bbGridEI", null, (IGridTimeSeriesExchangeItem) precipitationEI, null, null);
		double[] valuesTime0 = bbGridEI.getValuesAsDoublesForSingleTimeIndex(0);
		int gridSize = 80 * 88;
		assertEquals(gridSize, valuesTime0.length);
		double[] valuesTime1 = bbGridEI.getValuesAsDoublesForSingleTimeIndex(1);
		assertEquals(gridSize, valuesTime1.length);
		double[] valuesTime2 = bbGridEI.getValuesAsDoublesForSingleTimeIndex(2);
		assertEquals(gridSize, valuesTime2.length);
		double[] valuesTime6 = bbGridEI.getValuesAsDoublesForSingleTimeIndex(6);
		assertEquals(gridSize, valuesTime6.length);
		double[] multiplicationFactors = new double[gridSize];
		Arrays.fill(multiplicationFactors, 1.1);
		bbGridEI.multiplyValuesForSingleTimeIndex(1, multiplicationFactors);
		double[] valuesTime1Multiplied = bbGridEI.getValuesAsDoublesForSingleTimeIndex(1);
		for (int i = 0; i < valuesTime1Multiplied.length; i++) {
			double valueMultiplied = valuesTime1Multiplied[i];
			assertEquals(1.1 * valuesTime1[i], valueMultiplied, 0.000001);
		}
		bbGridEI.axpyOnValuesForSingleTimeIndex(6, 2, multiplicationFactors);
		double[] valuesTime6Axpy = bbGridEI.getValuesAsDoublesForSingleTimeIndex(6);
		for (int i = 0; i < valuesTime6Axpy.length; i++) {
			assertEquals(valuesTime6[i] + 2 * 1.1, valuesTime6Axpy[i], 0.000001);
		}
	}

	/*public void testWFlowJuliaState2() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		File testRunDataDir = new File(this.testRunDataDir, "WFlowJuliaState");
		dataObject.initialize(testRunDataDir, new String[]{"outstates.nc", "true", "false", "layerDimensionName=layer"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(18, exchangeItemIDs.length);
		IGridTimeSeriesExchangeItem layer0 = (IGridTimeSeriesExchangeItem) dataObject.getDataObjectExchangeItem("ustorelayerdepth.layer0");
		assertNotNull(layer0);
		double[] layer0Values = layer0.getValuesAsDoublesForSingleTimeIndex(0);
		double[] axpyValues = new double[layer0Values.length];
		Arrays.fill(axpyValues, 1);
		layer0.axpyOnValuesForSingleTimeIndex(0, 1, axpyValues);
		IGridTimeSeriesExchangeItem layer1 = (IGridTimeSeriesExchangeItem) dataObject.getDataObjectExchangeItem("ustorelayerdepth.layer1");
		assertNotNull(layer1);
		double[] layer1Values = layer1.getValuesAsDoublesForSingleTimeIndex(0);
		layer1.axpyOnValuesForSingleTimeIndex(0, 1, axpyValues);

		dataObject.finish();

		NetcdfDataObject dataObject2 = new NetcdfDataObject();
		dataObject2.initialize(testRunDataDir, new String[]{"outstates.nc", "true", "false", "layerDimensionName=layer"});
		String[] exchangeItemIDs2 = dataObject2.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs2);
		assertEquals(18, exchangeItemIDs2.length);
		IGridTimeSeriesExchangeItem layer02 = (IGridTimeSeriesExchangeItem) dataObject2.getDataObjectExchangeItem("ustorelayerdepth.layer0");
		assertNotNull(layer02);
		double[] layer0Values2 = layer02.getValuesAsDoublesForSingleTimeIndex(0);
		IGridTimeSeriesExchangeItem layer12 = (IGridTimeSeriesExchangeItem) dataObject.getDataObjectExchangeItem("ustorelayerdepth.layer1");
		assertNotNull(layer12);
		double[] layer1Values2 = layer12.getValuesAsDoublesForSingleTimeIndex(0);
		for (int n = 0; n < layer0Values2.length; n++) {
			assertEquals(layer0Values2[n], layer0Values[n] + 1 * axpyValues[n]);
			assertEquals(layer1Values2[n], layer1Values[n] + 1 * axpyValues[n]);
		}
	}*/
}
