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
import org.openda.exchange.NetcdfScalarTimeSeriesExchangeItem;
import org.openda.exchange.PointGeometryInfo;
import org.openda.exchange.timeseries.TimeUtils;
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

	public void testConfigurableStationVariable() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"structures.nc", "true", "false", "stationIdVarName=structure_id", "stationDimensionVarName=id"});
		String[] itemIds = dataObject.getExchangeItemIDs();
		assertEquals(260, itemIds.length);
		assertEquals("03_Wehr_Kobl~~2.crest_level", itemIds[5]);
		assertEquals("06_Wehr_Ahl~~2.crest_level", itemIds[20]);
		assertEquals("Mai_32_Wehr_Grie~~2.water_level", itemIds[259]);
		IExchangeItem crestLevelEI = dataObject.getDataObjectExchangeItem("06_Wehr_Ahl~~2.crest_level");
		double[] crestLevels = crestLevelEI.getValuesAsDoubles();
		assertEquals(601, crestLevels.length);
		assertEquals(69.4000015d, crestLevels[33], 1e-7);
		assertEquals(69.4000015d, crestLevels[192], 1e-7);
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

	public void testRoundingTimesToWholeSeconds() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		File testRunDataDir = new File(this.testRunDataDir, "roundingTimesToWholeSeconds");
		dataObject.initialize(testRunDataDir, new String[]{"synthetic_wl_obs.nc", "true", "false"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(12, exchangeItemIDs.length);
		IExchangeItem dataObjectExchangeItem = dataObject.getDataObjectExchangeItem("VLISSGN.waterlevel");
		assertNotNull(dataObjectExchangeItem);
		double[] times = dataObjectExchangeItem.getTimes();
		assertEquals(633, times.length);
		assertEquals(60310.041666666664, times[1]);
		assertEquals("202401010100", TimeUtils.mjdToString(times[1]));
		assertEquals(60336.333333333336, times[632]);
		assertEquals("202401270800", TimeUtils.mjdToString(times[632]));
	}
}
