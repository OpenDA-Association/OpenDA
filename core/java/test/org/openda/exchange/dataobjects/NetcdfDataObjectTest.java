package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.NetcdfDataObjectDelft3DTrih;
import org.openda.interfaces.IExchangeItem;
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

	public void testReadTimeSeriesDelft3d() {
		NetcdfDataObject dataObject = new NetcdfDataObjectDelft3DTrih();
		dataObject.initialize(this.testRunDataDir, new String[]{"trih-estuary.nc", "true"});
		String[] itemIds = dataObject.getExchangeItemIDs();
		assertEquals(21, itemIds.length);
		assertEquals("station03.ZWL", itemIds[5]);
		assertEquals("station03.DPS", itemIds[20]);
		IExchangeItem stat3_zwl = dataObject.getDataObjectExchangeItem("station03.ZWL");
		double[] stat3_zwl_values = stat3_zwl.getValuesAsDoubles();
		assertEquals(193, stat3_zwl_values.length);
		assertEquals(0.963498d, stat3_zwl_values[33], 1e-7);
		assertEquals(-0.10826d, stat3_zwl_values[192], 1e-7);
	}
}
