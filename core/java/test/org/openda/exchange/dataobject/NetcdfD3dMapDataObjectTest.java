package org.openda.exchange.dataobject;

import junit.framework.TestCase;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.exchange.dataobjects.NetcdfD3dMapDataObject;
import org.openda.interfaces.IDataObject;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

/**
 * Tests for D3D netcdf map and state file
 */
public class NetcdfD3dMapDataObjectTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(NetcdfD3dMapDataObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	public void testReadGrid() {
		IDataObject dataObject = new NetcdfD3dMapDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"trim-estuary.nc"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
//		assertEquals(200, exchangeItemIDs.length);
//		NetcdfGridTimeSeriesExchangeItem s1 = (NetcdfGridTimeSeriesExchangeItem)dataObject.getDataObjectExchangeItem("S1");
//		assertFalse(s1 == null);
//		int timeIndex = 0;
//		double[] itemValues = s1.getValuesAsDoublesForSingleTimeIndex(timeIndex);
//		assertEquals(2500, itemValues.length);
	}

}
