package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

/**
 * Created by hummel on 14-Apr-16.
 */
public class D3dNetcdfHisDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(D3dNetcdfHisDataObjectTest.class, "public", "model_delft3d");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}


	public void testReadTimeSeriesDelft3d() {
		IDataObject dataObject = new D3dNetcdfHisDataObject();
		dataObject.initialize(new File(this.testRunDataDir, "netcdf"), new String[]{"trih-estuary.nc", "true"});
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