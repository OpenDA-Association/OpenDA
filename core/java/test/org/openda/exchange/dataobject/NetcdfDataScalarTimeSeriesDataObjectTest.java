package org.openda.exchange.dataobject;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

/**
 * Tests for NetcdfDataObject NetcdfDataScalarTimeSeriesDataObject
 */
public class NetcdfDataScalarTimeSeriesDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(NetcdfDataScalarTimeSeriesDataObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	public void testReadTimeSeriesSobek3Structures() {
		IDataObject dataObject = new org.openda.exchange.dataobjects.NetcdfDataScalarTimeSeriesDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"structures.nc", "structure_id"});
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
}