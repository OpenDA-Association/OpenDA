package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
import org.openda.exchange.RegularGridGeometryInfo;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for for an ESRI ASCII Grid DataObjects
 */
public class EsriAsciiGridDataObjectTest extends TestCase {

	private OpenDaTestSupport testData = null;
	File testRunDataDir = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(EsriAsciiGridDataObjectTest.class, "public", "core");
		testRunDataDir = new File(testData.getTestRunDataDir(), "esriAsciiGrid");
	}

	public void testDepthFile() {
		String[] arguments = {"depth.asc"};
		IExchangeItem exchangeItem = createAndTestDepthDataObject(arguments);
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertEquals(null, timeInfo);
	}

	public void testDepthFileWithTimeInfo() {
		String[] arguments = {"depth.asc", ""};
		IExchangeItem exchangeItem = createAndTestDepthDataObject(arguments);
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertEquals(null, timeInfo);
	}

	private IExchangeItem createAndTestDepthDataObject(String[] arguments) {
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertEquals("depth", exchangeItemIDs[0]);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("depth");
		double[] values = exchangeItem.getValuesAsDoubles();
		assertEquals(10*4, values.length);
		IArrayGeometryInfo geometryInfo = (IArrayGeometryInfo) exchangeItem.getGeometryInfo();
		IArray xCoordinates = geometryInfo.getLongitudeArray();
		IArray yCoordinates = geometryInfo.getLatitudeArray();
		assertEquals(6.5d, xCoordinates.getValueAsDouble(6));
		assertEquals(13.5d, yCoordinates.getValueAsDouble(2));
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
		assertEquals(17d, valuesAsDoubles[25]);
		return exchangeItem;
	}

	public void testParamAFile() {
		String[] arguments = {"depth.asc"};
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
	}

	public void testGetExchangeItemIDs() {
	}

	public void testGetExchangeItemIDs1() {
	}

	public void testGetDataObjectExchangeItem() {
	}

	public void testFinish1() {
	}

}
