package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
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
		testData = new OpenDaTestSupport(EsriAsciiGridDataObjectTest.class, "core");
		testRunDataDir = new File(testData.getTestRunDataDir(), "esriAsciiGrid");
	}

	public void testDepthFile() {
		String[] arguments = {"depth.asc"};
		IExchangeItem exchangeItem = createAndTestDepthDataObject(arguments);
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertEquals(null, timeInfo);
	}

	public void testDepthFileWithTimeInfo() {
		String[] arguments = {"depth.asc", "timeStamp=20180206"};
		IExchangeItem exchangeItem = createAndTestDepthDataObject(arguments);
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(1, times.length);
		assertEquals(58155d, times[0]);
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
		String[] arguments = {"paramA_20020101.asc"};
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("paramA");
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(1, times.length);
		assertEquals(52275d, times[0]);
	}

	public void testVAR1_File() {
		String[] arguments = {"VAR1_20020101120000.asc"};
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("VAR1");
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(1, times.length);
		assertEquals(52275.5d, times[0]);
	}

	public void testVAR1_File_OverruleTime() {
		String[] arguments = {"VAR1_20020101120000.asc", "timeStamp=20020103180000"};
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("VAR1");
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(1, times.length);
		assertEquals(52277.75d, times[0]);
	}

	public void testVAR1_FileNameFromTimeStamp() {
		String[] arguments = {"VAR1", "timeStampFormat=yyyyMMddhhmmss", "timeStamp=20020101120000"};
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("VAR1");
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(1, times.length);
		assertEquals(52275.5d, times[0]);
	}

	public void testParamA_Adjustment() {
		String[] arguments = {"paramA_20020101.asc"};
		EsriAsciiGridDataObject dataObject = new EsriAsciiGridDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("paramA");
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
		for (int i = 0; i < valuesAsDoubles.length; i++) {
			valuesAsDoubles[i] += (i+1) * 3.3;
		}
		exchangeItem.setValuesAsDoubles(valuesAsDoubles);
		dataObject.finish();
		File adjustedFile = new File(testRunDataDir,"paramA_20020101.asc");
		File expectedFile = new File(testRunDataDir,"paramA_20020101_expected.asc");
		assertTrue(testData.FilesAreIdentical(adjustedFile, expectedFile, 0, 1e-6));
	}
}
