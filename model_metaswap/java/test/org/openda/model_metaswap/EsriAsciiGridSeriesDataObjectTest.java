package org.openda.model_metaswap;

import junit.framework.TestCase;
import org.openda.exchange.dataobjects.EsriAsciiGridDataObject;
import org.openda.exchange.dataobjects.EsriAsciiGridDataObjectTest;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Created by hummel on 2018-02-12.
 */
public class EsriAsciiGridSeriesDataObjectTest extends TestCase {

	private OpenDaTestSupport testData = null;
	File testRunDataDir = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(EsriAsciiGridSeriesDataObjectTest.class, "model_metaswap");
		testRunDataDir = new File(testData.getTestRunDataDir(), "gridSeries");
	}

	public void testParamAFiles() {
		String[] arguments = {
			"./meteo/paramA",
			"outputFilePrefix=./paramA",
			"timeStampFormat=yyyyMMdd",
			"currentTime=20020101",
			"targetTime=20020104"
		};
		EsriAsciiGridSeriesDataObject dataObject = new EsriAsciiGridSeriesDataObject();
		dataObject.initialize(testRunDataDir, arguments);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("paramA");
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(4, times.length);
		assertEquals(52275d, times[0]);
		assertEquals(52276d, times[1]);
		assertEquals(52277d, times[2]);
		assertEquals(52278d, times[3]);
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
		assertEquals(160, valuesAsDoubles.length);
		assertEquals(1.1d, valuesAsDoubles[0]);
		assertEquals(111d, valuesAsDoubles[11]);
		assertEquals(100d, valuesAsDoubles[12]);
		assertEquals(2.2d, valuesAsDoubles[41]);
		assertEquals(222d, valuesAsDoubles[51]);
		assertEquals(222.2d, valuesAsDoubles[64]);
		assertEquals(3.3d, valuesAsDoubles[82]);
		assertEquals(333d, valuesAsDoubles[91]);
		assertEquals(333.3d, valuesAsDoubles[119]);
		assertEquals(4.4d, valuesAsDoubles[123]);
		assertEquals(444d, valuesAsDoubles[131]);
		assertEquals(444.4d, valuesAsDoubles[157]);
		for (int i = 0; i < valuesAsDoubles.length; i++) {
			valuesAsDoubles[i] += 0.00001d * (i+1);
		}
		exchangeItem.setValuesAsDoubles(valuesAsDoubles);
		dataObject.finish();
		assertTrue(new File(testRunDataDir, "paramA_20020101.asc").exists());
		assertTrue(new File(testRunDataDir, "paramA_20020102.asc").exists());
		assertTrue(new File(testRunDataDir, "paramA_20020103.asc").exists());
		assertTrue(new File(testRunDataDir, "paramA_20020104.asc").exists());
	}
}
