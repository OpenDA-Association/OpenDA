package org.openda.model_damflow;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 29-6-12
 * Time: 14:40
 * To change this template use File | Settings | File Templates.
 */
public class DupuitCFileTest extends TestCase {

	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DupuitCFileTest.class, "model_damflow");
	}

	public void testDupuitCFileTest_1(){
		DupuitCFile damflowCFile = new DupuitCFile();
		damflowCFile.initialize(testData.getTestRunDataDir(),new String[]{"dupuit.C000"});

		// test getId:
		String[] exchangeItemId = damflowCFile.getExchangeItemIDs();
		String trueId = "grid.hydraulichead";
		assertEquals("ExchangeID: ",trueId,exchangeItemId[0]);

		// test getValues:
		IExchangeItem exchangeItem = damflowCFile.getDataObjectExchangeItem(exchangeItemId[0]);
		double[] hydraulicHeadOrg = exchangeItem.getValuesAsDoubles();
		assertEquals("getValues tests; Hydraulic head :",hydraulicHeadOrg[0],5.0000e+000);
		assertEquals("getValues tests; Hydraulic head :",hydraulicHeadOrg[27],2.4474e+000);
		assertEquals("getValues tests; Hydraulic head :",hydraulicHeadOrg[34],6.0492e-001);
		assertEquals("getValues tests; Hydraulic head :",hydraulicHeadOrg[65],4.9992e+000);
		assertEquals("getValues tests; Hydraulic head :",hydraulicHeadOrg[332],2.6139e+000);

		double[] hydraulicHeadB = new double[hydraulicHeadOrg.length];
		double alpha = 0.7;
		for (int i=0; i<hydraulicHeadB.length;i++){
			hydraulicHeadB[i] = alpha * hydraulicHeadOrg[i];
		}

		// test setValues:
		exchangeItem.setValuesAsDoubles(hydraulicHeadB);
		double[] hydraulicHeadNew = exchangeItem.getValuesAsDoubles();
		for (int i=0;i<hydraulicHeadNew.length;i++){
			assertEquals("setValues tests; Hydraulic head :", hydraulicHeadB[i], hydraulicHeadNew[i]);
		}

		// test axpyValues:
		exchangeItem.setValuesAsDoubles(hydraulicHeadOrg);
		exchangeItem.axpyOnValues(alpha,hydraulicHeadB);
		hydraulicHeadNew = exchangeItem.getValuesAsDoubles();
		for (int i=0;i<hydraulicHeadNew.length;i++){
			assertEquals("axpyValues test; Hydraulic head :", alpha * hydraulicHeadB[i] + hydraulicHeadOrg[i], hydraulicHeadNew[i]);
		}

		// test getTimes:
		assertEquals("getTimes test; Time :", 0.0, exchangeItem.getTimes()[0]);

		damflowCFile.finish();
	}

}
