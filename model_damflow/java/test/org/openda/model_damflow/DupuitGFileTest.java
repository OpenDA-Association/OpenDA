package org.openda.model_damflow;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 4-7-12
 * Time: 14:08
 * To change this template use File | Settings | File Templates.
 */
public class DupuitGFileTest extends TestCase {
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DupuitPFileTest.class, "model_damflow");
	}

	public void testDupuitGFileTest(){
		DupuitGFile damflowGFile = new DupuitGFile();
		String fileName = "dupuit.G";
		damflowGFile.initialize(testData.getTestRunDataDir(), fileName, new String[]{});
		System.out.println(fileName);

		// test getIDs:
		String[] trueIDs = new String[]{"layer0.bottom","layer0.gW","layer0.gD","layer0.mv","layer0.phi","layer0.c",
				"layer0.n","layer0.Kx","layer0.Ky","layer1.bottom","layer1.gW","layer1.gD","layer1.mv","layer1.phi",
				"layer1.c","layer1.n","layer1.Kx","layer1.Ky","layer2.bottom","layer2.gW","layer2.gD","layer2.mv",
				"layer2.phi","layer2.c","layer2.n","layer2.Kx","layer2.Ky","layer3.bottom","layer3.gW","layer3.gD",
				"layer3.mv","layer3.phi","layer3.c","layer3.n","layer3.Kx","layer3.Ky","layer4.bottom","layer4.gW",
				"layer4.gD","layer4.mv","layer4.phi","layer4.c","layer4.n","layer4.Kx","layer4.Ky","layer5.bottom",
				"layer5.gW","layer5.gD","layer5.mv","layer5.phi","layer5.c","layer5.n","layer5.Kx","layer5.Ky",
				"cDitch","cRiverBed","cPolder","cDike","beta"};
		String[] exchangeIDs = damflowGFile.getExchangeItemIDs();
		int nExchangeItem = exchangeIDs.length;
		for (int i=0; i<nExchangeItem; i++){
			assertEquals("exchangeIDs["+i+"]", trueIDs[i], exchangeIDs[i]);
			System.out.println("ExchangeItem id: "+exchangeIDs[i]);
		}

		// test getValues:
		double[] trueKxs = new double[]{1.0000e+01,0.0000e+00,0.0000,0.0000e+00,0.0000e+00,0.0000e+00,0.0000e+00,0.0000e+00,
				0.0000e+00,0.0000e+00,2.0000e+01,1.7000e+01,0.0000e+00,2.0000e+01,8.0000e+00,4.0000e-01,1.0000e+00,
				1.0000e+00,0.0000e+00,1.8000e+01,1.6000e+01,1.0000e-06,2.0000e+01,3.0000e+00,0.0000e+00,1.0000e-02,
				1.0000e-02,0.0000e+00,2.0000e+01,1.7000e+01,0.0000e+00,3.1000e+01,0.0000e+00,0.0000e+00,2.5000e+01,
				2.5000e+01,0.0000e+00,1.9000e+01,1.6000e+01,1.0000e-06,2.0000e+01,8.0000e+00,0.0000e+00,1.0000e-02,
				1.0000e-02,0.0000e+00,2.0000e+01,1.7000e+01,0.0000e+00,3.0000e+01,0.0000e+00,0.0000e+00,7.5000e+01,
				7.5000e+01,1.0000e+02,0.0000e+00,5.0000e+03,0.0000e+00,5.0000e-07};

		for (int i=0; i<nExchangeItem; i++){
			IExchangeItem exchangeItem = damflowGFile.getDataObjectExchangeItem(exchangeIDs[i]);
			double Kx = exchangeItem.getValuesAsDoubles()[0];
			assertEquals("Parameter " + exchangeItem.getId() + " layer"+i+"", trueKxs[i], Kx);
		}

		// test setValues:
		double[] setKx = new double[]{2.1,3.4,5.0,2.8,9.2,6.5};
		for (int i=0; i<setKx.length; i++){
			IExchangeItem exchangeItem = damflowGFile.getDataObjectExchangeItem(exchangeIDs[i]);
			exchangeItem.setValuesAsDoubles(new double[]{setKx[i]});
			double newKx = exchangeItem.getValuesAsDoubles()[0];
			assertEquals("Kx layer"+i+"", setKx[i], newKx);
		}

		// test axpyOnValues:
		double alpha = 0.3;
		double[] axpyValue = new double[]{0.8};
		for (int i=0; i<setKx.length; i++){
			IExchangeItem exchangeItem = damflowGFile.getDataObjectExchangeItem(exchangeIDs[i]);
			double oldKx = exchangeItem.getValuesAsDoubles()[0];
			exchangeItem.axpyOnValues(alpha,axpyValue);
			double Kx = exchangeItem.getValuesAsDoubles()[0];
			System.out.println(exchangeItem.getId() + ": "+Kx);
			assertEquals("Kx layer"+i+"", setKx[i]+alpha*axpyValue[0], Kx);
		}

		damflowGFile.finish();

	}

}
