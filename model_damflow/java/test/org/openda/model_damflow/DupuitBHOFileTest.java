package org.openda.model_damflow;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 3-7-12
 * Time: 11:43
 * To change this template use File | Settings | File Templates.
 */
public class DupuitBHOFileTest extends TestCase {
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DupuitPFileTest.class, "model_damflow");
	}

	public void testDupuitOFileTest(){
		DupuitBHOFile damflowOFile = new DupuitBHOFile();
		String fileName = "dupuit.O";
		damflowOFile.initialize(testData.getTestRunDataDir(),fileName,new String[]{});

		// test getIDs:
		String[] trueIDs = new String[]{"location0.hydraulichead","location1.hydraulichead"};
		String[] exchangeIDs = damflowOFile.getExchangeItemIDs();
		for (int i=0; i<exchangeIDs.length; i++){
			assertEquals("exchangeIDs["+i+"]", trueIDs[i], exchangeIDs[i]);
			System.out.println(exchangeIDs[i]);
		}

		// test getValues:
		// 		location 0:
		IExchangeItem exchangeItem = damflowOFile.getDataObjectExchangeItem(exchangeIDs[0]);
		double[] phi = exchangeItem.getValuesAsDoubles();
		double[] time = exchangeItem.getTimes();
		int index = 10;
		assertEquals("phi["+index+"]", 3.2061e+000, phi[index]);
		assertEquals("time["+index+"]", 2.0000e+000, time[index]);
		//		location 1:
		exchangeItem = damflowOFile.getDataObjectExchangeItem(exchangeIDs[1]);
		phi = exchangeItem.getValuesAsDoubles();
		time = exchangeItem.getTimes();
		index = 10;
		assertEquals("phi["+index+"]", 6.8211e+000, phi[index]);
		assertEquals("time["+index+"]", 2.0000e+000, time[index]);

		damflowOFile.finish();
	}

	public void testDupuitHFileTest(){
		DupuitBHOFile damflowHile = new DupuitBHOFile();
		String fileName = "dupuit.H";
		damflowHile.initialize(testData.getTestRunDataDir(), fileName, new String[]{});

		// test getIDs:
		String[] trueIDs = new String[]{"location0.hydraulichead"};
		String[] exchangeIDs = damflowHile.getExchangeItemIDs();
		for (int i=0; i<exchangeIDs.length; i++){
			assertEquals("exchangeIDs["+i+"]", trueIDs[i], exchangeIDs[i]);
			System.out.println(exchangeIDs[i]);
		}

		// test getValues:
		// 		location 0:
		IExchangeItem exchangeItem = damflowHile.getDataObjectExchangeItem(exchangeIDs[0]);
		double[] phi = exchangeItem.getValuesAsDoubles();
		double[] time = exchangeItem.getTimes();
		int index = 10;
		assertEquals("phi["+index+"]", 3.5328e+000, phi[index]);
		assertEquals("time["+index+"]", 3.0000e+000, time[index]);

		damflowHile.finish();
	}

	public void testDupuitBFileTest(){
		DupuitBHOFile damflowBFile = new DupuitBHOFile();
		String fileName = "dupuit.B";
		damflowBFile.initialize(testData.getTestRunDataDir(), fileName, new String[]{});

		// test getIDs:
		String[] trueIDs = new String[]{"riverwaterlevel"};
		String[] exchangeIDs = damflowBFile.getExchangeItemIDs();
		for (int i=0; i<exchangeIDs.length; i++){
			assertEquals("exchangeIDs["+i+"]", trueIDs[i], exchangeIDs[i]);
			System.out.println(exchangeIDs[i]);
		}

		// test getValues:
		IExchangeItem exchangeItem = damflowBFile.getDataObjectExchangeItem(exchangeIDs[0]);
		double[] rwl = exchangeItem.getValuesAsDoubles();
		double[] time = exchangeItem.getTimes();
		int index = 10;
		assertEquals("rwl["+index+"]", 7.3511e+00, rwl[index]);
		assertEquals("time["+index+"]", 1.0000e+00, time[index]);

		index=26;
		assertEquals("rwl["+index+"]", 5.9948e+00, rwl[index]);
		assertEquals("time["+index+"]", 2.6000e+00, time[index]);

		// test setValues:
		double alpha = 0.8d;
		double[] newRwl = new double[rwl.length];
		for (int i=0; i<rwl.length; i++){
			newRwl[i] = rwl[i] * alpha;
		}
		exchangeItem.setValuesAsDoubles(newRwl);
		double[] newRwl2 = exchangeItem.getValuesAsDoubles();
		for (int i=0; i<rwl.length; i++){
			assertEquals("setValues tests; riverwaterlevel :", newRwl2[i], rwl[i] * alpha);
		}

		// test axpyOnValues:
		exchangeItem.setValuesAsDoubles(rwl);
		exchangeItem.axpyOnValues(alpha,newRwl);
		newRwl2 = exchangeItem.getValuesAsDoubles();
		for (int i=0; i<rwl.length; i++){
			assertEquals("axpyOnValues tests; riverwaterlevel :", newRwl2[i], rwl[i] +  newRwl[i] * alpha);
		}

		damflowBFile.finish();
	}
}
