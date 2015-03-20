/* MOD_V2.0 
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.observers;
import junit.framework.TestCase;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Statistics;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

/**
 * Test for statistics on the series in some Noos observers.
 */
public class NoosStochObserverStatisticsTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(NoosStochObserverStatisticsTest.class, "observers");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testStatistics_Overall() {
		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosObservations.xml"});

		int trueNumOfData = 38;
		double trueMinimum = -0.99;
		double trueMaximum = 0.73;

		System.out.println("obs1.getCount(): " + obs1.getCount());
		int nData = obs1.getCount();
		double trueBias = 0.0;
		for (int i = 0; i < nData; i++) {
			trueBias += 1 / (double) nData * obs1.getExpectations().getValue(i);
		}
		double trueSTD = 0.0;
		for (int i = 0; i < nData; i++) {
			trueSTD += 1 / (double) (nData - 1) * (obs1.getExpectations().getValue(i) - trueBias) * (obs1.getExpectations().getValue(i) - trueBias);
		}
		trueSTD = Math.sqrt(trueSTD);
		double trueRMS = 0.0;
		for (int i = 0; i < nData; i++) {
			trueRMS += 1 / (double) nData * obs1.getExpectations().getValue(i) * obs1.getExpectations().getValue(i);
		}
		trueRMS = Math.sqrt(trueRMS);

		IVector allValues = obs1.getExpectations();
		Statistics stat1 = new Statistics(allValues);
		HashMap<String, Double> numOfData = stat1.getNumberOfData();
		HashMap<String, Double> min = stat1.getMinimum();
		HashMap<String, Double> max = stat1.getMaximum();
		HashMap<String, Double> bias = stat1.getBias();
		HashMap<String, Double> stdev = stat1.getSTD();
		HashMap<String, Double> rms = stat1.getRMS();
		System.out.println("numOfData true: " + trueNumOfData + ", while estimate: " + numOfData.get("Overall").intValue());
		System.out.println("min true: " + trueMinimum + ", while estimate: " + min.get("Overall"));
		System.out.println("max true: " + trueMaximum + ", while estimate: " + max.get("Overall"));
		System.out.println("Bias true: " + trueBias + ", while estimate: " + bias.get("Overall"));
		System.out.println("STD true: " + trueSTD + ", while estimate: " + stdev.get("Overall"));
		System.out.println("RMS true: " + trueRMS + ", while estimate: " + rms.get("Overall"));

		assertEquals("Size: ", 1, bias.size());
		assertEquals("number of data: ", trueNumOfData, numOfData.get("Overall").intValue());
		assertEquals("minimum value: ", trueMinimum, min.get("Overall"));
		assertEquals("maximum value: ", trueMaximum, max.get("Overall"));
		assertEquals("Bias: ", trueBias, bias.get("Overall"));
		assertEquals("STDev: ", trueSTD, stdev.get("Overall"));
		assertEquals("RMS: ", trueRMS, rms.get("Overall"));

		ITreeVector nDataTree = stat1.asTreeVector(numOfData);
		ITreeVector minTree = stat1.asTreeVector(min);
		ITreeVector maxTree = stat1.asTreeVector(max);
		ITreeVector biasTree = stat1.asTreeVector(bias);
		ITreeVector stdTree = stat1.asTreeVector(stdev);
		ITreeVector rmsTree = stat1.asTreeVector(rms);
		System.out.println("nDataTree: " + nDataTree.toString());
		System.out.println("minTree: " + minTree.toString());
		System.out.println("maxTree: " + maxTree.toString());
		System.out.println("biasTree: " + biasTree.toString());
		System.out.println("stdTree: " + stdTree.toString());
		System.out.println("rmsTree: " + rmsTree.toString());
		System.out.println(stat1.toString());
	}

	public void testStatistics_Grouped() {
		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosObservations.xml"});

		IVector allValues = obs1.getExpectations();
		IObservationDescriptions obsdescr = obs1.getObservationDescriptions();

		int nData = obs1.getCount();
		double[] valueDenhdr = new double[19];
		double[] valueAbdn = new double[19];
		int j = 0;
		for (int i = 0; i < nData; i++) {
			if (i < 19) {
				valueDenhdr[i] = obs1.getExpectations().getValue(i);
			} else {
				valueAbdn[j] = obs1.getExpectations().getValue(i);
				j++;
			}
		}

		// compute correct statistics:
		double biasDenhdr = 0.0;
		double biasAbdn = 0.0;
		for (double aValueDenhdr : valueDenhdr) {
			biasDenhdr += 1 / (double) (valueDenhdr.length) * aValueDenhdr;
		}
		for (double aValueAbdn : valueAbdn) {
			biasAbdn += 1 / (double) (valueAbdn.length) * aValueAbdn;
		}
		double stdDenhdr = 0.0;
		double stdAbdn = 0.0;
		for (double aValueDenhdr : valueDenhdr) {
			stdDenhdr += 1 / (double) (valueDenhdr.length - 1) * (aValueDenhdr - biasDenhdr) * (aValueDenhdr - biasDenhdr);
		}
		for (double aValueAbdn : valueAbdn) {
			stdAbdn += 1 / (double) (valueAbdn.length - 1) * (aValueAbdn - biasAbdn) * (aValueAbdn - biasAbdn);
		}
		stdDenhdr = Math.sqrt(stdDenhdr);
		stdAbdn = Math.sqrt(stdAbdn);
		double rmsDenhdr = 0.0;
		double rmsAbdn = 0.0;
		for (double aValueDenhdr : valueDenhdr) {
			rmsDenhdr += 1 / (double) (valueDenhdr.length) * aValueDenhdr * aValueDenhdr;
		}
		for (double aValueAbdn : valueAbdn) {
			rmsAbdn += 1 / (double) (valueAbdn.length) * aValueAbdn * aValueAbdn;
		}
		rmsDenhdr = Math.sqrt(rmsDenhdr);
		rmsAbdn = Math.sqrt(rmsAbdn);

		// get statistics from Statistics class:
		Statistics stat1 = new Statistics(allValues, obsdescr);
		HashMap<String, Double> numOfData = stat1.getNumberOfData();
		HashMap<String, Double> min = stat1.getMinimum();
		HashMap<String, Double> max = stat1.getMaximum();
		HashMap<String, Double> bias = stat1.getBias();
		assertEquals("Size: ", 2, bias.size());
		HashMap<String, Double> std = stat1.getSTD();
		HashMap<String, Double> rms = stat1.getRMS();

		String key1 = obsdescr.getExchangeItems().get(0).getId();
		String key2 = obsdescr.getExchangeItems().get(1).getId();
		assertEquals("number of data, den helder: ", 19, numOfData.get(key1).intValue());
		assertEquals("number of data, aberdeen: ", 19, numOfData.get(key2).intValue());
		assertEquals("minimum data, den helder: ", 0.0, min.get(key1));
		assertEquals("minimum data, aberdeen: ", -0.99, min.get(key2));
		assertEquals("maximum data, den helder: ", 0.73, max.get(key1));
		assertEquals("maximum data, aberdeen: ", -0.6, max.get(key2));
		assertEquals("Bias, den helder: ", biasDenhdr, bias.get(key1));
		assertEquals("Bias, aberdeen: ", biasAbdn, bias.get(key2));
		assertEquals("STD, den helder: ", stdDenhdr, std.get(key1), 1e-15);
		assertEquals("STD, aberdeen: ", stdAbdn, std.get(key2), 1e-15);
		assertEquals("RMS, den helder: ", rmsDenhdr, rms.get(key1), 1e-15);
		assertEquals("RMS, aberdeen: ", rmsAbdn, rms.get(key2), 1e-15);

		ITreeVector nDataTree = stat1.asTreeVector(numOfData);
		ITreeVector minTree = stat1.asTreeVector(min);
		ITreeVector maxTree = stat1.asTreeVector(max);
		ITreeVector biasTree = stat1.asTreeVector(bias);
		ITreeVector stdTree = stat1.asTreeVector(std);
		ITreeVector rmsTree = stat1.asTreeVector(rms);
		assertEquals("Size treevector: ", 2, biasTree.getSize());
		System.out.println("nDataTree: " + nDataTree.toString());
		System.out.println("minTree: " + minTree.toString());
		System.out.println("maxTree: " + maxTree.toString());
		System.out.println("biasTree: " + biasTree.toString());
		System.out.println("stdTree: " + stdTree.toString());
		System.out.println("rmsTree: " + rmsTree.toString());

		System.out.println(stat1.toString());

	}

}
