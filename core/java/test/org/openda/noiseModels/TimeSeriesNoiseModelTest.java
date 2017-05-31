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
package org.openda.noiseModels;
import junit.framework.TestCase;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class TimeSeriesNoiseModelTest extends TestCase {

    //File testDir = null;
    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(TimeSeriesNoiseModelTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testNoiseForTimeSeries_basics(){
    	IStochModelFactory factory = new TimeSeriesNoiseModelFactory();
    	String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> "
    			+"<timeSeriesNoiseModelConfig>"
    			+"	<simulationTimespan timeFormat=\"mjd\">0.0,0.05,...,10.0</simulationTimespan>"
    			+"	<timeSeries location=\"location1\" quantity=\"quantity1\" standardDeviation=\"10.0\""
    			+"		timeCorrelationScale=\"6.0\" timeCorrelationScaleUnit=\"hours\" />"
    			+"	<timeSeries location=\"location2\" quantity=\"quantity2\" standardDeviation=\"1.0\""
    			+"		timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" />"
    			+"	<timeSeries location=\"location3\" quantity=\"quantity3\" standardDeviation=\"0.1\""
    			+"		timeCorrelationScale=\"1.0\" timeCorrelationScaleUnit=\"days\" />"
    			+"</timeSeriesNoiseModelConfig>";
    	factory.initialize(testRunDataDir, new String[]{configString});
    	IStochModelInstance model = factory.getInstance(OutputLevel.Debug);

    	StochVector.setSeed(10111213);
    	model.setAutomaticNoiseGeneration(true);
    	model.compute(new Time(10.0));


    	String[] ids=model.getExchangeItemIDs();
    	assertEquals(3, ids.length);
    	System.out.println("ids[0]="+ids[0]);
    	assertEquals("location1.quantity1", ids[0]);
    	IPrevExchangeItem series1=model.getExchangeItem("location1.quantity1");
    	assertTrue(series1 instanceof TimeSeries);
    	System.out.println("location1.quantity1 =>"+series1.toString());
    	double[] times=series1.getTimes();
    	assertEquals("times.length",201, times.length);
    	assertEquals("times[200]",10.0, times[200],0.00001);
    	double[] values1=series1.getValuesAsDoubles();
    	assertEquals("values1.length",201, values1.length);
    	assertEquals("values1[200]",5.508788635641277, values1[200],0.00001);
    	Vector valueVector1 = new Vector(values1);
    	valueVector1.maxFullExpandLength=1000;
    	System.out.println("values1="+valueVector1.toString());
    	//series 2
    	IPrevExchangeItem series2=model.getExchangeItem("location2.quantity2");
    	assertTrue(series2 instanceof TimeSeries);
    	System.out.println("location2.quantity2 =>"+series2.toString());
    	double[] values2=series2.getValuesAsDoubles();
    	assertEquals("values2.length",201, values2.length);
    	assertEquals("values2[200]",1.3616460794356013, values2[200],0.00001);
    	Vector valueVector2 = new Vector(values2);
    	valueVector2.maxFullExpandLength=1000;
    	System.out.println("values2="+valueVector2.toString());

    	//series 3
    	IPrevExchangeItem series3=model.getExchangeItem("location3.quantity3");
    	assertTrue(series3 instanceof TimeSeries);
    	System.out.println("location3.quantity3 =>"+series3.toString());
    	double[] values3=series3.getValuesAsDoubles();
    	assertEquals("values3.length",201, values3.length);
    	assertEquals("values3[200]",-0.10769688278657658, values3[200],0.00001);
    	Vector valueVector3 = new Vector(values3);
    	valueVector3.maxFullExpandLength=1000;
    	System.out.println("values3="+valueVector3.toString());
	}

	public void testNoiseForTimeSeries_initialValue(){
		IStochModelFactory factory = new TimeSeriesNoiseModelFactory();
		int endTimeInDays = 1000;
		String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> "
				+"<timeSeriesNoiseModelConfig>"
				+"	<simulationTimespan timeFormat=\"mjd\">0.0,0.05,...,"+ endTimeInDays + "</simulationTimespan>"
				+"	<timeSeries location=\"location1\" quantity=\"quantity1\" standardDeviation=\"0.5\""
				+"		timeCorrelationScale=\"6.0\" initialValue=\"15\" timeCorrelationScaleUnit=\"hours\" />"
				+"</timeSeriesNoiseModelConfig>";
		factory.initialize(testRunDataDir, new String[]{configString});
		IStochModelInstance model = factory.getInstance(OutputLevel.Debug);

		StochVector.setSeed(19920411);
		model.setAutomaticNoiseGeneration(true);
		model.compute(new Time((double)endTimeInDays));

		String noiseModelExchItemId=model.getExchangeItemIDs()[0];
		IPrevExchangeItem series1=model.getExchangeItem(noiseModelExchItemId);
		double[] times=series1.getTimes();
		double[] values=series1.getValuesAsDoubles();
		File file = new File(testRunDataDir, "noise_with_initial_values.csv");
		BufferedWriter noiseFileWriter = null;
		try {
			noiseFileWriter = new BufferedWriter(new FileWriter(file));
			noiseFileWriter.write("mjd,value-"+noiseModelExchItemId);
			noiseFileWriter.newLine();

			double average = 0.0;
			int numValues = 0;
			for (int i=0; i < values.length ; i++) {
				noiseFileWriter.write(times[i] + "," + values[i]);
				noiseFileWriter.newLine();
				average += values[i];
				numValues++;
			}
			average /= numValues;
			noiseFileWriter.write("AVERAGE," + average);
			System.out.println("AVERAGE: " + average + " (#values: " + numValues + ")");
			assertEquals("Expected average", 15.0127328, average, 1.0e-6);

		} catch (IOException e) {
			throw new RuntimeException("Could not open log file for writing: " + file.getAbsolutePath() +
					"(" + e.getMessage() + ")");
		} finally {
			if (noiseFileWriter != null) {
				try {
					noiseFileWriter.close();
				} catch (IOException e) {
					// no action needed
				}
			}
		}

	}

	public void testNoiseForTimeSeries_config(){
    	IStochModelFactory factory = new TimeSeriesNoiseModelFactory();
    	String inputFile="timeseries_noise.xml";
    	factory.initialize(testRunDataDir, new String[]{inputFile});
    	IStochModelInstance model = factory.getInstance(OutputLevel.Debug);

    	StochVector.setSeed(10L);
    	model.setAutomaticNoiseGeneration(true);
    	ITime targetTime = model.getTimeHorizon().getEndTime();
    	model.compute(targetTime);


    	String[] ids=model.getExchangeItemIDs();
    	assertEquals(2, ids.length);
    	System.out.println("ids[0]="+ids[0]);
    	assertEquals("waterlevel@aberdeen", ids[0]);
    	IPrevExchangeItem series1=model.getExchangeItem("waterlevel@aberdeen");
    	assertTrue(series1 instanceof TimeSeries);
    	System.out.println("location1.quantity1 =>"+series1.toString());
    	double[] times=series1.getTimes();
    	assertEquals("times.length",71, times.length);
    	assertEquals("times[70]",55432.98611111117, times[70],0.00001);
    	double[] values=series1.getValuesAsDoubles();
    	assertEquals("values.length",71, values.length);
    	assertEquals("values[70]",0.05608590539600699, values[70],0.00001);
    }
    public void testNoiseForTimeSeries_multistart(){
    	IStochModelFactory factory = new TimeSeriesNoiseModelFactory();
    	String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> "
    			+"<timeSeriesNoiseModelConfig>"
    			+"	<simulationTimespan timeFormat=\"mjd\">0.0,0.05,...,10.0</simulationTimespan>"
    			+"	<timeSeries location=\"location1\" quantity=\"quantity1\" standardDeviation=\"10.0\""
    			+"		timeCorrelationScale=\"6.0\" timeCorrelationScaleUnit=\"hours\" />"
    			+"	<timeSeries location=\"location2\" quantity=\"quantity2\" standardDeviation=\"1.0\""
    			+"		timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" />"
    			+"	<timeSeries location=\"location3\" quantity=\"quantity3\" standardDeviation=\"0.1\""
    			+"		timeCorrelationScale=\"1.0\" timeCorrelationScaleUnit=\"days\" />"
    			+"</timeSeriesNoiseModelConfig>";
    	factory.initialize(testRunDataDir, new String[]{configString});

    	IStochModelInstance model1 = factory.getInstance(OutputLevel.Debug);
    	StochVector.setSeed(10111213);
    	model1.setAutomaticNoiseGeneration(true);
    	model1.compute(new Time(10.0));
    	IVector state1_t10 = model1.getState();

    	//multiple starts with internal state
    	IStochModelInstance model2 = factory.getInstance(OutputLevel.Debug);
    	StochVector.setSeed(10111213);
    	model2.setAutomaticNoiseGeneration(true);
    	model2.compute(new Time(5.0));
    	IVector state2_t5 = model2.getState();
    	model2.compute(new Time(10.0));
    	IVector state2_t10 = model2.getState();
    	IVector diff2_t10 = state2_t10.clone();
    	diff2_t10.axpy(-1.0,state1_t10);
    	assertEquals(0.0, diff2_t10.norm2(), 0.0001);

    	//multiple starts with restart
    	IStochModelInstance model3 = factory.getInstance(OutputLevel.Debug);
    	StochVector.setSeed(10111213);
    	model3.setAutomaticNoiseGeneration(true);
    	model3.compute(new Time(5.0));
        IVector state3_t5_pre = model3.getState();

    	IVector diff3_t5_pre = state3_t5_pre.clone();
    	diff3_t5_pre.axpy(-1.0,state2_t5);
    	assertEquals(0.0, diff3_t5_pre.norm2(), 0.0001);

        IModelState restart3_t5_pre = model3.saveInternalState();
    	File savedStateFile=new File(testRunDataDir,"noise_series_restart");
       	restart3_t5_pre.savePersistentState(savedStateFile);
       	//change state so we know that restart file is realy used
    	model3.axpyOnState(1.0, new Vector("[1000.0,1000.0,1000.0]"));

    	IModelState restart3_t5_post =model3.loadPersistentState(savedStateFile);
    	model3.restoreInternalState(restart3_t5_post);
    	IVector state3_t5_post = model3.getState();

    	IVector diff3_t5_post = state3_t5_post.clone();
    	diff3_t5_post.axpy(-1.0,state2_t5);
    	assertEquals(0.0, diff3_t5_post.norm2(), 0.0001);

    	model3.compute(new Time(10.0));
    	IVector state3_t10 = model3.getState();

    	IVector diff3_t10 = state3_t10.clone();
    	diff3_t10.axpy(-1.0,state2_t10);
    	assertEquals(0.0, diff3_t10.norm2(), 0.0001);
    }

	public void testNoiseFixedVersusRandom(){

		IStochModelInstance modelWithRandomSeed = null;

		double[][] resultsFixedSeed = new double[2][];
		double[][] resultsRandomSeed = new double[2][];
		for (int i = 0; i < 2; i++) {
			StochVector.setInitialSeedType(StochVector.InitialSeedType.fixed);
			IStochModelFactory factoryWithFixedSeed = new TimeSeriesNoiseModelFactory();
			factoryWithFixedSeed.initialize(testRunDataDir, new String[]{"timeseries_noise.xml"});
			IStochModelInstance modelWithFixedSeed = factoryWithFixedSeed.getInstance(OutputLevel.Debug);
			modelWithFixedSeed.setAutomaticNoiseGeneration(true);
			ITime targetTime = modelWithFixedSeed.getTimeHorizon().getEndTime();
			modelWithFixedSeed.compute(targetTime);
			IPrevExchangeItem seriesWithFixedSeed=modelWithFixedSeed.getExchangeItem("waterlevel@aberdeen");
			resultsFixedSeed[i]=seriesWithFixedSeed.getValuesAsDoubles();

			StochVector.setInitialSeedType(StochVector.InitialSeedType.random);
			IStochModelFactory factoryWithRandomSeed = new TimeSeriesNoiseModelFactory();
			factoryWithRandomSeed.initialize(testRunDataDir, new String[]{"timeseries_noise.xml"});
			modelWithRandomSeed = factoryWithRandomSeed.getInstance(OutputLevel.Debug);
			modelWithRandomSeed.setAutomaticNoiseGeneration(true);
			modelWithRandomSeed.compute(targetTime);
			IPrevExchangeItem seriesWithRandomSeed=modelWithRandomSeed.getExchangeItem("waterlevel@aberdeen");
			resultsRandomSeed[i]=seriesWithRandomSeed.getValuesAsDoubles();
		}

		assertEquals("series length equal", resultsFixedSeed.length, resultsRandomSeed.length);
		for (int i = 1; i < resultsFixedSeed[0].length; i++) {
			assertEquals("fixed values[" + i + "]" , resultsFixedSeed[0][i], resultsFixedSeed[1][i]);
			// java does not support assertNotEqual
			if (epsilonCompare(resultsFixedSeed[0][i], resultsRandomSeed[1][i])) {
				fail("resultsRandomSeed[" + i + "] should differ from resultsFixedSeed");
			}
			if (epsilonCompare(resultsRandomSeed[0][i], resultsRandomSeed[1][i])) {
				fail("resultsRandomSeed[run0][" + i + "] should differ from resultsFixedSeed[run1]" + i + "]");
			}
		}
	}
	private boolean epsilonCompare(double d1, double d2) {
		final double epsilon = 1.e-7;
		return (d1 + epsilon > d2) && (d2 + epsilon > d1);
	}
}
