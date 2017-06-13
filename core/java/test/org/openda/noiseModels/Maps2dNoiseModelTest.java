/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

public class Maps2dNoiseModelTest extends TestCase {

	//File testDir = null;
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(Maps2dNoiseModelTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testMaps2dNoise_basics(){
		IStochModelFactory factory = new MapsNoiseModelFactory();
		String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+"<mapsNoiseModelConfig>"
				+"	<simulationTimespan timeFormat=\"dateTimeString\">201008241200,201008241210,...,201008241220</simulationTimespan>"
				+"	<noiseItem id=\"windU\" quantity=\"wind-u\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"false\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"	<noiseItem id=\"windV\" quantity=\"wind-v\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"false\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"</mapsNoiseModelConfig>";
		factory.initialize(testRunDataDir, new String[]{configString});
		IStochModelInstance model = factory.getInstance(OutputLevel.Debug);

		StochVector.setSeed(10111213);
		model.setAutomaticNoiseGeneration(true);

		double targetTime=0.0;
		try {
			targetTime=TimeUtils.date2Mjd("201008241220");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		model.compute(new Time(targetTime));

		//
		//check output items
		//
		//id's
		String[] ids=model.getExchangeItemIDs();
		assertEquals(2, ids.length);
		System.out.println("ids[0]="+ids[0]);
		assertEquals("windU", ids[0]);
		//item 'windU'
		ArrayExchangeItem item0 = (ArrayExchangeItem)model.getExchangeItem(ids[0]);
		String idCheck=item0.getId();
		System.out.println("item['windU']="+item0.toString());
		assertEquals("windU", idCheck);
		// values
		IArray valuesArray = item0.getValues();
		int dimensions[]=valuesArray.getDimensions();
		assertEquals(3,dimensions.length);
		assertEquals(3,dimensions[0]);  // time
		assertEquals(3,dimensions[1]);  // longitude y
		assertEquals(5,dimensions[2]);  // latitude x
		double values[] = valuesArray.getValuesAsDoubles();
		System.out.println("values="+new Vector(values));
		assertEquals(3*5*3,values.length);
		double delta=1.0E-8;
		System.out.println("values[15+0]="+values[15+0]);
		assertEquals(-8.657673091235027E-4, values[15+0], delta); //initial value of 0 is boring.    
		System.out.println("values[15+29]="+values[15+29]);
		assertEquals(-0.10794060690367921, values[15+29], delta);
		// quantity
		IQuantityInfo qInfo = item0.getQuantityInfo();
		String unit = qInfo.getUnit();
		assertEquals("m/s",unit);
		String quantity = qInfo.getQuantity();
		assertEquals("wind-u",quantity);
		// geometry
		ArrayGeometryInfo geomInfo = (ArrayGeometryInfo)item0.getGeometryInfo();
		IArray lon = geomInfo.getLongitudeArray();
		System.out.println("lon="+lon);
		assertEquals("lon","{-5.0,-2.5,0.0,2.5,5.0}",lon.toString());
		IArray lat = geomInfo.getLatitudeArray();
		System.out.println("lat="+lat);
		assertEquals("lat","{50.0,55.0,60.0}",lat.toString());

		ArrayExchangeItem item1 = (ArrayExchangeItem)model.getExchangeItem(ids[1]);
		String idCheck1=item1.getId();
		System.out.println("item['windV']="+item1.toString());
		assertEquals("windV", idCheck1);
		//Values
		IArray valuesArray1 = item1.getValues();
		int dimensions1[]=valuesArray1.getDimensions();
		double values1[] = valuesArray1.getValuesAsDoubles();
		System.out.println("values1="+new Vector(values1));
		assertEquals(3*5*3,values1.length);
		System.out.println("values1[15+0]="+values1[15+0]);
		assertEquals(-0.02765171365830534, values1[15+0], delta);    	
		System.out.println("values1[15+29]="+values1[15+29]);
		assertEquals(-0.20139530207750905, values1[15+29], delta);
		// quantity
		IQuantityInfo qInfo1 = item1.getQuantityInfo();
		String unit1 = qInfo1.getUnit();
		assertEquals("m/s",unit1);
		String quantity1 = qInfo1.getQuantity();
		assertEquals("wind-v",quantity1);

		//check write restart
		IModelState state=model.saveInternalState();
		File stateFile=new File(testRunDataDir,"state.txt");
		state.savePersistentState(stateFile);
		File referenceStateFile=new File(testRunDataDir,"reference_state.txt");

		IModelState referenceState = model.loadPersistentState(referenceStateFile);
		double[] referenceStateValues = ((MapsNoiseModelInstance.savedState)referenceState).state.getValues();
		double[] createdStateValues = ((MapsNoiseModelInstance.savedState)state).state.getValues();
		for (int i = 0; i < referenceStateValues.length ; i++) {
			assertEquals("stateValue[" + i + "]", referenceStateValues[i], createdStateValues[i], 1e-10);
		}

		// read restart
		IModelState stateFromFile=model.loadPersistentState(stateFile);
		model.restoreInternalState(stateFromFile);
		model.compute(new Time(targetTime+1));
		
	}

	public void testMaps2dNoiseRecompute(){
		IStochModelFactory factory = new MapsNoiseModelFactory();
		String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+"<mapsNoiseModelConfig>"
				+"	<simulationTimespan timeFormat=\"dateTimeString\">201008241200,201008241210,...,201008241220</simulationTimespan>"
				+"	<noiseItem id=\"windU\" quantity=\"wind-u\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"false\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"	<noiseItem id=\"windV\" quantity=\"wind-v\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"false\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"</mapsNoiseModelConfig>";
		factory.initialize(testRunDataDir, new String[]{configString});

		StochVector.setSeed(10111213);

		IStochModelInstance[] models = new IStochModelInstance[]{factory.getInstance(OutputLevel.Debug), factory.getInstance(OutputLevel.Debug), factory.getInstance(OutputLevel.Debug), factory.getInstance(OutputLevel.Debug)};
		double targetTime;
		try {
			targetTime=TimeUtils.date2Mjd("201008241220");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		for (IStochModelInstance model : models) {
			model.setAutomaticNoiseGeneration(true);
			model.compute(new Time(targetTime));
		}

		String[] ids=models[0].getExchangeItemIDs();
		assertEquals(2, ids.length);
		ArrayExchangeItem item0 = (ArrayExchangeItem)models[0].getExchangeItem(ids[0]);
		// values
		IArray valuesArray = item0.getValues();
		double values[] = valuesArray.getValuesAsDoubles();
		assertEquals(3*5*3,values.length);
		double delta=1.0E-8;
		assertEquals(-8.657673091235027E-4, values[15], delta);
		assertEquals(-0.10794060690367921, values[15+29], delta);

		ArrayExchangeItem item11 = (ArrayExchangeItem)models[1].getExchangeItem(ids[1]);
		//Values
		double values11[] = item11.getValues().getValuesAsDoubles();
		assertEquals(3*5*3,values11.length);
		assertEquals(-0.1938966950067343, values11[15], delta);
		assertEquals(0.11502807799798048, values11[44], delta);


		try {
			targetTime = TimeUtils.date2Mjd("201008241230");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		for (IStochModelInstance model : models) {
			model.compute(new Time(targetTime));
		}

		ArrayExchangeItem item02 = (ArrayExchangeItem) models[2].getExchangeItem(ids[0]);
		double values02[] = item02.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, values02.length);
		assertEquals(-0.09892819740275002, values02[5], delta);
		assertEquals(0.06776375651132155, values02[20], delta);

		ArrayExchangeItem item13 = (ArrayExchangeItem) models[3].getExchangeItem(ids[0]);
		double values13[] = item13.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, values13.length);
		assertEquals(0.2633708108815276, values13[6], delta);
		assertEquals(0.17063458293582934, values13[21], delta);

		try {
			targetTime = TimeUtils.date2Mjd("201008241240");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		for (IStochModelInstance model : models) {
			model.compute(new Time(targetTime));
		}

		double valuesc[] = item02.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, valuesc.length);
		assertEquals(-0.0605033630221406, valuesc[14], delta);
		assertEquals(-0.06965218966444367, valuesc[28], delta);

		double values1c[] = item11.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, values1c.length);
		assertEquals(-0.029133622729570444, values1c[13], delta);
		assertEquals(-0.3693470288643908, values1c[27], delta);
	}
	

	public void testMaps2dNoise_separable(){
		IStochModelFactory factory = new MapsNoiseModelFactory();
		String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+"<mapsNoiseModelConfig>"
				+"	<simulationTimespan timeFormat=\"dateTimeString\">201008241200,201008241210,...,201008241220</simulationTimespan>"
				+"	<noiseItem id=\"windU\" quantity=\"wind-u\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"true\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"	<noiseItem id=\"windV\" quantity=\"wind-v\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"true\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"</mapsNoiseModelConfig>";
		factory.initialize(testRunDataDir, new String[]{configString});
		IStochModelInstance model = factory.getInstance(OutputLevel.Debug);

		StochVector.setSeed(10111213);
		model.setAutomaticNoiseGeneration(true);

		double targetTime=0.0;
		try {
			targetTime=TimeUtils.date2Mjd("201008241220");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		model.compute(new Time(targetTime));

		//
		//check output items
		//
		//id's
		String[] ids=model.getExchangeItemIDs();
		assertEquals(2, ids.length);
		System.out.println("ids[0]="+ids[0]);
		assertEquals("windU", ids[0]);
		//item 'windU'
		ArrayExchangeItem item0 = (ArrayExchangeItem)model.getExchangeItem(ids[0]);
		String idCheck=item0.getId();
		System.out.println("item['windU']="+item0.toString());
		assertEquals("windU", idCheck);
		// values
		IArray valuesArray = item0.getValues();
		int dimensions[]=valuesArray.getDimensions();
		assertEquals(3,dimensions.length);
		assertEquals(3,dimensions[0]);  // time
		assertEquals(3,dimensions[1]);  // longitude y
		assertEquals(5,dimensions[2]);  // latitude x
		double values[] = valuesArray.getValuesAsDoubles();
		System.out.println("values="+new Vector(values));
		assertEquals(3*5*3,values.length);
		double delta=1.0E-8;
		System.out.println("values[15]="+values[15]);
		assertEquals(0.04444044051354764, values[15+0], delta); //initial value of 0 is boring.    	
		System.out.println("values[15+29]="+values[15+29]);
		assertEquals(-0.13799014454381836, values[15+29], delta);
		// quantity
		IQuantityInfo qInfo = item0.getQuantityInfo();
		String unit = qInfo.getUnit();
		assertEquals("m/s",unit);
		String quantity = qInfo.getQuantity();
		assertEquals("wind-u",quantity);
		// geometry
		ArrayGeometryInfo geomInfo = (ArrayGeometryInfo)item0.getGeometryInfo();
		IArray lon = geomInfo.getLongitudeArray();
		System.out.println("lon="+lon);
		assertEquals("lon","{-5.0,-2.5,0.0,2.5,5.0}",lon.toString());
		IArray lat = geomInfo.getLatitudeArray();
		System.out.println("lat="+lat);
		assertEquals("lat","{50.0,55.0,60.0}",lat.toString());

		ArrayExchangeItem item1 = (ArrayExchangeItem)model.getExchangeItem(ids[1]);
		String idCheck1=item1.getId();
		System.out.println("item['windV']="+item1.toString());
		assertEquals("windV", idCheck1);
		//Values
		IArray valuesArray1 = item1.getValues();
		int dimensions1[]=valuesArray1.getDimensions();
		double values1[] = valuesArray1.getValuesAsDoubles();
		System.out.println("values1="+new Vector(values1));
		assertEquals(3*5*3,values1.length);
		System.out.println("values1[15]="+values1[15]);
		assertEquals(-0.01558694998581234, values1[15+0], delta);
		System.out.println("values1[15+29]="+values1[15+29]);
		assertEquals(-0.4586428325241929, values1[15+29], delta);
		// quantity
		IQuantityInfo qInfo1 = item1.getQuantityInfo();
		String unit1 = qInfo1.getUnit();
		assertEquals("m/s",unit1);
		String quantity1 = qInfo1.getQuantity();
		assertEquals("wind-v",quantity1);
	}

	public void testMaps2dNoiseSeparableRecompute(){
		IStochModelFactory factory = new MapsNoiseModelFactory();
		String configString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+"<mapsNoiseModelConfig>"
				+"	<simulationTimespan timeFormat=\"dateTimeString\">201008241200,201008241210,...,201008241220</simulationTimespan>"
				+"	<noiseItem id=\"windU\" quantity=\"wind-u\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"true\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"	<noiseItem id=\"windV\" quantity=\"wind-v\" unit=\"m/s\" height=\"10.0\" "
				+"	 standardDeviation=\"1.0\" timeCorrelationScale=\"12.0\" timeCorrelationScaleUnit=\"hours\" "
				+"	 initialValue=\"0.0\" horizontalCorrelationScale=\"500\" horizontalCorrelationScaleUnit=\"km\" >"
				+"	 	<grid type=\"cartesian\" coordinates=\"wgs84\" separable=\"true\">"
				+"	 		<x>-5,-2.5,...,5</x>"
				+"	 		<y>50,55,...,60</y>"
				+"	 	</grid>"
				+"	</noiseItem>"
				+"</mapsNoiseModelConfig>";
		factory.initialize(testRunDataDir, new String[]{configString});

		StochVector.setSeed(10111213);
		IStochModelInstance[] models = new IStochModelInstance[]{factory.getInstance(OutputLevel.Debug), factory.getInstance(OutputLevel.Debug), factory.getInstance(OutputLevel.Debug), factory.getInstance(OutputLevel.Debug)};

		double targetTime=0.0;
		try {
			targetTime=TimeUtils.date2Mjd("201008241220");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		for (IStochModelInstance model : models) {
			model.setAutomaticNoiseGeneration(true);
			model.compute(new Time(targetTime));
		}

		String[] ids = models[0].getExchangeItemIDs();
		assertEquals(2, ids.length);
		ArrayExchangeItem item0 = (ArrayExchangeItem) models[0].getExchangeItem(ids[0]);

		IArray valuesArray = item0.getValues();
		double values[] = valuesArray.getValuesAsDoubles();
		assertEquals(3 * 5 * 3, values.length);
		double delta = 1.0E-8;
		assertEquals(0.044440440513547695, values[15], delta);
		assertEquals(-0.13799014454381836, values[15 + 29], delta);

		ArrayExchangeItem item11 = (ArrayExchangeItem) models[1].getExchangeItem(ids[1]);

		double values11[] = item11.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 3, values11.length);
		assertEquals(-0.2447698680730515, values11[15], delta);
		assertEquals(0.09416643539767057, values11[44], delta);


		try {
			targetTime = TimeUtils.date2Mjd("201008241230");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		for (IStochModelInstance model : models) {
			model.compute(new Time(targetTime));
		}

		ArrayExchangeItem item02 = (ArrayExchangeItem) models[2].getExchangeItem(ids[0]);
		double values02[] = item02.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, values02.length);
		assertEquals(0.06863119589582024, values02[5], delta);
		assertEquals(0.20564389358927038, values02[20], delta);

		ArrayExchangeItem item13 = (ArrayExchangeItem) models[3].getExchangeItem(ids[0]);
		double values13[] = item13.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, values13.length);
		assertEquals(0.17082941225155487, values13[6], delta);
		assertEquals(0.15717291807348657, values13[21], delta);

		try {
			targetTime = TimeUtils.date2Mjd("201008241240");
		} catch (Exception e) {
			throw new RuntimeException("invalid dateformat for targetTime");
		}
		for (IStochModelInstance model : models) {
			model.compute(new Time(targetTime));
		}

		double valuesc[] = item02.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, valuesc.length);
		assertEquals(-0.19859950505308566, valuesc[14], delta);
		assertEquals(-0.039060025056036785, valuesc[28], delta);

		double values1c[] = item11.getValues().getValuesAsDoubles();
		assertEquals(3 * 5 * 2, values1c.length);
		assertEquals(0.17064301817872968, values1c[13], delta);
		assertEquals(-0.07268958795944595, values1c[27], delta);
	}

	/*    public void testMaps2dNoise_config(){
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
    	assertEquals("times.length",70, times.length);
    	assertEquals("times[69]",55432.98611111117, times[69],0.00001);
    	double[] values=series1.getValuesAsDoubles();
    	assertEquals("values.length",70, values.length);
    	assertEquals("values[69]",0.05608590539600699, values[69],0.00001);
    }

    public void testMaps2dNoise_multistart(){
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
	 */
}
