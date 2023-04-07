/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.mywrapper;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IExchangeItem;
import org.openda.model_reactive_advection.myCopier;
import org.openda.model_reactive_advection.myWrapper;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;


public class myWrapperTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(myWrapperTest.class,"model_reactive_advection");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		myWrapper wrapper = new myWrapper();
		String[] args = {"reactive_pollution_model.input"};
		wrapper.initialize(testRunDataDir, args);

		String[] exchangeItemIds = wrapper.getExchangeItemIDs();

        for(String id:exchangeItemIds){
        	IExchangeItem exchangeItem = wrapper.getDataObjectExchangeItem(id);
        	String exId = exchangeItem.getId();
        	assertEquals(id, exId);
			if(exId.equalsIgnoreCase("source.factory1,discharge")){
				IExchangeItem ex = wrapper.getDataObjectExchangeItem(exchangeItemIds[0]);
				String quantityId = ((TimeSeries) ex).getQuantityId();
				assertEquals("ex.getQuantityId()", "discharge", quantityId);

				String unitId = ((TimeSeries) ex).getUnitId();
				assertEquals("ex.getUnitId()", "kg/s", unitId);

				IExchangeItem.ValueType valueType = ex.getValuesType();
				assertSame(valueType, IExchangeItem.ValueType.IArrayType);

				TimeSeries seriesRef = (TimeSeries) ex.getValues();
				double[] timesRef = seriesRef.getTimesRef();
				assertEquals("times[0]", 51513.0, timesRef[0], 0.0001);
				//public double[] getValuesAsDoubles();
				double[] valuesCopy = ex.getValuesAsDoubles();
				assertEquals("values[1]", 0.0, valuesCopy[0], 0.0001);
			}

			if(exchangeItem instanceof TimeSeries){
				TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
				TimeSeries series= (TimeSeries)exchangeItem;
				noosFormatter.writeToStandardOut(series);
			}else if(exchangeItem instanceof DoublesExchangeItem){
				System.out.println("Item["+exchangeItem.getId()+"] ="+ exchangeItem.toString());
			}
		}
	}

	public void testReadOutput() {
		myWrapper wrapper = new myWrapper();
		String[] args = {"reactive_pollution_model.output"};
		wrapper.initialize(testRunDataDir, args);
		//TODO fix data string
		String[] exchangeItemIDs = wrapper.getExchangeItemIDs();
		// TODO deze test heeft nooit iets gedaan omdat exchangeItems ids = output.c[1,2]_loc[A,B,C].concentration
		for(String itemId:exchangeItemIDs){
			IExchangeItem exchangeItem = wrapper.getDataObjectExchangeItem(itemId);
			String exId = exchangeItem.getId();
			assertEquals(itemId, exId);
			if(exId.equalsIgnoreCase("source.factory1,discharge")){
				IExchangeItem ex = wrapper.getDataObjectExchangeItem(exchangeItemIDs[0]);

				//String getQuantityId();
				String quantityId = ((TimeSeries) ex).getQuantityId();
				assertEquals("ex.getQuantityId()", "discharge", quantityId);

				//String getUnitId();
				String unitId = ((TimeSeries) ex).getUnitId();
				assertEquals("ex.getUnitId()", "kg/s", unitId);

				//TODO als IExchangeItem ex dan ex.getValuesType() test toevoegen
				// public Type getObjectType();
				//Type valueType = ex.getValueType();
				//assertTrue(valueType == org.openda.exchange.timeseries.TimeSeries.class);

				//public Object getValues();
				TimeSeries seriesRef = (TimeSeries) ex.getValues();
				double[] timesRef = seriesRef.getTimesRef();
				assertEquals("times[0]", 51513.0, timesRef[0], 0.0001);
				//public double[] getValuesAsDoubles();
				double[] valuesCopy = ex.getValuesAsDoubles();
				assertEquals("values[1]", 0.0, valuesCopy[0], 0.0001);
			}

			if(exchangeItem instanceof TimeSeries){
				TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
				noosFormatter.writeToStandardOut((TimeSeries)exchangeItem);
			}
		}
	}

	public void testWriteInput() {
		//First read input
		myWrapper wrapper = new myWrapper();
		String[] args = {"reactive_pollution_model_copy.input"};
		File original = new File(testRunDataDir,"reactive_pollution_model.input");
		File copy = new File(testRunDataDir,"reactive_pollution_model_copy.input");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		wrapper.initialize(testRunDataDir, args);

		String[] exchangeItemIDs = wrapper.getExchangeItemIDs();

		//change some things
		for(String id:exchangeItemIDs){
			IExchangeItem exchangeItem = wrapper.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);
			System.out.println("exchange item id = " + exId);
			if(exId.equalsIgnoreCase("concentration1.grid")){
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0]= 100.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if(exId.equalsIgnoreCase("source.c1_factory1.discharge")){
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0]= 123.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if(exId.equalsIgnoreCase("bound.c1_left.concentration")){
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0]= 10.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if(exId.equalsIgnoreCase("output.c1_locA.concentration")){
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0]= 102030.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if(exId.equalsIgnoreCase("reactionTime")){
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0]= 700.0;
				exchangeItem.setValuesAsDoubles(values);
			}
		}

		//write to file
		wrapper.finish();
		boolean containsLocA =testData.FileContains(copy, "102030");
		assertTrue(containsLocA);

		File reference = new File(testRunDataDir,"reactive_pollution_model_changed.input");
		boolean identical = testData.FilesAreIdentical(copy,reference);
		assertTrue(identical);
	}

	public void testCopyState() {
		String[] args = new String[2];
		File original = new File(testRunDataDir,"reactive_pollution_model.input");
		File destination = new File(testRunDataDir,"reactive_pollution_model_copy3.input");
		try {
			BBUtils.copyFile(original,destination);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+destination.getAbsolutePath());
		}

		File source = new File(testRunDataDir,"reactive_pollution_model.output");
		args[1]=destination.getAbsolutePath();
		args[0]=source.getAbsolutePath();
		myCopier.main(args);

		File reference = new File(testRunDataDir,"reactive_pollution_model.refinput3");
		boolean identical = testData.FilesAreIdentical(destination,reference);
		assertTrue(identical);
	}

}
