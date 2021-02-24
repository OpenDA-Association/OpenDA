/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.blackbox.io;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class SimpleBbAsciiFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(SimpleBbAsciiFileTest.class,"model_example_blackbox");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		SimpleBbAsciiFile dataObject = new SimpleBbAsciiFile();
		String[] args = {"pollution_model.input"};
		dataObject.initialize(testRunDataDir, args);

		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();

        for(String id:exchangeItemIDs){
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);

			if(exId.equalsIgnoreCase("source.factory1,discharge")){
				assertEquals("exId", "source.factory1.discharge", exId);

				String quantityId = ((TimeSeries) exchangeItem).getQuantityId();
				assertEquals("ex.getQuantityId()", "discharge", quantityId);

				String unitId = ((TimeSeries) exchangeItem).getUnitId();
				assertEquals("ex.getUnitId()", "kg/s", unitId);

				// TODO weer activeren als IExchangeItem ex
				//public Type getObjectType();
				//Type valueType = ex.getValueType();
				//assertTrue(valueType == org.openda.exchange.timeseries.TimeSeries.class);

				//public Object getValues();
				TimeSeries seriesRef = (TimeSeries) exchangeItem.getValues();
				double[] timesRef = seriesRef.getTimesRef();
				assertEquals("times[0]", 51513.0, timesRef[0], 0.0001);
				//public double[] getValuesAsDoubles();
				double[] valuesCopy = exchangeItem.getValuesAsDoubles();
				assertEquals("values[1]", 0.0, valuesCopy[0], 0.0001);
			}

			if(exchangeItem instanceof TimeSeries){
				TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
				TimeSeries series= (TimeSeries)exchangeItem;
				noosFormatter.writeToStandardOut(series);
			}else if(exchangeItem instanceof DoublesExchangeItem){
				System.out.println("Item "+exId+" ="+ exchangeItem.toString());
			}
		}
	}

	public void testReadOutput() {
		SimpleBbAsciiFile dataObject = new SimpleBbAsciiFile();
		String[] args = {"pollution_model.refoutput"};
		dataObject.initialize(testRunDataDir, args);
		//TODO fix data string
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		for(String id:exchangeItemIDs){
			IExchangeItem ex = dataObject.getDataObjectExchangeItem(id);
			String exId = ex.getId();
			assertEquals(id, exId);
			if(exId.equalsIgnoreCase("source.factory1,discharge")){
				assertEquals("exId", "source.factory1.discharge", exId);

				String quantityId = ((TimeSeries) ex).getQuantityId();
				assertEquals("ex.getQuantityId()", "discharge", quantityId);

				String unitId = ((TimeSeries) ex).getUnitId();
				assertEquals("ex.getUnitId()", "kg/s", unitId);

                // TODO weer activeren als IExchangeItem ex
				//public Type getObjectType(); //
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

			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem(exId);
			if(exchangeItem instanceof TimeSeries){
				TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
				noosFormatter.writeToStandardOut((TimeSeries)exchangeItem);
			}
		}
	}

	public void testWriteInput() {
		//First read input
		SimpleBbAsciiFile dataObject = new SimpleBbAsciiFile();
		String[] args = {"pollution_model_copy.input"};
		File original = new File(testRunDataDir,"pollution_model.input");
		File copy = new File(testRunDataDir,"pollution_model_copy.input");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		dataObject.initialize(testRunDataDir, args);

		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();

		//change some things
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			String id = exchangeItemIDs[i];
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);
			System.out.println("item = " + i + " id=" + exId);
			if (exId.equalsIgnoreCase("concentration.grid")) {
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0] = 100.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if (exId.equalsIgnoreCase("source.factory1.discharge")) {
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0] = 123.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if (exId.equalsIgnoreCase("bound.left.concentration")) {
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0] = 10.0;
				exchangeItem.setValuesAsDoubles(values);
			}
			if (exId.equalsIgnoreCase("output.locA.concentration")) {
				double[] values = exchangeItem.getValuesAsDoubles();
				values[0] = 102030.0;
				exchangeItem.setValuesAsDoubles(values);
			}
		}

		//write to file
		dataObject.finish();
		File reference = new File(testRunDataDir,"pollution_model_changed.input");
		boolean containsLocA =testData.FileContains(reference, "102030");
		assertTrue(containsLocA);
		boolean identical = testData.FilesAreIdentical(copy,reference);
		assertTrue(identical);
	}

	public void testReadWriteTime() {

		SimpleBbAsciiFile dataObject = new SimpleBbAsciiFile();
		String[] args = {"pollution_model_copy2.input"};
		File original = new File(testRunDataDir,"pollution_model.input");
		File copy = new File(testRunDataDir,"pollution_model_copy2.input");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		dataObject.initialize(testRunDataDir, args);

		for(String id:dataObject.getExchangeItemIDs()){
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);
			System.out.println("looking at item: "+exId);
			if(exId.equalsIgnoreCase("startTime")){
				System.out.println("changing item: "+exId);
				double[] startAsMjd = exchangeItem.getValuesAsDoubles();
				System.out.println("startTime="+startAsMjd[0]);
				assertEquals(51513.0, startAsMjd[0], 0.0001);
				//change
				startAsMjd[0]=startAsMjd[0]+2.0/60.0/24.0; //start+2minutes
				exchangeItem.setValuesAsDoubles(startAsMjd);
			}
			if(exId.equalsIgnoreCase("endTime")){
				System.out.println("changing item: "+exId);
				double[] endAsMjd = exchangeItem.getValuesAsDoubles();
				System.out.println("endTime="+endAsMjd[0]);
				assertEquals(51513.069444444, endAsMjd[0], 0.0001);
				// change
				endAsMjd[0]=51513.0+5.0/60.0/24.0; //start+5minutes
				exchangeItem.setValuesAsDoubles(endAsMjd);
			}
		}
		dataObject.finish();

		File reference = new File(testRunDataDir,"pollution_model.refinput");
		boolean identical = testData.FilesAreIdentical(copy,reference);
		assertTrue(identical);

	}

	public void testCopyState() {
		String[] args = new String[2];
		File original = new File(testRunDataDir,"pollution_model.input");
		File destination = new File(testRunDataDir,"pollution_model_copy3.input");
		try {
			BBUtils.copyFile(original,destination);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+destination.getAbsolutePath());
		}

		File source = new File(testRunDataDir,"pollution_model.refoutput");
		args[1]=destination.getAbsolutePath();
		args[0]=source.getAbsolutePath();
		SimpleBbAsciiCopier.main(args);

		File reference = new File(testRunDataDir,"pollution_model.refinput3");
		boolean identical = testData.FilesAreIdentical(destination,reference);
		assertTrue(identical);
	}

//  TODO: check with MV (SimpleWait was not added to repos)
// 	public void testWait() {
//		String args[] = new String[]{"1.0"}; //wait some seconds
//		long tstart = System.currentTimeMillis();
//		SimpleWait.main(args);
//		long tafter = System.currentTimeMillis();
//		int dif = (int)(tafter-tstart);
//		assertTrue(dif>900); //more than 0.9sec
//		assertTrue(dif<1100); //more than 1.1sec
//	}
}
