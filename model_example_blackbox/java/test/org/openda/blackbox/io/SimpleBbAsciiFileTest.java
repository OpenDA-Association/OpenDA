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
package org.openda.blackbox.io;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;

public class SimpleBbAsciiFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(SimpleBbAsciiFileTest.class,"model_example_blackbox");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		IoObjectInterface ioObject = new SimpleBbAsciiFile();
		String args[] = {};
		ioObject.initialize(testRunDataDir, "pollution_model.input", args);

		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();

        for(int item=0;item<exchangeItems.length;item++){

			if(exchangeItems[item].getId().equalsIgnoreCase("source.factory1,discharge")){
				IPrevExchangeItem ex = exchangeItems[0];
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()", "source.factory1.discharge", id);

				//String getQuantityId();
				String quantityId = ((TimeSeries) ex).getQuantityId();
				assertEquals("ex.getQuantityId()", "discharge", quantityId);

				//String getUnitId();
				String unitId = ((TimeSeries) ex).getUnitId();
				assertEquals("ex.getUnitId()", "kg/s", unitId);

				//public Type getObjectType(); //
				Type valueType = ex.getValueType();
				assertTrue(valueType == org.openda.exchange.timeseries.TimeSeries.class);

				//public Object getValues();
				TimeSeries seriesRef = (TimeSeries) ex.getValues();
				double[] timesRef = seriesRef.getTimesRef();
				assertEquals("times[0]", 51513.0, timesRef[0], 0.0001);
				//public double[] getValuesAsDoubles();
				double[] valuesCopy = ex.getValuesAsDoubles();
				assertEquals("values[1]", 0.0, valuesCopy[0], 0.0001);
			}

			if(exchangeItems[item] instanceof TimeSeries){
				TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
				TimeSeries series= (TimeSeries)exchangeItems[item];
				noosFormatter.writeToStandardOut(series);
			}else if(exchangeItems[item] instanceof DoublesExchangeItem){
				System.out.println("Item["+exchangeItems[item].getId()+"] ="+ exchangeItems[item].toString());
			}
		}
	}

	public void testReadOutput() {
		IoObjectInterface ioObject = new SimpleBbAsciiFile();
		String args[] = {};
		ioObject.initialize(testRunDataDir, "pollution_model.refoutput", args);
		//TODO fix data string
		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();
		for(int item=0;item<exchangeItems.length;item++){
			if(exchangeItems[item].getId().equalsIgnoreCase("source.factory1,discharge")){
				IPrevExchangeItem ex = exchangeItems[0];
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()", "source.factory1.discharge", id);

				//String getQuantityId();
				String quantityId = ((TimeSeries) ex).getQuantityId();
				assertEquals("ex.getQuantityId()", "discharge", quantityId);

				//String getUnitId();
				String unitId = ((TimeSeries) ex).getUnitId();
				assertEquals("ex.getUnitId()", "kg/s", unitId);

				//public Type getObjectType(); //
				Type valueType = ex.getValueType();
				assertTrue(valueType == org.openda.exchange.timeseries.TimeSeries.class);

				//public Object getValues();
				TimeSeries seriesRef = (TimeSeries) ex.getValues();
				double[] timesRef = seriesRef.getTimesRef();
				assertEquals("times[0]", 51513.0, timesRef[0], 0.0001);
				//public double[] getValuesAsDoubles();
				double[] valuesCopy = ex.getValuesAsDoubles();
				assertEquals("values[1]", 0.0, valuesCopy[0], 0.0001);
			}

			if(exchangeItems[item] instanceof TimeSeries){
				TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
				noosFormatter.writeToStandardOut((TimeSeries)exchangeItems[item]);
			}
		}
	}

	public void testWriteInput() {
		//First read input
		IoObjectInterface ioObject = new SimpleBbAsciiFile();
		String args[] = {};
		File original = new File(testRunDataDir,"pollution_model.input");
		File copy = new File(testRunDataDir,"pollution_model_copy.input");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		ioObject.initialize(testRunDataDir, "pollution_model_copy.input", args);

		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();

		//change some things
		for(int item=0;item<exchangeItems.length;item++){
			System.out.println("item = "+item+" id="+exchangeItems[item].getId());
			if(exchangeItems[item].getId().equalsIgnoreCase("concentration.grid")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 100.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("source.factory1.discharge")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 123.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("bound.left.concentration")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 10.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("output.locA.concentration")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 102030.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
		}

		//write to file
		ioObject.finish();
		File reference = new File(testRunDataDir,"pollution_model_changed.input");
		boolean containsLocA =testData.FileContains(reference, "102030");
		assertTrue(containsLocA);
		boolean identical = testData.FilesAreIdentical(copy,reference);
		assertTrue(identical);
	}

	public void testReadWriteTime() {

		IoObjectInterface ioObject = new SimpleBbAsciiFile();
		String args[] = {};
		File original = new File(testRunDataDir,"pollution_model.input");
		File copy = new File(testRunDataDir,"pollution_model_copy2.input");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		ioObject.initialize(testRunDataDir, "pollution_model_copy2.input", args);

		for(IPrevExchangeItem item:ioObject.getExchangeItems()){
			String itemId = item.getId();
			System.out.println("looking at item: "+itemId);
			if(itemId.equalsIgnoreCase("startTime")){
				System.out.println("changing item: "+itemId);
				double startAsMjd[] = item.getValuesAsDoubles();
				System.out.println("startTime="+startAsMjd[0]);
				assertEquals(51513.0, startAsMjd[0], 0.0001);
				//change
				startAsMjd[0]=startAsMjd[0]+2.0/60.0/24.0; //start+2minutes
				item.setValuesAsDoubles(startAsMjd);
			}
			if(itemId.equalsIgnoreCase("endTime")){
				System.out.println("changing item: "+itemId);
				double endAsMjd[] = item.getValuesAsDoubles();
				System.out.println("endTime="+endAsMjd[0]);
				assertEquals(51513.069444444, endAsMjd[0], 0.0001);
				// change
				endAsMjd[0]=51513.0+5.0/60.0/24.0; //start+5minutes
				item.setValuesAsDoubles(endAsMjd);
			}
		}
		ioObject.finish();

		File reference = new File(testRunDataDir,"pollution_model.refinput");
		boolean identical = testData.FilesAreIdentical(copy,reference);
		assertTrue(identical);

	}

	public void testCopyState() {
		String args[] = new String[2];
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
