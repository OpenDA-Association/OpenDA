/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association
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
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.model_reactive_advection.myCopier;
import org.openda.model_reactive_advection.myWrapper;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;


public class myWrapperTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(myWrapperTest.class,"model_reactive_advection");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		IoObjectInterface ioObject = new myWrapper();
		String args[] = {};
		ioObject.initialize(testRunDataDir, "reactive_pollution_model.input", args);

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
		IoObjectInterface ioObject = new myWrapper();
		String args[] = {};
		ioObject.initialize(testRunDataDir, "reactive_pollution_model.output", args);
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
		IoObjectInterface ioObject = new myWrapper();
		String args[] = {};
		File original = new File(testRunDataDir,"reactive_pollution_model.input");
		File copy = new File(testRunDataDir,"reactive_pollution_model_copy.input");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		ioObject.initialize(testRunDataDir, "reactive_pollution_model_copy.input", args);

		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();

		//change some things
		for(int item=0;item<exchangeItems.length;item++){
			System.out.println("item = "+item+" id="+exchangeItems[item].getId());
			if(exchangeItems[item].getId().equalsIgnoreCase("concentration1.grid")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 100.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("source.c1_factory1.discharge")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 123.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("bound.c1_left.concentration")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 10.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("output.c1_locA.concentration")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 102030.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
			if(exchangeItems[item].getId().equalsIgnoreCase("reactionTime")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 700.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
		}

		//write to file
		ioObject.finish();
		boolean containsLocA =testData.FileContains(copy, "102030");
		assertTrue(containsLocA);

		File reference = new File(testRunDataDir,"reactive_pollution_model_changed.input");
		boolean identical = testData.FilesAreIdentical(copy,reference);
		assertTrue(identical);
	}

	public void testCopyState() {
		String args[] = new String[2];
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
