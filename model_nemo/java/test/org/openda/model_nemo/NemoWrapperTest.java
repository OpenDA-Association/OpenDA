/* OpenDA v2.4.1 
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
package org.openda.model_nemo;

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


public class NemoWrapperTest extends TestCase {

	OpenDaTestSupport testData = null;
	private File testRunDataDir;
	private File testCopyDir;


	protected void setUp() throws IOException {
	    testData = new OpenDaTestSupport(NemoWrapperTest.class, "model_nemo");
		testRunDataDir = testData.getTestRunDataDir();
		testCopyDir = new File(testRunDataDir,"copy");
	}


	public void testReadInput() {

		IoObjectInterface ioObject = new NemoRestartFileWrapper();
		String args[] = {};
		ioObject.initialize(testRunDataDir, "SQB_00010368_restart.nc", args);

		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();

        for(int item=0;item<exchangeItems.length;item++){

			if(exchangeItems[item].getId().equalsIgnoreCase("gcx(t=1, y=81, x=121)")){
				IPrevExchangeItem ex = exchangeItems[item];
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()","gcx(t=1, y=81, x=121)", id);

/*				//String getQuantityId();
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
*/
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

	public void testWriteInput() {
		IoObjectInterface ioObjectNameList = new NemoNamelistFileWrapper();
		IoObjectInterface ioObjectRestartList = new NemoRestartFileWrapper();

			String args[] = {};
		    // Copy the relevant files to a "copy" directory such that originals are not overwritten
			File original1 = new File(testRunDataDir,"namelist");
			File copy1 = new File(testRunDataDir+File.separator+"copy","namelist");

			File original2 = new File(testRunDataDir,"SQB_00010368_restart.nc");
			File copy2 = new File(testRunDataDir+File.separator+"copy","SQB_00010368_restart.nc");
			try {
					BBUtils.copyFile(original1, copy1);
					BBUtils.copyFile(original2, copy2);
				} catch (IOException e) {
					throw new RuntimeException("Could not copy files");
			}

		ioObjectNameList.initialize(testCopyDir, "namelist", args);
		ioObjectRestartList.initialize(testCopyDir, "SQB_00010368_restart.nc", args);


		IPrevExchangeItem[] exchangeItems = ioObjectRestartList.getExchangeItems();

		//change some things
		for(int item=0;item<exchangeItems.length;item++){
			System.out.println("item = "+item+" id="+exchangeItems[item].getId());
			if(exchangeItems[item].getId().equalsIgnoreCase("gcx(t=1, y=81, x=121)")){
				double values[] = exchangeItems[item].getValuesAsDoubles();
				values[0]= 100.0;
				exchangeItems[item].setValuesAsDoubles(values);
			}
		}

		//write to file
		ioObjectRestartList.finish();
//		boolean containsLocA =testData.FileContains(copy, "102030");
//		assertTrue(containsLocA);

//		File reference = new File(testRunDataDir,"reactive_pollution_model_changed.input");
//		boolean identical = testData.FilesAreIdentical(copy,reference);
//		assertTrue(identical);
	}


	public void testReadWriteTime() {
	   if (false){
			IoObjectInterface ioObject = new NemoNamelistFileWrapper();
			String args[] = {};
		    // Copy the relevant files to a "copy" directory such that originals are not overwritten
			File original1 = new File(testRunDataDir,"namelist");
			File copy1 = new File(testRunDataDir+File.separator+"copy","namelist");

			File original2 = new File(testRunDataDir,"SQB_00010368_restart.nc");
			File copy2 = new File(testRunDataDir+File.separator+"copy","SQB_00010368_restart.nc");
			try {
					BBUtils.copyFile(original1, copy1);
					BBUtils.copyFile(original2, copy2);
				} catch (IOException e) {
					throw new RuntimeException("Could not copy files");
			}
		ioObject.initialize(testCopyDir, "SQB_00010368_restart.nc", args);

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
					assertEquals(51513.208333, endAsMjd[0], 0.0001);
					// change
					endAsMjd[0]=51513.0+5.0/60.0/24.0; //start+5minutes
					item.setValuesAsDoubles(endAsMjd);
				}
			}
			ioObject.finish();

//			File reference = new File(testRunDataDir,"reactive_pollution_model.refinput");
//			boolean identical = testData.FilesAreIdentical(copy,reference);
//			assertTrue(identical);

	   }
	   }




}
