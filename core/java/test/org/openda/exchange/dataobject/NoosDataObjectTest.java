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
package org.openda.exchange.dataobject;

import junit.framework.TestCase;

import org.openda.exchange.dataobjects.NoosDataObject;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * @author johan, verlaanm
 *
 */
public class NoosDataObjectTest extends TestCase {
	private File              testRunDataDir;
	private OpenDaTestSupport testData;

	@Override
   protected void setUp() throws IOException {
		this.testData = new OpenDaTestSupport(NoosDataObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	@SuppressWarnings("boxing")
   public void testRead1() {
		System.out.println("-------------------------------------------------------------------");
		System.out.println("NoosDataObject - read a single file");
		System.out.println("-------------------------------------------------------------------");
		// read noos file and create object
		NoosDataObject noosDO = new NoosDataObject();
		noosDO.initialize(this.testRunDataDir, "aberdeen_waterlevel_astro.noos");

		String ids[]=noosDO.getExchangeItemIDs();
		assertEquals(1, ids.length);
		
		IExchangeItem noosEx = noosDO.getDataObjectExchangeItem(ids[0]);
		assertEquals("TimeSeries", noosEx.getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) (noosEx);
		assertEquals("aberdeen.waterlevel_astro", ts.getId());
		assertEquals("aberdeen", ts.getLocation());
		assertEquals("observed", ts.getSource());
		assertEquals("m", ts.getUnitId());
		assertEquals("waterlevel_astro", ts.getQuantityId());
		assertEquals(-2.045543, ts.getPosition()[0]);
		assertEquals(57.361939, ts.getPosition()[1]);

		assertEquals(54466.0, ts.getStartTime());
		assertEquals(54466.125, ts.getStopTime());

		assertEquals(19, ts.getTimes().length);
		assertEquals(54466.0, ts.getTimes()[0]);
		assertEquals(54466.125, ts.getTimes()[18]);

		assertEquals(19, ts.getValuesAsDoubles().length);
		assertEquals(-0.83, ts.getValuesAsDoubles()[0]);
		assertEquals(-0.6, ts.getValuesAsDoubles()[18]);

		assertEquals(0, ts.getExtraValuesKeySet().size());
	}

	   public void testRead2() {
			System.out.println("-------------------------------------------------------------------");
			System.out.println("NoosDataObject - read a multiple files");
			System.out.println("-------------------------------------------------------------------");
			// read noos files and create object
			NoosDataObject noosDO = new NoosDataObject();
			noosDO.initialize(this.testRunDataDir, "*_waterlevel_astro.noos");

			String ids[]=noosDO.getExchangeItemIDs();
			assertEquals(2, ids.length);
			
			IExchangeItem noosEx = noosDO.getDataObjectExchangeItem("den helder.waterlevel_astro");
			assertEquals("TimeSeries", noosEx.getClass().getSimpleName());

			TimeSeries ts = (TimeSeries) (noosEx);
			assertEquals("den helder.waterlevel_astro", ts.getId());
			assertEquals("den helder", ts.getLocation());
			assertEquals("observed", ts.getSource());
			assertEquals("m", ts.getUnitId());
			assertEquals("waterlevel_astro", ts.getQuantityId());
			assertEquals(4.745356, ts.getPosition()[0]);
			assertEquals(52.966001, ts.getPosition()[1]);

			assertEquals(54466.0, ts.getStartTime());
			assertEquals(54466.125, ts.getStopTime());

			assertEquals(19, ts.getTimes().length);
			assertEquals(54466.0, ts.getTimes()[0]);
			assertEquals(54466.125, ts.getTimes()[18]);

			assertEquals(19, ts.getValuesAsDoubles().length);
			assertEquals(0.73, ts.getValuesAsDoubles()[0]);
			assertEquals(0.0, ts.getValuesAsDoubles()[18]);

			assertEquals(0, ts.getExtraValuesKeySet().size());
		}

	   public void testWrite() {
			System.out.println("-------------------------------------------------------------------");
			System.out.println("NoosDataObject - read and write a multiple files");
			System.out.println("-------------------------------------------------------------------");
			// read noos files and create object
			NoosDataObject noosDO = new NoosDataObject();
			noosDO.initialize(this.testRunDataDir, "series*.noos");

			String ids[]=noosDO.getExchangeItemIDs();
			assertEquals(2, ids.length);

			// check original series
			IExchangeItem noosEx = noosDO.getDataObjectExchangeItem("location1.pressure");
			assertEquals("TimeSeries", noosEx.getClass().getSimpleName());

			TimeSeries ts = (TimeSeries) (noosEx);
			assertEquals("location1.pressure", ts.getId());
			assertEquals("location1", ts.getLocation());
			assertEquals("observed", ts.getSource());
			assertEquals("m", ts.getUnitId());
			assertEquals("pressure", ts.getQuantityId());
			assertEquals(-2.0, ts.getPosition()[0]);
			assertEquals(53.3, ts.getPosition()[1]);

			assertEquals(54466.0, ts.getStartTime());
			assertEquals(54466.0833333, ts.getStopTime(),1e-3);

			assertEquals(5, ts.getTimes().length);
			assertEquals(54466.0, ts.getTimes()[0]);
			assertEquals(54466.0833333, ts.getTimes()[4],1e-3);

			assertEquals(5, ts.getValuesAsDoubles().length);
			assertEquals(-0.83, ts.getValuesAsDoubles()[0]);
			assertEquals(-0.89, ts.getValuesAsDoubles()[4]);

			assertEquals(0, ts.getExtraValuesKeySet().size());
			
			// modify content
			Array values = (Array)ts.getValues();
			values=new Array("{0.,1.,2.,3.,4.}");
			ts.setValues(values);
			
			// write to disc
			noosDO.finish();
			
			// check file
			File outputFile = new File(this.testRunDataDir,"series1.noos");
			File referenceFile = new File(this.testRunDataDir,"series1.noos.ref");
			boolean test=testData.FilesAreIdentical(outputFile, referenceFile,3);
			assertTrue(test);
		}

	   public void testCreateWrite() {
			System.out.println("-------------------------------------------------------------------");
			System.out.println("NoosDataObject - create and write a multiple series");
			System.out.println("-------------------------------------------------------------------");
			// read noos files and create object
			NoosDataObject noosDO = new NoosDataObject();
			noosDO.initialize(this.testRunDataDir, ""); // no data yet

			String ids[]=noosDO.getExchangeItemIDs();
			assertEquals(0, ids.length); //empty	
			
			// create and add some timeseries
			double[] t1= new double[]{0.0,1.0,2.0,3.0,4.0,5.0};
			double[] v1= new double[]{1.0,1.1,1.2,1.3,1.4,1.5};
			TimeSeries ts1 = new TimeSeries(t1, v1, 0., 50., "manual input", "quantity1", "unit1", "location1");
			noosDO.addExchangeItem(ts1);
			double[] t2= new double[]{0.0,1.0,2.0,3.0,4.0,5.0};
			double[] v2= new double[]{2.0,2.1,2.2,2.3,2.4,2.5};
			TimeSeries ts2 = new TimeSeries(t2, v2, 0., 50., "manual input", "quantity2", "unit2", "location2");
			noosDO.addExchangeItem(ts2);
			
			// write and test
			noosDO.finish();
			
			File outputFile1 = new File(this.testRunDataDir,"quantity1_location1.noos");
			File referenceFile1 = new File(this.testRunDataDir,"location1_quantity1.noos.ref");
			boolean test1=testData.FilesAreIdentical(outputFile1, referenceFile1,3);
			assertTrue(test1);

			File outputFile2 = new File(this.testRunDataDir,"quantity2_location2.noos");
			File referenceFile2 = new File(this.testRunDataDir,"location2_quantity2.noos.ref");
			boolean test2=testData.FilesAreIdentical(outputFile2, referenceFile2,3);
			assertTrue(test2);
	   }

//	public void testWrite1() {
//
//		// read noos file and create object
//		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();
//
//		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData1.txt");
//		assertEquals(1, noosIO.getExchangeItems().length);
//		assertEquals(1, noosIO.getTimeSeriesSet().size());
//
//		// then write it again under another name
//		File noosFile = new File(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData1.txt");
//		NoosTimeSeriesIoObject.writeNoosTimeSeries((TimeSeries) noosIO.getExchangeItems()[0], noosFile);
//
//		// finally, test it
//		TimeSeries series1 = (TimeSeries) noosIO.getExchangeItems()[0];
//		NoosTimeSeriesIoObject noosIO2 = new NoosTimeSeriesIoObject();
//		noosIO2.initialize(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData1.txt");
//		TimeSeries series2 = (TimeSeries) noosIO2.getExchangeItems()[0];
//		assertTrue(series1.equals(series2));
//	}
//
//	@SuppressWarnings("boxing")
//   public void testRead2() {
//		// data with analysis times as additional column
//		// read noos file and create object
//		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();
//
//		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData2.txt");
//
//		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
//		assertEquals(1, noosEx.length);
//		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());
//
//		TimeSeries ts = (TimeSeries) (noosEx[0]);
//		assertEquals("hoekvanholland.waterlevel", ts.getId());
//		assertEquals("hoekvanholland", ts.getLocation());
//		assertEquals("observed", ts.getSource());
//		assertEquals("m", ts.getUnitId());
//		assertEquals("waterlevel", ts.getQuantityId());
//		assertEquals(4.120131, ts.getPosition()[0]);
//		assertEquals(51.978539, ts.getPosition()[1]);
//
//		assertEquals(55391.0, ts.getStartTime());
//		assertEquals(55392.0, ts.getStopTime());
//		assertTrue(ts.intersectsWithTimeInterval(55389.0, 55391.0));
//		assertTrue(ts.intersectsWithTimeInterval(55391.0, 55392.0));
//		assertTrue(ts.intersectsWithTimeInterval(55389.0, 55394.0));
//		assertTrue(ts.intersectsWithTimeInterval(55392.0, 55394.0));
//		assertFalse(ts.intersectsWithTimeInterval(55389.0, 55390.9));
//		assertFalse(ts.intersectsWithTimeInterval(55392.1, 55394.0));
//
//		assertEquals(145, ts.getTimes().length);
//		assertEquals(55391.0, ts.getTimes()[0]);
//		assertEquals(55392.0, ts.getTimes()[144]);
//
//		assertEquals(145, ts.getValuesAsDoubles().length);
//		assertEquals(-0.59, ts.getValuesAsDoubles()[0]);
//		assertEquals(-0.71, ts.getValuesAsDoubles()[144]);
//
//		assertEquals(1, ts.getExtraValuesKeySet().size());
//		assertTrue(ts.getExtraValuesKeySet().contains("analTimes"));
//	}
//
//	public void testWrite2() {
//
//		// read noos file and create object
//		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();
//
//		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData2.txt");
//		assertEquals(1, noosIO.getExchangeItems().length);
//
//		// then write it again under another name
//		File noosFile = new File(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData2.txt");
//		NoosTimeSeriesIoObject.writeNoosTimeSeries((TimeSeries) noosIO.getExchangeItems()[0], noosFile);
//
//		// finally, test it
//		TimeSeries series1 = (TimeSeries) noosIO.getExchangeItems()[0];
//		NoosTimeSeriesIoObject noosIO2 = new NoosTimeSeriesIoObject();
//		noosIO2.initialize(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData2.txt");
//		TimeSeries series2 = (TimeSeries) noosIO2.getExchangeItems()[0];
//		assertTrue(series1.equals(series2));
//	}
//
//	public void testRead3() {
//
//		// read noos files and create object
//		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();
//
//		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData*.txt");
//		assertEquals(2, noosIO.getTimeSeriesSet().size());
//
//		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
//		assertEquals(2, noosEx.length);
//		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());
//		assertEquals("TimeSeries", noosEx[1].getClass().getSimpleName());
//
//		TimeSeries ts0 = (TimeSeries) (noosEx[0]);
//		TimeSeries ts1 = (TimeSeries) (noosEx[1]);
//		assertTrue((ts0.hasExtraValues("analTimes") && !ts1.hasExtraValues("analTimes"))
//		      || (ts1.hasExtraValues("analTimes") && !ts0.hasExtraValues("analTimes")));
//	}
//
//	public void testWrite3() {
//
//		// read noos files and create object
//		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();
//
//		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData*.txt");
//		assertEquals(2, noosIO.getTimeSeriesSet().size());
//
//		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
//		assertEquals(2, noosEx.length);
//		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());
//		assertEquals("TimeSeries", noosEx[1].getClass().getSimpleName());
//
//		TimeSeries ts0 = (TimeSeries) (noosEx[0]);
//		TimeSeries ts1 = (TimeSeries) (noosEx[1]);
//		assertTrue((ts0.hasExtraValues("analTimes") && !ts1.hasExtraValues("analTimes"))
//		      || (ts1.hasExtraValues("analTimes") && !ts0.hasExtraValues("analTimes")));
//
//		ts0.setProperty(NoosTimeSeriesIoObject.PROPERTY_PATHNAME, new File(this.testRunDataDir, "test0.txt").getAbsolutePath());
//		ts1.setProperty(NoosTimeSeriesIoObject.PROPERTY_PATHNAME, new File(this.testRunDataDir, "test1.txt").getAbsolutePath());
//
//		noosIO.finish();
//
//		NoosTimeSeriesIoObject noosIO2 = new NoosTimeSeriesIoObject();
//
//		noosIO2.initialize(this.testRunDataDir, "test?.txt");
//		assertEquals(2, noosIO2.getTimeSeriesSet().size());
//
//		IPrevExchangeItem noosEx2[] = noosIO2.getExchangeItems();
//		TimeSeries ts0a = (TimeSeries) (noosEx2[0]);
//		TimeSeries ts1a = (TimeSeries) (noosEx2[1]);
//
//		assertTrue(ts0.equals(ts0a) || ts0.equals(ts1a));
//		assertTrue(ts1.equals(ts1a) || ts1.equals(ts0a));
//	}

}
