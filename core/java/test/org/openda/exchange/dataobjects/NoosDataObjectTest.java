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
package org.openda.exchange.dataobjects;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;

/**
 * @author johan, verlaanm
 *
 */
public class NoosDataObjectTest extends TestCase {
	private File              testRunDataDir;
	private OpenDaTestSupport testData;

	
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
		noosDO.initialize(this.testRunDataDir, new String[]{"aberdeen_waterlevel_astro.noos"});

		String[] ids =noosDO.getExchangeItemIDs();
		assertEquals(1, ids.length);
		
		IExchangeItem noosEx = noosDO.getDataObjectExchangeItem(ids[0]);
		assertEquals("TimeSeries", noosEx.getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) (noosEx);
		assertEquals("Aberdeen.waterlevel_astro", ts.getId());
		assertEquals("Aberdeen", ts.getLocation());
		assertEquals("observed", ts.getSource());
		assertEquals("m", ts.getUnitId());
		assertEquals("waterlevel_astro", ts.getQuantityId());
		assertEquals(-2.073333, ts.getPosition()[0]);
		assertEquals(57.143333, ts.getPosition()[1]);

		assertEquals("202001010000", TimeUtils.mjdToString(ts.getStartTime()));
		assertEquals("202001010300", TimeUtils.mjdToString(ts.getStopTime()));

		assertEquals(19, ts.getTimes().length);
		assertEquals("202001010000", TimeUtils.mjdToString(ts.getTimes()[0]));
		assertEquals("202001010300", TimeUtils.mjdToString(ts.getTimes()[18]));

		assertEquals(19, ts.getValuesAsDoubles().length);
		assertEquals(-1.09, ts.getValuesAsDoubles()[0]);
		assertEquals(0.4, ts.getValuesAsDoubles()[18]);

		assertEquals(0, ts.getExtraValuesKeySet().size());
	}

	   public void testRead2() {
			System.out.println("-------------------------------------------------------------------");
			System.out.println("NoosDataObject - read a multiple files");
			System.out.println("-------------------------------------------------------------------");
			// read noos files and create object
			NoosDataObject noosDO = new NoosDataObject();
			noosDO.initialize(this.testRunDataDir, new String[]{"*_waterlevel*.noos"});

			String[] ids =noosDO.getExchangeItemIDs();
			assertEquals(2, ids.length);
			
			IExchangeItem noosEx = noosDO.getDataObjectExchangeItem("Den Helder.waterlevel");
			assertEquals("TimeSeries", noosEx.getClass().getSimpleName());

			TimeSeries ts = (TimeSeries) (noosEx);
			assertEquals("Den Helder.waterlevel", ts.getId());
			assertEquals("Den Helder", ts.getLocation());
			assertEquals("dcsm_v6_kf_hirlam", ts.getSource());
			assertEquals("m", ts.getUnitId());
			assertEquals("waterlevel", ts.getQuantityId());
			assertEquals(4.745326, ts.getPosition()[0]);
			assertEquals(52.965441, ts.getPosition()[1]);

		    assertEquals("202001010000", TimeUtils.mjdToString(ts.getStartTime()));
		    assertEquals("202001010300", TimeUtils.mjdToString(ts.getStopTime()));

			assertEquals(19, ts.getTimes().length);
		    assertEquals("202001010000", TimeUtils.mjdToString(ts.getTimes()[0]));
		    assertEquals("202001010300", TimeUtils.mjdToString(ts.getTimes()[18]));

			assertEquals(19, ts.getValuesAsDoubles().length);
			assertEquals(0.0997, ts.getValuesAsDoubles()[0]);
			assertEquals(-0.7394, ts.getValuesAsDoubles()[18]);

			assertEquals(0, ts.getExtraValuesKeySet().size());
		}

	   public void testWrite() {
			System.out.println("-------------------------------------------------------------------");
			System.out.println("NoosDataObject - read and write a multiple files");
			System.out.println("-------------------------------------------------------------------");
			// read noos files and create object
			NoosDataObject noosDO = new NoosDataObject();
			noosDO.initialize(this.testRunDataDir, new String[]{"series*.noos"});

			String[] ids =noosDO.getExchangeItemIDs();
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

		    assertEquals("200801010000", TimeUtils.mjdToString(ts.getStartTime()));
		    assertEquals("200801010200", TimeUtils.mjdToString(ts.getStopTime()));

			assertEquals(5, ts.getTimes().length);
			assertEquals("200801010000", TimeUtils.mjdToString(ts.getTimes()[0]));
			assertEquals("200801010200", TimeUtils.mjdToString(ts.getTimes()[4]));

			assertEquals(5, ts.getValuesAsDoubles().length);
			assertEquals(-0.83, ts.getValuesAsDoubles()[0]);
			assertEquals(-0.89, ts.getValuesAsDoubles()[4]);

			assertEquals(0, ts.getExtraValuesKeySet().size());
			
			// modify content
			Array values;
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
			noosDO.initialize(this.testRunDataDir, new String[]{""}); // no data yet

			String[] ids =noosDO.getExchangeItemIDs();
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

	public void testReadTimeZoneMET() throws ParseException {

		// read noos file and create object
		NoosDataObject noosDO = new NoosDataObject();

		noosDO.initialize(this.testRunDataDir, new String[]{"NoosTimeSeriesDataObjectTestMETData.txt"});

		String[] ids = noosDO.getExchangeItemIDs();
		assertEquals(1, ids.length);
		IExchangeItem noosEx = noosDO.getDataObjectExchangeItem(ids[0]);

		assertEquals("TimeSeries", noosEx.getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) noosEx;
		assertEquals("Hoek van Holland.waterlevel", ts.getId());
		assertEquals("Hoek van Holland", ts.getLocation());
		assertEquals("dcsm_v6_kf_hirlam", ts.getSource());
		assertEquals("m", ts.getUnitId());
		assertEquals("waterlevel", ts.getQuantityId());
		assertEquals(4.120131, ts.getPosition()[0]);
		assertEquals(51.978539, ts.getPosition()[1]);

		//Note: in NoosTimeSeriesFormatter, a correction from the actual time zone to GMT is done,
		// so therefore there is 1 hour difference with the noos data.
		assertEquals("201912312300", TimeUtils.mjdToString(ts.getStartTime()));
		assertEquals("202001010200", TimeUtils.mjdToString(ts.getStopTime()));
		assertTrue(ts.intersectsWithTimeInterval(TimeUtils.date2Mjd("201912310000"), TimeUtils.date2Mjd("202001010200")));
		assertTrue(ts.intersectsWithTimeInterval(TimeUtils.date2Mjd("202001010100"), TimeUtils.date2Mjd("202001020000")));
		assertTrue(ts.intersectsWithTimeInterval(TimeUtils.date2Mjd("201912290000"), TimeUtils.date2Mjd("202001030000")));
		assertTrue(ts.intersectsWithTimeInterval(TimeUtils.date2Mjd("201912312300"), TimeUtils.date2Mjd("202001030000")));
		assertFalse(ts.intersectsWithTimeInterval(TimeUtils.date2Mjd("201912290000"), TimeUtils.date2Mjd("201912312200")));
		assertFalse(ts.intersectsWithTimeInterval(TimeUtils.date2Mjd("202001010400"), TimeUtils.date2Mjd("202001030000")));

		assertEquals(19, ts.getTimes().length);
		assertEquals("201912312300", TimeUtils.mjdToString(ts.getTimes()[0]));
		assertEquals("202001010200", TimeUtils.mjdToString(ts.getTimes()[18]));

		assertEquals(19, ts.getValuesAsDoubles().length);
		assertEquals(-0.5444, ts.getValuesAsDoubles()[0]);
		assertEquals(-0.4781, ts.getValuesAsDoubles()[18]);

		assertEquals(0, ts.getExtraValuesKeySet().size());
		assertEquals("MET", ts.getProperty(NoosTimeSeriesFormatter.PROPERTY_TIMEZONE));
	}

	public void testWriteTimeZoneMET() {

		// read noos file and create object
		NoosDataObject noosDO = new NoosDataObject();
		noosDO.initialize(this.testRunDataDir, new String[]{"NoosTimeSeriesDataObjectTestMETData.txt"});

		String[] ids = noosDO.getExchangeItemIDs();
		assertEquals(1, ids.length);
		IExchangeItem noosEx = noosDO.getDataObjectExchangeItem(ids[0]);

		// then write it again under another name
		File noosFile = new File(this.testRunDataDir, "copy_of_NoosTimeSeriesDataObjectTestMETData.txt");
		NoosDataObject.writeNoosTimeSeries((TimeSeries) noosEx, noosFile);

		// finally, test it
		TimeSeries series1 = (TimeSeries) noosEx;
		NoosDataObject noosDO2 = new NoosDataObject();
		noosDO2.initialize(this.testRunDataDir, new String[]{"copy_of_NoosTimeSeriesDataObjectTestMETData.txt"});

		ids = noosDO2.getExchangeItemIDs();
		IExchangeItem noosEx2 = noosDO2.getDataObjectExchangeItem(ids[0]);
		TimeSeries series2 = (TimeSeries) noosEx2;
		assertTrue(series1.equals(series2));
	}

}
