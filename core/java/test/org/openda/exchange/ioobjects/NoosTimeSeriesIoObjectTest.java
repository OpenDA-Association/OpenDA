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
package org.openda.exchange.ioobjects;

import junit.framework.TestCase;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * @author johan
 *
 */
public class NoosTimeSeriesIoObjectTest extends TestCase {
	private File              testRunDataDir;
	private OpenDaTestSupport testData;

	@Override
   protected void setUp() throws IOException {
		this.testData = new OpenDaTestSupport(NoosTimeSeriesIoObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	@SuppressWarnings("boxing")
   public void testRead1() {

		// read noos file and create object
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();

		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData1.txt");

		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
		assertEquals(1, noosEx.length);
		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) (noosEx[0]);
		assertEquals("hoekvanholland.waterlevel", ts.getId());
		assertEquals("hoekvanholland", ts.getLocation());
		assertEquals("hmcn_csm8", ts.getSource());
		assertEquals("m", ts.getUnitId());
		assertEquals("waterlevel", ts.getQuantityId());
		assertEquals(4.120131, ts.getPosition()[0]);
		assertEquals(51.978539, ts.getPosition()[1]);

		assertEquals(55391.0, ts.getStartTime());
		assertEquals(55392.0, ts.getStopTime());
		assertTrue(ts.intersectsWithTimeInterval(55389.0, 55391.0));
		assertTrue(ts.intersectsWithTimeInterval(55391.0, 55392.0));
		assertTrue(ts.intersectsWithTimeInterval(55389.0, 55394.0));
		assertTrue(ts.intersectsWithTimeInterval(55392.0, 55394.0));
		assertFalse(ts.intersectsWithTimeInterval(55389.0, 55390.9));
		assertFalse(ts.intersectsWithTimeInterval(55392.1, 55394.0));

		assertEquals(145, ts.getTimes().length);
		assertEquals(55391.0, ts.getTimes()[0]);
		assertEquals(55392.0, ts.getTimes()[144]);

		assertEquals(145, ts.getValuesAsDoubles().length);
		assertEquals(-0.54, ts.getValuesAsDoubles()[0]);
		assertEquals(-0.77, ts.getValuesAsDoubles()[144]);

		assertEquals(0, ts.getExtraValuesKeySet().size());
	}

	public void testWrite1() {

		// read noos file and create object
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();

		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData1.txt");
		assertEquals(1, noosIO.getExchangeItems().length);
		assertEquals(1, noosIO.getTimeSeriesSet().size());

		// then write it again under another name
		File noosFile = new File(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData1.txt");
		NoosTimeSeriesIoObject.writeNoosTimeSeries((TimeSeries) noosIO.getExchangeItems()[0], noosFile);

		// finally, test it
		TimeSeries series1 = (TimeSeries) noosIO.getExchangeItems()[0];
		NoosTimeSeriesIoObject noosIO2 = new NoosTimeSeriesIoObject();
		noosIO2.initialize(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData1.txt");
		TimeSeries series2 = (TimeSeries) noosIO2.getExchangeItems()[0];
		assertTrue(series1.equals(series2));
	}

	@SuppressWarnings("boxing")
   public void testRead2() {
		// data with analysis times as additional column
		// read noos file and create object
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();

		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData2.txt");

		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
		assertEquals(1, noosEx.length);
		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) (noosEx[0]);
		assertEquals("hoekvanholland.waterlevel", ts.getId());
		assertEquals("hoekvanholland", ts.getLocation());
		assertEquals("observed", ts.getSource());
		assertEquals("m", ts.getUnitId());
		assertEquals("waterlevel", ts.getQuantityId());
		assertEquals(4.120131, ts.getPosition()[0]);
		assertEquals(51.978539, ts.getPosition()[1]);

		assertEquals(55391.0, ts.getStartTime());
		assertEquals(55392.0, ts.getStopTime());
		assertTrue(ts.intersectsWithTimeInterval(55389.0, 55391.0));
		assertTrue(ts.intersectsWithTimeInterval(55391.0, 55392.0));
		assertTrue(ts.intersectsWithTimeInterval(55389.0, 55394.0));
		assertTrue(ts.intersectsWithTimeInterval(55392.0, 55394.0));
		assertFalse(ts.intersectsWithTimeInterval(55389.0, 55390.9));
		assertFalse(ts.intersectsWithTimeInterval(55392.1, 55394.0));

		assertEquals(145, ts.getTimes().length);
		assertEquals(55391.0, ts.getTimes()[0]);
		assertEquals(55392.0, ts.getTimes()[144]);

		assertEquals(145, ts.getValuesAsDoubles().length);
		assertEquals(-0.59, ts.getValuesAsDoubles()[0]);
		assertEquals(-0.71, ts.getValuesAsDoubles()[144]);

		assertEquals(1, ts.getExtraValuesKeySet().size());
		assertTrue(ts.getExtraValuesKeySet().contains("analTimes"));
	}

	public void testWrite2() {

		// read noos file and create object
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();

		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData2.txt");
		assertEquals(1, noosIO.getExchangeItems().length);

		// then write it again under another name
		File noosFile = new File(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData2.txt");
		NoosTimeSeriesIoObject.writeNoosTimeSeries((TimeSeries) noosIO.getExchangeItems()[0], noosFile);

		// finally, test it
		TimeSeries series1 = (TimeSeries) noosIO.getExchangeItems()[0];
		NoosTimeSeriesIoObject noosIO2 = new NoosTimeSeriesIoObject();
		noosIO2.initialize(this.testRunDataDir, "copy_of_NoosTimeSeriesIoObjectTestData2.txt");
		TimeSeries series2 = (TimeSeries) noosIO2.getExchangeItems()[0];
		assertTrue(series1.equals(series2));
	}

	public void testRead3() {

		// read noos files and create object
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();

		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData*.txt");
		assertEquals(2, noosIO.getTimeSeriesSet().size());

		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
		assertEquals(2, noosEx.length);
		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());
		assertEquals("TimeSeries", noosEx[1].getClass().getSimpleName());

		TimeSeries ts0 = (TimeSeries) (noosEx[0]);
		TimeSeries ts1 = (TimeSeries) (noosEx[1]);
		assertTrue((ts0.hasExtraValues("analTimes") && !ts1.hasExtraValues("analTimes"))
		      || (ts1.hasExtraValues("analTimes") && !ts0.hasExtraValues("analTimes")));
	}

	public void testWrite3() {

		// read noos files and create object
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();

		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData*.txt");
		assertEquals(2, noosIO.getTimeSeriesSet().size());

		IPrevExchangeItem noosEx[] = noosIO.getExchangeItems();
		assertEquals(2, noosEx.length);
		assertEquals("TimeSeries", noosEx[0].getClass().getSimpleName());
		assertEquals("TimeSeries", noosEx[1].getClass().getSimpleName());

		TimeSeries ts0 = (TimeSeries) (noosEx[0]);
		TimeSeries ts1 = (TimeSeries) (noosEx[1]);
		assertTrue((ts0.hasExtraValues("analTimes") && !ts1.hasExtraValues("analTimes"))
		      || (ts1.hasExtraValues("analTimes") && !ts0.hasExtraValues("analTimes")));

		ts0.setProperty(NoosTimeSeriesIoObject.PROPERTY_PATHNAME, new File(this.testRunDataDir, "test0.txt").getAbsolutePath());
		ts1.setProperty(NoosTimeSeriesIoObject.PROPERTY_PATHNAME, new File(this.testRunDataDir, "test1.txt").getAbsolutePath());

		noosIO.finish();

		NoosTimeSeriesIoObject noosIO2 = new NoosTimeSeriesIoObject();

		noosIO2.initialize(this.testRunDataDir, "test?.txt");
		assertEquals(2, noosIO2.getTimeSeriesSet().size());

		IPrevExchangeItem noosEx2[] = noosIO2.getExchangeItems();
		TimeSeries ts0a = (TimeSeries) (noosEx2[0]);
		TimeSeries ts1a = (TimeSeries) (noosEx2[1]);

		assertTrue(ts0.equals(ts0a) || ts0.equals(ts1a));
		assertTrue(ts1.equals(ts1a) || ts1.equals(ts0a));
	}

}
