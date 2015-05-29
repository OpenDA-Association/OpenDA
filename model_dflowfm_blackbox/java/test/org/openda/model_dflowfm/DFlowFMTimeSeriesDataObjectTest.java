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
package org.openda.model_dflowfm;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;

/**
 * @author Werner Kramer
 *
 */
public class DFlowFMTimeSeriesDataObjectTest extends TestCase {
	private File              testRunDataDir;
	private OpenDaTestSupport testData;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMTimeSeriesDataObject.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Timeseries");
	}

	public void testRead1() {

		// read noos file and create object
		DFlowFMTimeSeriesDataObject dataObject = new DFlowFMTimeSeriesDataObject();

		String[] args = new String[1];
		args[0] = "estuary.mdu";
		dataObject.initialize(this.testRunDataDir, args);

		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertEquals(2, exchangeItemIDs.length);
		
		IExchangeItem timEx = dataObject.getDataObjectExchangeItem("estuary_02.1:dischargebnd");
		assertEquals("TimeSeries", timEx.getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) (timEx);
		assertEquals("estuary_02.1:dischargebnd", ts.getId());
		assertEquals("estuary_02.1", ts.getLocation());
		assertEquals("", ts.getSource());
		assertEquals("", ts.getUnitId());
		assertEquals("dischargebnd", ts.getQuantityId());
		assertEquals(99250.0, ts.getPosition()[0]);
		assertEquals(0.0, ts.getPosition()[1]);

		Double refDate = 0.0;
		try {
			refDate = TimeUtils.date2Mjd("19910101" + "0000");
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Double startTime = refDate + 0.0;
		Double endTime   = refDate + 23040.0 / 24.0 / 60.0;
		
		assertEquals( startTime, ts.getStartTime());
		assertEquals( endTime  , ts.getStopTime());
		assertTrue(ts.intersectsWithTimeInterval(startTime -1, startTime));
		assertTrue(ts.intersectsWithTimeInterval(startTime, endTime));
		assertTrue(ts.intersectsWithTimeInterval(startTime-1, endTime+1));
		assertTrue(ts.intersectsWithTimeInterval(endTime, endTime+1));
		assertFalse(ts.intersectsWithTimeInterval(startTime-1., startTime-1.e6));
		assertFalse(ts.intersectsWithTimeInterval(endTime+1.e6, endTime+1));

		assertEquals(2, ts.getTimes().length);
		assertEquals(startTime, ts.getTimes()[0]);
		assertEquals(endTime, ts.getTimes()[1]);

		assertEquals(2, ts.getValuesAsDoubles().length);
		assertEquals(-500., ts.getValuesAsDoubles()[0]);
		assertEquals(-500., ts.getValuesAsDoubles()[1]);

		assertEquals(0, ts.getExtraValuesKeySet().size());
	}

	public void testWrite1() {

		// read noos file and create object
		DFlowFMTimeSeriesDataObject dataObject = new DFlowFMTimeSeriesDataObject();

	    String[] args = new String[1];
	    args[0] = "estuary.mdu"; 
		dataObject.initialize(this.testRunDataDir, args);
		assertEquals(2, dataObject.getExchangeItemIDs().length);
		assertEquals(2, dataObject.getTimeSeriesSet().size());

		// then write it again under another name
		String[] exchangeIDs = dataObject.getExchangeItemIDs();
		
		File timFile = new File(this.testRunDataDir, "estuary_test_02_0001.tim");
		DFlowFMTimeSeriesDataObject.writeTimTimeSeries((TimeSeries) dataObject.getDataObjectExchangeItem(exchangeIDs[0]), timFile);
		timFile = new File(this.testRunDataDir, "estuary_test_02_0002.tim");
		DFlowFMTimeSeriesDataObject.writeTimTimeSeries((TimeSeries) dataObject.getDataObjectExchangeItem(exchangeIDs[1]), timFile);
		
		// finally, test it
		TimeSeries series1 = (TimeSeries) dataObject.getDataObjectExchangeItem("estuary_02.2:dischargebnd");
		DFlowFMTimeSeriesDataObject dataObject2 = new DFlowFMTimeSeriesDataObject();
		args[0] = "estuary_test.mdu"; 
		dataObject2.initialize(this.testRunDataDir, args);
		TimeSeries series2 = (TimeSeries) dataObject2.getDataObjectExchangeItem("estuary_02.2:dischargebnd");
		//System.out.println( series2.getProperty("pathName") );
		assertEquals(timFile.getAbsolutePath(),series2.getProperty("pathName"));
		assertTrue(series1.equals(series2));
	}

}
