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
package org.openda.model_openfoam;

import junit.framework.TestCase;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;

/**
 * @author Werner Kramer
 *
 */
public class CsvTimeSeriesDataObjectTest extends TestCase {
	private File              testRunDataDir;
	private OpenDaTestSupport testData;
	private final double mjdPrecission = 1.0 / 24.0 / 60.0 / 60.0 / 1000; // 1 millisecond
	private final double valuePression = 1e-6; // 1 millisecond


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(CsvTimeSeriesDataObject.class,"model_openfoam");
		testRunDataDir = new File(testData.getTestRunDataDir(),"CsvTimeSeries");
	}

	public void testRead1() {

		CsvTimeSeriesDataObject dataObject = new CsvTimeSeriesDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"SENSORID_QUANTITY_DATETIME.csv"} );
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertEquals(1, exchangeItemIDs.length);
		
		IExchangeItem timEx = dataObject.getDataObjectExchangeItem("Koude_Gang.temperature");
		assertEquals("TimeSeries", timEx.getClass().getSimpleName());

		TimeSeries ts = (TimeSeries) (timEx);
		assertEquals("Koude_Gang.temperature", ts.getId());
		assertEquals("Koude_Gang", ts.getLocation());
		assertEquals("observed", ts.getSource());
		assertEquals("celcius", ts.getUnitId());
		assertEquals("temperature", ts.getQuantityId());
		//assertEquals(99250.0, ts.getPosition()[0]);
		//assertEquals(0.0, ts.getPosition()[1]);

		Double refDate = 0.0;
		try {
			refDate = TimeUtils.date2Mjd("201512010001");
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Double startTime = refDate + 0.0;
		Double endTime   = refDate + 11.0 / 24.0 / 60.0;
		
		assertEquals("Start time", startTime, ts.getStartTime(), mjdPrecission);
		assertEquals("End time", endTime  , ts.getStopTime(), mjdPrecission );
		assertTrue(ts.intersectsWithTimeInterval(startTime -1, startTime));
		assertTrue(ts.intersectsWithTimeInterval(startTime, endTime));
		assertTrue(ts.intersectsWithTimeInterval(startTime-1, endTime+1));
		//assertTrue(ts.intersectsWithTimeInterval(endTime, endTime+1));
		assertFalse(ts.intersectsWithTimeInterval(startTime-1., startTime-mjdPrecission));
		assertFalse(ts.intersectsWithTimeInterval(endTime+mjdPrecission, endTime+1));

		assertEquals("Time series length", 12,  ts.getTimes().length);
		assertEquals("First time point", startTime, ts.getTimes()[0], mjdPrecission);
		assertEquals("Last time point", endTime, ts.getTimes()[11], mjdPrecission);

		assertEquals("Time series number of data points", 12, ts.getValuesAsDoubles().length);
		assertEquals("First data value",      0.0, ts.getValuesAsDoubles()[0],  valuePression);
		assertEquals("Last data value",  116.5200, ts.getValuesAsDoubles()[11], valuePression);

		assertEquals(0, ts.getExtraValuesKeySet().size());
	}

	public void testWrite1() {

		// read noos file and create object
		CsvTimeSeriesDataObject dataObject = new CsvTimeSeriesDataObject();

		dataObject.initialize(this.testRunDataDir, new String[]{"SENSORID_QUANTITY_DATETIME.csv"});

		// then write it again under another name
		String[] exchangeIDs = dataObject.getExchangeItemIDs();
		File originalFile = new File(this.testRunDataDir, "SENSORID_QUANTITY_DATETIME.csv");
		File file = new File(this.testRunDataDir, "written.SENSORID_QUANTITY_DATETIME.csv");
		TimeSeries series1 = (TimeSeries) dataObject.getDataObjectExchangeItem(exchangeIDs[0]);
		series1.setProperty("pathName", file.getAbsolutePath());
		CsvTimeSeriesDataObject.writeTimeSeries(series1);
		testData.FilesAreIdentical(originalFile, file, 0);
	}

	
//	public void test_components() {
//		// read noos file and create object
//		DFlowFMTimeSeriesDataObject dataObject = new DFlowFMTimeSeriesDataObject();
//
//		String[] args = new String[1];
//		args[0] = "estuary_b.mdu";
//		dataObject.initialize(this.testRunDataDir, args);
//
//		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
//		assertEquals(4, exchangeItemIDs.length);
//		System.out.println("Component ids are:");
//		for(String id : exchangeItemIDs){
//			System.out.println(id);
//		}
//
//		IExchangeItem cmpEx = dataObject.getDataObjectExchangeItem("estuary_01.1:waterlevelbnd.M2_amplitude");
//		assertEquals("DoubleExchangeItem", cmpEx.getClass().getSimpleName());
//
//		System.out.println();
//		System.out.print(cmpEx.getId()+":");
//		System.out.println(cmpEx.toString());
//
//		DoubleExchangeItem ampl = (DoubleExchangeItem) (cmpEx);
//		assertEquals("estuary_01.1:waterlevelbnd.M2_amplitude", ampl.getId());
//		assertEquals("m", ampl.getQuantityInfo().getUnit());
//		assertEquals("waterlevelbnd.amplitude", ampl.getQuantityInfo().getQuantity());
//		assertEquals(-250.0, ampl.getPosition()[0]);
//		assertEquals(0.0, ampl.getPosition()[1]);
//		assertEquals("estuary_01.1", ampl.getLocation());
//
//		double a=ampl.getValue();
//		assertEquals(0.6,a);
//
//		IExchangeItem phaseEx = dataObject.getDataObjectExchangeItem("estuary_01.1:waterlevelbnd.S2_phase");
//		assertEquals("DoubleExchangeItem", phaseEx.getClass().getSimpleName());
//
//		System.out.println();
//		System.out.print(phaseEx.getId()+":");
//		System.out.println(phaseEx.toString());
//
//		DoubleExchangeItem phase = (DoubleExchangeItem) (phaseEx);
//		assertEquals("estuary_01.1:waterlevelbnd.S2_phase", phase.getId());
//		assertEquals("degrees", phase.getQuantityInfo().getUnit());
//		assertEquals("waterlevelbnd.phase", phase.getQuantityInfo().getQuantity());
//		assertEquals(-250.0, phase.getPosition()[0]);
//		assertEquals(0.0, phase.getPosition()[1]);
//		assertEquals("estuary_01.1", ampl.getLocation());
//
//		double f=phase.getValue();
//		assertEquals(0.0,f);
//
//		// Now start changing values
//		ampl.setValue(2.2);
//		phase.setValue(0.22);
//		dataObject.finish(); //write changes
//
//		assertTrue(testData.FilesAreIdentical(new File(testRunDataDir,"estuary_b_comp_0001.cmp"),new File(testRunDataDir,"estuary_b_comp_0001.cmp.check")));
//	}
}
