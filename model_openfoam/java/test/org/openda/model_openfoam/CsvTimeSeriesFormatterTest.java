/* MOD_V2.0
* Copyright (c) 2013 OpenDA Association
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
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class CsvTimeSeriesFormatterTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(CsvTimeSeriesFormatterTest.class, "model_openfoam");
		testRunDataDir = new File(testData.getTestRunDataDir(), "CsvTimeSeries");
	}

	public void testTimFormatter() {
		TimeSeriesFormatter timeSeriesFormatter = new CsvTimeSeriesFormatter( ";"  );
		File csvFile = new File(testRunDataDir, "SENSORID_QUANTITY_DATETIME.csv");
		TimeSeries series = timeSeriesFormatter.readFile(csvFile.getAbsolutePath());
		//String location = series1.getLocation();
		//assertEquals("1",location);

		// write to screen
		timeSeriesFormatter.writeToStandardOut(series);

		// write to file
		File referenceFile = new File(testRunDataDir, "reference.SENSORID_QUANTITY_DATETIME.csv");
		timeSeriesFormatter.writeFile(referenceFile.getAbsolutePath(), series, true);
		assertTrue(testData.FilesAreIdentical(referenceFile, csvFile, 0));
	}


}
