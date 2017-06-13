/* OpenDA v2.4 
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
package org.openda.model_dflowfm;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class DFlowFMTimTimeSeriesFormatterTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMMduInputFile.class, "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "Timeseries");
	}

	public void testTimFormatter() {
		TimeSeriesFormatter timFormatter = new DFlowFMTimTimeSeriesFormatter(48257.0, 24.0 * 60.0);
		File timFile = new File(testRunDataDir, "estuary_02_0001.tim");
		TimeSeries series1 = timFormatter.readFile(timFile.getAbsolutePath());
		//String location = series1.getLocation();
		//assertEquals("1",location);

		// write to screen
		timFormatter.writeToStandardOut(series1);

		// write to file
		boolean overwriteExistingFiles = true;
		File copyOfTimFile = new File(testRunDataDir, "estuary_02_0001.tim.test");
		timFormatter.writeFile(copyOfTimFile.getAbsolutePath(), series1, overwriteExistingFiles);
		assertFalse(testData.FilesAreIdentical(copyOfTimFile, timFile, 0));
	}


}
