/* V2.2
* Copyright (c) 2015 OpenDA Association
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
package org.openda.model_RainfallRunoffZhang;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;

public class ASCIITimeSeriesFormatterTest extends TestCase {
   
	// Use openDA test suite.
	private File testRunDataDir;
	private OpenDaTestSupport testData;


   protected void setUp() throws IOException {
      testData = new OpenDaTestSupport(ASCIITimeSeriesFormatterTest.class,"model_RainfallRunoffZhang");
      testRunDataDir = testData.getTestRunDataDir();
   }
   
   protected void tearDown() throws Exception {
		
		try {
			File file = new File(testRunDataDir, "testObservationWriteTest.txt");
			Path path = file.toPath();
			System.out.println("ASCIITimeSeriesFormatterTest.tearDown(): removing file: " + path);
			Files.delete(path);
		} catch (NoSuchFileException x) {
			System.err.format("%s: no such" + " file or directory%n", "testObservationWriteTest.txt");
		} catch (IOException x) {
			// File permission problems are caught here.
			System.err.println(x);
		}
   }
   
   public void testASCIIFormatter() {
      double delta=0.0001;
      TimeSeriesFormatter ASCIIFormatter = new ASCIITimeSeriesFormatter();
	  File testObservation = new File(testRunDataDir, "testObservation.txt");
	  TimeSeries series1 = ASCIIFormatter.readFile(testObservation.getAbsolutePath());
      String location = series1.getLocation();
      assertEquals("Catchment",location);
      String analTime = series1.getProperty(ASCIITimeSeriesFormatter.PROPERTY_ANALTIME);
      assertEquals("most recent",analTime);
      double times[] = series1.getTimesRef();
      assertEquals("times[0]",25999,times[0],delta);
      double values[] = series1.getValuesAsDoubles();
      assertTrue(Double.isNaN(values[0]));
      String quantity = series1.getQuantityId();
      assertEquals("discharge_mm",quantity);

      // write to screen
      ASCIIFormatter.writeToStandardOut(series1);

      // write to file
      boolean overwriteExistingFiles=true;
      File copyOfTestObservation = new File(testRunDataDir,"testObservationWriteTest.txt");
      ASCIIFormatter.writeFile(copyOfTestObservation.getAbsolutePath(), series1, overwriteExistingFiles);
	  testData.FilesAreIdentical(copyOfTestObservation, testObservation, 0);
   }


}
