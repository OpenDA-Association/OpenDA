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
package org.openda.exchange.timeseries;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class TimeSeriesFormatterTest extends TestCase {
   private File testRunDataDir;
   private OpenDaTestSupport testData;

   protected void setUp() throws IOException {
      testData = new OpenDaTestSupport(TimeSeriesFormatterTest.class,"core");
      testRunDataDir = testData.getTestRunDataDir();
   }

   public void testNoosFormatter() {
      double delta=0.0001;
      TimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
	  File denHelderNoos = new File(testRunDataDir, "den_helder_waterlevel_astro.noos");
	  TimeSeries series1 = noosFormatter.readFile(denHelderNoos.getAbsolutePath());
      String location = series1.getLocation();
      assertEquals("den helder",location);
      String analTime = series1.getProperty(NoosTimeSeriesFormatter.PROPERTY_ANALTIME);
      assertEquals("most recent",analTime);
      double times[] = series1.getTimesRef();
      assertEquals("times[0]", 54466.0,times[0], delta);

      // write to screen
      noosFormatter.writeToStandardOut(series1);

      // write to file
      boolean overwriteExistingFiles=true;
      File copyOfDenHelderNoos = new File(testRunDataDir,"den_helder_test.txt");
      noosFormatter.writeFile(copyOfDenHelderNoos.getAbsolutePath(), series1, overwriteExistingFiles);
	  testData.FilesAreIdentical(copyOfDenHelderNoos, denHelderNoos, 4);
   }


}
