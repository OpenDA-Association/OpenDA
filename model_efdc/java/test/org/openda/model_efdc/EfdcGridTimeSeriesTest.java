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

package org.openda.model_efdc;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

/**
 * Test class for testing EfdcGridTimeSeriesIoObject and EfdcGridTimeSeriesExchangeItem.
 *
 * @author Arno Kockx
 */
public class EfdcGridTimeSeriesTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(EfdcGridTimeSeriesTest.class, "model_efdc");
        testRunDataDir = testData.getTestRunDataDir();
    }

    /**
     * Test reading time series from a .DAT (EFDC grid output) file that contains
     * several grid time series.
     * The .DAT file with grid data is generated from the binary model output
     * files from the EFDC (Environmental Fluid Dynamics Code) model,
     * using a tool called POST_PCfile_ASCII.exe. This tool was developed by
     * EnssoHitech for NIER (National Institute of Environmental Research) in Korea.
     *
     * @throws Exception
     */
    public void testReadGridTimeSeries() throws Exception {
        //read period from MJD 55679.375 (2011-04-28 09:00) to MJD 55682.375 (2011-05-01 09:00).
        Calendar calendar = Calendar.getInstance();
        //startTime is in GMT.
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.set(2011, 04, 28, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        long startTime = calendar.getTimeInMillis();
        //expectedTimes4 are in GMT.
        double[] expectedTimes4 = new double[]{55679.0, 55680.0, 55681.0, 55682.0};

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String outputFileName = "efdcGridTimeSeriesTest/input/HOR_WQ_TEM.DAT";
        File outputFile = new File(testRunDataDir, outputFileName);
        assertTrue(outputFile.exists());

        EfdcGridTimeSeriesIoObject efdcGridTimeSeriesIoObject = new EfdcGridTimeSeriesIoObject();
        String[] arguments = new String[]{"9", TimeUtils.mjdToString(Time.milliesToMjd(startTime))};
        efdcGridTimeSeriesIoObject.initialize(testRunDataDir, outputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = efdcGridTimeSeriesIoObject.getExchangeItems();
        assertEquals(23, exchangeItems.length);

        //check times and values.
        IPrevExchangeItem exchangeItem4 = exchangeItems[3];
        //the id is the one-based columnNumber of the column in the file.
        assertEquals("8", exchangeItem4.getId());

        double[] times4 = exchangeItem4.getTimes();
        assertNotNull(times4);
        assertEquals(expectedTimes4.length, times4.length);
        assertNotNull(times4);
        assertEquals(expectedTimes4.length, times4.length);
        for (int n = 0; n < expectedTimes4.length; n++) {
            assertEquals(expectedTimes4[n], times4[n]);
        }

        double[][] data4 = (double[][]) exchangeItem4.getValues();
        assertNotNull(data4);
        assertEquals(expectedTimes4.length, data4.length);
        assertNotNull(data4[3]);
        assertEquals(10914, data4[3].length);
        assertEquals(0.328239E-03, data4[3][0]);
        assertEquals(0.416177E-02, data4[3][10]);
        assertEquals(0.202678E-02, data4[3][20]);
        assertEquals(0.274228, data4[3][data4[1].length - 1]);
    }
}
