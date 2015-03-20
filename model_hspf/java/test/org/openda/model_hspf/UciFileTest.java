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

package org.openda.model_hspf;

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
 * Test class for testing UciIoObject and UciExchangeItem.
 *
 * @author Arno Kockx
 */
public class UciFileTest extends TestCase {

    private OpenDaTestSupport testData;
    private File testRunDataDir;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(UciFileTest.class, "model_hspf");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testWriteUciFile() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeUtils.createTimeZoneFromDouble(9));
        calendar.set(2005, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
        calendar.set(2007, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double stopDate = Time.milliesToMjd(calendar.getTimeInMillis());

        UciIoObject uciIoObject = new UciIoObject();
        String uciFilename = "uciFileTest/input/ndriver.uci";
        String[] arguments = new String[]{"9", "TSTART", "TSTOP"};
        uciIoObject.initialize(testRunDataDir, uciFilename, arguments);

        //Get all exchangeItems items
        IPrevExchangeItem[] exchangeItems = uciIoObject.getExchangeItems();
        assertEquals(2, exchangeItems.length);

        //Loop over all exchangeItems items and request the ID, name and value
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            String id = exchangeItem.getId();
            if ("TSTART".equals(id)) {
                exchangeItem.setValues(startDate);
            } else if ("TSTOP".equals(id)) {
                exchangeItem.setValues(stopDate);
            }
        }

        //This command actually replaces the tags in the uci file by the values
        //of the corresponding exchangeItems.
        uciIoObject.finish();

        //compare actual result file with expected result file.
        File actualOutputFile = new File(testRunDataDir, uciFilename);
        File expectedOutputFile = new File(testRunDataDir, "uciFileTest/expectedResult/ndriver.uci");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }

    public void testWriteUciFileWithExtendedPeriod() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));

        calendar.set(2007, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
        calendar.set(2007, 0, 5, 9, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double stopDate = Time.milliesToMjd(calendar.getTimeInMillis());

        UciIoObject uciIoObject = new UciIoObject();
        String uciFilename = "uciFileTest/input/ndriver_extended_period.uci";
        String[] arguments = new String[]{"0", "TSTART", "TSTOP", "-43800"};
        uciIoObject.initialize(testRunDataDir, uciFilename, arguments);

        //Get all exchangeItems items
        IPrevExchangeItem[] exchangeItems = uciIoObject.getExchangeItems();
        assertEquals(2, exchangeItems.length);

        //Loop over all exchangeItems items and request the ID, name and value
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            String id = exchangeItem.getId();
            if ("TSTART".equals(id)) {
                exchangeItem.setValues(startDate);
            } else if ("TSTOP".equals(id)) {
                exchangeItem.setValues(stopDate);
            }
        }

        //This command actually replaces the tags in the uci file by the values
        //of the corresponding exchangeItems.
        uciIoObject.finish();

        //compare actual result file with expected result file.
        File actualOutputFile = new File(testRunDataDir, uciFilename);
        File expectedOutputFile = new File(testRunDataDir, "uciFileTest/expectedResult/ndriver_extended_period.uci");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }
}
