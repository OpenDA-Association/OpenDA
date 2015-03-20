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

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

/**
 * Test class for testing EfdcEventTox2InpIoObject.
 *
 * @author Arno Kockx
 */
public class EfdcEventTox2InpFileTest extends TestCase {

    private OpenDaTestSupport testData;
    private File testRunDataDir;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(EfdcEventTox2InpFileTest.class, "model_efdc");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testWriteEfdcEventTox2InpFile() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));

        calendar.set(2009, 0, 1, 9, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
        calendar.set(2009, 0, 3, 9, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double endDate = Time.milliesToMjd(calendar.getTimeInMillis());

        EfdcEventTox2InpIoObject efdcEventTox2InpIoObject = new EfdcEventTox2InpIoObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String eventTox2InpFilename = "efdcEventTox2InpFileTest/input/EVENT_TOX2.INP";
        String startTimeExchangeItemId = "TSTART1";
        String endTimeExchangeItemId = "TSTOP1";
        efdcEventTox2InpIoObject.initialize(testRunDataDir, eventTox2InpFilename,
                new String[]{"0", startTimeExchangeItemId, endTimeExchangeItemId});

        //Get all exchangeItems items
        IPrevExchangeItem[] exchangeItems = efdcEventTox2InpIoObject.getExchangeItems();
        //Loop over all exchangeItems items and request the ID, name and value
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            String id = exchangeItem.getId();
            if (id.equals(startTimeExchangeItemId)) {
                exchangeItem.setValues(startDate);
            } else if (id.equals(endTimeExchangeItemId)) {
                exchangeItem.setValues(endDate);
            }
        }

        //This command actually replaces the tags in the file by the values
        //of the corresponding exchangeItems.
        efdcEventTox2InpIoObject.finish();

        //compare actual result file with expected result file.
        File actualOutputFile = new File(testRunDataDir, eventTox2InpFilename);
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcEventTox2InpFileTest/expectedResult/EVENT_TOX2.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }
}
