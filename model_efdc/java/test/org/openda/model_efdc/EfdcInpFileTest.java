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

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * Test class for testing EfdcInpIoObject.
 *
 * @author Arno Kockx
 */
public class EfdcInpFileTest extends TestCase {

    private OpenDaTestSupport testData;
    private File testRunDataDir;

    protected void setUp() {
        testData = new OpenDaTestSupport(EfdcInpFileTest.class, "model_efdc");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testWriteEfdcInpFile() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));

        calendar.set(2009, Calendar.JANUARY, 1, 9, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
        calendar.set(2009, Calendar.JANUARY, 3, 9, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double endDate = Time.milliesToMjd(calendar.getTimeInMillis());

        EfdcInpIoObject efdcInpIoObject = new EfdcInpIoObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String efdcInpFilename = "efdcInpFileTest/input/EFDC.INP";
        String startTimeExchangeItemId = "TSTART1";
        String endTimeExchangeItemId = "TSTOP1";
        efdcInpIoObject.initialize(testRunDataDir,
                new String[]{efdcInpFilename, "0", startTimeExchangeItemId, endTimeExchangeItemId});

        //Get all exchangeItems items
        String[] exchangeItemIDs = efdcInpIoObject.getExchangeItemIDs();
        //Loop over all exchangeItems items and request the ID, name and value
        for (String id  : exchangeItemIDs) {
        	IExchangeItem exchangeItem = efdcInpIoObject.getDataObjectExchangeItem(id);
            if (id.equals(startTimeExchangeItemId)) {
                exchangeItem.setValues(startDate);
            } else if (id.equals(endTimeExchangeItemId)) {
                exchangeItem.setValues(endDate);
            }
        }

        //This command actually replaces the tags in the file by the values
        //of the corresponding exchangeItems.
        efdcInpIoObject.finish();

        //compare actual result file with expected result file.
        File actualOutputFile = new File(testRunDataDir, efdcInpFilename);
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcInpFileTest/expectedResult/EFDC.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }
}
