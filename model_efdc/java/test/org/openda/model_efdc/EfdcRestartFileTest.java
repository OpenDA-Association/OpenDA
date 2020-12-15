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
 * Test class for testing EfdcRestartFileIoObject.
 *
 * @author Arno Kockx
 */
public class EfdcRestartFileTest extends TestCase {

    private OpenDaTestSupport testData;
    private File testRunDataDir;

    protected void setUp() {
        testData = new OpenDaTestSupport(EfdcInpFileTest.class, "model_efdc");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testWriteEfdcRestartFile1() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));

        calendar.set(2009, Calendar.JANUARY, 9, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());

        EfdcRestartFileDataObject efdcRestartFileDataObject = new EfdcRestartFileDataObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String efdcRestartFilename = "efdcRestartFileTest/input/RESTART1.INP";
        String startTimeExchangeItemId = "TSTART1";
        efdcRestartFileDataObject.initialize(testRunDataDir,
                new String[]{efdcRestartFilename, "0", startTimeExchangeItemId});

        //Get all exchangeItems items
        String[] exchangeItemIDs = efdcRestartFileDataObject.getExchangeItemIDs();
        //Loop over all exchangeItems items and request the ID, name and value
        for (String id : exchangeItemIDs) {
        	IExchangeItem exchangeItem = efdcRestartFileDataObject.getDataObjectExchangeItem(id);
        	String exId = exchangeItem.getId();
        	assertEquals(id, exId);
            if (exId.equals(startTimeExchangeItemId)) {
                exchangeItem.setValues(startDate);
            }
        }

        //This command actually replaces the startTime in the restart file.
        efdcRestartFileDataObject.finish();

        //compare actual result file with expected result file.
        File actualOutputFile = new File(testRunDataDir, efdcRestartFilename);
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcRestartFileTest/expectedResult/RESTART1.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }

    public void testWriteEfdcRestartFile2() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));

        calendar.set(2009, Calendar.MAY, 23, 9, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());

        EfdcRestartFileDataObject efdcRestartFileDataObject = new EfdcRestartFileDataObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String efdcRestartFilename = "efdcRestartFileTest/input/RESTART2.INP";
        String startTimeExchangeItemId = "TSTART1";
        efdcRestartFileDataObject.initialize(testRunDataDir,
                new String[]{efdcRestartFilename, "0", startTimeExchangeItemId});

        //Get all exchangeItems items
        String[] exchangeItemIDs = efdcRestartFileDataObject.getExchangeItemIDs();
        //Loop over all exchangeItems items and request the ID, name and value
        for (String id : exchangeItemIDs) {
			IExchangeItem exchangeItem = efdcRestartFileDataObject.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);
			if (exId.equals(startTimeExchangeItemId)) {
                exchangeItem.setValues(startDate);
            }
        }

        //This command actually replaces the startTime in the restart file.
        efdcRestartFileDataObject.finish();

        //compare actual result file with expected result file.
        File actualOutputFile = new File(testRunDataDir, efdcRestartFilename);
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcRestartFileTest/expectedResult/RESTART2.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }
}
