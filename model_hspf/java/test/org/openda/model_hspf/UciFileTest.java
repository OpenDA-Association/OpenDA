/* OpenDA v2.4.1 
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

package org.openda.model_hspf;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;
import java.util.TimeZone;

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
        File expectedOutputFile = new File(testRunDataDir, "uciFileTest/expectedResult/ndriver_expected.uci");
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
        File expectedOutputFile = new File(testRunDataDir, "uciFileTest/expectedResult/ndriver_extended_period_expected.uci");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, actualOutputFile, 0));
    }

	public void testWriteUciFileWithoutTags() {
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeZone(TimeUtils.createTimeZoneFromDouble(9));
		calendar.set(2005, 0, 1, 0, 0, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
		calendar.set(2007, 0, 1, 0, 0, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		double endDate = Time.milliesToMjd(calendar.getTimeInMillis());

		UciDataObject uciDataObject = new UciDataObject();
		String uciFilename = "uciFileTest/input/ndriver_without_tags.uci";
		String[] arguments = new String[]{uciFilename, "9", "TSTART", "TSTOP"};
		uciDataObject.initialize(testRunDataDir, arguments);

		String[] exchangeItemIds = uciDataObject.getExchangeItemIDs();
		assertEquals(2, exchangeItemIds.length);

		//set start and end time in exchangeItems.
		IExchangeItem startTimeExchangeItem = uciDataObject.getDataObjectExchangeItem("TSTART");
		assertNotNull(startTimeExchangeItem);
		startTimeExchangeItem.setValues(startDate);
		IExchangeItem endTimeExchangeItem = uciDataObject.getDataObjectExchangeItem("TSTOP");
		assertNotNull(endTimeExchangeItem);
		endTimeExchangeItem.setValues(endDate);

		//the call to method finish actually writes the data in the exchangeItems to the uci file.
		uciDataObject.finish();

		File actualOutputFile = new File(testRunDataDir, uciFilename);
		File expectedOutputFile = new File(testRunDataDir, "uciFileTest/expectedResult/ndriver_without_tags_expected.uci");
		assertEquals("Actual output file '" + actualOutputFile + "' does not equal expected output file '" + expectedOutputFile + "'.",
				AsciiFileUtils.readText(expectedOutputFile), AsciiFileUtils.readText(actualOutputFile));
	}

    public void testReadUciStateFile() {
        UciStateDataObject uciStateDataObject = new UciStateDataObject();
        String uciFilename = "uciFileTest/input/ndriver_state_file.uci";
        String[] arguments = new String[]{uciFilename, "200612311500"};
        uciStateDataObject.initialize(testRunDataDir, arguments);

        String[] exchangeItemIds = uciStateDataObject.getExchangeItemIDs();
        assertEquals(19611, exchangeItemIds.length);

        //RCHRES HEAT-INIT
        IExchangeItem item = uciStateDataObject.getDataObjectExchangeItem("RCH1.AIRTMP");
        assertNotNull(item);
        assertEquals(34d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("RCH42.AIRTMP");
        assertNotNull(item);
        assertEquals(34d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("RCH232.AIRTMP");
        assertNotNull(item);
        assertEquals(34d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("RCH233.AIRTMP");
        assertNull(item);

        //RCHRES HYDR-INIT
        item = uciStateDataObject.getDataObjectExchangeItem("RCH56.VOL");
        assertNotNull(item);
        assertEquals(100d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("RCH57.VOL");
        assertNotNull(item);
        assertEquals(400000d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("RCH58.VOL");
        assertNotNull(item);
        assertEquals(100d, item.getValues());

        //PERLND PWT-GASES
        item = uciStateDataObject.getDataObjectExchangeItem("P11.SODOX");
        assertNotNull(item);
        assertEquals(14.5d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("P42.IODOX");
        assertNotNull(item);
        assertEquals(12.7d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("P295.AOCO2");
        assertNotNull(item);
        assertEquals(0d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("P296.AOCO2");
        assertNull(item);

        //IMPLND IWAT-STATE1
        item = uciStateDataObject.getDataObjectExchangeItem("IMP11.SURS");
        assertNotNull(item);
        assertEquals(0.01d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("IMP42.SURS");
        assertNotNull(item);
        assertEquals(0.01d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("IMP291.SURS");
        assertNotNull(item);
        assertEquals(0.01d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("IMP292.SURS");
        assertNull(item);

        //PERLND QUAL-INPUT
        item = uciStateDataObject.getDataObjectExchangeItem("P11.ACQOP1");
        assertNotNull(item);
        assertEquals(0.034d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("P11.AOQC3");
        assertNotNull(item);
        assertEquals(0.0d, item.getValues());

        //IMPLND QUAL-INPUT
        item = uciStateDataObject.getDataObjectExchangeItem("IMP291.SQO4");
        assertNotNull(item);
        assertEquals(0.81d, item.getValues());
    }

    public void testWriteUciStateFile() throws IOException {
        UciStateDataObject uciStateDataObject = new UciStateDataObject();
        String uciFilename = "uciFileTest/input/ndriver_state_file.uci";
        String defaultUciFilename = "uciFileTest/input/ndriver_state_file_default.uci";
        File uciFile = new File(testRunDataDir, uciFilename);
        File defaultUciFile = new File(testRunDataDir, defaultUciFilename);
        BBUtils.copyFile(uciFile, defaultUciFile);
        String[] arguments = new String[]{uciFilename, "200612311500", "0", defaultUciFilename};
        uciStateDataObject.initialize(testRunDataDir, arguments);

        //set values in exchangeItems.
        //RCHRES HEAT-INIT
        IExchangeItem item = uciStateDataObject.getDataObjectExchangeItem("RCH1.AIRTMP");
        assertNotNull(item);
        item.setValues(-1d);
        item = uciStateDataObject.getDataObjectExchangeItem("RCH42.AIRTMP");
        assertNotNull(item);
        item.setValues(42d);
        item = uciStateDataObject.getDataObjectExchangeItem("RCH232.AIRTMP");
        assertNotNull(item);
        item.setValues(10d);

        //RCHRES HYDR-INIT
        item = uciStateDataObject.getDataObjectExchangeItem("RCH56.VOL");
        assertNotNull(item);
        item.setValues(0.56d);
        item = uciStateDataObject.getDataObjectExchangeItem("RCH57.VOL");
        assertNotNull(item);
        item.setValues(0.57d);
        item = uciStateDataObject.getDataObjectExchangeItem("RCH58.VOL");
        assertNotNull(item);
        item.setValues(0.58d);

        //PERLND PWT-GASES
        item = uciStateDataObject.getDataObjectExchangeItem("P11.SODOX");
        assertNotNull(item);
        item.setValues(0.11d);
        item = uciStateDataObject.getDataObjectExchangeItem("P12.IODOX");
        assertNotNull(item);
        item.setValues(0.12d);
        item = uciStateDataObject.getDataObjectExchangeItem("P295.AOCO2");
        assertNotNull(item);
        item.setValues(2.95d);

        //IMPLND IWAT-STATE1
        item = uciStateDataObject.getDataObjectExchangeItem("IMP11.SURS");
        assertNotNull(item);
        item.setValues(0.11d);
        item = uciStateDataObject.getDataObjectExchangeItem("IMP42.SURS");
        assertNotNull(item);
        item.setValues(0.42d);
        item = uciStateDataObject.getDataObjectExchangeItem("IMP291.SURS");
        assertNotNull(item);
        item.setValues(2.91d);

        //PERLND QUAL-INPUT
        item = uciStateDataObject.getDataObjectExchangeItem("P11.ACQOP1");
        assertNotNull(item);
        assertEquals(0.034d, item.getValues());
        item = uciStateDataObject.getDataObjectExchangeItem("P11.AOQC3");
        assertNotNull(item);
        assertEquals(0.0d, item.getValues());

        //IMPLND QUAL-INPUT
        item = uciStateDataObject.getDataObjectExchangeItem("IMP291.SQO4");
        assertNotNull(item);
        assertEquals(0.81d, item.getValues());

        //the call to method finish actually writes the data in the exchangeItems to the uci file.
        uciStateDataObject.finish();

        File actualOutputFile = new File(testRunDataDir, uciFilename);
        File expectedOutputFile = new File(testRunDataDir, "uciFileTest/expectedResult/ndriver_state_file_expected.uci");
        assertEquals("Actual output file '" + actualOutputFile + "' does not equal expected output file '" + expectedOutputFile + "'.",
                AsciiFileUtils.readText(expectedOutputFile), AsciiFileUtils.readText(actualOutputFile));
    }
}
