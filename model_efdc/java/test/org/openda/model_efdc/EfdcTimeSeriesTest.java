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
 * Test class for testing EfdcTimeSeriesIoObject, EfdcTimeSeriesExchangeItem,
 * EfdcQserTimeSeriesFormatter, EfdcTserTimeSeriesFormatter,
 * EfdcPserTimeSeriesFormatter, EfdcCwqsrTimeSeriesFormatter and EfdcAserTimeSeriesFormatter.
 *
 * @author Arno Kockx
 */
public class EfdcTimeSeriesTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(EfdcTimeSeriesTest.class, "model_efdc");
        testRunDataDir = testData.getTestRunDataDir();
    }

    /**
     * Test writing time series to an existing ASER.INP file.
     *
     * @throws Exception
     */
    public void testWriteAserTimeSeries() throws Exception {
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String outputFileName = "efdcTimeSeriesTest/output/ASER.INP";
        File outputFile = new File(testRunDataDir, outputFileName);
        assertTrue(outputFile.exists());

        String[] locationIds = new String[]{"1", "2", "3"};
        String[] parameterIds = new String[]{"PATM", "TDRY", "TWET", "RAIN", "EVAP", "SOLSWR", "CLOUD"};
        EfdcTimeSeriesIoObject efdcTimeSeriesIoObject = new EfdcTimeSeriesIoObject();
        String[] arguments = new String[locationIds.length*parameterIds.length + 3];
        arguments[0] = "ASER";
        arguments[1] = "0";
        arguments[2] = "TSTART1";
        for (int n = 0; n < locationIds.length; n++) {
            for (int k = 0; k < parameterIds.length; k++) {
                String timeSeriesId = locationIds[n] + "." + parameterIds[k];
                arguments[n*parameterIds.length + k + 3] = timeSeriesId;
            }
        }
        efdcTimeSeriesIoObject.initialize(testRunDataDir, outputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = efdcTimeSeriesIoObject.getExchangeItems();
        assertEquals(arguments.length - 2, exchangeItems.length);

        //set start time.
        //write period from MJD 55562.0 (2011-01-01 00:00) to MJD 55564.0 (2011-01-03 00:00).
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.set(2011, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startTime = Time.milliesToMjd(calendar.getTimeInMillis());
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            if ("TSTART1".equalsIgnoreCase(exchangeItem.getId())) {
                exchangeItem.setValues(startTime);
            }
        }

        //set times and values.
        //write period from MJD 55562.0 (2011-01-01 00:00) to MJD 55564.0 (2011-01-03 00:00).
        double[] times = new double[]{55562.0, 55563.0, 55564.0};
        double currentValue = 1;
        for (int n = 0; n < locationIds.length; n++) {
            for (int k = 0; k < parameterIds.length; k++) {
                String timeSeriesId = locationIds[n] + "." + parameterIds[k];

                //get exchangeItem.
                IPrevExchangeItem currentExchangeItem = null;
                for (IPrevExchangeItem exchangeItem : exchangeItems) {
                    if (timeSeriesId.equalsIgnoreCase(exchangeItem.getId())) {
                        currentExchangeItem = exchangeItem;
                        break;
                    }
                }
                if (currentExchangeItem == null) {
                    fail("ExchangeItem for time series id '" + timeSeriesId + "' not found.");
                    return;
                }

                double[] values = new double[times.length];
                for (int i = 0; i < values.length; i++) {
                    values[i] = currentValue;
                    currentValue++;
                }
                currentExchangeItem.setTimes(times);
                currentExchangeItem.setValuesAsDoubles(values);
            }
        }

        //write data to file.
        efdcTimeSeriesIoObject.finish();

        //compare actual result file with expected result file.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcTimeSeriesTest/expectedResult/ASER.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, outputFile, 0));
    }

    /**
     * Test writing time series to an existing CWQSR**.INP file.
     *
     * @throws Exception
     */
    public void testWriteCwqsrTimeSeries() throws Exception {
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String outputFileName = "efdcTimeSeriesTest/output/CWQSR02.INP";
        File outputFile = new File(testRunDataDir, outputFileName);
        assertTrue(outputFile.exists());

        EfdcTimeSeriesIoObject efdcTimeSeriesIoObject = new EfdcTimeSeriesIoObject();
        //here 1 is the location (can be multiple locations with series per file)
        //and CWQSR02 is the parameter.
        String[] arguments = new String[]{"CWQSR02", "0", "TSTART1", "1.CWQSR02", "2.CWQSR02"};
        efdcTimeSeriesIoObject.initialize(testRunDataDir, outputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = efdcTimeSeriesIoObject.getExchangeItems();
        assertEquals(arguments.length - 2, exchangeItems.length);

        //set times and values.
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.set(2011, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startTime = Time.milliesToMjd(calendar.getTimeInMillis());
        //write period from MJD 55561.0 (2010-31-12 00:00) to MJD 55927.5 (2012-01-01 12:00).
        double[] times = new double[]{0.000, 7.500, 34.500, 69.500, 91.500, 122.500, 155.500,
                182.500, 213.500, 245.500, 274.500, 308.500, 337.500, 366.500};
        for (int n = 0; n < times.length; n++) {
            times[n] = times[n] + 55561;
        }
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            if ("TSTART1".equalsIgnoreCase(exchangeItem.getId())) {
                exchangeItem.setValues(startTime);

            } else if ("1.CWQSR02".equalsIgnoreCase(exchangeItem.getId())) {
                double[] values = new double[]{6.7181, 6.7181, 7.1966, 5.7133, 4.5170, 1.3589, 0.4306,
                        1.1867, 1.2728, 1.7896, 2.1628, 1.7800, 0.1436, 0.1436};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("2.CWQSR02".equalsIgnoreCase(exchangeItem.getId())) {
                double[] values = new double[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);
            }
        }

        //write data to file.
        efdcTimeSeriesIoObject.finish();

        //compare actual result file with expected result file.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcTimeSeriesTest/expectedResult/CWQSR02.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, outputFile, 0));
    }

    /**
     * Test writing time series to an existing PSER.INP file.
     *
     * @throws Exception
     */
    public void testWritePserTimeSeriesWithTimeZone() throws Exception {
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String outputFileName = "efdcTimeSeriesTest/output/PSER.INP";
        File outputFile = new File(testRunDataDir, outputFileName);
        assertTrue(outputFile.exists());

        EfdcTimeSeriesIoObject efdcTimeSeriesIoObject = new EfdcTimeSeriesIoObject();
        String[] arguments = new String[]{"PSER", "9", "TSTART1", "1.PSER"};
        efdcTimeSeriesIoObject.initialize(testRunDataDir, outputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = efdcTimeSeriesIoObject.getExchangeItems();
        assertEquals(arguments.length - 2, exchangeItems.length);

        //set times and values.
        //write period from MJD 55563.625 GMT (2011-01-02 15:00 GMT) to MJD 55568.625 GMT (2011-01-07 15:00 GMT).
        Calendar calendar = Calendar.getInstance();
        //startTimeDouble in exchangeItems is in GMT.
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.set(2011, 0, 2, 15, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startTime = Time.milliesToMjd(calendar.getTimeInMillis());
        //1.PSER
        //timeDoubles in times array are in GMT.
        double[] times = new double[]{2, 3, 5, 7};
        for (int n = 0; n < times.length; n++) {
            times[n] = times[n] + 55561.625;
        }
        double[] values = new double[]{4.7, 4.8, 4.9, 2.06};
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            if ("TSTART1".equalsIgnoreCase(exchangeItem.getId())) {
                exchangeItem.setValues(startTime);
            } else if ("1.PSER".equalsIgnoreCase(exchangeItem.getId())) {
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);
            }
        }

        //write data to file.
        efdcTimeSeriesIoObject.finish();

        //compare actual result file with expected result file.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcTimeSeriesTest/expectedResult/PSER.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, outputFile, 0));
    }

    /**
     * Test writing time series to an existing QSER.INP file.
     *
     * @throws Exception
     */
    public void testWriteQserTimeSeries() throws Exception {
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String outputFileName = "efdcTimeSeriesTest/output/QSER.INP";
        File outputFile = new File(testRunDataDir, outputFileName);
        assertTrue(outputFile.exists());

        EfdcTimeSeriesIoObject efdcTimeSeriesIoObject = new EfdcTimeSeriesIoObject();
        String[] arguments = new String[]{"QSER", "0", "TSTART23", "3.QSER", "1.QSER", "4.QSER", "2.QSER"};
        efdcTimeSeriesIoObject.initialize(testRunDataDir, outputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = efdcTimeSeriesIoObject.getExchangeItems();
        assertEquals(arguments.length - 2, exchangeItems.length);

        //set times and values.
        //write period from MJD 55562.0 (2011-01-01 00:00) to MJD 55564.0 (2011-01-03 00:00).
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.set(2011, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startTime = Time.milliesToMjd(calendar.getTimeInMillis());
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            if ("TSTART23".equalsIgnoreCase(exchangeItem.getId())) {
                exchangeItem.setValues(startTime);

            } else if ("1.QSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55562.5, 55563.0, 55563.5, 55564.0};
                double[] values = new double[]{1, -2.2, 3.33, -4.444, 5.5555};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("2.QSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55562.5, 55563.0, 55563.5, 55564.0};
                double[] values = new double[]{8888.8888, 99999.9999, 1000000000, 10000000.00000001, 0.0000000001};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("3.QSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55562.5, 55563.0, Double.NaN, 55564.0};
                double[] values = new double[]{1, Double.NaN, Double.NaN, 4, 5};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("4.QSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55563.0, 55564.0};
                double[] values = new double[]{1.1574E-05, 0, 2.8E-06};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);
            }
        }

        //write data to file.
        efdcTimeSeriesIoObject.finish();

        //compare actual result file with expected result file.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcTimeSeriesTest/expectedResult/QSER.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, outputFile, 0));
    }

    /**
     * Test writing time series to an existing TSER.INP file.
     *
     * @throws Exception
     */
    public void testWriteTserTimeSeries() throws Exception {
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        String outputFileName = "efdcTimeSeriesTest/output/TSER.INP";
        File outputFile = new File(testRunDataDir, outputFileName);
        assertTrue(outputFile.exists());

        EfdcTimeSeriesIoObject efdcTimeSeriesIoObject = new EfdcTimeSeriesIoObject();
        String[] arguments = new String[]{"TSER","0",  "TSTART1", "4.TSER", "1.TSER", "2.TSER", "3.TSER"};
        efdcTimeSeriesIoObject.initialize(testRunDataDir, outputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = efdcTimeSeriesIoObject.getExchangeItems();
        assertEquals(arguments.length - 2, exchangeItems.length);

        //set times and values.
        //write period from MJD 55562.0 (2011-01-01 00:00) to MJD 55564.0 (2011-01-03 00:00).
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.set(2011, 0, 1, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startTime = Time.milliesToMjd(calendar.getTimeInMillis());
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            if ("TSTART1".equalsIgnoreCase(exchangeItem.getId())) {
                exchangeItem.setValues(startTime);

            } else if ("1.TSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55562.5, 55563.0, 55563.5, 55564.0};
                double[] values = new double[]{1, -2.2, 3.33, -4.444, 5.5555};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("2.TSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55562.5, 55563.0, 55563.5, 55564.0};
                double[] values = new double[]{8888.8888, 99999.9999, 1000000000, 10000000.00000001, 0.0000000001};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("3.TSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55562.5, 55563.0, Double.NaN, 55564.0};
                double[] values = new double[]{1, Double.NaN, Double.NaN, 4, 5};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);

            } else if ("4.TSER".equalsIgnoreCase(exchangeItem.getId())) {
                double[] times = new double[]{55562.0, 55563.0, 55564.0};
                double[] values = new double[]{1.1574E-05, 0, 2.8E-06};
                exchangeItem.setTimes(times);
                exchangeItem.setValuesAsDoubles(values);
            }
        }

        //write data to file.
        efdcTimeSeriesIoObject.finish();

        //compare actual result file with expected result file.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_efdc/org/openda/model_efdc
        File expectedOutputFile = new File(testRunDataDir, "efdcTimeSeriesTest/expectedResult/TSER.INP");
        assertTrue(testData.FilesAreIdentical(expectedOutputFile, outputFile, 0));
    }
}
