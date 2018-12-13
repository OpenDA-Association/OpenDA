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

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.iotools.DataCopier;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.io.AsciiFileUtils;

/**
 * Test class for testing WdmTimeSeriesIoObject and WdmTimeSeriesExchangeItem.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class WdmTimeSeriesTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(WdmTimeSeriesTest.class, "model_hspf");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testReadTimeSeries() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testReadTimeSeries: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm file from template to work directory to start with a fresh file before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String templateInputFileName = "wdmTimeSeriesTest/template/OBS(ND).wdm";
        File templateInputFile = new File(testRunDataDir, templateInputFileName);
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmTimeSeriesTest/work/OBS(ND).wdm";
        File inputFile = new File(testRunDataDir, inputFileName);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile.exists()) {
            inputFile.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile);
        assertTrue(inputFile.exists());

        //MJD 54466.0 is 2008-01-01 00:00.
        double startModifiedJulianDate = 54466;
        //MJD 55562.0 is 2011-01-01 00:00.
        double endModifiedJulianDate = 55562;

        WdmTimeSeriesIoObject wdmTimeSeriesIoObject = new WdmTimeSeriesIoObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String[] arguments = new String[]{"../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll",
                "../../../../../model_hspf/native_bin/MESSAGE.WDM", "output", "0",
                TimeUtils.mjdToString(startModifiedJulianDate), TimeUtils.mjdToString(endModifiedJulianDate),
                "RCH103.FLOW", "RCH104.BOD"};
        wdmTimeSeriesIoObject.initialize(testRunDataDir, inputFileName, arguments);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems = wdmTimeSeriesIoObject.getExchangeItems();
        assertEquals(2, exchangeItems.length);

        //check id for RCH103.FLOW
        String id = exchangeItems[0].getId();
        assertEquals("RCH103.FLOW", id);
        //check times for RCH103.FLOW
        double[] times1 = exchangeItems[0].getTimes();
        assertNotNull(times1);
        assertEquals(731, times1.length);
        double currentModifiedJulianDate = startModifiedJulianDate;
        for (int n = 0; n < times1.length; n++) {
            assertEquals(currentModifiedJulianDate, times1[n]);
            currentModifiedJulianDate++;
        }
        //check values for RCH103.FLOW
        double[] values1 = exchangeItems[0].getValuesAsDoubles();
        assertNotNull(values1);
        assertEquals(731, values1.length);
        assertEquals(38.29, values1[0], 1e-5);
        assertEquals(111.96, values1[64], 1e-6);
        assertEquals(1772.92, values1[207], 1e-3);
        assertEquals(1757.17, values1[228], 1e-3);
        assertEquals(2075.54, values1[229], 1e-3);
        assertEquals(27.17, values1[times1.length - 1], 1e-6);

        //check id for RCH104.BOD
        id = exchangeItems[1].getId();
        assertEquals("RCH104.BOD", id);
        //check times for RCH104.BOD
        double[] times2 = exchangeItems[1].getTimes();
        assertNotNull(times2);
        assertEquals(1096, times2.length);
        currentModifiedJulianDate = startModifiedJulianDate;
        for (int n = 0; n < times1.length; n++) {
            assertEquals(currentModifiedJulianDate, times1[n]);
            currentModifiedJulianDate++;
        }
        //check values for RCH104.BOD
        double[] values2 = exchangeItems[1].getValuesAsDoubles();
        assertNotNull(values2);
        assertEquals(1096, values2.length);
        assertEquals(-999.0, values2[0], 1e-6);
        assertEquals(1.2, values2[8], 1e-6);
        assertEquals(1.3, values2[43], 1e-6);
        assertEquals(0.9, values2[71], 1e-6);
        assertEquals(0.8, values2[104], 1e-6);
        assertEquals(-999.0, values2[times2.length - 1], 1e-6);
    }

    /**
     * Copied and adapted code from method testReadTimeSeries.
     */
    public void testReadEnsembleTimeSeries() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testReadEnsembleTimeSeries: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm file from template to work directory to start with a fresh file before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String templateInputFileName = "wdmTimeSeriesTest/template/OBS(ND).wdm";
        File templateInputFile = new File(testRunDataDir, templateInputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName1 = "wdmTimeSeriesTest/work/OBS(ND)_1.wdm";
        File inputFile1 = new File(testRunDataDir, inputFileName1);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile1.exists()) {
            inputFile1.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile1);
        assertTrue(inputFile1.exists());

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName2 = "wdmTimeSeriesTest/work/OBS(ND)_2.wdm";
        File inputFile2 = new File(testRunDataDir, inputFileName2);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile2.exists()) {
            inputFile2.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile2);
        assertTrue(inputFile2.exists());

        //MJD 54466.0 is 2008-01-01 00:00.
        double startModifiedJulianDate = 54466;
        //MJD 55562.0 is 2011-01-01 00:00.
        double endModifiedJulianDate = 55562;

        WdmEnsembleTimeSeriesOutputDataObject wdmEnsembleTimeSeriesOutputDataObject = new WdmEnsembleTimeSeriesOutputDataObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String[] arguments = new String[]{"wdmTimeSeriesTest/work/OBS(ND)_",
                "../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll", "../../../../../model_hspf/native_bin/MESSAGE.WDM",
                "output", "0", TimeUtils.mjdToString(startModifiedJulianDate), TimeUtils.mjdToString(endModifiedJulianDate),
                "RCH103.FLOW", "RCH104.BOD"};
        wdmEnsembleTimeSeriesOutputDataObject.initialize(testRunDataDir, arguments);


        //check exchangeItems.
        String[] nonEnsembleExchangeItemIds = wdmEnsembleTimeSeriesOutputDataObject.getExchangeItemIDs();
        assertNotNull(nonEnsembleExchangeItemIds);
        assertEquals(0, nonEnsembleExchangeItemIds.length);
        try {
            wdmEnsembleTimeSeriesOutputDataObject.getDataObjectExchangeItem("RCH103.FLOW");
            fail("Expected exception not thrown.");
        } catch (IllegalStateException e) {
        }

        int[] ensembleMemberIndices = wdmEnsembleTimeSeriesOutputDataObject.getEnsembleMemberIndices();
        assertEquals(2, ensembleMemberIndices.length);
        assertEquals(1, ensembleMemberIndices[0]);
        assertEquals(2, ensembleMemberIndices[1]);

        String[] ensembleExchangeItemIds = wdmEnsembleTimeSeriesOutputDataObject.getEnsembleExchangeItemIds();
        assertEquals(2, ensembleExchangeItemIds.length);
        assertEquals("RCH103.FLOW", ensembleExchangeItemIds[0]);
        assertEquals("RCH104.BOD", ensembleExchangeItemIds[1]);


        //check id for RCH103.FLOW
        IExchangeItem exchangeItem = wdmEnsembleTimeSeriesOutputDataObject.getDataObjectExchangeItem("RCH103.FLOW", 2);
        assertEquals("RCH103.FLOW", exchangeItem.getId());

        //check times for RCH103.FLOW
        double[] times1 = exchangeItem.getTimes();
        assertNotNull(times1);
        assertEquals(731, times1.length);
        double currentModifiedJulianDate = startModifiedJulianDate;
        for (int n = 0; n < times1.length; n++) {
            assertEquals(currentModifiedJulianDate, times1[n]);
            currentModifiedJulianDate++;
        }
        //check values for RCH103.FLOW
        double[] values1 = exchangeItem.getValuesAsDoubles();
        assertNotNull(values1);
        assertEquals(731, values1.length);
        assertEquals(38.29, values1[0], 1e-5);
        assertEquals(111.96, values1[64], 1e-6);
        assertEquals(1772.92, values1[207], 1e-3);
        assertEquals(1757.17, values1[228], 1e-3);
        assertEquals(2075.54, values1[229], 1e-3);
        assertEquals(27.17, values1[times1.length - 1], 1e-6);

        //check id for RCH104.BOD
        exchangeItem = wdmEnsembleTimeSeriesOutputDataObject.getDataObjectExchangeItem("RCH104.BOD", 1);
        assertEquals("RCH104.BOD", exchangeItem.getId());

        //check times for RCH104.BOD
        double[] times2 = exchangeItem.getTimes();
        assertNotNull(times2);
        assertEquals(1096, times2.length);
        currentModifiedJulianDate = startModifiedJulianDate;
        for (int n = 0; n < times1.length; n++) {
            assertEquals(currentModifiedJulianDate, times1[n]);
            currentModifiedJulianDate++;
        }
        //check values for RCH104.BOD
        double[] values2 = exchangeItem.getValuesAsDoubles();
        assertNotNull(values2);
        assertEquals(1096, values2.length);
        assertEquals(-999.0, values2[0], 1e-6);
        assertEquals(1.2, values2[8], 1e-6);
        assertEquals(1.3, values2[43], 1e-6);
        assertEquals(0.9, values2[71], 1e-6);
        assertEquals(0.8, values2[104], 1e-6);
        assertEquals(-999.0, values2[times2.length - 1], 1e-6);
    }

    /**
     * Test writing time series into an existing dataSet in an existing wdm file.
     *
     * @throws Exception
     */
    public void testWriteTimeSeries() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testWriteTimeSeries: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm file from template to work directory to start with a fresh file before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String templateOutputFileName = "wdmTimeSeriesTest/template/OBS(ND).wdm";
        File templateOutputFile = new File(testRunDataDir, templateOutputFileName);
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeOutputFilePath = "wdmTimeSeriesTest/work/OBS(ND).wdm";
        File outputFile = new File(testRunDataDir, relativeOutputFilePath);
        //delete outputFile if present (e.g. from previous test).
        if (outputFile.exists()) {
            outputFile.delete();
        }
        BBUtils.copyFile(templateOutputFile, outputFile);
        assertTrue(outputFile.exists());

        WdmTimeSeriesIoObject wdmTimeSeriesIoObject1 = new WdmTimeSeriesIoObject();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmDllFilePath = "../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll";
        String relativeWdmMessageFilePath = "../../../../../model_hspf/native_bin/MESSAGE.WDM";
        String[] arguments1 = new String[]{relativeWdmDllFilePath, relativeWdmMessageFilePath,
                "input", "0", "TSTART1", "TSTOP1", "RCH103.FLOW", "RCH104.BOD"};
        wdmTimeSeriesIoObject1.initialize(testRunDataDir, relativeOutputFilePath, arguments1);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems1 = wdmTimeSeriesIoObject1.getExchangeItems();
        assertEquals(4, exchangeItems1.length);

        //set times and values.
        double[] expectedTimes = new double[546];
        //written period: MJD 54832.0 (2009-01-01 00:00) to MJD 55378.0 (2010-07-01 00:00).
        double currentModifiedJulianDate = 54832;
        for (int n = 0; n < expectedTimes.length; n++) {
            expectedTimes[n] = currentModifiedJulianDate;
            currentModifiedJulianDate++;
        }
        double[] expectedValues = new double[expectedTimes.length];
        for (int n = 0; n < expectedValues.length; n++) {
            expectedValues[n] = n;
        }
        expectedValues[0] = 3.5;
        expectedValues[1] = 2.5;
        expectedValues[2] = -999;
        expectedValues[3] = -30000;
        expectedValues[4] = 105;
        expectedValues[5] = -5;
        expectedValues[6] = 0;
        expectedValues[7] = 5;
        expectedValues[8] = 15000;
        expectedValues[9] = 1;
        for (IPrevExchangeItem exchangeItem1 : exchangeItems1) {
            if ("TSTART1".equalsIgnoreCase(exchangeItem1.getId())) {
                exchangeItem1.setValues(expectedTimes[0]);

            } else if ("TSTOP1".equalsIgnoreCase(exchangeItem1.getId())) {
                exchangeItem1.setValues(expectedTimes[expectedTimes.length - 1]);

            } else if ("RCH103.FLOW".equalsIgnoreCase(exchangeItem1.getId())) {
                exchangeItem1.setTimes(expectedTimes);
                exchangeItem1.setValuesAsDoubles(expectedValues);

            } else if ("RCH104.BOD".equalsIgnoreCase(exchangeItem1.getId())) {
                exchangeItem1.setTimes(expectedTimes);
                exchangeItem1.setValuesAsDoubles(expectedValues);
            }
        }

        //write data to file.
        wdmTimeSeriesIoObject1.finish();

        //create a new empty WdmTimeSeriesIoObject to read and check the data.
        //existing period: MJD 54466.0 (2008-01-01 00:00) to MJD 55197.0 (2010-01-01 00:00).
        //written period: MJD 54832.0 (2009-01-01 00:00) to MJD 55378.0 (2010-07-01 00:00).
        //resulting period: MJD 54466.0 (2008-01-01 00:00) to MJD 55378.0 (2010-07-01 00:00).
        //check period: MJD 54832.0 (2009-01-01 00:00) to MJD 55378.0 (2010-07-01 00:00).
        //MJD 54832.0 is 2009-01-01 00:00.
        double startModifiedJulianDate = 54832;
        //MJD 55378.0 is 2010-07-01 00:00.
        double endModifiedJulianDate = 55378;
        WdmTimeSeriesIoObject wdmTimeSeriesIoObject2 = new WdmTimeSeriesIoObject();
        String[] arguments2 = new String[]{relativeWdmDllFilePath, relativeWdmMessageFilePath, "output", "0",
                TimeUtils.mjdToString(startModifiedJulianDate), TimeUtils.mjdToString(endModifiedJulianDate),
                "RCH103.FLOW", "RCH104.BOD"};
        wdmTimeSeriesIoObject2.initialize(testRunDataDir, relativeOutputFilePath, arguments2);

        //get all exchangeItems.
        IPrevExchangeItem[] exchangeItems2 = wdmTimeSeriesIoObject2.getExchangeItems();
        assertEquals(2, exchangeItems2.length);

        //get exchangeItem for RCH103.FLOW
        String timeSeriesId = "RCH103.FLOW";
        IPrevExchangeItem currentExchangeItem = null;
        for (IPrevExchangeItem exchangeItem : exchangeItems2) {
            if (timeSeriesId.equalsIgnoreCase(exchangeItem.getId())) {
                currentExchangeItem = exchangeItem;
                break;
            }
        }
        if (currentExchangeItem == null) {
            fail("ExchangeItem for time series id '" + timeSeriesId + "' not found.");
            return;
        }

        //check times.
        double[] times = currentExchangeItem.getTimes();
        assertNotNull(times);
        //check times.
        //check period: MJD 54832.0 (2009-01-01 00:00) to MJD 55378.0 (2010-07-01 00:00).
        assertEquals(expectedTimes.length, times.length);
        for (int n = 0; n < expectedTimes.length; n++) {
            assertEquals(expectedTimes[n], times[n]);
        }

        //check values.
        double[] values = currentExchangeItem.getValuesAsDoubles();
        assertNotNull(values);
        //check values.
        //check period: MJD 54832.0 (2009-01-01 00:00) to MJD 54378.0 (2010-07-01 00:00).
        assertEquals(expectedValues.length, values.length);
        for (int n = 0; n < expectedValues.length; n++) {
            assertEquals(expectedValues[n], values[n]);
        }
    }

    /**
     * Tests that no exceptions occur when opening, reading, writing
     * and closing wdm files multiple times. This can happen if the
     * wdm message file is not used in the proper way.
     *
     * @throws Exception
     */
    public void testReadAndWriteTimeSeriesMultipleTimes() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testReadAndWriteTimeSeriesMultipleTimes: wdm.dll only available for win32.");
			return;
		}

        for (int n = 1; n <= 20; n++) {
            testReadTimeSeries();
            testWriteTimeSeries();
            System.out.println(n);
        }
    }

    public void testConvertWdmToNetcdfWithDataCopier() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testConvertWdmToNetcdfWithDataCopier: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm file from template to work directory to start with a fresh file before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String templateInputFileName = "wdmTimeSeriesTest/template/OBS(ND).wdm";
        File templateInputFile = new File(testRunDataDir, templateInputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmTimeSeriesTest/work/OBS(ND).wdm";
        File inputFile = new File(testRunDataDir, inputFileName);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile.exists()) {
            inputFile.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile);
        assertTrue(inputFile.exists());

        //delete outputFile if present (e.g. from previous test).
        File outputFile = new File(testRunDataDir, "wdmTimeSeriesTest/work/output.nc");
        if (outputFile.exists()) {
            outputFile.delete();
        }

        //MJD 54466.0 is 2008-01-01 00:00.
        double startModifiedJulianDate = 54466;
        //MJD 55562.0 is 2011-01-01 00:00.
        double endModifiedJulianDate = 55562;

        String inputClassName = WdmTimeSeriesDataObject.class.getName();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        //Note 1: DataCopier passes the parent folder of the input/output file as workingDir to the input/output DataObject.
        //Note 2: when running DataCopier from command line, these input arguments should be surrounded by double quotes as a whole, so that it becomes one large argument.
        String inputArgumentsAsOne = "../../../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll ../../../../../../../model_hspf/native_bin/MESSAGE.WDM output 0 "
                + TimeUtils.mjdToString(startModifiedJulianDate) + " " + TimeUtils.mjdToString(endModifiedJulianDate) + " RCH103.FLOW RCH104.BOD";
        String outputClassName = NetcdfDataObject.class.getName();
        String outputArgumentsAsOne = "true false";

        DataCopier.main(new String[]{"-c", inputClassName, "-a", inputArgumentsAsOne, inputFile.getAbsolutePath(), "-c", outputClassName, "-a", outputArgumentsAsOne, outputFile.getAbsolutePath()});

        //compare actual output file with expected output file.
        //convert netcdf data to text for human readable text comparison.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        File expectedOutputFile = new File(this.testRunDataDir, "wdmTimeSeriesTest/expected_results/TestConvertWdmToNetcdfWithDataCopier_expected_output.nc");
        OpenDaTestSupport.compareNetcdfFiles(expectedOutputFile, outputFile);
    }

    public void testConvertEnsembleWdmToEnsembleNetcdfWithDataCopier() throws Exception {
        //currently only wdm.dll (32 bit) available, so only run this test on win32.
        if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testConvertEnsembleWdmToEnsembleNetcdfWithDataCopier: wdm.dll only available for win32.");
            return;
        }

        //first copy input wdm file from template to work directory to start with a fresh file before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String templateInputFileName = "wdmTimeSeriesTest/template/OBS(ND).wdm";
        File templateInputFile = new File(testRunDataDir, templateInputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName1 = "wdmTimeSeriesTest/work/OBS(ND)_1.wdm";
        File inputFile1 = new File(testRunDataDir, inputFileName1);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile1.exists()) {
            inputFile1.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile1);
        assertTrue(inputFile1.exists());

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName2 = "wdmTimeSeriesTest/work/OBS(ND)_2.wdm";
        File inputFile2 = new File(testRunDataDir, inputFileName2);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile2.exists()) {
            inputFile2.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile2);
        assertTrue(inputFile2.exists());

        //delete outputFile if present (e.g. from previous test).
        File outputFile = new File(testRunDataDir, "wdmTimeSeriesTest/work/ensemble_output.nc");
        if (outputFile.exists()) {
            outputFile.delete();
        }

        //MJD 54466.0 is 2008-01-01 00:00.
        double startModifiedJulianDate = 54466;
        //MJD 55562.0 is 2011-01-01 00:00.
        double endModifiedJulianDate = 55562;

        String inputClassName = WdmEnsembleTimeSeriesOutputDataObject.class.getName();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        File inputFilePrefix = new File(testRunDataDir, "wdmTimeSeriesTest/work/OBS(ND)_");
        //Note 1: DataCopier passes the parent folder of the input/output file as workingDir to the input/output DataObject.
        //Note 2: when running DataCopier from command line, these input arguments should be surrounded by double quotes as a whole, so that it becomes one large argument.
        String inputArgumentsAsOne = "../../../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll ../../../../../../../model_hspf/native_bin/MESSAGE.WDM output 0 "
                + TimeUtils.mjdToString(startModifiedJulianDate) + " " + TimeUtils.mjdToString(endModifiedJulianDate) + " RCH103.FLOW RCH104.BOD";

        String outputClassName = NetcdfDataObject.class.getName();
        String outputArgumentsAsOne = "true false";

        DataCopier.main(new String[]{"-c", inputClassName, "-a", inputArgumentsAsOne, inputFilePrefix.getAbsolutePath(), "-c", outputClassName, "-a", outputArgumentsAsOne, outputFile.getAbsolutePath()});

        //compare actual output file with expected output file.
        //convert netcdf data to text for human readable text comparison.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        File expectedOutputFile = new File(this.testRunDataDir, "wdmTimeSeriesTest/expected_results/TestConvertEnsembleWdmToEnsembleNetcdfWithDataCopier_expected_output.nc");
        OpenDaTestSupport.compareNetcdfFiles(expectedOutputFile, outputFile, null);
    }

    public void testConvertWdmToUciWithDataCopier() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testConvertWdmToUciWithDataCopier: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm file from template to work directory to start with a fresh file before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        File templateInputFile = new File(testRunDataDir, "wdmTimeSeriesTest/template/nd-out.wdm");
        //copy output uci file that should be modified from template to work directory to start with a fresh file before running the test.
        File templateOutputFile = new File(testRunDataDir, "wdmTimeSeriesTest/template/ndriver_state_file.uci");
		String defaultUciFilename = "ndriver_state_file_default.uci";
		File defaultUciFile = new File(testRunDataDir + "/wdmTimeSeriesTest/work/", defaultUciFilename);
		BBUtils.copyFile(templateOutputFile, defaultUciFile);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmTimeSeriesTest/work/nd-out.wdm";
        File inputFile = new File(testRunDataDir, inputFileName);
        //delete inputFile if present (e.g. from previous test).
        if (inputFile.exists()) {
            inputFile.delete();
        }
        BBUtils.copyFile(templateInputFile, inputFile);
        assertTrue(inputFile.exists());

        //delete outputFile if present (e.g. from previous test).
        File outputFile = new File(testRunDataDir, "wdmTimeSeriesTest/work/ndriver_state_file.uci");
        if (outputFile.exists()) {
            outputFile.delete();
        }
        BBUtils.copyFile(templateOutputFile, outputFile);
        assertTrue(outputFile.exists());

        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeUtils.createTimeZoneFromDouble(9));
        calendar.set(2011, 7, 9, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
        calendar.set(2011, 7, 10, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        double endDate = Time.milliesToMjd(calendar.getTimeInMillis());

        String inputClassName = WdmTimeSeriesDataObject.class.getName();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        //Note 1: DataCopier passes the parent folder of the input/output file as workingDir to the input/output DataObject.
        //Note 2: when running DataCopier from command line, these input arguments should be surrounded by double quotes as a whole, so that it becomes one large argument.
        String inputArgumentsAsOne = "../../../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll ../../../../../../../model_hspf/native_bin/MESSAGE.WDM output 0 "
                + TimeUtils.mjdToString(startDate) + " " + TimeUtils.mjdToString(endDate);

        String outputClassName = UciStateDataObject.class.getName();
        String outputArgumentsAsOne = TimeUtils.mjdToString(endDate) + " -3600 " + defaultUciFilename;

        DataCopier.main(new String[]{"-c", inputClassName, "-a", inputArgumentsAsOne, inputFile.getAbsolutePath(), "-c", outputClassName, "-a", outputArgumentsAsOne, outputFile.getAbsolutePath()});

        //compare actual output file with expected output file.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        File expectedOutputFile = new File(this.testRunDataDir, "wdmTimeSeriesTest/expected_results/TestConvertWdmToUciWithDataCopier_expected_output.uci");
        assertEquals("Actual output file '" + outputFile + "' does not equal expected output file '" + expectedOutputFile + "'.",
                AsciiFileUtils.readText(expectedOutputFile), AsciiFileUtils.readText(outputFile));
    }
}
