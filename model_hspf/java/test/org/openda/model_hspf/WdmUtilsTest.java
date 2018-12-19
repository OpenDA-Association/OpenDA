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
import java.util.List;

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

/**
 * Test class for testing WdmUtils.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class WdmUtilsTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(WdmUtilsTest.class, "model_hspf");
        testRunDataDir = testData.getTestRunDataDir();
    }

    /**
     * This method was run manually to create the POINT_mapping.txt file that is
     * used for testGetDataSetNumber.
     *
     * @throws Exception
     */
    public void manualCreateMapping() throws Exception {
        //currently only wdm.dll available (not wdm.so), so only run this on windows.
        if (!BBUtils.RUNNING_ON_WINDOWS) {
            return;
        }

        //first copy input wdm files to directory openda_public/opendaTestRuns before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmUtilsTest/POINT.wdm";
        File inputFile = new File(testRunDataDir, inputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmDllPath = "../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll";
        File wdmDllFile = new File(testRunDataDir, relativeWdmDllPath);
        WdmDll.initialize(wdmDllFile);
        WdmDll wdmDll = WdmDll.getInstance();

        //open wdm file.
        int wdmFileNumber = WdmUtils.generateUniqueFortranFileUnitNumber();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmMessageFilePath = "../../../../../model_hspf/native_bin/MESSAGE.WDM";
        File wdmMessageFile = new File(testRunDataDir, relativeWdmMessageFilePath);
        WdmUtils.openWdmFile(wdmDll, wdmFileNumber, inputFile.getAbsolutePath(), wdmMessageFile.getAbsolutePath());

        System.out.println("DSN Constituent Location StationName");
        int dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, 1);
        while (dataSetNumber != -1) {
            //get the first attributeValue.
            String constituent = WdmUtils.getConstituentAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            String location = WdmUtils.getLocationAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            String stationName = WdmUtils.getStationNameAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            System.out.println(dataSetNumber + " constituent=" + constituent + " location=" + location + " stationName=" + stationName);

            //get next existing dataSet and put its number in the variable dataSetNumber.
            dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, dataSetNumber + 1);
        }

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmFileNumber);
    }

    /**
     * Tests method WdmUtils.searchDataSetNumber.
     */
    public void testSearchDataSetNumber() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testSearchDataSetNumber: wdm.dll only available for win32.");
			return;
		}

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmUtilsTest/POINT.wdm";
        File inputFile = new File(testRunDataDir, inputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmDllPath = "../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll";
        File wdmDllFile = new File(testRunDataDir, relativeWdmDllPath);
        WdmDll.initialize(wdmDllFile);
        WdmDll wdmDll = WdmDll.getInstance();

        //get expected mapping.
        File mappingFile = new File(testRunDataDir, "wdmUtilsTest/POINT_mapping.txt");
        List<String> content = AsciiFileUtils.readLines(mappingFile);
        String[][] mapping = new String[content.size() - 1][];
        for (int n = 1; n < content.size(); n++) {
            String[] strings = content.get(n).trim().split("\\s+");
            String dataSetNumber = strings[0];
            String parameter = strings[2];
            String description = strings[3];
            mapping[n - 1] = new String[]{dataSetNumber, description, parameter};
        }

        //open wdm file.
        int wdmFileNumber = WdmUtils.generateUniqueFortranFileUnitNumber();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmMessageFilePath = "../../../../../model_hspf/native_bin/MESSAGE.WDM";
        File wdmMessageFile = new File(testRunDataDir, relativeWdmMessageFilePath);
        WdmUtils.openWdmFile(wdmDll, wdmFileNumber, inputFile.getAbsolutePath(), wdmMessageFile.getAbsolutePath());

        //for each location and parameter search the dataSetNumber and compare with expected dataSetNumber.
        for (int n = 0; n < mapping.length; n++) {
            int expectedDataSetNumber = Integer.parseInt(mapping[n][0]);

            String location = mapping[n][1];
            String parameter = mapping[n][2];
            int actualDataSetNumber = WdmUtils.searchDataSetNumber(wdmDll, wdmFileNumber, location, parameter);

            System.out.println(expectedDataSetNumber + " " + parameter + " " + location + " " + actualDataSetNumber);
            assertEquals(expectedDataSetNumber, actualDataSetNumber);
        }

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmFileNumber);
    }

    /**
     * Tests method WdmUtils.createExchangeItemsFromFile.
     */
    public void testCreateExchangeItemsFromFile() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testCreateExchangeItemsFromFile: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm files to directory openda_public/opendaTestRuns before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmUtilsTest/POINT.wdm";
        File inputFile = new File(testRunDataDir, inputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmDllPath = "../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll";
        File wdmDllFile = new File(testRunDataDir, relativeWdmDllPath);
        WdmDll.initialize(wdmDllFile);
        WdmDll wdmDll = WdmDll.getInstance();

        //open wdm file.
        int wdmFileNumber = WdmUtils.generateUniqueFortranFileUnitNumber();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmMessageFilePath = "../../../../../model_hspf/native_bin/MESSAGE.WDM";
        File wdmMessageFile = new File(testRunDataDir, relativeWdmMessageFilePath);
        WdmUtils.openWdmFile(wdmDll, wdmFileNumber, inputFile.getAbsolutePath(), wdmMessageFile.getAbsolutePath());

        List<WdmTimeSeriesExchangeItem> exchangeItems = WdmUtils.createExchangeItemsFromFile(wdmDll, wdmFileNumber, IPrevExchangeItem.Role.Input);

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmFileNumber);

        assertEquals(923, exchangeItems.size());
        WdmTimeSeriesExchangeItem exchangeItem = exchangeItems.get(42);
        assertEquals(IPrevExchangeItem.Role.Input, exchangeItem.getRole());
        assertEquals("48A0341.FLOW", exchangeItem.getId());
        assertEquals("48A0341", exchangeItem.getLocation());
        assertEquals("FLOW", exchangeItem.getQuantityId());
    }

    /**
     * Tests method WdmUtils.createExchangeItemsFromList.
     */
    public void testCreateExchangeItemsFromList() throws Exception {
		//currently only wdm.dll (32 bit) available, so only run this test on win32.
		if (!BBUtils.RUNNING_ON_WINDOWS || BBUtils.RUNNING_ON_64bit) {
			System.out.println("testCreateExchangeItemsFromList: wdm.dll only available for win32.");
			return;
		}

        //first copy input wdm files to directory openda_public/opendaTestRuns before running the test.
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String inputFileName = "wdmUtilsTest/POINT.wdm";
        File inputFile = new File(testRunDataDir, inputFileName);

        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmDllPath = "../../../../../model_hspf/native_bin/win32_gfortran/wdm.dll";
        File wdmDllFile = new File(testRunDataDir, relativeWdmDllPath);
        WdmDll.initialize(wdmDllFile);
        WdmDll wdmDll = WdmDll.getInstance();

        //open wdm file.
        int wdmFileNumber = WdmUtils.generateUniqueFortranFileUnitNumber();
        //working directory (testRunDataDir) is openda_public/opendaTestRuns/model_hspf/org/openda/model_hspf
        String relativeWdmMessageFilePath = "../../../../../model_hspf/native_bin/MESSAGE.WDM";
        File wdmMessageFile = new File(testRunDataDir, relativeWdmMessageFilePath);
        WdmUtils.openWdmFile(wdmDll, wdmFileNumber, inputFile.getAbsolutePath(), wdmMessageFile.getAbsolutePath());

        String[] timeSeriesIdList = new String[]{"48A0341.FLOW", "47A0211.ORP"};
        List<WdmTimeSeriesExchangeItem> exchangeItems = WdmUtils.createExchangeItemsFromList(wdmDll, wdmFileNumber, inputFile.getAbsolutePath(), IPrevExchangeItem.Role.Input, timeSeriesIdList);

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmFileNumber);

        assertEquals(2, exchangeItems.size());
        WdmTimeSeriesExchangeItem exchangeItem1 = exchangeItems.get(0);
        assertEquals(IPrevExchangeItem.Role.Input, exchangeItem1.getRole());
        assertEquals("48A0341.FLOW", exchangeItem1.getId());
        assertEquals("48A0341", exchangeItem1.getLocation());
        assertEquals("FLOW", exchangeItem1.getQuantityId());
        WdmTimeSeriesExchangeItem exchangeItem2 = exchangeItems.get(1);
        assertEquals(IPrevExchangeItem.Role.Input, exchangeItem2.getRole());
        assertEquals("47A0211.ORP", exchangeItem2.getId());
        assertEquals("47A0211", exchangeItem2.getLocation());
        assertEquals("ORP", exchangeItem2.getQuantityId());
    }
}
