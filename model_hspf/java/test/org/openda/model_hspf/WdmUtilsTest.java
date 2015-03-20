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
import org.openda.utils.OpenDaTestSupport;

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

        //parameter is called constituent in WDMUtil.
        //description is called stationName in WDMUtil.
        System.out.println("DSN Location Constituent StationName");
        int dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, 1);
        while (dataSetNumber != -1) {
            //get the first attributeValue.
            String string;
            string = wdmDll.getAttributeValue(wdmFileNumber, dataSetNumber, 1);

            //see method WdmUtils.getDataSetNumber:
            //assumed here that the attribute string always contains
            //the scenario in characters with index 32 to 39 (both inclusive),
            //the parameter (constituent) in characters with index 40 to 47 (both inclusive),
            //the location in characters with index 48 to 55 (both inclusive),
            //the description (stationName) in characters with index 56 to 103 (both inclusive).
            if (string != null && string.length() >= 104) {
                String parameter = string.substring(40, 48).trim();
                String location = string.substring(48, 56).trim();
                String description = string.substring(56, 104).trim();
                System.out.println(dataSetNumber + " " + location
                        + " " + parameter + " " + description);
            } else {//if cannot get location and parameter from attribute.
                //ignore this dataSet (do nothing).
            }

            //get next existing dataSet and put its number in the variable dataSetNumber.
            dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, dataSetNumber + 1);
        }

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmFileNumber);
    }

    /**
     * Tests method WdmUtils.getDataSetNumber.
     *
     * @throws Exception
     */
    public void testGetDataSetNumber() throws Exception {
        //currently only wdm.dll available (not wdm.so), so only run this on windows.
        if (!BBUtils.RUNNING_ON_WINDOWS) {
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
        List<String> content = HspfUtils.readFile(mappingFile);
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
            int actualDataSetNumber = WdmUtils.getDataSetNumber(wdmDll, wdmFileNumber, location, parameter);

            System.out.println(expectedDataSetNumber + " " + parameter + " " + location + " " + actualDataSetNumber);
            assertEquals(expectedDataSetNumber, actualDataSetNumber);
        }

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmFileNumber);
    }
}
