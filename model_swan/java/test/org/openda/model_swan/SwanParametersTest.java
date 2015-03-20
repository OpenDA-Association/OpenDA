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

package org.openda.model_swan;

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.model_swan.SwanParameters;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test for putting parameters in the swan input file
 */
public class SwanParametersTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanParametersTest.class, "model_swan");
    }

    public void testSwivtParameters() throws IOException {

		File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "SwanParameters");

        // swivt 1_2
        File xmlParametersFile = new File(testRunDataParamFilesDir, "l021triad001-1-2.xml");
        File swanInputTemplateFile = new File(testRunDataParamFilesDir, "l021triad001-1-2.swn");
        performParameterTests(xmlParametersFile, swanInputTemplateFile);

        // swivt 1_2_1
        xmlParametersFile = new File(testRunDataParamFilesDir, "l021triad001-1-2-1.xml");
        swanInputTemplateFile = new File(testRunDataParamFilesDir, "l021triad001-1-2-1.swn");
        performParameterTests(xmlParametersFile, swanInputTemplateFile);
    }

    private void performParameterTests(File xmlParametersFile, File swanInputTemplateFile) throws IOException {
        SwanXMLParameterSettings parameterSettings = new SwanXMLParameterSettings(
                xmlParametersFile);
        SwanParameters swanInput = new SwanParameters(swanInputTemplateFile);
        for (String groupKey : SwanParameters.groupKeys) {
            swanInput.setGroupActive(groupKey, parameterSettings.getGroupActive(groupKey));
        }
        for (String paramID : parameterSettings.getIds()) {
            swanInput.setParameterValue(SwanXMLParameterSettings.parseParamFromID(paramID), parameterSettings.getValue(paramID), false);
        }
        swanInput.write(new File(BBUtils.getFileNameWithOtherExtension(swanInputTemplateFile.getPath(), "input")));
    }

    public void testGetModelDescription() throws IOException {
        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "SwanParameters");
        File swanInputTemplateFile = new File(testRunDataParamFilesDir, "swantest_circle.swn");
        SwanParameters swanInput = new SwanParameters(swanInputTemplateFile);
//        assertEquals("getMMax", 251,swanInput.getMMax());
//        assertEquals("getNMax", 501,swanInput.getNMax());
        assertEquals("getNMax", 501*251,swanInput.getNMMax());
        assertEquals("getRFreq", 31,swanInput.getRFreq());
        assertEquals("getCDir", 36,swanInput.getCDir());

        File swanInputTemplateFile2 = new File(testRunDataParamFilesDir, "swantest_sector.swn");
        SwanParameters swanInput2 = new SwanParameters(swanInputTemplateFile2);
//        assertEquals("getMMax", 151,swanInput2.getMMax());
//        assertEquals("getNMax", 51,swanInput2.getNMax());
        assertEquals("getNMax", 151*51,swanInput2.getNMMax());
        assertEquals("getRFreq", 31,swanInput.getRFreq());
        assertEquals("getCDir", 37,swanInput2.getCDir());
        assertEquals("getMXInpWind", 2,swanInput2.getMXInpWind());
        assertEquals("getMYInpWind", 2,swanInput2.getMYInpWind());
        assertEquals("getTStartWind", "20081121.0000",swanInput2.getTStartWind());
        assertEquals("getTStopWind", "20081125.0000",swanInput2.getTStopWind());
        assertEquals("getFacWind", 1.0,swanInput2.getFacWind());
        assertEquals("getIDLAWind", 1,swanInput2.getIDLAWind());
        assertEquals("getNHEDFWind", 1,swanInput2.getNHEDFWind());
        assertEquals("getNHEDTWind", 1,swanInput2.getNHEDTWind());
        assertEquals("getNHEDVECWind", 1,swanInput2.getNHEDVECWind());
        assertEquals("getFormatWind", "FREE",swanInput2.getFormatWind());
        System.out.println("Wind file name: "+swanInput2.getFileNameWind());
        assertEquals("getGridType","REGULAR",swanInput2.getGridType());

        File swanInputTemplateFile3 = new File(testRunDataParamFilesDir, "swantestunstruct_circle.swn");
        SwanParameters swanInput3 = new SwanParameters(swanInputTemplateFile3);
        assertEquals("getCDir", 36,swanInput3.getCDir());
        assertEquals("getRFreq", 31,swanInput3.getRFreq());
        assertEquals("getGridType","UNSTRUCTURED",swanInput3.getGridType());
        System.out.println("node file name: "+swanInput3.getNodeFileName());

        assertEquals("swanInput3.getDtSimulation(): ","60",swanInput3.getDtSimulation());
        assertEquals("swanInput3.getTStartSimulation(): ","20100101.0000",swanInput3.getTStartSimulation());
        assertEquals("swanInput3.getTStopSimulation(): ","20100102.0000",swanInput3.getTStopSimulation());
        assertEquals("swanInput3.getDtUnitSimulation(): ","MIN",swanInput3.getDtUnitSimulation());

        File swanInputTemplateFile4 = new File(testRunDataParamFilesDir, "swan_nautboom.swn");
        SwanParameters swanInput4 = new SwanParameters(swanInputTemplateFile4);
        assertEquals("swanInput4.getMXInpWLevel(): ",201,swanInput4.getMXInpWLevel());
        assertEquals("swanInput4.getMYInpWLevel(): ",172,swanInput4.getMYInpWLevel());
        assertEquals("swanInput4.dtWLevel: ","30",swanInput4.getDtWLevel());
        assertEquals("swanInput4.getDtUnitWLevel(): ","MIN",swanInput4.getDtUnitWLevel());
        assertEquals("swanInput4.getTStartWLevel(): ","20100304.040000",swanInput4.getTStartWLevel());
        assertEquals("swanInput4.getTStopWLevel(): ","20100305.160000",swanInput4.getTStopWLevel());
        System.out.println("swanInput4.getFileNameWLevel(): "+swanInput4.getFileNameWLevel());
        System.out.println("swanInput4.getFactorWLevel(): "+swanInput4.getFactorWLevel());
        System.out.println("swanInput4.getIDLAWLevel(): "+swanInput4.getIDLAWLevel());

    }
}

