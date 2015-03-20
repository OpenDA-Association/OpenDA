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
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for swan parameter specified in XML file
 */
public class SwanXMLParameterSettingsTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanXMLParameterSettingsTest.class, "model_swan");
    }

    public void testSwivtXmlParameters_1_2() {

        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "SwanParameters");

        // swivt 1_2
        File xmlParametersFile = new File(testRunDataParamFilesDir, "l021triad001-1-2.xml");
        File adjustedParameterFile = new File(testRunDataParamFilesDir, "l021triad001-1-2-adjusted.xml");
        performParameterTest(xmlParametersFile, adjustedParameterFile);

    }

    public void testSwivtXmlParameters_1_2_1() {

        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "SwanParameters");

        // swivt 1_2_1
        File xmlParametersFile = new File(testRunDataParamFilesDir, "l021triad001-1-2-1.xml");
        File adjustedParameterFile = new File(testRunDataParamFilesDir, "l021triad001-1-2-1-adjusted.xml");
        performParameterTest(xmlParametersFile, adjustedParameterFile);
    }

    private void performParameterTest(File xmlParametersFile, File adjustedParameterFile) {

        SwanXMLParameterSettings parameterSettings = new SwanXMLParameterSettings(xmlParametersFile);
        for (String id : parameterSettings.getIds()) {
                        Object value = parameterSettings.getValue(id);
                        if (value instanceof Double) {
                            double dValue = (Double) value;
                            dValue += 1e+5;
                            parameterSettings.setValue(id, dValue);
                        }
                        else if (value instanceof Integer) {
                            int iValue = (Integer) value;
                            iValue += 1e+5;
                            parameterSettings.setValue(id, iValue);
                        }
                    }

        parameterSettings.writeToFile(adjustedParameterFile);

        SwanXMLParameterSettings orgParameterSettings = new SwanXMLParameterSettings(xmlParametersFile);
        SwanXMLParameterSettings adjustedParameterSettings = new SwanXMLParameterSettings(adjustedParameterFile);
        for (String id : parameterSettings.getIds()) {
                        Object value = adjustedParameterSettings.getValue(id);
                        if (value instanceof Double) {
                            double adjustedDValue = (Double) value;
                            double orgDValue = (Double) orgParameterSettings.getValue(id);
                            assertEquals("org/adjusted for " + id, orgDValue + 1e+5, adjustedDValue);
                        }
                        else if (value instanceof Integer) {
                            int adjustedIValue = (Integer) value;
                            int orgIValue = (Integer) orgParameterSettings.getValue(id);
                            assertEquals("org/adjusted for " + id, orgIValue + 1e+5, adjustedIValue);
                        }
                    }
                }
}
