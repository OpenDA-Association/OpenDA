/* OpenDA v2.4.3 
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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Delft3D result files
 */
public class D3dResultsTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dResultsTest.class,"public","model_delft3d");
    }

    public static void testDummy() {
        // No action. Test only exist to avoid warnings on empty test class when
        //            the test below is de-activated by renaming it to tst...()
    }

    public void tstTrihFileNoSelection() throws IOException {
    	// library with libODS.dll
        File moduleRootDir = testData.getModuleRootDir();
        System.out.println(moduleRootDir);

        File fortranDll;
        if (BBUtils.RUNNING_ON_WINDOWS) {
            fortranDll = new File(moduleRootDir, "native_bin/win32_ifort/ods.dll");
        } else if (System.getProperty("sun.arch.data.model").equals("64")){
            System.out.println("native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libODS.so.0.0.0");
            fortranDll = new File(moduleRootDir, "native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libODS.so.0.0.0");
        } else {
        	return;
        }

        File testDir = new File(testData.getTestRunDataDir(), "results");

        D3dResults d3dResults = new D3dResults();
        d3dResults.initialize(testDir, "trih-m27.dat", new String[] {fortranDll.getAbsolutePath()});

        assertEquals("#exchange items", 472, d3dResults.getExchangeItems().length);

        d3dResults.finish();
    }

    public void tstTrihFileThreeStations() throws IOException {
    	// library with libODS.dll
        File moduleRootDir = testData.getModuleRootDir();
        System.out.println(moduleRootDir);

        File fortranDll;
        if (BBUtils.RUNNING_ON_WINDOWS) {
            fortranDll = new File(moduleRootDir, "native_bin/win32_ifort/ods.dll");
        } else  if (System.getProperty("sun.arch.data.model").equals("64")){
            System.out.println("native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libODS.so.0.0.0");
            fortranDll = new File(moduleRootDir, "native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libODS.so.0.0.0");
        } else {
        	return;
        }

        String[] arguments = new String[] {
                fortranDll.getAbsolutePath(),
                "PORT SWETTENHAM  IHO.water level",
                "BROTHERS LIGHT HOIHO.water level",
                "LHO SEUMAWE      IHO.water level",
                "ANGLER BANK      IHO.water level" };

        double[] expectedFirstFiveValuesAtBrothersLight = {
                        0,
            -7.73189e-016,
            -8.17293e-016,
             4.50771e-016,
             6.80994e-016,
        };

        File testDir = new File(testData.getTestRunDataDir(), "results");

        D3dResults d3dResults = new D3dResults();
        d3dResults.initialize(testDir, "trih-m27.dat", arguments);

        IPrevExchangeItem[] exchangeItems = d3dResults.getExchangeItems();
        assertEquals("#exchange items", 4, exchangeItems.length);
        assertEquals("exchangeItems[2].getId", "LHO SEUMAWE      IHO.water level", exchangeItems[2].getId());
        double[] valuesAtBrothersLight = exchangeItems[1].getValuesAsDoubles();
        for (int i = 0; i < expectedFirstFiveValuesAtBrothersLight.length; i++) {
            assertEquals("exchangeItems[2].getId", expectedFirstFiveValuesAtBrothersLight[i], valuesAtBrothersLight[i], 1e-7);
        }

        d3dResults.finish();
    }
}
