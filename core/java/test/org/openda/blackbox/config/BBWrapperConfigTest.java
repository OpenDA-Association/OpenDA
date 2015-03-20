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
package org.openda.blackbox.config;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

/**
 * Test class for testing BBWrapperConfig reader and class.
 */
public class BBWrapperConfigTest extends TestCase {
    private OpenDaTestSupport testData;
    private File testRunDataDir;

    private static final String CONFIG_FILENAME = "BBWrapperConfigTest_config.xml";
    private static final String TEST_SUBDIR = "wrapper";

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(BBWrapperConfigTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testAliasesInActionArgumentsInWrapperConfig() {
        BBWrapperConfigReader wrapperConfigReader =
                new BBWrapperConfigReader(new File(testRunDataDir, TEST_SUBDIR + "/" + CONFIG_FILENAME));
        BBWrapperConfig bbWrapperConfig = wrapperConfigReader.getBBWrapperConfig();
        bbWrapperConfig.getAliasDefinitions().add("instanceNumber", "%", "%", "0", null);

        //get all action arguments.
        Iterator<BBAction> iterator = bbWrapperConfig.getComputeActions().iterator();
        String[][] allArguments = new String[bbWrapperConfig.getComputeActions().size()][];
        int count = 0;
        while (iterator.hasNext()) {
            BBAction bbAction = iterator.next();
            allArguments[count] = bbAction.getArguments();
            count++;
        }

        assertEquals(1, allArguments.length);
        assertEquals(4, allArguments[0].length);
        assertEquals("128.303.0.1", allArguments[0][0]);
        assertEquals("dir/work0", allArguments[0][1]);
        assertEquals("anotherDir/file.DAT", allArguments[0][2]);
        assertEquals("9.0", allArguments[0][3]);
    }
}
