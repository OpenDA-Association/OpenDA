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
import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

/**
 * Test class for testing BBTemplateConfig reader and class
 */
public class BBTemplateConfigTest extends TestCase {
    private OpenDaTestSupport testData;
    private File testRunDataDir;
    private static String CONFIG_FILENAME = "BBTemplateConfig.xml";
    private static String TEST_SUBDIR = "template";
    static String FIRST_TEMPLATEFILENAME = "template.inp";
    static String FIRST_TARGETFILENAME = "output.inp";
    static String SECOND_TEMPLATEFILENAME = "barriers.template";
    static String SECOND_TARGETFILENAME = "barriers.inc";

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(BBTemplateConfigTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }


    public void testReadTemplateConfigFile() {
        BBTemplateConfigReader templateConfigReader = new BBTemplateConfigReader(new File(testRunDataDir, TEST_SUBDIR + "/" + CONFIG_FILENAME));
        BBTemplateConfig bbTemplateConfig = templateConfigReader.getBBTemplateConfig();

        TemplateKeyDefinitions keyDefinitions = bbTemplateConfig.getKeyDefinitions();
        assertTrue("keyDefinitions is null", keyDefinitions != null);
        assertTrue("keyDefinitions.getKeyDefinitions() is null", keyDefinitions.getKeyDefinitions() != null);

        assertEquals("Number of keyDefinitions not equal", 5, keyDefinitions.getKeyDefinitions().values().size());


        assertTrue("bbTemplateConfig.getTemplateFiles() is null", bbTemplateConfig.getTemplateFiles() != null);
        assertEquals("Number of templateFile defintions not equal", 2, bbTemplateConfig.getTemplateFiles().size());

        //Take first template file definition
        for (BBTemplateFile bbTemplateFile : bbTemplateConfig.getTemplateFiles()) {
            if (bbTemplateFile.getTemplateFileName().equals(FIRST_TEMPLATEFILENAME)) {
                assertEquals("Target file name not equal", FIRST_TARGETFILENAME, bbTemplateFile.getTargetFileName());

                assertTrue("bbTemplateFile.getKeys() is null", bbTemplateFile.getKeys() != null);
                assertEquals("Number of used keys in templateFile not equal",4, bbTemplateFile.getKeys().size());

            }  else if (bbTemplateFile.getTemplateFileName().equals(SECOND_TEMPLATEFILENAME)) {
                assertEquals("Target file name not equal", SECOND_TARGETFILENAME, bbTemplateFile.getTargetFileName());

                assertTrue("bbTemplateFile.getKeys() is null", bbTemplateFile.getKeys() != null);
                assertEquals("Number of used keys in templateFile not equal",2, bbTemplateFile.getKeys().size());
            } else {
                //should not occur
                assertTrue("Template file names do not match ", false);
            }
        }


        assertEquals("valuesFile name not equal", "myValues.txt", bbTemplateConfig.getValuesFileName());
    }

    public void testPerformTemplateConfig() {
        BBTemplateConfigReader templateConfigReader = new BBTemplateConfigReader(new File(testRunDataDir, TEST_SUBDIR + "/" + CONFIG_FILENAME));
        BBTemplateConfig bbTemplateConfig = templateConfigReader.getBBTemplateConfig();

        assertTrue("Template config not found", bbTemplateConfig != null);
        assertTrue("Template Files not found", bbTemplateConfig.getTemplateFiles() != null);

        Collection<BBTemplateFile> templateFiles = bbTemplateConfig.getTemplateFiles();
        assertEquals("Number of defined template files not equal", 2, templateFiles.size());

        for (BBTemplateFile bbTemplateFile : templateFiles) {
            assertTrue("bbTemplateFile is null", bbTemplateFile != null);
            bbTemplateFile.generateTargetFile(new File(testRunDataDir, TEST_SUBDIR));
            String targetFileName = bbTemplateFile.getTargetFileName();
            File targetFile = new File(testRunDataDir, TEST_SUBDIR + "/" +  targetFileName);
            assertTrue("Target file does not exist", targetFile.exists());

            File outputRefFile=new File(testRunDataDir,TEST_SUBDIR + "/" + bbTemplateFile.getTargetFileName() + "_expected");
            boolean identical = testData.FilesAreIdentical(targetFile, outputRefFile);
            assertTrue("Files are not identical: actual file " + targetFile + "\n does not equal expected file " + outputRefFile, identical);
        }

    }
}
