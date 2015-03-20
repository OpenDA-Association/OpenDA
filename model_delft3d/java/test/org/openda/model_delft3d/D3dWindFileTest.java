package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for D3D flow black box wrapper wind field classes
 */
public class D3dWindFileTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dWindFileTest.class,"public","model_delft3d");
    }

    public void testWindFiles() throws IOException {

        File testDir = new File(testData.getTestRunDataDir(), "test_3");

        // Place undisturbed copy of files
        File winduBase = new File(testDir, "curvi_ll_corner_gridrow-base.amu");
        File windu = new File(testDir, "curvi_ll_corner_gridrow.amu");
        BBUtils.copyFile(winduBase, windu);


        // Read wind file, check content
        D3dWindFile winduFile = new D3dWindFile();
        winduFile.initialize(testDir, "test.mdf", new String[] {"gu"});
        IPrevExchangeItem[] windExchItems = winduFile.getExchangeItems();
        assertEquals("windExchItems.length", 1, windExchItems.length);
        assertEquals("exchItemwinduFile[0].id", "windgu", windExchItems[0].getId());
        double[] windValues = windExchItems[0].getValuesAsDoubles();
        assertEquals("exchItemWindFile[0].values[25]", 6.0, windValues[25]);
        assertEquals("exchItemWindFile[0].values[26]", 7.0, windValues[26]);



        // Adjust values in wind file, write file
        for (int i = 0; i < windValues.length; i++) {
            windValues[i] += 0.01;
        }
        windExchItems[0].setValuesAsDoubles(windValues);
        winduFile.finish();

        // Re-read roughness file, check changed vValues
        D3dWindFile adjustedWindFile = new D3dWindFile();
        adjustedWindFile.initialize(testDir, "test.mdf", new String[] {"gu"});
        double[] adjustedVValues = adjustedWindFile.getExchangeItems()[0].getValuesAsDoubles();
        assertEquals("exchItemWindFile[0].values[6]", 7.01, adjustedVValues[6]);
        assertEquals("exchItemWindFile[0].values[7]", 8.01, adjustedVValues[7]);
    }
}
