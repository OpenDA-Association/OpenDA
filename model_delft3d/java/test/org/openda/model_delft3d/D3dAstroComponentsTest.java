package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for D3D flow black box wrapper astro component boundaries
 */
public class D3dAstroComponentsTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dAstroComponentsTest.class,"public","model_delft3d");
    }

    public void testAstroFiles() throws IOException {
    	double delta=0.0001;

        File testDir = new File(testData.getTestRunDataDir(), "test_2");

        // Place undisturbed copy of files
        File astroBase = new File(testDir, "astro-base.bca");
        File astro = new File(testDir, "astro.bca");
        BBUtils.copyFile(astroBase, astro);
        File correctionBase = new File(testDir, "correction-base.cor");
        File correction = new File(testDir, "correction.cor");
        BBUtils.copyFile(correctionBase, correction);

        // Read astro files, check content
        D3dAstroComponentFiles astroFiles = new D3dAstroComponentFiles();
        astroFiles.initialize(testDir, "m27.mdf", new String[]{});
        IPrevExchangeItem[] bcaExchItems = astroFiles.getExchangeItems();
        assertEquals("bcaExchItems.length", 374, bcaExchItems.length);
        assertEquals("bcaExchItems[ 0].id", "BND01.A0", bcaExchItems[ 0].getId());
        assertEquals("bcaExchItems[ 5].id", "BND01.P1.Amplitude", bcaExchItems[ 5].getId());

        for (IPrevExchangeItem bcaExchItem : bcaExchItems) {
            System.out.println(bcaExchItem.getId() + "\t" + bcaExchItem.getValues().toString());
        }
        assertEquals("bcaExchItems[ 0].values[0]",  0.0, bcaExchItems[ 0].getValuesAsDoubles()[0]);
        assertEquals("bcaExchItems[ 5].values[0]",  0.0309, bcaExchItems[ 5].getValuesAsDoubles()[0]);

        assertEquals("bcaExchItems[ 0].value", 0.0, bcaExchItems[ 0].getValues());
        assertEquals("bcaExchItems[ 5].value", 0.0309, bcaExchItems[ 5].getValues());

        // Adjust astro-values, write file
        bcaExchItems[ 0].setValues(12.0);
        bcaExchItems[ 5].setValues(14.0);
        astroFiles.finish();

        // Re-read astro file, check changed vales
        astroFiles = new D3dAstroComponentFiles();
        astroFiles.initialize(testDir, "m27.mdf", new String[]{});
        bcaExchItems = astroFiles.getExchangeItems();
        assertEquals("bcaExchItems.length", 374, bcaExchItems.length);
        assertEquals("bcaExchItems[ 0].id", "BND01.A0", bcaExchItems[ 0].getId());
        assertEquals("bcaExchItems[ 5].id", "BND01.P1.Amplitude", bcaExchItems[ 5].getId());

        assertEquals("bcaExchItems[ 0].values[0]",  12.0, bcaExchItems[ 0].getValuesAsDoubles()[0],delta);
        assertEquals("bcaExchItems[ 5].values[0]",  14.0, bcaExchItems[ 5].getValuesAsDoubles()[0],delta);

        assertEquals("bcaExchItems[ 0].value", 12.0, new Double(bcaExchItems[ 0].getValues().toString()),delta);
        assertEquals("bcaExchItems[ 5].value", 14.0, new Double(bcaExchItems[ 5].getValues().toString()),delta);

        astroFiles.finish();
    }
}
