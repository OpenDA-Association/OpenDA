package org.openda.model_openfoam;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by werner on 25/03/16.
 */
public class DictionaryNoisyDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(DictionaryNoisyDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testInitialize() {
        IDataObject dataObject = new DictionaryNoisyDataObject();
        dataObject.initialize(testRunDataDir, new String[]{"system/HEATING_ALLEY01" });
    }

    public void testGetExchangeItems() {
        IDataObject dataObject = new DictionaryNoisyDataObject();
        dataObject.initialize(testRunDataDir, new String[]{"system/HEATING_ALLEY01" });
        String[] ids = dataObject.getExchangeItemIDs();
        assertEquals( 4, ids.length);
        Arrays.sort(ids);
        assertEquals("oda:HeatAG04@1", ids[0]);
        assertEquals("oda:HeatAG04@1.noise", ids[1]);
        assertEquals("oda:HeatAG04@2", ids[2]);
        assertEquals("oda:HeatAG04@2.noise", ids[3]);
    }

    public void testSetExchangeItems() {
        IDataObject dataObject = new DictionaryNoisyDataObject();
        File original = new File(testRunDataDir, "system/HEATING_ALLEY01.noise");
        File modified = new File(testRunDataDir, "output/HEATING_ALLEY01");
        dataObject.initialize(testRunDataDir, new String[]{"output/HEATING_ALLEY01" });
        IExchangeItem item = dataObject.getDataObjectExchangeItem("oda:HeatAG04@1");
        item.setValues( 1.0);
        IExchangeItem noiseItem = dataObject.getDataObjectExchangeItem("oda:HeatAG04@1.noise");
        noiseItem.setValues(0.273);

        IExchangeItem item2 = dataObject.getDataObjectExchangeItem("oda:HeatAG04@2");
        item2.setValues(1.1);
        noiseItem = dataObject.getDataObjectExchangeItem("oda:HeatAG04@2.noise");
        noiseItem.setValues(-0.1);
        dataObject.finish();
        testData.FilesAreIdentical(original, modified, 0);

        item.setValues(1.273);
        item2.setValues(1.0);
        dataObject.finish();
        testData.FilesAreIdentical(original, modified, 0);

    }

}
