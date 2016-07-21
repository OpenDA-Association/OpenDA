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
public class DictionaryDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(DictionaryDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testInitializeControlDict() {
		IDataObject dataObject = new DictionaryDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"system/controlDict" });
	}

	public void testGetControlDictExchangeItems() {
		IDataObject dataObject = new DictionaryDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"system/controlDict", "2015-12-01T00:00:00Z"});
		String[] ids = dataObject.getExchangeItemIDs();
		assertEquals( 2, ids.length);
		Arrays.sort(ids);
		assertEquals("oda:endTime", ids[0]);
		assertEquals("oda:startTime", ids[1]);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("oda:startTime");
		assertEquals(57357.0, (Double) exchangeItem.getValues(), java.lang.Math.ulp(57357.0) );
		exchangeItem = dataObject.getDataObjectExchangeItem("oda:endTime");
		assertEquals(57358.0, (Double) exchangeItem.getValues(), java.lang.Math.ulp(57358.0));
	}

	public void testSetControlDictExchangeItems() {
		IDataObject dataObject = new DictionaryDataObject();
        File reference = new File(testRunDataDir, "reference/controlDict");
        File modified = new File(testRunDataDir, "output/controlDict");
		dataObject.initialize(testRunDataDir, new String[]{"output/controlDict" });
        IExchangeItem item = dataObject.getDataObjectExchangeItem("oda:startTime");
        item.setValues(57357.0);
        item = dataObject.getDataObjectExchangeItem("oda:endTime");
        item.setValues(57358.0);
        dataObject.finish();
        testData.FilesAreIdentical(reference, modified, 0);

	}

}
