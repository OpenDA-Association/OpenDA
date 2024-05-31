package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class WandaSeawatGridDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;


	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatGridDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatGridDataObject");
	}


	public void testReadAxpyWrite() {
		File file = new File(testRunDataDir, "HTO_TEMP_20220531000100.ASC");
		WandaSeawatGridDataObject wandaSeawatGridDataObject = new WandaSeawatGridDataObject();
		wandaSeawatGridDataObject.initialize(testRunDataDir, new String[]{file.getName()});
	}
}
