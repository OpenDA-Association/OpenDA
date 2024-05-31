package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class WandaSeawatModelRunIniFileDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;


	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatModelRunIniFileDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatModelRunIniFileDataObject");
	}


	public void testReadAxpyWrite() {
		File file = new File(testRunDataDir, "example.ini");
		WandaSeawatGridDataObject wandaSeawatGridDataObject = new WandaSeawatGridDataObject();
		wandaSeawatGridDataObject.initialize(testRunDataDir, new String[]{file.getName()});
	}
}
