package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class D3dBctFileDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dRoughParamsTest.class, "public", "model_delft3d");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testBctFile() {

		File dataDir = new File(testRunDataDir, "D3dBctFileDataObject");
		D3dBctFileDataObject bctFile = new D3dBctFileDataObject();
		bctFile.initialize(dataDir,  new String[]{"Est1D.bct"});


		String[] exchangeItemIDs = bctFile.getExchangeItemIDs();
		assertEquals(2, exchangeItemIDs.length);

		for (int i = 1; i < exchangeItemIDs.length; i++) {
			String exchangeItemID = exchangeItemIDs[i];
			IExchangeItem exchangeItem = bctFile.getDataObjectExchangeItem(exchangeItemID);
			double[] multiplicationFactors = new double[exchangeItem.getTimes().length];
			Arrays.fill(multiplicationFactors, 1.1);
			exchangeItem.multiplyValues(multiplicationFactors);
		}

		bctFile.finish();

		// check output
		File outputFile = new File(dataDir, "Est1D.bct");
		File referenceFile = new File(dataDir, "Est1D_expected.bct");
		assertTrue(outputFile.exists());
		assertTrue(referenceFile.exists());

		assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

	}
}
