package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by Theo on 12.04.2016.
 */
public class D3dMdFileDataObjectTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dMdFileDataObjectTest.class,"public","model_delft3d");
	}

	public void testGetDataObjectExchangeItem() {

		D3dMdFileDataObject mdFile = new D3dMdFileDataObject();
		mdFile.initialize(testData.getTestRunDataDir(), new String[] {"simple-mdfile.mdf","simple-mdfile-out.mdf"});
		String[] exchangeItemIDs = mdFile.getExchangeItemIDs();
		assertEquals("#exch.items", 4, exchangeItemIDs.length);
		IExchangeItem exchangeItem = mdFile.getDataObjectExchangeItem("Stantn");
		assertNotNull("stantn must be there", exchangeItem);
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
		assertEquals("#values", 1, valuesAsDoubles.length);
		assertEquals("stantn value", 0.0013d, valuesAsDoubles[0]);
		exchangeItem.multiplyValues(new double[] {2d});
		mdFile.finish();

		File mdFileOrg = new File(testData.getTestRunDataDir(), "simple-mdfile-out.mdf");
		File mdFileExpected = new File(testData.getTestRunDataDir(), "simple-mdfile-expected.mdf");
		testData.FilesAreIdentical(mdFileOrg, mdFileExpected);
	}
}