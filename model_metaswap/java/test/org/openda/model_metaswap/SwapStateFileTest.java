package org.openda.model_metaswap;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;

import static org.openda.model_metaswap.SwapStateFile.PRESSURE_HEAD_ROOT_ZONE;

public class SwapStateFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(SwapResultFileTest.class, "model_metaswap");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testEdit() {
		SwapStateFile swapStateFile = new SwapStateFile();
		String sourceFileName = "init_svat.inp";
		swapStateFile.initialize(testRunDataDir, new String[]{sourceFileName});
		String[] exchangeItemIDs = swapStateFile.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(1, exchangeItemIDs.length);
		IExchangeItem exchangeItem = swapStateFile.getDataObjectExchangeItem(PRESSURE_HEAD_ROOT_ZONE);
		assertNotNull(exchangeItem);
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
		assertNotNull(valuesAsDoubles);
		assertEquals(14, valuesAsDoubles.length);
		double[] multiplicationFactors = new double[valuesAsDoubles.length];
		for (int i = 0; i < multiplicationFactors.length; i++) {
			multiplicationFactors[i] = 1 + 1.0 / (i + 1);
		}
		exchangeItem.multiplyValues(multiplicationFactors);
		swapStateFile.finish();

		File sourceFile = new File(testRunDataDir, sourceFileName);
		String text = AsciiFileUtils.readText(sourceFile);

		File expectedFile = new File(testRunDataDir, "init_svat_expected.inp");
		String expectedText = AsciiFileUtils.readText(expectedFile);

		assertEquals(expectedText, text);
	}
}
