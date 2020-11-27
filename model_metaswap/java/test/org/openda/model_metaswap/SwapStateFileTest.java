package org.openda.model_metaswap;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

import static org.openda.model_metaswap.SwapStateFile.PRESSURE_HEAD_ROOT_ZONE;

public class SwapStateFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(SwapResultFileTest.class, "model_metaswap");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testEdit() {
		runFileAdjustmentTest();
	}

	public void testLogging() {
		File svatIdsFile = new File(testRunDataDir, "svatids.txt");
		try {
			Writer writer = new FileWriter(svatIdsFile);
			writer.write("# svat id's to be logged\n");
			writer.write("2\n");
			writer.write("5\n");
			writer.write("# last one\n");
			writer.write("7\n");
			writer.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not write file with svatids");
		}
		runFileAdjustmentTest();

		File logFile = new File(testRunDataDir, "statevalues.log");
		assertTrue(logFile.exists());
		String text = AsciiFileUtils.readText(logFile);

		File expectedFile = new File(testRunDataDir, "statevalues_expected.log");
		String expectedText = AsciiFileUtils.readText(expectedFile);

		assertEquals(expectedText, text);

		if (!svatIdsFile.delete()) {
			throw new RuntimeException("Could not delete file with svatids");
		}
	}

	private void runFileAdjustmentTest() {
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
		assertEquals(9, valuesAsDoubles.length);
		double[] additions = new double[valuesAsDoubles.length];
		for (int i = 0; i < additions.length; i++) {
			additions[i] = 1001;
		}
		exchangeItem.axpyOnValues(1, additions);
		swapStateFile.finish();

		File sourceFile = new File(testRunDataDir, sourceFileName);
		String text = AsciiFileUtils.readText(sourceFile);

		File expectedFile = new File(testRunDataDir, "init_svat_expected.inp");
		String expectedText = AsciiFileUtils.readText(expectedFile);

		assertEquals(expectedText, text);
	}
}
