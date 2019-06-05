/*
 * Copyright (c) 2019 OpenDA Association
 * All rights reserved.
 *
 * This file is part of OpenDA.
 *
 * OpenDA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * OpenDA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
 */
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
