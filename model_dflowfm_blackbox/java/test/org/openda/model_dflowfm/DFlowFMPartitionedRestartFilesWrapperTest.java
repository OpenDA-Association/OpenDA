/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.model_dflowfm;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class DFlowFMPartitionedRestartFilesWrapperTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMPartitionedRestartFilesWrapperTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "DFlowFMPartionedRestartFilesWrapper");
	}


	public void testRenameNewestRestartFile() {
		DFlowFMPartitionedRestartFilesWrapper dFlowFMRestartFileWrapper = new DFlowFMPartitionedRestartFilesWrapper();
		dFlowFMRestartFileWrapper.initialize(testRunDataRestartFileDir, new String[]{"runId=dcsmv5", "numberOfPartitions=3"});
		String[] exchangeItemIDs = dFlowFMRestartFileWrapper.getExchangeItemIDs();
		assertEquals(42, exchangeItemIDs.length);
		IExchangeItem s1_0001 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0001");
		assertNotNull(s1_0001);

		double[] originalValuesS1_0001 = s1_0001.getValuesAsDoubles();
		double[] axpyValues = new double[originalValuesS1_0001.length];
		Arrays.fill(axpyValues, 1.1);
		s1_0001.axpyOnValues(1.0, axpyValues);

		IExchangeItem s1_0000 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0000");
		assertNotNull(s1_0000);
		double[] originalValuesS1_0000 = s1_0000.getValuesAsDoubles();

		IExchangeItem s1_0002 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0002");
		assertNotNull(s1_0002);
		double[] originalValuesS1_0002 = s1_0002.getValuesAsDoubles();

		dFlowFMRestartFileWrapper.finish();

		DFlowFMPartitionedRestartFilesWrapper dFlowFMRestartFileWrapper2 = new DFlowFMPartitionedRestartFilesWrapper();
		dFlowFMRestartFileWrapper2.initialize(testRunDataRestartFileDir, new String[]{"runId=dcsmv5", "numberOfPartitions=3"});
		String[] exchangeItemIDs2 = dFlowFMRestartFileWrapper2.getExchangeItemIDs();
		assertEquals(42, exchangeItemIDs2.length);
		IExchangeItem s1_0001_2 = dFlowFMRestartFileWrapper2.getDataObjectExchangeItem("s1_0001");
		assertNotNull(s1_0001_2);
		double[] valuesAsDoublesS1_0001_2 = s1_0001_2.getValuesAsDoubles();
		assertEquals(originalValuesS1_0001.length, valuesAsDoublesS1_0001_2.length);
		for (int i = 0; i < valuesAsDoublesS1_0001_2.length; i++) {
			assertEquals(axpyValues[i] + originalValuesS1_0001[i], valuesAsDoublesS1_0001_2[i], 0.00001);
		}

		IExchangeItem s1_0000_2 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0000");
		assertNotNull(s1_0000_2);
		double[] rewrittenValuesS1_0000 = s1_0000_2.getValuesAsDoubles();
		assertEquals(originalValuesS1_0000.length, rewrittenValuesS1_0000.length);
		for (int i = 0; i < originalValuesS1_0000.length; i++) {
			assertEquals(originalValuesS1_0000[i], rewrittenValuesS1_0000[i], 0.00001);
		}

		IExchangeItem s1_0002_2 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0002");
		assertNotNull(s1_0002_2);
		double[] rewrittenValuesS1_0002 = s1_0002_2.getValuesAsDoubles();
		assertEquals(originalValuesS1_0002.length, rewrittenValuesS1_0002.length);
		for (int i = 0; i < originalValuesS1_0002.length; i++) {
			assertEquals(originalValuesS1_0002[i], rewrittenValuesS1_0002[i], 0.00001);
		}
	}

}
