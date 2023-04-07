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
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class DFlowFMRestartFilePostProcessorTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMRestartTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "RestartFilePostProcessor");
	}


	public void testRenameNewestRestartFile() {
		File restartFile3 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_20070103_000000_rst.nc");
		assertTrue(restartFile3.exists());
		File restartFile4 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_20070104_000000_rst.nc");
		assertTrue(restartFile4.exists());
		File restartFile5 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_20070105_000000_rst.nc");
		assertTrue(restartFile5.exists());
		DFlowFMRestartFilePostProcessor dFlowFMRestartFilePostProcessor = new DFlowFMRestartFilePostProcessor();
		dFlowFMRestartFilePostProcessor.initialize(testRunDataRestartFileDir, new String[]{"runId=notARealRestartFile", "sourceRestartFileSubDir=subdir", "targetRestartFileNamePostFix=20220101_000000_rst.nc"});
		assertTrue(restartFile3.exists());
		assertTrue(restartFile4.exists());
		assertFalse(restartFile5.exists());
		File restartFile2022 = new File(testRunDataRestartFileDir, "notARealRestartFile_20220101_000000_rst.nc");
		assertTrue(restartFile2022.exists());
	}


	public void testDeleteOlderRestartFiles() {
		File restartFile3 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_20070103_000000_rst.nc");
		assertTrue(restartFile3.exists());
		File restartFile4 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_20070104_000000_rst.nc");
		assertTrue(restartFile4.exists());
		File restartFile5 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_20070105_000000_rst.nc");
		assertTrue(restartFile5.exists());
		DFlowFMRestartFilePostProcessor dFlowFMRestartFilePostProcessor = new DFlowFMRestartFilePostProcessor();
		dFlowFMRestartFilePostProcessor.initialize(testRunDataRestartFileDir, new String[]{"runId=notARealRestartFile", "sourceRestartFileSubDir=subdir", "targetRestartFileNamePostFix=20220101_000000_rst.nc", "deleteOlderRstFiles=true"});
		assertFalse(restartFile3.exists());
		assertFalse(restartFile4.exists());
		assertFalse(restartFile5.exists());
		File restartFile2022 = new File(testRunDataRestartFileDir, "notARealRestartFile_20220101_000000_rst.nc");
		assertTrue(restartFile2022.exists());
	}


	public void testPartitionedRestartFilesDeleteOlder() {
		File restartFile30 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0000_20070103_000000_rst.nc");
		assertTrue(restartFile30.exists());
		File restartFile40 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0000_20070104_000000_rst.nc");
		assertTrue(restartFile40.exists());
		File restartFile50 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0000_20070105_000000_rst.nc");
		assertTrue(restartFile50.exists());
		File restartFile31 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0001_20070103_000000_rst.nc");
		assertTrue(restartFile31.exists());
		File restartFile41 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0001_20070104_000000_rst.nc");
		assertTrue(restartFile41.exists());
		File restartFile51 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0001_20070105_000000_rst.nc");
		assertTrue(restartFile51.exists());
		File restartFile32 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0002_20070103_000000_rst.nc");
		assertTrue(restartFile32.exists());
		File restartFile42 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0002_20070104_000000_rst.nc");
		assertTrue(restartFile42.exists());
		File restartFile52 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0002_20070105_000000_rst.nc");
		assertTrue(restartFile52.exists());
		DFlowFMRestartFilePostProcessor dFlowFMRestartFilePostProcessor = new DFlowFMRestartFilePostProcessor();
		dFlowFMRestartFilePostProcessor.initialize(testRunDataRestartFileDir, new String[]{"runId=notARealRestartFile", "sourceRestartFileSubDir=subdir", "targetRestartFileNamePostFix=20220101_000000_rst.nc", "deleteOlderRstFiles=true", "numberOfPartitions=3"});
		assertFalse(restartFile30.exists());
		assertFalse(restartFile40.exists());
		assertFalse(restartFile50.exists());
		assertFalse(restartFile31.exists());
		assertFalse(restartFile41.exists());
		assertFalse(restartFile51.exists());
		assertFalse(restartFile32.exists());
		assertFalse(restartFile42.exists());
		assertFalse(restartFile52.exists());
		File restartFile2022_0 = new File(testRunDataRestartFileDir, "notARealRestartFile_0000_20220101_000000_rst.nc");
		assertTrue(restartFile2022_0.exists());
		File restartFile2022_1 = new File(testRunDataRestartFileDir, "notARealRestartFile_0001_20220101_000000_rst.nc");
		assertTrue(restartFile2022_1.exists());
		File restartFile2022_2 = new File(testRunDataRestartFileDir, "notARealRestartFile_0002_20220101_000000_rst.nc");
		assertTrue(restartFile2022_2.exists());
	}


	public void testPartitionedRestartFilesKeepOlder() {
		File restartFile30 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0000_20070103_000000_rst.nc");
		assertTrue(restartFile30.exists());
		File restartFile40 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0000_20070104_000000_rst.nc");
		assertTrue(restartFile40.exists());
		File restartFile50 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0000_20070105_000000_rst.nc");
		assertTrue(restartFile50.exists());
		File restartFile31 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0001_20070103_000000_rst.nc");
		assertTrue(restartFile31.exists());
		File restartFile41 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0001_20070104_000000_rst.nc");
		assertTrue(restartFile41.exists());
		File restartFile51 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0001_20070105_000000_rst.nc");
		assertTrue(restartFile51.exists());
		File restartFile32 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0002_20070103_000000_rst.nc");
		assertTrue(restartFile32.exists());
		File restartFile42 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0002_20070104_000000_rst.nc");
		assertTrue(restartFile42.exists());
		File restartFile52 = new File(testRunDataRestartFileDir, "subdir/notARealRestartFile_0002_20070105_000000_rst.nc");
		assertTrue(restartFile52.exists());
		DFlowFMRestartFilePostProcessor dFlowFMRestartFilePostProcessor = new DFlowFMRestartFilePostProcessor();
		dFlowFMRestartFilePostProcessor.initialize(testRunDataRestartFileDir, new String[]{"runId=notARealRestartFile", "sourceRestartFileSubDir=subdir", "targetRestartFileNamePostFix=20220101_000000_rst.nc", "deleteOlderRstFiles=false", "numberOfPartitions=3"});
		assertTrue(restartFile30.exists());
		assertTrue(restartFile40.exists());
		assertFalse(restartFile50.exists());
		assertTrue(restartFile31.exists());
		assertTrue(restartFile41.exists());
		assertFalse(restartFile51.exists());
		assertTrue(restartFile32.exists());
		assertTrue(restartFile42.exists());
		assertFalse(restartFile52.exists());
		File restartFile2022_0 = new File(testRunDataRestartFileDir, "notARealRestartFile_0000_20220101_000000_rst.nc");
		assertTrue(restartFile2022_0.exists());
		File restartFile2022_1 = new File(testRunDataRestartFileDir, "notARealRestartFile_0001_20220101_000000_rst.nc");
		assertTrue(restartFile2022_1.exists());
		File restartFile2022_2 = new File(testRunDataRestartFileDir, "notARealRestartFile_0002_20220101_000000_rst.nc");
		assertTrue(restartFile2022_2.exists());
	}
}
