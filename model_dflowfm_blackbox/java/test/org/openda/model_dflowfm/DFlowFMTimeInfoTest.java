/* MOD_V2.0
 * Copyright (c) 2013 OpenDA Association
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
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.io.IOException;

/**
 * Test of exchange item for reading and writing simulation time information of DFlowFM.
 */
public class DFlowFMTimeInfoTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMTimeInfoTest.class, "model_dflowfm_blackbox");
	}

	public void testDFlowFMTimeInfo(){
/*      MDU file contains 2 TimeInfoExchangeItems TStart and TStop, relative to RefDate in unit of time
*       Hours, Minutes or Seconds. RefDate and Tunit are NOT overwritten by OpenDA.
*/

//      Test Tunit=M
		File testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir,new String[]{"dflowfm_minute.mdu"});

		IExchangeItem[] DFlowFMTimeInfoExchangeItemMin = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ",2,DFlowFMTimeInfoExchangeItemMin.length);
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ","start_time",DFlowFMTimeInfoExchangeItemMin[0].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ","end_time",DFlowFMTimeInfoExchangeItemMin[1].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getValues(): ",48865.0,DFlowFMTimeInfoExchangeItemMin[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getValues(): ",48870.0,DFlowFMTimeInfoExchangeItemMin[1].getValues());

//		Object times;
		double[] timestart = DFlowFMTimeInfoExchangeItemMin[0].getValuesAsDoubles();
		double[] timeend = DFlowFMTimeInfoExchangeItemMin[1].getValuesAsDoubles();
//      add 1 MJD (1440 minutes) to TStart, add 2 MJD (2880 minutes) to TStop
		timestart[0]++;
		timeend[0] = timeend[0]+2.0;
		DFlowFMTimeInfoExchangeItemMin[0].setValuesAsDoubles(timestart);
		DFlowFMTimeInfoExchangeItemMin[1].setValuesAsDoubles(timeend);
		DFlowFMTimeInfo.finish();

		// reread input file:
		DFlowFMTimeInfo DFlowFMTimeInfo2 = new DFlowFMTimeInfo();
		DFlowFMTimeInfo2.initialize(testRunDataMDUFileDir,new String[]{"dflowfm_minute.mdu"});
		IExchangeItem[] DFlowFMTimeInfoExchangeItem2 = DFlowFMTimeInfo2.getExchangeItems();
		assertEquals("DFlowFMTimeInfoExchangeItem2[0].getValues(): ",timestart[0],DFlowFMTimeInfoExchangeItem2[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem2[1].getValues(): ",timeend[0],DFlowFMTimeInfoExchangeItem2[1].getValues());
		DFlowFMTimeInfo2.finish();

//      Test Tunit=H
		testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir,new String[]{"dflowfm_hour.mdu"});

		IExchangeItem[] DFlowFMTimeInfoExchangeItemHour = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ",2,DFlowFMTimeInfoExchangeItemHour.length);
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ","start_time",DFlowFMTimeInfoExchangeItemHour[0].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ","end_time",DFlowFMTimeInfoExchangeItemHour[1].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getValues(): ",48865.0,DFlowFMTimeInfoExchangeItemHour[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getValues(): ",48865.75,DFlowFMTimeInfoExchangeItemHour[1].getValues());

//		Object times;
		timestart = DFlowFMTimeInfoExchangeItemHour[0].getValuesAsDoubles();
		timeend = DFlowFMTimeInfoExchangeItemHour[1].getValuesAsDoubles();
//      add 1 MJD (24 hours) to TStart, add 2 MJD (48 hours) to TStop
		timestart[0]++;
		timeend[0] = timeend[0]+2.0;
		DFlowFMTimeInfoExchangeItemHour[0].setValuesAsDoubles(timestart);
		DFlowFMTimeInfoExchangeItemHour[1].setValuesAsDoubles(timeend);
		DFlowFMTimeInfo.finish();

		// reread input file:
		DFlowFMTimeInfo2 = new DFlowFMTimeInfo();
		DFlowFMTimeInfo2.initialize(testRunDataMDUFileDir,new String[]{"dflowfm_hour.mdu"});
		DFlowFMTimeInfoExchangeItem2 = DFlowFMTimeInfo2.getExchangeItems();
		assertEquals("DFlowFMTimeInfoExchangeItem2[0].getValues(): ",timestart[0],DFlowFMTimeInfoExchangeItem2[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem2[1].getValues(): ",timeend[0],DFlowFMTimeInfoExchangeItem2[1].getValues());
		DFlowFMTimeInfo2.finish();

//      Test Tunit=S
		testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir,new String[]{"dflowfm_second.mdu"});

		IExchangeItem[] DFlowFMTimeInfoExchangeItemSec = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ",2,DFlowFMTimeInfoExchangeItemSec.length);
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ","start_time",DFlowFMTimeInfoExchangeItemSec[0].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ","end_time",DFlowFMTimeInfoExchangeItemSec[1].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getValues(): ",48865.0,DFlowFMTimeInfoExchangeItemSec[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getValues(): ",48865.5,DFlowFMTimeInfoExchangeItemSec[1].getValues());

//		Object times;
		timestart = DFlowFMTimeInfoExchangeItemSec[0].getValuesAsDoubles();
		timeend = DFlowFMTimeInfoExchangeItemSec[1].getValuesAsDoubles();
//      add 1 MJD (86400 seconds) to TStart, add 0.5 MJD (43200 seconds) to TStop
		timestart[0]++;
		timeend[0] = timeend[0] + 0.5;
		DFlowFMTimeInfoExchangeItemSec[0].setValuesAsDoubles(timestart);
		DFlowFMTimeInfoExchangeItemSec[1].setValuesAsDoubles(timeend);
		DFlowFMTimeInfo.finish();

		// reread input file:
		DFlowFMTimeInfo2 = new DFlowFMTimeInfo();
		DFlowFMTimeInfo2.initialize(testRunDataMDUFileDir, new String[]{"dflowfm_second.mdu"});
		DFlowFMTimeInfoExchangeItem2 = DFlowFMTimeInfo2.getExchangeItems();
		assertEquals("DFlowFMTimeInfoExchangeItem2[0].getValues(): ", timestart[0], DFlowFMTimeInfoExchangeItem2[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem2[1].getValues(): ", timeend[0], DFlowFMTimeInfoExchangeItem2[1].getValues());
		DFlowFMTimeInfo2.finish();
	}

	public void testGivenStartAndStopDatetimeWhenRunThenLoadsCorrectTimes() {
		File testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir, new String[]{"dflowfm_hour_start_end_datetime.mdu"});

		IExchangeItem[] DFlowFMTimeInfoExchangeItemHour = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("dFlowFMTimeInfoExchangeItem.length: ", 2, DFlowFMTimeInfoExchangeItemHour.length);
		assertEquals("dFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", DFlowFMTimeInfoExchangeItemHour[0].getId());
		assertEquals("dFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", DFlowFMTimeInfoExchangeItemHour[1].getId());
		assertEquals("dFlowFMTimeInfoExchangeItem[0].getValues(): ", 48866.0, DFlowFMTimeInfoExchangeItemHour[0].getValues());
		assertEquals("dFlowFMTimeInfoExchangeItem[1].getValues(): ", 48867.75, DFlowFMTimeInfoExchangeItemHour[1].getValues());

		testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir, new String[]{"dflowfm_minute_start_end_datetime.mdu"});

		DFlowFMTimeInfoExchangeItemHour = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("dFlowFMTimeInfoExchangeItem.length: ", 2, DFlowFMTimeInfoExchangeItemHour.length);
		assertEquals("dFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", DFlowFMTimeInfoExchangeItemHour[0].getId());
		assertEquals("dFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", DFlowFMTimeInfoExchangeItemHour[1].getId());
		assertEquals("dFlowFMTimeInfoExchangeItem[0].getValues(): ", 48866.0, DFlowFMTimeInfoExchangeItemHour[0].getValues());
		assertEquals("dFlowFMTimeInfoExchangeItem[1].getValues(): ", 48867.774305555555, DFlowFMTimeInfoExchangeItemHour[1].getValues());

		testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir, new String[]{"dflowfm_second_start_end_datetime.mdu"});

		DFlowFMTimeInfoExchangeItemHour = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("dFlowFMTimeInfoExchangeItem.length: ", 2, DFlowFMTimeInfoExchangeItemHour.length);
		assertEquals("dFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", DFlowFMTimeInfoExchangeItemHour[0].getId());
		assertEquals("dFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", DFlowFMTimeInfoExchangeItemHour[1].getId());
		assertEquals("dFlowFMTimeInfoExchangeItem[0].getValues(): ", 48866.0, DFlowFMTimeInfoExchangeItemHour[0].getValues());
		assertEquals("dFlowFMTimeInfoExchangeItem[1].getValues(): ", 48867.774826388886, DFlowFMTimeInfoExchangeItemHour[1].getValues());
	}

	public void testDFlowFMRestartFile() {
		/*      MDU file contains 2 TimeInfoExchangeItems TStart and TStop, relative to RefDate in unit of time
		 *       Hours, Minutes or Seconds. RefDate and Tunit are NOT overwritten by OpenDA.
		 */

//      Test Tunit=M
		File testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir, new String[]{"dcsmv5.mdu", "useRstForRestart=true"});

		IExchangeItem[] DFlowFMTimeInfoExchangeItemMin = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ", 2, DFlowFMTimeInfoExchangeItemMin.length);
		DFlowFMTimeInfoExchangeItem startTimeEI = (DFlowFMTimeInfoExchangeItem) DFlowFMTimeInfoExchangeItemMin[0];
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", startTimeEI.getId());
		DFlowFMTimeInfoExchangeItem endTimeEI = (DFlowFMTimeInfoExchangeItem) DFlowFMTimeInfoExchangeItemMin[1];
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", endTimeEI.getId());
		Double startValue = (Double) startTimeEI.getValues();
		assertEquals(59813.50694, startValue, 0.00001);
		Double endValue = (Double) endTimeEI.getValues();
		assertEquals( 59819.5, endValue, 0.00001);

		startTimeEI.setValues(startValue + 1);
		endTimeEI.setValues(endValue + 1);

		DFlowFMTimeInfo.finish();

		DFlowFMTimeInfo dFlowFMTimeInfoRebuild = new DFlowFMTimeInfo();
		dFlowFMTimeInfoRebuild.initialize(testRunDataMDUFileDir, new String[]{"dcsmv5.mdu", "useRstForRestart=true"});

		IExchangeItem[] dFlowFMTimeInfoExchangeItemsRebuild = dFlowFMTimeInfoRebuild.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ", 2, dFlowFMTimeInfoExchangeItemsRebuild.length);
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", dFlowFMTimeInfoExchangeItemsRebuild[0].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", dFlowFMTimeInfoExchangeItemsRebuild[1].getId());
		Double startValueRebuild = (Double) dFlowFMTimeInfoExchangeItemsRebuild[0].getValues();
		assertEquals(59814.50694, startValueRebuild, 0.00001);
		Double endValueRebuild = (Double) dFlowFMTimeInfoExchangeItemsRebuild[1].getValues();
		assertEquals( 59820.5, endValueRebuild, 0.00001);

		DFlowFMMduInputFile dFlowFMMduInputFile = new DFlowFMMduInputFile(testRunDataMDUFileDir, "dcsmv5.mdu");
		String restartFileText = dFlowFMMduInputFile.get("restart", "RestartFile");
		assertEquals("", restartFileText);
		String restartDateTimeText = dFlowFMMduInputFile.get("restart", "RestartDateTime");
		assertEquals("", restartDateTimeText);
	}

	public void testDFlowFMPartitionedMDU() {
		File testRunDataMDUFileDir = new File(testData.getTestRunDataDir(), "MDUfile");
		DFlowFMTimeInfo DFlowFMTimeInfo = new DFlowFMTimeInfo();
		DFlowFMTimeInfo.initialize(testRunDataMDUFileDir, new String[]{"dcsmv5.mdu", "useRstForRestart=true", "numberOfPartitions=3"});

		IExchangeItem[] DFlowFMTimeInfoExchangeItemMin = DFlowFMTimeInfo.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ", 2, DFlowFMTimeInfoExchangeItemMin.length);
		DFlowFMTimeInfoExchangeItem startTimeEI = (DFlowFMTimeInfoExchangeItem) DFlowFMTimeInfoExchangeItemMin[0];
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", startTimeEI.getId());
		DFlowFMTimeInfoExchangeItem endTimeEI = (DFlowFMTimeInfoExchangeItem) DFlowFMTimeInfoExchangeItemMin[1];
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", endTimeEI.getId());
		Double startValue = (Double) startTimeEI.getValues();
		assertEquals(54101.0, startValue, 0.00001);
		Double endValue = (Double) endTimeEI.getValues();
		assertEquals( 54101.041666, endValue, 0.00001);

		startTimeEI.setValues(startValue + 1);
		endTimeEI.setValues(endValue + 1);

		DFlowFMTimeInfo.finish();

		checkPartitionedMduFile(testRunDataMDUFileDir, "dcsmv5_0000.mdu");
		checkPartitionedMduFile(testRunDataMDUFileDir, "dcsmv5_0001.mdu");
		checkPartitionedMduFile(testRunDataMDUFileDir, "dcsmv5_0002.mdu");
	}

	private static void checkPartitionedMduFile(File testRunDataMDUFileDir, String mduFileName) {
		DFlowFMTimeInfo dFlowFMTimeInfoRebuild = new DFlowFMTimeInfo();
		dFlowFMTimeInfoRebuild.initialize(testRunDataMDUFileDir, new String[]{mduFileName, "useRstForRestart=true"});

		IExchangeItem[] dFlowFMTimeInfoExchangeItemsRebuild = dFlowFMTimeInfoRebuild.getExchangeItems();

		assertEquals("DflowFMTimeInfoExchangeItem.length: ", 2, dFlowFMTimeInfoExchangeItemsRebuild.length);
		assertEquals("DFlowFMTimeInfoExchangeItem[0].getId(): ", "start_time", dFlowFMTimeInfoExchangeItemsRebuild[0].getId());
		assertEquals("DFlowFMTimeInfoExchangeItem[1].getId(): ", "end_time", dFlowFMTimeInfoExchangeItemsRebuild[1].getId());
		Double startValueRebuild = (Double) dFlowFMTimeInfoExchangeItemsRebuild[0].getValues();
		assertEquals(54102.0, startValueRebuild, 0.00001);
		Double endValueRebuild = (Double) dFlowFMTimeInfoExchangeItemsRebuild[1].getValues();
		assertEquals( 54102.041666, endValueRebuild, 0.00001);
	}
}
