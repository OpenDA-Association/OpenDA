/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
		timeend[0] = timeend[0]+0.5;
		DFlowFMTimeInfoExchangeItemSec[0].setValuesAsDoubles(timestart);
		DFlowFMTimeInfoExchangeItemSec[1].setValuesAsDoubles(timeend);
		DFlowFMTimeInfo.finish();

		// reread input file:
		DFlowFMTimeInfo2 = new DFlowFMTimeInfo();
		DFlowFMTimeInfo2.initialize(testRunDataMDUFileDir,new String[]{"dflowfm_second.mdu"});
		DFlowFMTimeInfoExchangeItem2 = DFlowFMTimeInfo2.getExchangeItems();
		assertEquals("DFlowFMTimeInfoExchangeItem2[0].getValues(): ",timestart[0],DFlowFMTimeInfoExchangeItem2[0].getValues());
		assertEquals("DFlowFMTimeInfoExchangeItem2[1].getValues(): ",timeend[0],DFlowFMTimeInfoExchangeItem2[1].getValues());
		DFlowFMTimeInfo2.finish();
	}
}
