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
package org.openda.model_efdc_dll;
import junit.framework.TestCase;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.ITime;
import org.openda.observers.IoObjectStochObserver;
import org.openda.utils.ObjectSupport;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 23-4-13
 * Time: 17:04
 * To change this template use File | Settings | File Templates.
 */
public class EfdcStochObserverTest extends TestCase {
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(EfdcStochObserverTest.class,"model_efdc_dll");
	}

	public void testGetIdsAndValuesTimeDependent() throws Exception {
		File test_1_dir = new File(testData.getTestRunDataDir(), "observation");
		IStochObserver efdcStochObs = (IStochObserver) ObjectSupport.createConfigurable("IoObjectStochObserver",
				IoObjectStochObserver.class.getName(), test_1_dir, new String[]{"stochObserverConfig.xml"});
		efdcStochObs.getTimes();

		ITime[] times = efdcStochObs.getTimes();
		assertEquals("times[0]",56061.208333333336,times[0].getMJD());
        assertEquals("times[1]",56062.208333333336,times[1].getMJD());
        assertEquals("times[2]",56063.208333333336,times[2].getMJD());
		assertEquals("times[3]",56064.208333333336,times[3].getMJD());

		double tDelta = 0.0001;
		int indexTime = 0;
		ITime selectedTimes = new Time(times[indexTime].getMJD()-tDelta,times[indexTime].getMJD()+tDelta);
		IStochObserver selectedStochObs = efdcStochObs.createSelection(selectedTimes);
        assertEquals("Number of observations", 6, selectedStochObs.getCount());
        assertEquals("observationIDs[0]", "101.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[0]);
        assertEquals("observationIDs[5]", "108.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[5]);

		indexTime = 1;
		selectedTimes = new Time(times[indexTime].getMJD()-tDelta,times[indexTime].getMJD()+tDelta);
		selectedStochObs = efdcStochObs.createSelection(selectedTimes);
        assertEquals("Number of observations", 1, selectedStochObs.getCount());
        assertEquals("observationIDs[0]", "107.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[0]);
        assertEquals("observationIDs[0] value",1.420799970626831, selectedStochObs.getValues().getValue(0));


		indexTime = 2;
		selectedTimes = new Time(times[indexTime].getMJD()-tDelta,times[indexTime].getMJD()+tDelta);
		selectedStochObs = efdcStochObs.createSelection(selectedTimes);
        assertEquals("Number of observations", 8, selectedStochObs.getCount());
        assertEquals("observationIDs[0]", "100.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[0]);
        assertEquals("observationIDs[7]", "117.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[7]);
        assertEquals("observationIDs[0] value",0.1632000058889389, selectedStochObs.getValues().getValue(0));
        assertEquals("observationIDs[7] value",0.8543999791145325, selectedStochObs.getValues().getValue(7));

		indexTime = 3;
		selectedTimes = new Time(times[indexTime].getMJD()-tDelta,times[indexTime].getMJD()+tDelta);
		selectedStochObs = efdcStochObs.createSelection(selectedTimes);
        assertEquals("Number of observations", 3, selectedStochObs.getCount());
        assertEquals("observationIDs[0]", "104.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[0]);
        assertEquals("observationIDs[5]", "114.AlgalGreenAlgae", selectedStochObs.getObservationDescriptions().getStringProperties("id")[2]);
	}


}
