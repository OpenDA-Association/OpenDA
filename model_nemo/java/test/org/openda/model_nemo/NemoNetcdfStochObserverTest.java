/* OpenDA v2.4.1 
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
package org.openda.model_nemo;
import junit.framework.TestCase;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;

/**
 * Created by nils on 12/05/14.
 */
public class NemoNetcdfStochObserverTest extends TestCase {

	OpenDaTestSupport testData = null;
	private File testRunDataDir;
	private File testCopyDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NemoWrapperTest.class, "model_nemo");
		testRunDataDir = testData.getTestRunDataDir();
		testCopyDir = new File(testRunDataDir,"copy");
	}

	public void testAllDates(){
		IStochObserver observer = new NemoNetcdfStochObserver();
		String args[] = {};
		observer.initialize(testRunDataDir, args);

		/* Note: this method is not implemented correctly
		         when selecting multiple days it will return all possible
		         different days but not the time of each and every observations
		 */
		ITime times[]=observer.getTimes();
		assertEquals("Checking different dates:",times.length,3);
		assertEquals(times[0].getMJD(), 55993, 0.001);
		assertEquals(times[1].getMJD(), 56000, 0.001);
		assertEquals(times[2].getMJD(), 56007, 0.001);

	};

	public void testDateSelection(){

		IStochObserver observer = new NemoNetcdfStochObserver();
		String args[] = {};
		observer.initialize(testRunDataDir, args);
		Time selection = new Time(56000);
		IStochObserver subObs=observer.createSelection(selection);
		ITime times[]=subObs.getTimes();
		assertEquals("Checking date:",times.length,1);
		assertEquals(times[0].getMJD(), 56000, 0.001);
	}

	public void testGetValues(){

		IStochObserver observer = new NemoNetcdfStochObserver();
		String args[] = {};
		observer.initialize(testRunDataDir, args);
		Time selection = new Time(56000);
		IStochObserver subObs=observer.createSelection(selection);
		ITime times[]=subObs.getTimes();
		assertEquals("Checking date:",times.length,1);
		assertEquals(times[0].getMJD(), 56000, 0.001);

		IVector values = subObs.getValues();
		double norm=values.norm2();
		assertEquals("Checking number of measurements", 143,values.getSize());
		assertEquals("Checking norm of vector with measured values", 2.518143502444074,norm, 0.001);



	}





}
