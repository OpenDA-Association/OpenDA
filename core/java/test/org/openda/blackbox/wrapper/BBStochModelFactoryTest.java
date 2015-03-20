/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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


package org.openda.blackbox.wrapper;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.ITime;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;
import java.util.GregorianCalendar;

/**
 * Tests for black box stoch model factory
 */
public class BBStochModelFactoryTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(BBStochModelFactoryTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testTimeInfoWithTime () {
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File testData = new File(testRunDataDir,"timeInfo");
        bbStochModelFactory.initialize(testData,new String[] {"stochModelConfWithTime.xml"});
        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        ITime timeHorizon = stochModelInstance.getTimeHorizon();
        GregorianCalendar beginCalendar = new GregorianCalendar(2008,1,1,0,0,0);
        double expectedBeginTimeAsMJD = Time.milliesToMjd(beginCalendar.getTimeInMillis());
        GregorianCalendar endCalendar = new GregorianCalendar(2008,2,3,23,0,0);
        double expectedEndTimeAsMJD = Time.milliesToMjd(endCalendar.getTimeInMillis());
        assertEquals("timeHorizon.getBeginTime()", expectedBeginTimeAsMJD, timeHorizon.getBeginTime().getMJD(), 1e-9);
        assertEquals("timeHorizon.getEndTime()", expectedEndTimeAsMJD, timeHorizon.getEndTime().getMJD(), 1e-9);
    }

	//TODO: make TestIoObject to test if BBStochModelFactory is working correctly

}
