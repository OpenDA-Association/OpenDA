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

package org.openda.blackbox.wrapper;

import junit.framework.TestCase;

import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.ITime;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.GregorianCalendar;

/**
 * Test for deterministic part of Black Box Model
 */

public class BBModelFactoryTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(BBModelFactoryTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
   }

    public void testTimeInfoNoTime() {
        IModelFactory modelFactory = new BBModelFactory();
        File testData = new File(testRunDataDir,"timeInfo");
        modelFactory.initialize(testData,new String[] {"modelConfNoTime.xml"});
        IModelInstance modelInstance = modelFactory.getInstance(new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);
        ITime timeHorizon = modelInstance.getTimeHorizon();
        assertEquals("timeHorizon.getBeginTime()", Double.NEGATIVE_INFINITY, timeHorizon.getBeginTime().getMJD(), 1e-9);
        assertEquals("timeHorizon.getEndTime()", Double.POSITIVE_INFINITY, timeHorizon.getEndTime().getMJD(), 1e-9);
    }

    public void testTimeInfoWithTimeEIs() {
        IModelFactory modelFactory = new BBModelFactory();
        File testData = new File(testRunDataDir,"timeInfo");
        modelFactory.initialize(testData,new String[] {"modelConfWithTimeEIs.xml"});
        IModelInstance modelInstance = modelFactory.getInstance(new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);
        ITime timeHorizon = modelInstance.getTimeHorizon();
        GregorianCalendar beginCalendar = new GregorianCalendar(2010,6,16,23,59,59);
        double expectedBeginTimeAsMJD = Time.milliesToMjd(beginCalendar.getTimeInMillis());
        GregorianCalendar endCalendar = new GregorianCalendar(2010,6,17,0,1,1);
        double expectedEndTimeAsMJD = Time.milliesToMjd(endCalendar.getTimeInMillis());
        assertEquals("timeHorizon.getBeginTime()", expectedBeginTimeAsMJD, timeHorizon.getBeginTime().getMJD(), 1e-9);
        assertEquals("timeHorizon.getEndTime()", expectedEndTimeAsMJD, timeHorizon.getEndTime().getMJD(), 1e-9);
    }

    public void testExternalTimeInfo() {
        IModelFactory modelFactory = new BBModelFactory();
        File testData = new File(testRunDataDir,"timeInfo");
        modelFactory.initialize(testData,new String[] {"modelConfNoTime.xml"});
        if(modelFactory instanceof ITimeHorizonConsumer){
        	ITimeHorizonConsumer tempTimeConsumer = (ITimeHorizonConsumer) modelFactory;
    		double tStart=Double.NEGATIVE_INFINITY;
    		double tEnd=Double.POSITIVE_INFINITY;
        	try{
        		tStart = TimeUtils.date2Mjd("201201020000");
        		tEnd   = TimeUtils.date2Mjd("201201030000");
        	}catch (ParseException e){
        		throw new RuntimeException("Error Parsing dates.");
        	}
        	ITime timeHorizon = new Time(tStart,tEnd);
        	tempTimeConsumer.setTimeHorizon(timeHorizon);
        }
        IModelInstance modelInstance = modelFactory.getInstance(new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);
        ITime timeHorizon = modelInstance.getTimeHorizon();
        assertEquals("timeHorizon.getBeginTime()", "201201020000", TimeUtils.mjdToString(timeHorizon.getBeginTime().getMJD()));
        assertEquals("timeHorizon.getEndTime()", "201201030000", TimeUtils.mjdToString(timeHorizon.getEndTime().getMJD()));
    }

	//TODO: make TestIoObject to test if BBModelFactory is working correctly
}
