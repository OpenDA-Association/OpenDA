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
package org.openda.models.doublePendulum;

/**
 * Main class for testing DoublePendulumModel class
 */


import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Test for simple vector
 */
public class DoublePendulumModelTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(DoublePendulumModelTest.class,"models");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public static void testDoublePendulumModel_1() {
        System.out.println("=========================================================");
        System.out.println("Model constructor");
        System.out.println("=========================================================");
        // Constructors
        IStochModelFactory fact1 = new DoublePendulumStochModelFactory();
        fact1.initialize(null, new String[]{""});
        IStochModelInstance mod1 = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public String toString(){
        String mod1String = mod1.toString();
        System.out.println("mod1=" + mod1String);
        System.out.println("Should be "
                + "mod1=org.openda.models.doublePendulum.doublePendulumStochModelInstance 1 ...");
        assertEquals("mod1.tostring()"
                , mod1String.substring(0, "org.openda.models.doublePendulum.DoublePendulumStochModelInstance ".length())
                , "org.openda.models.doublePendulum.DoublePendulumStochModelInstance ");
    }

    public static void testDoublePendulumModel_2() {
        System.out.println("=========================================================");
        System.out.println("Model time");
        System.out.println("=========================================================");
        IStochModelFactory fact2 = new DoublePendulumStochModelFactory();
        fact2.initialize(null, new String[]{""});
        IStochModelInstance mod2 = fact2.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        System.out.println("mod2=" + mod2.toString());
        //   public Time getTimeHorizon() {
        String timeHorizonString = mod2.getTimeHorizon().toString();
        System.out.println("mod2.getTimeHorizon=" + timeHorizonString);
        System.out.println("Should be mod2.getTimeHorizon=0.0:0.01:3.0");
        assertTrue("mod2.getTimeHorizon()", timeHorizonString.equalsIgnoreCase("0.0:0.01:3.0"));
        //   public Time getCurrentTime() {
        ITime currentTime = mod2.getCurrentTime();
        System.out.println("mod2.getCurrentTime=" + currentTime);
        System.out.println("Should be mod2.getCurrentTime=0.0");
        assertEquals("mod2.getCurrentTime()", currentTime.toString(), "0.0");
    }

    public static void testDoublePendulumModel_3() {
        System.out.println("=========================================================");
        System.out.println("Model state");
        System.out.println("=========================================================");
        IStochModelFactory fact3 = new DoublePendulumStochModelFactory();
        fact3.initialize(null, new String[]{""});
        IStochModelInstance mod3 = fact3.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getState() {
        IVector x3 = mod3.getState();
        System.out.println("mod3.getState()=" + x3.toString());
        System.out.println("Should be mod3.getState() =[3.1415,1.5707,0.0,0.0]");
        assertEquals("mod3.getState()", x3.toString(), "[3.1415,1.5707,0.0,0.0]");
        //   public void axpyOnState(double alpha, Vector vector)
        IVector x3_delta = new Vector("[0.0,0.0,0.01,0.0]");
        mod3.axpyOnState(1.0, x3_delta);
        IVector x3b = mod3.getState();
        System.out.println("mod3.getState()=" + x3b.toString());
        System.out.println("Should be mod3.getState() =[3.1415,1.5707,0.01,0.0]");
        assertEquals("mod33getState()", x3b.toString(), "[3.1415,1.5707,0.01,0.0]");
        //   public StochVector getStateUncertainty() {
        IStochVector x3Stoch = mod3.getStateUncertainty();
        System.out.println("mod3.getStateUncertainty()=" + x3Stoch.toString());
        System.out.println("Should be mod3.getStateUncertainty() ={[0.0,0.0,0.0,0.0],[0.01,0.0,0.0,0.0]}");
        assertEquals("mod3.getState()", x3Stoch.toString(), "{[0.0,0.0,0.0,0.0],[0.01,0.0,0.0,0.0]}");
    }

    public static void testDoublePendulumModel_4() {
        System.out.println("=========================================================");
        System.out.println("Model observed values");
        System.out.println("=========================================================");
        IStochModelFactory fact5 = new DoublePendulumStochModelFactory();
        fact5.initialize(null, new String[]{""});
        IStochModelInstance mod5 = fact5.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        IStochModelInstance mod5b = fact5.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std\n"
                + "0.0,0.0,8.1,0.1\n"
                + "0.4,0.0,8.2,0.1\n"
                + "0.5,0.0,8.3,0.1";
        IStochObserver obs5 = new CsvStochObserver(content);
        IObservationDescriptions descr5 = obs5.getObservationDescriptions();
        mod5.announceObservedValues(descr5);
        mod5.compute(new Time(0.5));
        System.out.println("mod5.compute(0.5)");
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector prd5 = mod5.getObservationOperator().getObservedValues(descr5);
        System.out.println("mod5.getObservedValues()=" + prd5.toString());
        // x(0) is taken at t=0.5 for all 3 obs because model did not save previous values
        System.out.println("Should be mod5.getObservedValues()=[3.1415,1.1953257830118194,0.623536080378373]");
        assertEquals("mod5.getObservedValues()", prd5.toString(), "[3.1415,1.1953257830118194,0.623536080378373]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
        mod5b.announceObservedValues(descr5);
        mod5b.compute(new Time(0.5));
        System.out.println("mod5b.compute()");
        IVector prd5b = mod5b.getObservationOperator().getObservedValues(descr5);
        System.out.println("mod5b.announceObservedValues(descr5)");
        System.out.println("mod5b.getObservedValues()=" + prd5b.toString());
        System.out.println("Should be mod5b.getObservedValues()=[3.1415,1.1953257830118194,0.623536080378373]");
        assertEquals("mod5b.getObservedValues()", prd5b.toString(), "[3.1415,1.1953257830118194,0.623536080378373]");
    }


    public static void testDoublePendulumModel_5() {
        //   public void compute(Time targetTime) {
        System.out.println("=========================================================");
        System.out.println("Model compute");
        System.out.println("=========================================================");
        IStochModelFactory fact10 = new DoublePendulumStochModelFactory();
        fact10.initialize(null, new String[]{""});
        IStochModelInstance mod10 = fact10.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod10.compute(new Time(1.0));
        IVector x10 = mod10.getState();
        System.out.println("mod10.compute(1.0).get_state() = " + x10);
        System.out.println("Should be mod10.compute(1.0).get_state() = [-2.9343286328421736,-6.161418280539435,-0.5991711878725424,0.2611134102627959]");
        assertEquals("mod10.compute(1.0", x10.toString(), "[-2.9343286328421736,-6.161418280539435,-0.5991711878725424,0.2611134102627959]");
        System.out.println("mod10=" + mod10);
    }


}//end class




