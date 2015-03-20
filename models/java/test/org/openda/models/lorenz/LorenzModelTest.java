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
package org.openda.models.lorenz;

/**
 * Main class for testing LorenzModel class
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
public class LorenzModelTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(LorenzModelTest.class,"models");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public static void testLorenzModel_1() {
        System.out.println("=========================================================");
        System.out.println("Model constructor");
        System.out.println("=========================================================");
        // Constructors
        IStochModelFactory fact1 = new LorenzStochModelFactory();
        fact1.initialize(null, new String[]{""});
        IStochModelInstance mod1 = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public String toString(){
        String mod1String = mod1.toString();
        System.out.println("mod1=" + mod1String);
        System.out.println("Should be "
                + "mod1=org.openda.models.lorenz.LorenzStochModelInstance 1 ...");
        assertEquals("mod1.tostring()"
                , mod1String.substring(0, "org.openda.models.lorenz.LorenzStochModelInstance ".length())
                , "org.openda.models.lorenz.LorenzStochModelInstance ");
    }

    public static void testLorenzModel_2() {
        System.out.println("=========================================================");
        System.out.println("Model time");
        System.out.println("=========================================================");
        IStochModelFactory fact2 = new LorenzStochModelFactory();
        fact2.initialize(null, new String[]{""});
        IStochModelInstance mod2 = fact2.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        System.out.println("mod2=" + mod2.toString());
        //   public Time getTimeHorizon() {
        String timeHorizonString = mod2.getTimeHorizon().toString();
        System.out.println("mod2.getTimeHorizon=" + timeHorizonString);
        System.out.println("Should be mod2.getTimeHorizon=0.0:0.025:30.0");
        assertTrue("mod2.getTimeHorizon()", timeHorizonString.equalsIgnoreCase("0.0:0.025:30.0"));
        //   public Time getCurrentTime() {
        ITime currentTime = mod2.getCurrentTime();
        System.out.println("mod2.getCurrentTime=" + currentTime);
        System.out.println("Should be mod2.getCurrentTime=0.0");
        assertEquals("mod2.getCurrentTime()", currentTime.toString(), "0.0");
    }

    public static void testLorenzModel_3() {
        System.out.println("=========================================================");
        System.out.println("Model state");
        System.out.println("=========================================================");
        IStochModelFactory fact3 = new LorenzStochModelFactory();
        fact3.initialize(null, new String[]{""});
        IStochModelInstance mod3 = fact3.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getState() {
        IVector x3 = mod3.getState();
        System.out.println("mod3.getState()=" + x3.toString());
        System.out.println("Should be mod3.getState() =[1.50887,-1.531271,25.46091]");
        assertEquals("mod3.getState()", x3.toString(), "[1.50887,-1.531271,25.46091]");
        //   public void axpyOnState(double alpha, Vector vector)
        IVector x3_delta = new Vector("[0.0,0.001,0.0]");
        mod3.axpyOnState(1.0, x3_delta);
        IVector x3b = mod3.getState();
        System.out.println("mod3.getState()=" + x3b.toString());
        System.out.println("Should be mod3.getState() =[1.508870,-1.532271,25.46091]");
        assertEquals("mod33getState()", x3b.toString(), "[1.50887,-1.5302710000000002,25.46091]");
        //   public StochVector getStateUncertainty() {
        IStochVector x3Stoch = mod3.getStateUncertainty();
        System.out.println("mod3.getStateUncertainty()=" + x3Stoch.toString());
        System.out.println("Should be mod3.getStateUncertainty() ={[0.0,0.0,0.0],[0.5,0.5,0.5]}");
        assertEquals("mod3.getState()", x3Stoch.toString(), "{[0.0,0.0,0.0],[0.5,0.5,0.5]}");
    }

    public static void testLorenzModel_4() {
        System.out.println("=========================================================");
        System.out.println("Model parameters");
        System.out.println("=========================================================");
        IStochModelFactory fact4 = new LorenzStochModelFactory();
        fact4.initialize(null, new String[]{""});
        IStochModelInstance mod4 = fact4.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getParameters() {
        IVector par4 = new Vector(mod4.getParameters());
        System.out.println("par4=" + par4);
        System.out.println("Should be par4=[10.0,28.0,2.6666666666666665]");
        assertEquals("mod4.getParameters()", par4.toString(), "[10.0,28.0,2.6666666666666665]");
        //   public void setParameters(Vector parameters) {
        IVector par4b = new Vector("[10.0,28.0,2.70]");
        mod4.setParameters(par4b);
        IVector par4bCheck = new Vector(mod4.getParameters());
        System.out.println("par4b=" + par4b);
        System.out.println("Should be par4b=[10.0,28.0,2.70]");
        assertEquals("mod4.getParameters()", par4b.toString(), par4bCheck.toString());
        //   public void axpyOnParameters(double alpha, Vector vector) {
        IVector par4delta = new Vector("[1.0,0.0,0.0]");
        mod4.axpyOnParameters(1.0, par4delta);
        IVector par4c = new Vector(mod4.getParameters());
        System.out.println("par4c=" + par4c);
        System.out.println("Should be par4c=[11.0,28.0,2.7]");
        assertEquals("mod4.getParameters()", par4c.toString(), "[11.0,28.0,2.7]");
        //   public StochVector getParameterUncertainty() {
        IStochVector p4Stoch = mod4.getParameterUncertainty();
        System.out.println("mod4.getParameterUncertainty()=" + p4Stoch.toString());
        System.out.println("Should be mod4.getParameterUncertainty() ={[10.0,28.0,2.6666666666666665],[1.0,2.0,0.3]}");
        assertEquals("mod4.getParameteruncertainty()", p4Stoch.toString(), "{[10.0,28.0,2.6666666666666665],[1.0,2.0,0.3]}");
    }

    public static void testLorenzModel_5() {
        System.out.println("=========================================================");
        System.out.println("Model observed values");
        System.out.println("=========================================================");
        IStochModelFactory fact5 = new LorenzStochModelFactory();
        fact5.initialize(null, new String[]{""});
        IStochModelInstance mod5 = fact5.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        IStochModelInstance mod5b = fact5.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std\n"
                + "0.0,0.0,8.1,0.1\n"
                + "0.1,0.0,8.2,0.1\n"
                + "0.2,0.0,8.3,0.1";
        IStochObserver obs5 = new CsvStochObserver(content);
        IObservationDescriptions descr5 = obs5.getObservationDescriptions();
        mod5.announceObservedValues(descr5);
        mod5.compute(new Time(0.5));
        System.out.println("mod5.compute(0.5)");
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector prd5 = mod5.getObservedValues(descr5);
        System.out.println("mod5.getObservedValues()=" + prd5.toString());
        // x(0) is taken at t=0.5 for all 3 obs because model did not save previous values
        System.out.println("Should be mod5.getObservedValues()=[1.50887,-0.2646532956909706,-1.0433774293961933]");
        assertEquals("mod5.getObservedValues()", prd5.toString(), "[1.50887,-0.2646532956909706,-1.0433774293961933]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
        mod5b.announceObservedValues(descr5);
        mod5b.compute(new Time(0.5));
        System.out.println("mod5b.compute()");
        IVector prd5b = mod5b.getObservedValues(descr5);
        System.out.println("mod5b.announceObservedValues(descr5)");
        System.out.println("mod5b.getObservedValues()=" + prd5b.toString());
        System.out.println("Should be mod5b.getObservedValues()=[1.50887,-0.2646532956909706,-1.0433774293961933]");
        assertEquals("mod5b.getObservedValues()", prd5b.toString(), "[1.50887,-0.2646532956909706,-1.0433774293961933]");
    }

    public void testLorenzModel_6() {
        System.out.println("=========================================================");
        System.out.println("Model save state");
        System.out.println("=========================================================");
        IStochModelFactory fact6 = new LorenzStochModelFactory();
        fact6.initialize(testRunDataDir, new String[]{""});
        IStochModelInstance mod6 = fact6.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Object restoreInternalState() {
		IModelState s1 = mod6.saveInternalState();
        mod6.compute(new Time(1.0));
        IVector x1 = mod6.getState();
        //   public void restoreInternalState(Object restoreInternalState) {
        mod6.restoreInternalState(s1); //restore previous time
        mod6.compute(new Time(1.0));
        IVector x2 = mod6.getState();
        System.out.println("saved state (before, after) =(" + x1.toString() + "," + x2.toString() + ")");
        System.out.println("Should be saved state (before, after) =([0.05987678390975121,-1.112492799311437],[0.05987678390975121,-1.112492799311437])");
        assertEquals("mod6.saveInernalState()", x1.getValue(1), x2.getValue(1));
        //   public void releaseInternalState(Object restoreInternalState) {
        //mod6.releaseInternalState(s1);
    }

    public static void tstLorenzModel_7() {
        System.out.println("=========================================================");
        System.out.println("Model system noise");
        System.out.println("=========================================================");
        IStochModelFactory fact7 = new LorenzStochModelFactory();
        fact7.initialize(null, new String[]{""});
        IStochModelInstance mod7 = fact7.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public StochVector[] getWhiteNoiseUncertainty(Time time) {
        //   public boolean isWhiteNoiseStationary() {
        //   public Time[] getWhiteNoiseTimes(Time timeSpan) {
        //   public Vector[] getWhiteNoise(Time timeSpan) {
        //   public void setWhiteNoise(Vector whiteNoise[]) {
        //   public void axpyOnWhiteNoise(double alpha, Vector vector[]) {
        //   public void setAutomaticNoiseGeneration(boolean value) {
        mod7.setAutomaticNoiseGeneration(true);
        mod7.compute(new Time(3.0));
        assertTrue("mod7 sysnoise", false); //TODO
    }

    public static void tstLorenzModel_8() {
        System.out.println("=========================================================");
        System.out.println("Model state scaling");
        System.out.println("=========================================================");
        IStochModelFactory fact8 = new LorenzStochModelFactory();
        fact8.initialize(null, new String[]{""});
        // StochModelInstance mod8 = fact8.getInstance(); // get a model
        //   public Vector getStateScaling() {
        //   public Vector[] getStateScaling(ObservationDescriptions observationDescriptions) {
        assertTrue("mod8 statescaling", false); //TODO
    }

    public static void tstLorenzModel_9() {
        System.out.println("=========================================================");
        System.out.println("Model exchangeItems");
        System.out.println("=========================================================");
        IStochModelFactory fact9 = new LorenzStochModelFactory();
        fact9.initialize(null, new String[]{""});
        // StochModelInstance mod9 = fact9.getInstance(); // get a model
        //   public String[] getExchangeItemIDs() {
        //   public Vector getValues(String exchangeItemID) {
        //   public Vector[] getValues(String exchangeItemID, Time time) {
        //   public Time[] getTimes(String exchangeItemID, Time time) {
        //   public Vector[] getValues(Time[] timeStamps) {
        //   public void setValues(String exchangeItemID, Vector[] Vectors) {
        assertTrue("mod9 exchangeItems", false); //TODO
    }

    public static void testLorenzModel_10() {
        //   public void compute(Time targetTime) {
        System.out.println("=========================================================");
        System.out.println("Model compute");
        System.out.println("=========================================================");
        IStochModelFactory fact10 = new LorenzStochModelFactory();
        fact10.initialize(null, new String[]{""});
        IStochModelInstance mod10 = fact10.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod10.compute(new Time(1.0));
        IVector x10 = mod10.getState();
        System.out.println("mod10.compute(1.0).get_state() = " + x10);
        System.out.println("Should be mod10.compute(1.0).get_state() = [2.6963608035999203,4.382852897036759,16.69715845293723]");
        assertEquals("mod10.compute(1.0", x10.toString(), "[2.6963608035999203,4.382852897036759,16.69715845293723]");
        System.out.println("mod10=" + mod10);
    }

}//end class




