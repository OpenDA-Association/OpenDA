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
package org.openda.models.simpleModel;

/**
 * Main class for testing OscillatorModel class
 */


import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;

/**
 * Test for simple vector
 */
public class SimpleModelTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
     	testData = new OpenDaTestSupport(SimpleModelTest.class,"models");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public static void testOscillatorModel_1a() {
        System.out.println("=========================================================");
        System.out.println("Model constructor with configuration from input");
        System.out.println("=========================================================");
        // Constructors
        IStochModelFactory fact1a = new SimpleOscillatorStochModelFactory();
        fact1a.initialize(null, new String[]{"<oscillatorConfig><simulationTimespan>[0.0,0.05,10.0]</simulationTimespan><parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters><parameterUncertainty names=\"t_damp,omega\">[1.0,0.1257]</parameterUncertainty><systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise><initialState>[0.8,0.2]</initialState><initialStateUncertainty>[0.8,0.8]</initialStateUncertainty></oscillatorConfig>"});
        IStochModelInstance mod1a = fact1a.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public String toString(){
        String mod1aString = mod1a.toString();
        System.out.println("mod1a=" + mod1aString);
        System.out.println("Should be "
                + "mod1a=OscillatorStochModelInstance 1 ...");
        assertEquals("mod1a.tostring()"
                , mod1aString.substring(0, "org.openda.models.simpleModel.SimpleOscillatorStochModelInstance".length())
                , "org.openda.models.simpleModel.SimpleOscillatorStochModelInstance");
        IVector x1a = mod1a.getState();
        System.out.println("mod1a.getState()=" + x1a.toString());
        System.out.println("Should be mod1a.getState() =[0.8,0.2]");
        assertEquals("mod1a.getState()", x1a.toString(), "[0.8,0.2]");
    }//end testOscilatorModel_1

    public static void testOscillatorModel_1b() {
        System.out.println("=========================================================");
        System.out.println("Model constructor with default configuration");
        System.out.println("=========================================================");
        // Constructors
        IStochModelFactory fact1b = new SimpleOscillatorStochModelFactory();
        fact1b.initialize(null, new String[]{""});
        IStochModelInstance mod1b = fact1b.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public String toString(){
        String mod1bString = mod1b.toString();
        System.out.println("mod1b=" + mod1bString);
        System.out.println("Should be "
                + "mod1b=OscillatorStochModelInstance 2 ...");
        assertEquals("mod1b.tostring()"
                , mod1bString.substring(0, "org.openda.models.simpleModel.SimpleOscillatorStochModelInstance".length())
                , "org.openda.models.simpleModel.SimpleOscillatorStochModelInstance");
    }//end testOscilatorModel_1

    public static void testOscillatorModel_2() {
        System.out.println("=========================================================");
        System.out.println("Model time");
        System.out.println("=========================================================");
        IStochModelFactory fact2 = new SimpleOscillatorStochModelFactory();
        fact2.initialize(null, new String[]{"<oscillatorConfig><parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters></oscillatorConfig>"});
        IStochModelInstance mod2 = fact2.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        System.out.println("mod2=" + mod2.toString());
        //   public Time getTimeHorizon() {
        String timeHorizonString = mod2.getTimeHorizon().toString();
        System.out.println("mod2.getTimeHorizon=" + timeHorizonString);
        System.out.println("Should be mod2.getTimeHorizon=0.0:0.05:10.0");
        assertTrue("mod2.getTimeHorizon()", timeHorizonString.equalsIgnoreCase("0.0:0.05:10.0"));
        //   public Time getCurrentTime() {
        ITime currentTime = mod2.getCurrentTime();
        System.out.println("mod2.getCurrentTime=" + currentTime);
        System.out.println("Should be mod2.getCurrentTime=0.0");
        assertEquals("mod2.getCurrentTime()", currentTime.toString(), "0.0");
    }//end testOscilatorModel_2

    public static void testOscillatorModel_3() {
        System.out.println("=========================================================");
        System.out.println("Model state");
        System.out.println("=========================================================");
        IStochModelFactory fact3 = new SimpleOscillatorStochModelFactory();
        fact3.initialize(null, new String[]{"<oscillatorConfig><parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters></oscillatorConfig>"});
        IStochModelInstance mod3 = fact3.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getState() {
        IVector x3 = mod3.getState();
        System.out.println("mod3.getState()=" + x3.toString());
        System.out.println("Should be mod3.getState() =[0.8,0.0]");
        assertEquals("mod3.getState()", x3.toString(), "[0.8,0.0]");
        //   public void axpyOnState(double alpha, Vector vector) {
        IVector x3_delta = new Vector("[0.0,1.0]");
        mod3.axpyOnState(1.0, x3_delta);
        IVector x3b = mod3.getState();
        System.out.println("mod3.getState()=" + x3b.toString());
        System.out.println("Should be mod3.getState() =[0.8,1.0]");
        assertEquals("mod33getState()", x3b.toString(), "[0.8,1.0]");
        //   public StochVector getStateUncertainty() {
        IStochVector x3Stoch = mod3.getStateUncertainty();
        System.out.println("mod3.getStateUncertainty()=" + x3Stoch.toString());
        System.out.println("Should be mod3.getStateUncertainty() ={[0.0,0.0],[0.8,0.8]}");
        assertEquals("mod3.getState()", x3Stoch.toString(), "{[0.0,0.0],[0.8,0.8]}");
    }//end testOscilatorModel_3

    public static void testOscillatorModel_4() {
        System.out.println("=========================================================");
        System.out.println("Model parameters");
        System.out.println("=========================================================");
        IStochModelFactory fact4 = new SimpleOscillatorStochModelFactory();
        fact4.initialize(null, new String[]{"<oscillatorConfig><parameters names=\"t_damp,omega\">"
        		+"[8.0,1.5708]</parameters><parameterUncertainty names=\"t_damp,omega\">[1.0,0.1257]</parameterUncertainty></oscillatorConfig>"});
        IStochModelInstance mod4 = fact4.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getParameters() {
        IVector par4 = new Vector(mod4.getParameters());
        System.out.println("par4=" + par4);
        System.out.println("Should be par4=[8.0,1.5708]");
        assertEquals("mod33getState()", par4.toString(), "[8.0,1.5708]");
        //   public void setParameters(Vector parameters) {
        IVector par4b = new Vector("[9.0,14.0]");
        mod4.setParameters(par4b);
        IVector par4bCheck = new Vector(mod4.getParameters());
        System.out.println("par4b=" + par4b);
        System.out.println("Should be par4b=[9.0,14.0]");
        assertEquals("mod4.getParameters()", par4b.toString(), par4bCheck.toString());
        //   public void axpyOnParameters(double alpha, Vector vector) {
        IVector par4delta = new Vector("[1.0,0.0]");
        mod4.axpyOnParameters(1.0, par4delta);
        IVector par4c = new Vector(mod4.getParameters());
        System.out.println("par4c=" + par4c);
        System.out.println("Should be par4c=[10.0,14.0]");
        assertEquals("mod4.getParameters()", par4c.toString(), "[10.0,14.0]");
        //   public StochVector getParameterUncertainty() {
        IStochVector p4Stoch = mod4.getParameterUncertainty();
        System.out.println("mod4.getParameterUncertainty()=" + p4Stoch.toString());
        System.out.println("Should be mod4.getParameterUncertainty() ={[8.0,1.5708],[1.0,0.1257]}");
        assertEquals("mod4.getParameteruncertainty()", p4Stoch.toString(), "{[8.0,1.5708],[1.0,0.1257]}");
    }//end testOscilatorModel_4

    public static void testOscillatorModel_5() {
        System.out.println("=========================================================");
        System.out.println("Model observed values");
        System.out.println("=========================================================");
        IStochModelFactory fact5 = new SimpleOscillatorStochModelFactory();
        fact5.initialize(null, new String[]{"<oscillatorConfig><parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters></oscillatorConfig>"});
        IStochModelInstance mod5 = fact5.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        IStochModelInstance mod5b = fact5.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std\n"
                + "0.0,0.0,8.1,0.1\n"
                + "0.1,0.0,8.2,0.1\n"
                + "0.2,0.0,8.3,0.1";
        IStochObserver obs5 = new CsvStochObserver(content);
        IObservationDescriptions descr5 = obs5.getObservationDescriptions();
        mod5.compute(new Time(0.5));
        System.out.println("mod5.compute(0.5)");
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector prd5 = mod5.getObservedValues(descr5);
        System.out.println("mod5.getObservedValues()=" + prd5.toString());
        // x(0) is taken at t=0.5 for all 3 obs because model did not save previous values
        System.out.println("Should be mod5.getObservedValues()=[0.5750505908505237,0.5750505908505237,0.5750505908505237]");
        assertEquals("mod5.getObservedValues()", prd5.toString(), "[0.5750505908505237,0.5750505908505237,0.5750505908505237]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
        mod5b.announceObservedValues(descr5);
        mod5b.compute(new Time(0.5));
        System.out.println("mod5b.compute()");
        IVector prd5b = mod5b.getObservedValues(descr5);
        System.out.println("mod5b.announceObservedValues(descr5)");
        System.out.println("mod5b.getObservedValues()=" + prd5b.toString());
        System.out.println("Should be mod5b.getObservedValues()=[0.8,0.790232176428735,0.7614885160245464]");
        assertEquals("mod5b.getObservedValues()", prd5b.toString(), "[0.8,0.790232176428735,0.7614885160245464]");
    }//end testOscilatorModel_5

    public void testOscillatorModel_6() {
        System.out.println("=========================================================");
        System.out.println("Model save state");
        System.out.println("=========================================================");
        IStochModelFactory fact6 = new SimpleOscillatorStochModelFactory();
        fact6.initialize(testRunDataDir, new String[]{""});
        IStochModelInstance mod6 = fact6.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Object saveInternalState() {
		IModelState s1 = mod6.saveInternalState();
        mod6.compute(new Time(1.0));
        IVector x1 = mod6.getState();
        //   public void saveInternalState(Object saveInternalState) {
        mod6.restoreInternalState(s1); //restore previous time
        mod6.compute(new Time(1.0));
        IVector x2 = mod6.getState();
        System.out.println("saved state (before, after) =(" + x1.toString() + "," + x2.toString() + ")");
        System.out.println("Should be saved state (before, after) =([0.05987678390975121,-1.112492799311437],[0.05987678390975121,-1.112492799311437])");
        assertEquals("mod6.saveInernalState()", x1.getValue(1), x2.getValue(1));
        //   public void releaseInternalState(Object saveInternalState) {
        //mod6.releaseInternalState(s1);
    }//end testOscilatorModel_6


   public static void testOscillatorModel_7() {
        System.out.println("=========================================================");
        System.out.println("Model system noise");
        System.out.println("=========================================================");
        IStochModelFactory fact7 = new SimpleOscillatorStochModelFactory();
        fact7.initialize(null, new String[]{""});
        // automatic noise generation
        IStochModelInstance mod7 = fact7.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod7.setAutomaticNoiseGeneration(true);
        mod7.compute(new Time(3.0));
        //TODO add serious test

        /*
         *  preset noise
         */
        IStochModelInstance mod7a = fact7.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod7a.setAutomaticNoiseGeneration(false); // we are setting noise explicitly here
        double tnow = mod7a.getCurrentTime().getMJD();
        double ttarget = 2.0;
        ITime tSpan = new Time(tnow,ttarget);
        // are we dealing with stationary noise?
        //   public boolean isWhiteNoiseStationary() {
        boolean isStationary = mod7a.isWhiteNoiseStationary();
        //   public StochVector[] getWhiteNoiseUncertainty(Time time) {

        //TODO test
        //   public Time[] getWhiteNoiseTimes(Time timeSpan) {
        //MVL Time noiseTimes[] = mod7a.getWhiteNoiseTimes(tSpan);

        //   public Vector[] getWhiteNoise(Time timeSpan) {
        //   public void setWhiteNoise(Vector whiteNoise[]) {
        //   public void axpyOnWhiteNoise(double alpha, Vector vector[]) {
        //   public void setAutomaticNoiseGeneration(boolean value) {


        assertTrue("mod5 sysnoise", true); //TODO
    }//end testOscilatorModel_7


    public static void testOscillatorModel_8() {
        System.out.println("=========================================================");
        System.out.println("Model state scaling");
        System.out.println("=========================================================");
        IStochModelFactory fact8 = new SimpleOscillatorStochModelFactory();
        fact8.initialize(null, new String[]{""});
        //   StochModelInstance mod8 = fact8.getInstance(); // get a model
        //   public Vector getStateScaling() {
        //   public Vector[] getStateScaling(ObservationDescriptions observationDescriptions) {
        assertTrue("mod8 statescaling", true); //TODO
    }//end testOscilatorModel_8

    public static void testOscillatorModel_9() {
        System.out.println("=========================================================");
        System.out.println("Model exchangeItems");
        System.out.println("=========================================================");
        IStochModelFactory fact9 = new SimpleOscillatorStochModelFactory();
        fact9.initialize(null, new String[]{""});
        //   StochModelInstance mod9 = fact9.getInstance(); // get a model
        //   public String[] getExchangeItemIDs() {
        //   public Vector getValues(String exchangeItemID) {
        //   public Vector[] getValues(String exchangeItemID, Time time) {
        //   public Time[] getTimes(String exchangeItemID, Time time) {
        //   public Vector[] getValues(Time[] timeStamps) {
        //   public void setValues(String exchangeItemID, Vector[] Vectors) {
        assertTrue("mod9 exchangeItems", true); //TODO
    }//end testOscilatorModel_9

    public static void testOscillatorModel_10() {
        //   public void compute(Time targetTime) {
        System.out.println("=========================================================");
        System.out.println("Model compute");
        System.out.println("=========================================================");
        IStochModelFactory fact10 = new SimpleOscillatorStochModelFactory();
        fact10.initialize(null, new String[]{""});
        IStochModelInstance mod10 = fact10.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod10.compute(new Time(1.0));
        IVector x10 = mod10.getState();
        System.out.println("mod10.compute(1.0).get_state() = " + x10);
        System.out.println("Should be mod10.compute(1.0).get_state() = [0.05987678390975121,-1.112492799311437]");
        assertEquals("mod10.compute(1.0", x10.toString(), "[0.05987678390975121,-1.112492799311437]");
        System.out.println("mod10=" + mod10);
    }

    public static void testOscillatorModel_11() {
        System.out.println("=========================================================");
        System.out.println("Model observed values with a quadratic transform");
        System.out.println("=========================================================");
        IStochModelFactory fact11 = new SimpleOscillatorStochModelFactory();
        fact11.initialize(null, new String[]{"<oscillatorConfig><parameters names=\"t_damp,omega\">[8.0,1.11708]</parameters></oscillatorConfig>"});
        IStochModelInstance mod11 = fact11.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        IStochModelInstance mod11b = fact11.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std,transform\n"
                + "0.0,0.0,8.1,0.1,2.0\n"
                + "0.1,0.0,8.2,0.1,2.0\n"
                + "0.2,0.0,8.3,0.1,2.0";
        IStochObserver obs11 = new CsvStochObserver(content);
        IObservationDescriptions descr11 = obs11.getObservationDescriptions();
        mod11.compute(new Time(0.10));
        System.out.println("mod11.compute(0.1)");
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector prd11 = mod11.getObservedValues(descr11);
        System.out.println("mod11.getObservedValues()=" + prd11.toString());
        // x(0) is taken at t=0.11 for all 3 obs because model did not save previous values
        // NOTE : time is ignored
        System.out.println("Should be mod11.getObservedValues()=[0.6321124632393615,0.6321124632393615,0.6321124632393615]");
        assertEquals("mod11.getObservedValues()", prd11.toString(), "[0.6321124632393615,0.6321124632393615,0.6321124632393615]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
        mod11b.announceObservedValues(descr11);
        mod11b.compute(new Time(0.5));
        System.out.println("mod11b.compute()");
        IVector prd11b = mod11b.getObservedValues(descr11);
        System.out.println("mod11b.announceObservedValues(descr11)");
        System.out.println("mod11b.getObservedValues()=" + prd11b.toString());
        System.out.println("Should be mod11b.getObservedValues()=[0.6400000000000001,0.6321124632393615,0.6090929071807221]");
        assertEquals("mod11b.getObservedValues()", prd11b.toString(), "[0.6400000000000001,0.6321124632393615,0.6090929071807221]");
    }//end testOscilatorModel_11
    
    public void testOscillatorModel_12() {
        System.out.println("=========================================================");
        System.out.println("Model restarts");
        System.out.println("=========================================================");
        File restartFile = new File(testRunDataDir, "oscillator_restart_1.0.txt");
        IStochModelFactory fact12 = new OscillatorStochModelFactory();
        fact12.initialize(testRunDataDir, new String[]{""});
        IStochModelInstance mod12 = fact12.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Object restoreInternalState() {
        mod12.compute(new Time(1.0));
        IVector x1a = mod12.getState(); // save this for checks
        IModelState s1 = mod12.saveInternalState();
        s1.savePersistentState(restartFile);
        // check restart file
        File refFile = new File(testRunDataDir, "oscillator_restart_1.0.txt_ref");
        assertTrue(testData.FilesAreIdentical(restartFile, refFile));
        // continue computation
        mod12.compute(new Time(2.0)); //continue original model
        IVector x2a = mod12.getState(); // save this for checks

        // start second model instance from restart
        //   public void restoreInternalState(Object restoreInternalState) {
        IStochModelInstance mod12b = fact12.getInstance(OutputLevel.Suppress);
        IModelState s2 = mod12b.loadPersistentState(restartFile);
        mod12b.restoreInternalState(s2); //restore previous time
        IVector x1b = mod12b.getState();
        mod12b.compute(new Time(2.0));
        IVector x2b = mod12b.getState();
        // check state at t=1.0
        System.out.println("saved state (before, after) =(" + x1a.toString() + "," + x1b.toString() + ")");
        System.out.println("Should be saved state (before, after) =([0.05987678390975121,-1.112492799311437],[0.05987678390975121,-1.112492799311437])");
        assertEquals("state at t=1.0:", x1a.getValue(1), x1b.getValue(1));
        // check state at t=2.0
        System.out.println("state at t=2.0 (before, after) =(" + x2a.toString() + "," + x2b.toString() + ")");
        System.out.println("Should be saved state (before, after) =([-0.6225143278207775,-0.009782261267563633],[-0.6225143278207775,-0.009782261267563633])");
        assertEquals("state at t=2.0:", x2a.getValue(1), x2b.getValue(1));
    }

    public static void testAdjointObservationOperator() {
        System.out.println("=========================================================");
        System.out.println("Tangent and adjoint operators for observations");
        System.out.println("=========================================================");
        IStochModelFactory fact = new SimpleOscillatorStochModelFactory();
        fact.initialize(null, new String[]{"<oscillatorConfig><parameters names=\"t_damp,omega\">[8.0,1.11708]</parameters></oscillatorConfig>"});
        IStochModelInstance mod = fact.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std,transform\n"
                + "0.1,0.0,8.1,0.1,1.0\n"   //position
                + "0.1,1.0,8.2,0.1,1.0\n"   //velocity
                + "0.1,1.0,8.3,0.1,2.0";    // velocity^2
        IStochObserver obs = new CsvStochObserver(content);
        IObservationDescriptions descr = obs.getObservationDescriptions();
        mod.compute(new Time(0.10));
        System.out.println("mod.compute(0.1)");
        System.out.println("mod.getSate()="+mod.getState());
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector predictions = mod.getObservedValues(descr);
        System.out.println("mod.getObservedValues()=" + predictions.toString());
        // x is taken at t=0.11 for all 3 obs because model did not save previous values
        // NOTE : time is ignored
        System.out.println("Should be mod.getObservedValues()=[0.7950550064236823,-0.09838695870499911,0.0096799936432192]");
        assertEquals("mod.getObservedValues()", predictions.toString(), "[0.7950550064236823,-0.09838695870499911,0.0096799936432192]");
        
        IVector linearPredictions = ((IModelAdjoint)mod).applyObservationTangent(mod.getState(), descr);
        System.out.println("mod.applyObservationTangent()=" + linearPredictions.toString());
        System.out.println("Should be mod.applyObservationTangent()=[0.7950550064236823,-0.09838695870499911,0.0193599872864384]");
        assertEquals("mod.applyObservationTangent()", linearPredictions.toString(), "[0.7950550064236823,-0.09838695870499911,0.0193599872864384]");
        
        IVector yAdjoint1 = new Vector("[1,0,0]");
        System.out.println("yAdjoint="+yAdjoint1);
        IVector xAdjoint1 = ((IModelAdjoint)mod).applyObservationAdjoint(yAdjoint1, descr);
        System.out.println("mod.applyObservationAdjoint()=" + xAdjoint1.toString());
        System.out.println("Should be mod.applyObservationAdjoint()=[1.0,0.0]");
        assertEquals("mod.applyObservationAdjoint()", xAdjoint1.toString(), "[1.0,0.0]");
        
        IVector yAdjoint2 = new Vector("[0,1,0]");
        System.out.println("yAdjoint="+yAdjoint2);
        IVector xAdjoint2 = ((IModelAdjoint)mod).applyObservationAdjoint(yAdjoint2, descr);
        System.out.println("mod.applyObservationAdjoint()=" + xAdjoint2.toString());
        System.out.println("Should be mod.applyObservationAdjoint()=[0.0,1.0]");
        assertEquals("mod.applyObservationAdjoint()", xAdjoint2.toString(), "[0.0,1.0]");

        IVector yAdjoint3 = new Vector("[0,0,1]");
        System.out.println("yAdjoint="+yAdjoint3);
        IVector xAdjoint3 = ((IModelAdjoint)mod).applyObservationAdjoint(yAdjoint3, descr);
        System.out.println("mod.applyObservationAdjoint()=" + xAdjoint3.toString());
        System.out.println("Should be mod.applyObservationAdjoint()=[0.0,-0.19677391740999822]");
        assertEquals("mod.applyObservationAdjoint()", xAdjoint3.toString(), "[0.0,-0.19677391740999822]");
     }

    
}//end class




