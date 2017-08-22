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
package org.openda.models.lorenz96;

/**
 * Main class for testing LorenzModel class
 */


import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Test for simple vector
 */
public class Lorenz96ModelTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(Lorenz96ModelTest.class,"models");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testLorenz96Model_1() {
        System.out.println("=========================================================");
        System.out.println("Model constructor");
        System.out.println("=========================================================");
        // Constructors
        String args[] = new String[]{""};
        IStochModelFactory fact1 = new Lorenz96StochModelFactory();
        fact1.initialize(testRunDataDir, args);
        IStochModelInstance mod1 = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public String toString(){
        String mod1String = mod1.toString();
        System.out.println("mod1=" + mod1String);
        System.out.println("Should be "
                + "mod1=org.openda.models.lorenz96.Lorenz96StochModelInstance 1 ...");
        assertEquals("mod1.tostring()"
                , mod1String.substring(0, "org.openda.models.lorenz96.Lorenz96StochModelInstance".length())
                , "org.openda.models.lorenz96.Lorenz96StochModelInstance");
    }

    public static void testModel_2() {
        System.out.println("=========================================================");
        System.out.println("Model time");
        System.out.println("=========================================================");
        IStochModelFactory fact2 = new Lorenz96StochModelFactory();
        fact2.initialize(null, new String[]{""});
        IStochModelInstance mod2 = fact2.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        System.out.println("mod2=" + mod2.toString());
        //   public Time getTimeHorizon() {
        String timeHorizonString = mod2.getTimeHorizon().toString();
        System.out.println("mod2.getTimeHorizon=" + timeHorizonString);
        System.out.println("Should be mod2.getTimeHorizon=0.0:0.025:30.0");
        assertTrue("mod2.getTimeHorizon()", timeHorizonString.equalsIgnoreCase("0.0:0.05:30.0"));
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
        IStochModelFactory fact3 = new Lorenz96StochModelFactory();
        fact3.initialize(null, new String[]{""});
        IStochModelInstance mod3 = fact3.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getState() {
        IVector x3 = mod3.getState();
        System.out.println("mod3.getState()=" + x3.toString());
        System.out.println("Should be mod3.getState() =[2.9432,8.279,5.6142,0.9046,-3.8377,-1.9521,-2.3153,7.8081,3.146,-8.4118,...,4.5963,3.9998,-4.5373,-2.5524,-1.8132,3.6624,6.6582,3.4951,0.4849,-1.6945]");
        assertEquals("mod3.getState()", x3.toString(), "[2.9432,8.279,5.6142,0.9046,-3.8377,-1.9521,-2.3153,7.8081,3.146,-8.4118,...,4.5963,3.9998,-4.5373,-2.5524,-1.8132,3.6624,6.6582,3.4951,0.4849,-1.6945]");
    }

    public static void testLorenzModel_4() {
        System.out.println("=========================================================");
        System.out.println("Model parameters");
        System.out.println("=========================================================");
        IStochModelFactory fact4 = new Lorenz96StochModelFactory();
        fact4.initialize(null, new String[]{""});
        IStochModelInstance mod4 = fact4.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public Vector getParameters() {
        IVector par4 = new Vector(mod4.getParameters());
        System.out.println("par4=" + par4);
        System.out.println("Should be par4=[8.0]");
        assertEquals("mod4.getParameters()", par4.toString(), "[8.0]");
        //   public void setParameters(Vector parameters) {
        IVector par4b = new Vector("[9.0]");
        mod4.setParameters(par4b);
        IVector par4bCheck = new Vector(mod4.getParameters());
        System.out.println("par4b=" + par4b);
        System.out.println("Should be par4b=[9.0]");
        assertEquals("mod4.getParameters()", par4b.toString(), par4bCheck.toString());
        //   public void axpyOnParameters(double alpha, Vector vector) {
        IVector par4delta = new Vector("[1.0]");
        mod4.axpyOnParameters(1.0, par4delta);
        IVector par4c = new Vector(mod4.getParameters());
        System.out.println("par4c=" + par4c);
        System.out.println("Should be par4c=[9.0]");
        assertEquals("mod4.getParameters()", par4c.toString(), "[10.0]");
        //   public StochVector getParameterUncertainty() {
        IStochVector p4Stoch = mod4.getParameterUncertainty();
        System.out.println("mod4.getParameterUncertainty()=" + p4Stoch.toString());
        System.out.println("Should be mod4.getParameterUncertainty() ={[8.0],[1.0]}");
        assertEquals("mod4.getParameteruncertainty()", p4Stoch.toString(), "{[8.0],[1.0]}");
    }

    public static void testLorenzModel_5() {
        System.out.println("=========================================================");
        System.out.println("Model observed values");
        System.out.println("=========================================================");
        IStochModelFactory fact5 = new Lorenz96StochModelFactory();
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
        System.out.println("Should be mod5.getObservedValues()=[2.9432,2.346008680606395,2.4632940939503487]");
        assertEquals("mod5.getObservedValues()", prd5.toString(), "[2.9432,2.346008680606395,2.4632940939503487]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
        mod5b.announceObservedValues(descr5);
        mod5b.compute(new Time(0.5));
        System.out.println("mod5b.compute()");
        IVector prd5b = mod5b.getObservedValues(descr5);
        System.out.println("mod5b.announceObservedValues(descr5)");
        System.out.println("mod5b.getObservedValues()=" + prd5b.toString());
        System.out.println("Should be mod5b.getObservedValues()=[2.9432,2.346008680606395,2.4632940939503487]");
        assertEquals("mod5b.getObservedValues()", prd5b.toString(), "[2.9432,2.346008680606395,2.4632940939503487]");
    }

    public static void testLorenzModel_6() {
        System.out.println("=========================================================");
        System.out.println("Model save state");
        System.out.println("=========================================================");
        IStochModelFactory fact6 = new Lorenz96StochModelFactory();
        fact6.initialize(null, new String[]{""});
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
        IStochModelFactory fact7 = new Lorenz96StochModelFactory();
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
        IStochModelFactory fact8 = new Lorenz96StochModelFactory();
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
        IStochModelFactory fact9 = new Lorenz96StochModelFactory();
        fact9.initialize(null, new String[]{""});
        // StochModelInstance mod9 = fact9.getInstance(); // get a model
        //   public String[] getExchangeItemIDs() {
        //   public Vector getValues(String exchangeItemID) {
        //   public Vector[] getValues(String exchangeItemID, Time time) {
        //   public Time[] getTimes(String exchangeItemID, Time time) {
        //   public Vector[] getValues(Time[] timeStamps) {
        //   public void setValues(String exchangeItemID, Vector[] Vectors) {
        //   public void axpyOnExchangeItem(String exchangeItemID, double alpha, Vector vector) {
        assertTrue("mod9 exchangeItems", false); //TODO
    }

    public static void testLorenzModel_10() {
        //   public void compute(Time targetTime) {
        System.out.println("=========================================================");
        System.out.println("Model compute");
        System.out.println("=========================================================");
        IStochModelFactory fact10 = new Lorenz96StochModelFactory();
        fact10.initialize(null, new String[]{""});
        IStochModelInstance mod10 = fact10.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod10.compute(new Time(1.0));
        IVector x10 = mod10.getState();
        System.out.println("mod10.compute(1.0).get_state() = " + x10);
        System.out.println("Should be mod10.compute(1.0).get_state() = [-2.7969745940766373,4.107260443716306,5.3017213434705015,6.234123180886789,3.6006770121034104,-0.7478377126242325,3.898961495596941,11.289460224024715,2.616840871889104,1.9955803467327498,...,7.901556022930961,-1.58589181204621,2.840475126133699,7.128466595831693,1.613203245246283,-1.1667570532665454,1.4904120661792288,2.265256034128074,5.342258000869002,5.710971961247007]");
        assertEquals("mod10.compute(1.0", x10.toString(), "[-2.7969745940766373,4.107260443716306,5.3017213434705015,6.234123180886789,3.6006770121034104,-0.7478377126242325,3.898961495596941,11.289460224024715,2.616840871889104,1.9955803467327498,...,7.901556022930961,-1.58589181204621,2.840475126133699,7.128466595831693,1.613203245246283,-1.1667570532665454,1.4904120661792288,2.265256034128074,5.342258000869002,5.710971961247007]");
        System.out.println("mod10=" + mod10);
    }

    public static void testLorenzModel_11() {
        //   public void compute(Time targetTime) {
        System.out.println("=========================================================");
        System.out.println("Model compute with config");
        System.out.println("=========================================================");
        IStochModelFactory fact11 = new Lorenz96StochModelFactory();
        String args[] = new String[1];
        args[0] = "<LorenzConfig>"
        	+"    <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>"
        	+"    <parameters names=\"F\">[8.0]</parameters>"
        	+"    <parameterUncertainty names=\"F\">[1.0]</parameterUncertainty>"
        	+"    <systemNoise>{[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]}</systemNoise>"
        	+"    <initialState>[2.9432, 8.2790, 5.6142, 0.9046,-3.8377,-1.9521,-2.3153,7.8081, 3.1460,-8.4118, 0.9981,-1.9022,-0.1401,6.3834, 7.1045, 3.8540, 3.4667, 2.5425,5.1582,3.6627,-2.9382, 1.3679, 8.8593, 1.6066,-3.9853,1.8962,12.8660,3.2394, 0.2166, 2.9771, 4.5963, 3.9998,-4.5373,-2.5524,-1.8132, 3.6624,6.6582,3.4951,0.4849,-1.6945]</initialState>"
        	+"    <initialStateUncertainty>[6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0]</initialStateUncertainty>"
        	+"</LorenzConfig>";
        fact11.initialize(null, args);
        IStochModelInstance mod11 = fact11.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        mod11.compute(new Time(10.0));
        IVector x11 = mod11.getState();
        System.out.println("mod10.compute(1.0).get_state() = " + x11);
        System.out.println("Should be mod10.compute(1.0).get_state() = [6.045796963594913,-1.1366813812223224,-2.096113736955042,2.3209224861669484,9.168089269511297,8.529553575989473,2.408432779673815,2.229016600456414,4.6431227977807845,4.302843927458458,...,-3.4411564817377847,2.5886429601014393,4.950565754775009,-0.7153949771499583,1.274073448200001,7.356296408936959,-2.3295353212417593,0.324958399749634,1.3431566310561796,7.319944894894655]");
        assertEquals("mod10.compute(1.0", x11.toString(), "[6.045796963594913,-1.1366813812223224,-2.096113736955042,2.3209224861669484,9.168089269511297,8.529553575989473,2.408432779673815,2.229016600456414,4.6431227977807845,4.302843927458458,...,-3.4411564817377847,2.5886429601014393,4.950565754775009,-0.7153949771499583,1.274073448200001,7.356296408936959,-2.3295353212417593,0.324958399749634,1.3431566310561796,7.319944894894655]");
        System.out.println("mod10=" + mod11);
    }

    
    public void tstLorenzModel_12() {   //depends on tests directory

        // TODO: move this test to algorithm tests
        //   public void compute(Time targetTime) {
        System.out.println("=========================================================");
        System.out.println("Model compute from file");
        System.out.println("=========================================================");
        File testDirMain = new File(testRunDataDir,"model");
        IStochModelFactory fact12 = new Lorenz96StochModelFactory();
        fact12.initialize(testDirMain, new String[]{"Lorenz96StochModel.xml"});
        File testDirObs = new File(testRunDataDir,"stochobserver");
        IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testDirObs,new String[]{"observations_lorenz_generated.csv"});
	    // Now start calibration through proper algorithm-class
//        Simulation algorithm = new Simulation();
//        File testDirAlgorithm = new File(testRunDataDir,"algorithm");
//        algorithm.initialize(testDirAlgorithm, new String[]{"simulationAlgorithm.xml"});
//        algorithm.setStochComponents(obsGenerated, fact12);
//        algorithm.prepare();
//        algorithm.run();
        //Vector par=algorithm.getBestEstimate().getParameters();
		//System.out.println("parameters = "+par);
		//System.out.println("Should be parameters = [8.0,1.5707963267948966]");
        //assertEquals("par",par.toString(),"[8.0,1.5707963267948966]");
    }

    public static void testLorenzModel_13() {
        System.out.println("=========================================================");
        System.out.println("Model state uncertainty");
        System.out.println("=========================================================");
        IStochModelFactory fact = new Lorenz96StochModelFactory();
        fact.initialize(null, new String[]{""});
        IStochModelInstance mod = fact.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public StochVector getStateUncertainty() {
        IStochVector xStoch = mod.getStateUncertainty();
        System.out.println("mod.getStateUncertainty()=" + xStoch.toString());
        String correct = "SpatialCorrelationStochVector(lengthScale=10.0,standardDeviation=6.0)";
        System.out.println("Should be mod.getStateUncertainty() ="+correct);
        assertEquals("mod.getState()", xStoch.toString(), correct);
        
        StochVector.setSeed(12345);
        //Vector.maxFullExpandLength=50;
        IVector sampleState = xStoch.createRealization();
		double [] values = sampleState.getValues();
        System.out.println("xSample=" + sampleState.toString());
		assertEquals(5.040116245421077,values[0],1e-5);
		assertEquals(4.6853598680203135,values[1],1e-5);
		assertEquals(4.227241813491862,values[2],1e-5);
		assertEquals(3.752178717638764,values[3],1e-5);
		assertEquals(3.3309235407207516,values[4],1e-5);
		assertEquals(3.0032322727417293,values[5],1e-5);
		assertEquals(2.770704510130689,values[6],1e-5);
		assertEquals(2.600288645712636,values[7],1e-5);
		assertEquals(2.436956297999765,values[8],1e-5);
		assertEquals(2.2205599573551678,values[9],1e-5);
    }

    
}//end class




