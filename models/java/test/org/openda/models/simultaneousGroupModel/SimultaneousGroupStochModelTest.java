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
package org.openda.models.simultaneousGroupModel;

/**
 * Main class for testing OscillatorModel class
 */


import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.observers.GroupStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;

/**
 * Test for SimultaneousStoch vector
 */
public class SimultaneousGroupStochModelTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SimultaneousGroupStochModelTest.class,"models");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public static void testSimultaneousGroupStochModel_1() {
		   System.out.println("=========================================================");
		   System.out.println("SimultaneousGroupStochModel: create and decompose (Array constructor)");
		   System.out.println("=========================================================");
		   // two separate ModelFactory's
		   IStochModelFactory fact1 = new OscillatorStochModelFactory();
		   String args[] = {""};
		   fact1.initialize(null, args);
		   IStochModelFactory fact2 = new OscillatorStochModelFactory();
		   fact2.initialize(null, args);
		   // create a group
		   IStochModelFactory modArray[] = new IStochModelFactory[2];
		   modArray[0] = fact1;
		   modArray[1] = fact2;
		   String ids[] = {"factory1","factory2"};
		   SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory(modArray,ids);
		   System.out.println("modGroup = "+modGroup.toString());

		   String idsFromGroup[] = modGroup.getIds();
		   assertEquals("modGroup.getIds()","factory1", idsFromGroup[0]);
		   IStochModelFactory modFromGroup1 = modGroup.getChild(0);
		   System.out.println("modGroup.getChild(0) = "+modFromGroup1.toString());
		   assertEquals("modGroup.getChild(0)",-1572018152, modFromGroup1.toString().hashCode());
		   IStochModelFactory modFromGroup2 = modGroup.getChild("factory2");
		   System.out.println("modGroup.getChild(\"factory2\") = "+modFromGroup2.toString());
		   assertEquals("modGroup.getChild(\"factory2\")",-1572018152, modFromGroup2.toString().hashCode());

		   IStochModelInstance mod1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		   String mod1String = mod1.toString();
		   System.out.println("modGroup.getInstance()="+mod1String);
		   assertTrue(mod1String.contains("factory1"));
		   assertTrue(mod1String.contains("factory2"));
		   assertTrue(mod1String.contains("x= [0.8,0.0]"));
		   assertTrue(mod1String.contains("p= [8.0,1.5707963267948966]"));

	    }

	public static void testSimultaneousGroupStochModel_2() {
		   System.out.println("=========================================================");
		   System.out.println("SimultaneousGroupStochModel: create and decompose (List constructor)");
		   System.out.println("=========================================================");
		   // two separate ModelFactory's
		   IStochModelFactory fact1 = new OscillatorStochModelFactory();
		   String args[] = {""};
		   fact1.initialize(null, args);
		   IStochModelFactory fact2 = new OscillatorStochModelFactory();
		   fact2.initialize(null, args);
		   // create a group
		   ArrayList<IStochModelFactory> modArray = new ArrayList<IStochModelFactory>();
		   modArray.add(fact1);
		   modArray.add(fact2);
		   ArrayList<String> ids = new ArrayList<String>();
		   ids.add("factory1"); ids.add("factory2");
		   SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory(modArray,ids);
		   System.out.println("modGroup = "+modGroup.toString());

		   String idsFromGroup[] = modGroup.getIds();
		   assertEquals("modGroup.getIds()","factory1", idsFromGroup[0]);
		   IStochModelFactory modFromGroup1 = modGroup.getChild(0);
		   System.out.println("modGroup.getChild(0) = "+modFromGroup1.toString());
		   assertEquals("modGroup.getChild(0)",-1572018152, modFromGroup1.toString().hashCode());
		   IStochModelFactory modFromGroup2 = modGroup.getChild("factory2");
		   System.out.println("modGroup.getChild(\"factory2\") = "+modFromGroup2.toString());
		   assertEquals("modGroup.getChild(\"factory2\")",-1572018152, modFromGroup2.toString().hashCode());
	    }

	public static void testSimultaneousGroupStochModel_3() {
		   System.out.println("=========================================================");
		   System.out.println("SimultaneousGroupStochModel: create and decompose (String constructor)");
		   System.out.println("=========================================================");
		   String config[] = {
				    "<stochModelFactory>"
				   +"   <stochModelFactory id=\"oscillator1\" className=\"org.openda.models.simpleModel.SimpleOscillatorStochModelFactory\">"
				   +"      <oscillatorConfig>"
				   +"         <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>"
				   +"         <parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters>"
				   +"         <parameterUncertainty names=\"t_damp,omega\">[1.0,0.1257]</parameterUncertainty>"
				   +"         <systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>"
				   +"         <initialState>[0.8,0.0]</initialState>"
				   +"         <initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>"
				   +"      </oscillatorConfig>"
				   +"   </stochModelFactory>"
				   +"   <stochModelFactory id=\"oscillator2\" className=\"org.openda.models.simpleModel.SimpleOscillatorStochModelFactory\">"
				   +"      <oscillatorConfig>"
				   +"         <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>"
				   +"         <parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters>"
				   +"         <parameterUncertainty names=\"t_damp,omega\">[1.0,0.1257]</parameterUncertainty>"
				   +"         <systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>"
				   +"         <initialState>[0.8,0.2]</initialState>"
				   +"         <initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>"
				   +"      </oscillatorConfig>"
				   +"   </stochModelFactory>"
				   +"</stochModelFactory>"};
		   SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		   modGroup.initialize(null, config);
		   System.out.println("modGroup = "+modGroup.toString());

		   String idsFromGroup[] = modGroup.getIds();
		   assertEquals("modGroup.getIds()","oscillator1", idsFromGroup[0]);
		   IStochModelFactory modFromGroup1 = modGroup.getChild(0);
		   System.out.println("modGroup.getChild(0) = "+modFromGroup1.toString());
		   assertEquals("modGroup.getChild(0)",-1404113011, modFromGroup1.toString().hashCode());
		   IStochModelFactory modFromGroup2 = modGroup.getChild("oscillator2");
		   System.out.println("modGroup.getChild(\"oscillator2\") = "+modFromGroup2.toString());
		   assertEquals("modGroup.getChild(\"oscillator2\")",-1404113011, modFromGroup2.toString().hashCode());

		   IStochModelInstance mod1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		   String mod1String = mod1.toString();
		   System.out.println("modGroup.getInstance()="+mod1String);
		   assertTrue(mod1String.contains("oscillator1"));
		   assertTrue(mod1String.contains("oscillator1"));
		   assertTrue(mod1String.contains("x= [0.8,0.0]"));
		   assertTrue(mod1String.contains("x= [0.8,0.2]"));

	    }

	public void testSimultaneousGroupStochModel_4() {
		   System.out.println("=========================================================");
		   System.out.println("SimultaneousGroupStochModel: create and decompose (File constructor)");
		   System.out.println("=========================================================");
		   String fileName[] = {"groupModel.xml"};
		   SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		   modGroup.initialize(testRunDataDir, fileName);

		   System.out.println("modGroup = "+modGroup.toString());

		   String idsFromGroup[] = modGroup.getIds();
		   assertEquals("modGroup.getIds()","oscillator1", idsFromGroup[0]);
		   IStochModelFactory modFromGroup1 = modGroup.getChild(0);
		   System.out.println("modGroup.getChild(0) = "+modFromGroup1.toString());
		   assertTrue(modFromGroup1.toString().contains("org.openda.models.simpleModel.SimpleOscillatorStochModelFactory"));
		   assertTrue(modFromGroup1.toString().contains("oscillator1.xml"));
		   IStochModelFactory modFromGroup2 = modGroup.getChild("oscillator2");
		   System.out.println("modGroup.getChild(\"oscillator2\") = "+modFromGroup2.toString());
		   assertTrue(modFromGroup2.toString().contains("org.openda.models.simpleModel.SimpleOscillatorStochModelFactory"));
		   assertTrue(modFromGroup2.toString().contains("oscillator2.xml"));

		   IStochModelInstance mod1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		   String mod1String = mod1.toString();
		   System.out.println("modGroup.getInstance()="+mod1String);
		   assertTrue(mod1String.contains("oscillator1"));
		   assertTrue(mod1String.contains("oscillator2"));
		   assertTrue(mod1String.contains("x= [0.8,0.0]"));
		   assertTrue(mod1String.contains("x= [0.8,0.2]"));

	}

	public void testSimultaneousGroupStochModel_5() {
		   System.out.println("=========================================================");
		   System.out.println("SimultaneousGroupStochModel: basic model methods");
		   System.out.println("=========================================================");
		   String fileName[] = {"groupModel.xml"};
		   SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		   modGroup.initialize(testRunDataDir, fileName);

		   System.out.println("modGroup = "+modGroup.toString());

		   IStochModelInstance instance1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		   System.out.println("instance1 = "+instance1.toString());

		   IVector x = instance1.getState();
		   System.out.println("instance1.getState()=" + x.toString());
		   assertTrue(x.toString().contains("oscillator1"));
		   assertTrue(x.toString().contains("oscillator2"));
		   assertTrue(x.toString().contains("[0.8,0.0]"));
		   assertTrue(x.toString().contains("[0.8,0.2]"));

		   // public Time get TimeHorizon()
	        String timeHorizonString = instance1.getTimeHorizon().toString();
	        System.out.println("instance1.getTimeHorizon=" + timeHorizonString);
	        System.out.println("Should be instance1.getTimeHorizon=0.0:20.0:20.0");
	        assertTrue("instance1.getTimeHorizon()", timeHorizonString.equalsIgnoreCase("0.0:20.0:20.0"));
	        //   public Time getCurrentTime() {
	        ITime currentTime = instance1.getCurrentTime();
	        System.out.println("instance1.getCurrentTime=" + currentTime);
	        System.out.println("Should be instance1.getCurrentTime=0.0");
	        assertEquals("instance1.getCurrentTime()", currentTime.toString(), "0.0");

	        // compute a bit
	        instance1.compute(new Time(12.0));
	        ITime newCurrentTime = instance1.getCurrentTime();
	        assertEquals("instance1.getCurrentTime()", 12.0, newCurrentTime.getMJD());

	    }

	public void testSimultaneousGroupStochModel_6() {
		System.out.println("=========================================================");
		System.out.println("SimultaneousGroupStochModel: parameters");
		System.out.println("=========================================================");
		String fileName[] = {"groupModel.xml"};
		SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		modGroup.initialize(testRunDataDir, fileName);

		//System.out.println("modGroup = "+modGroup.toString());

		IStochModelInstance instance1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		//System.out.println("instance1 = "+instance1.toString());

		//   public Vector getParameters() {
		IVector par = new Vector(instance1.getParameters());
		System.out.println("par=" + par);
		System.out.println("Should be par=[8.0,1.5708]");
		assertEquals("instance1.getParameters()", par.toString(), "[8.0,1.5708]");

		//   public void setParameters(Vector parameters) {
		IVector newPar = new Vector("[9.0,14.0]");
		instance1.setParameters(newPar);
		IVector newParCheck = new Vector(instance1.getParameters());
		System.out.println("newPar=" + newPar);
		System.out.println("Should be newPar=[9.0,14.0]");
		assertEquals("instance1.getParameters()", newPar.toString(), newParCheck.toString());

		//   public void axpyOnParameters(double alpha, Vector vector) {
		IVector parDelta = new Vector("[1.0,0.0]");
		instance1.axpyOnParameters(1.0, parDelta);
		IVector parChanged = new Vector(instance1.getParameters());
		System.out.println("parChanged=" + parChanged);
		System.out.println("Should be parChanged=[10.0,14.0]");
		assertEquals("instance1.getParameters()", parChanged.toString(), "[10.0,14.0]");

		//   public StochVector getParameterUncertainty() {
		IStochVector parStoch = instance1.getParameterUncertainty();
		System.out.println("parDelta.getParameterUncertainty()=" + parStoch.toString());
		System.out.println("Should be mod4.getParameterUncertainty() ={[8.0,1.5708],[1.0,0.1257]}");
		assertEquals("mod4.getParameteruncertainty()", parStoch.toString(), "{[8.0,1.5708],[1.0,0.1257]}");
	}


	public void testSimultaneousGroupStochModel_7() {
		System.out.println("=========================================================");
		System.out.println("Model state");
		System.out.println("=========================================================");
		String fileName[] = {"groupModel.xml"};
		SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		modGroup.initialize(testRunDataDir, fileName);

		//System.out.println("modGroup = "+modGroup.toString());

		IStochModelInstance instance1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		//System.out.println("instance1 = "+instance1.toString());
		//   public Vector getState() {
		IVector x = instance1.getState();
		System.out.println("instance1.getState()=" + x.toString());
		System.out.println("Should be instance1.getState() =[0.8,0.0,0.8,0.2]");
		assertTrue(checkIdentical(x, new Vector("[0.8,0.0,0.8,0.2]")));
		//   public void axpyOnState(double alpha, Vector vector) {
		IVector x_delta = new Vector("[0.0,1.0,2.0,3.0]");
		instance1.axpyOnState(1.0, x_delta);
		IVector xCheck = instance1.getState();
		System.out.println("instance1.getState()=" + xCheck.toString());
		System.out.println("Should be instance1.getState() =[0.8,1.0,2.8,3.2]");
		assertTrue(checkIdentical(xCheck,new Vector("[0.8,1.0,2.8,3.2]")));
		//   public StochVector getStateUncertainty() {
		IStochVector xStoch = instance1.getStateUncertainty();
		System.out.println("instance1.getStateUncertainty()=" + xStoch.toString());
		System.out.println("Should be instance1.getStateUncertainty() ={[0.0,0.0,0.0,0.0],[0.8,0.8,0.8,0.8]}");
		assertTrue(checkIdentical(xStoch.getExpectations(),new Vector("[0.0,0.0,0.0,0.0]")));
		assertTrue(checkIdentical(xStoch.getStandardDeviations(),new Vector("[0.8,0.8,0.8,0.8]")));
	}


    public void testSimultaneousGroupStochModel_8() {
        System.out.println("=========================================================");
        System.out.println("Model observed values");
        System.out.println("=========================================================");
		String fileName[] = {"groupModel.xml"};
		SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		modGroup.initialize(testRunDataDir, fileName);

		//System.out.println("modGroup = "+modGroup.toString());

		IStochModelInstance instance1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		//System.out.println("instance1 = "+instance1.toString());

		String config[] = {
				"<stochObserver>"
				+ "   <stochObserver id=\"oscillator1\" className=\"org.openda.utils.CsvStochObserver\">"
				+ "   time,index,value,std\n"
				+ "   0.0,0.0,1.1,0.1\n"
				+ "   0.1,0.0,1.2,0.1\n"
				+ "   0.2,0.0,1.3,0.1"
				+ "   </stochObserver>"
				+ "   <stochObserver id=\"oscillator2\" className=\"org.openda.utils.CsvStochObserver\">"
				+ "   time,index,value,std\n"
				+ "   2.0,0.0,2.1,0.1\n"
				+ "   2.1,0.0,2.2,0.1\n"
				+ "   2.2,0.0,2.3,0.1"
				+ "   </stochObserver>"
				+ "</stochObserver>"};
		GroupStochObserver obsGroup = new GroupStochObserver();
		obsGroup.initialize(null, config);
		System.out.println("obsGroup = "+obsGroup.toString());
		IObservationDescriptions obsDescriptions = obsGroup.getObservationDescriptions();

        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
        instance1.announceObservedValues(obsDescriptions);
        instance1.compute(new Time(2.5));
        System.out.println("mod5b.compute(2.5)");
        IVector predictions = instance1.getObservedValues(obsDescriptions);
        System.out.println("instance1(obsDescriptions)");
        System.out.println("instance1()=" + predictions.toString());
        System.out.println("Should be instance1.getObservedValues()=TreeVector oscillator1 [0.8,0.790232176428735,0.7614885160245464]\n"
	        +"TreeVector oscillator2 [0.8,0.8073421142783438,0.787582691868013]");
        assertTrue( predictions.toString().contains("[0.8,0.790232176428735,0.7614885160245464]"));
        assertTrue( predictions.toString().contains("[0.8,0.8073421142783438,0.787582691868013]"));
    }


//    public static void testSimultaneousGroupStochModel_9() {
//        System.out.println("=========================================================");
//        System.out.println("Model save state");
//        System.out.println("=========================================================");
//        StochModelFactory fact6 = new SimultaneousGroupStochModelFactory();
//        fact6.initialize(null, new String[]{""});
//        StochModelInstance mod6 = fact6.getInstance(); // get a model
//        //   public Object saveInternalState() {
//        Object s1 = mod6.saveInternalState();
//        mod6.compute(new Time(1.0));
//        Vector x1 = mod6.getState();
//        //   public void saveInternalState(Object saveInternalState) {
//        mod6.restoreInternalState(s1); //restore previous time
//        mod6.compute(new Time(1.0));
//        Vector x2 = mod6.getState();
//        System.out.println("saved state (before, after) =(" + x1.toString() + "," + x2.toString() + ")");
//        System.out.println("Should be saved state (before, after) =([0.05987678390975121,-1.112492799311437],[0.05987678390975121,-1.112492799311437])");
//        assertEquals("mod6.saveInernalState()", x1.getValue(1), x2.getValue(1));
//        //   public void releaseInternalState(Object saveInternalState) {
//        //mod6.releaseInternalState(s1);
//    }//end testOscilatorModel_6
//

    public void testSimultaneousGroupStochModel_10() {
    	System.out.println("=========================================================");
    	System.out.println("Model system noise");
    	System.out.println("=========================================================");
		String fileName[] = {"groupModel.xml"};
		SimultaneousGroupStochModelFactory modGroup = new SimultaneousGroupStochModelFactory();
		modGroup.initialize(testRunDataDir, fileName);

		//System.out.println("modGroup = "+modGroup.toString());

		IStochModelInstance instance1 = modGroup.getInstance(IStochModelFactory.OutputLevel.Suppress);
		//System.out.println("instance1 = "+instance1.toString());
    	//   public StochVector[] getWhiteNoiseUncertainty(Time time) {
    	//   public boolean isWhiteNoiseStationary() {
    	//   public Time[] getWhiteNoiseTimes(Time timeSpan) {
    	//   public Vector[] getWhiteNoise(Time timeSpan) {
    	//   public void setWhiteNoise(Vector whiteNoise[]) {
    	//   public void axpyOnWhiteNoise(double alpha, Vector vector[]) {
    	//   public void setAutomaticNoiseGeneration(boolean value) {
    	instance1.setAutomaticNoiseGeneration(true);
    	instance1.compute(new Time(3.0));
    	assertTrue("instance1 sysnoise", true); //TODO
    }

//    public static void testSimultaneousGroupStochModell_11() {
//        System.out.println("=========================================================");
//        System.out.println("Model state scaling");
//        System.out.println("=========================================================");
//        StochModelFactory fact8 = new SimultaneousGroupStochModelFactory();
//        fact8.initialize(null, new String[]{""});
//        //   StochModelInstance mod8 = fact8.getInstance(); // get a model
//        //   public Vector getStateScaling() {
//        //   public Vector[] getStateScaling(ObservationDescriptions observationDescriptions) {
//        assertTrue("mod8 statescaling", true); //TODO
//    }//end testOscilatorModel_8
//
//    public static void testSimultaneousGroupStochModel_12() {
//        System.out.println("=========================================================");
//        System.out.println("Model exchangeItems");
//        System.out.println("=========================================================");
//        StochModelFactory fact9 = new SimultaneousGroupStochModelFactory();
//        fact9.initialize(null, new String[]{""});
//        //   StochModelInstance mod9 = fact9.getInstance(); // get a model
//        //   public String[] getExchangeItemIDs() {
//        //   public Vector getValues(String exchangeItemID) {
//        //   public Vector[] getValues(String exchangeItemID, Time time) {
//        //   public Time[] getTimes(String exchangeItemID, Time time) {
//        //   public Vector[] getValues(Time[] timeStamps) {
//        //   public void setValues(String exchangeItemID, Vector[] Vectors) {
//        assertTrue("mod9 exchangeItems", true); //TODO
//    }//end testOscilatorModel_9
//


	/*
	 *
	 * private methods
	 *
	 */

	private static boolean checkIdentical(IVector v1, IVector v2){
		boolean result = true;
		if(v1.getSize()!=v2.getSize()){
			result = false;
		}else {
			// check norm of difference
			IVector diff = v1.clone();
			diff.axpy(-1.0, v2);
			double normDiff = diff.norm2();
			if(normDiff>1e-6){
				result=false;
			}
		}
		return result;
	}



}//end class




