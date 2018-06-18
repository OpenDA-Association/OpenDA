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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.algorithms.kalmanFilter.EnKF;
import org.openda.algorithms.kalmanFilter.SequentialSimulation;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.observers.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

public class SequentialAlgorithmRestartTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SequentialAlgorithmRestartTest.class,"algorithms");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testSequentialSimulationRestartOscillator() {
        generateObservations_oscillator();
		System.out.println("========================================================");
		System.out.println(" Test SequentialSimulation (oscillator, a single run)");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory factory = new OscillatorStochModelFactory();
        factory.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});
		//create and run filter
        IAlgorithm sim = new SequentialSimulation();
        sim.initialize(testRunDataDir, new String[]{""});
        sim.setStochComponents(obsGenerated,factory);
        sim.prepare();
        sim.next();sim.next();
        //
        // save state
        //
        IVector x_orig = sim.getState();
        IModelState state = sim.saveInternalState();
        File mainModelFile = new File(testRunDataDir,"algorithm_restart_tempdir_185811180000/mainmodel_185811180000.zip");
        assertTrue(mainModelFile.exists());
        File stateFile = new File(testRunDataDir,"sim_restart.zip");
        state.savePersistentState(stateFile);
        sim.releaseInternalState(state);
        assertTrue(stateFile.exists());
        sim.run();
        IVector x_orig_final = sim.getState();
        //
        // create new algorithm for restoring state
        //
		IStochModelFactory factory2 = new OscillatorStochModelFactory();
        factory2.initialize(null, new String[]{""});
		IStochObserver obsGenerated2 = new CsvStochObserver();
        obsGenerated2.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});
		//create and run filter
        IAlgorithm sim2 = new SequentialSimulation();
        sim2.initialize(testRunDataDir, new String[]{""});
        sim2.setStochComponents(obsGenerated,factory);
        sim2.prepare();
        //
        // restore state
        //
        IModelState state2 = sim2.loadPersistentState(stateFile);
        sim2.restoreInternalState(state2);
        IVector x_restart = sim2.getState();
        sim2.run();
        IVector x_restart_final = sim2.getState();
		// state at final time
        double eps=0.0001;
        System.out.println("State at restart time. t=???");
		System.out.println("x_orig = "+x_orig);
		System.out.println("x_restart = "+x_restart);
		assertEquals("x[0]",x_orig.getValue(0),x_restart.getValue(0),eps);
        System.out.println("State at final time. t=???");
		System.out.println("x_orig_final = "+x_orig_final);
		System.out.println("x_restart_final = "+x_restart_final);
		assertEquals("x[0]",x_orig_final.getValue(0),x_restart_final.getValue(0),eps);
		assertEquals("x[1]",x_orig_final.getValue(1),x_restart_final.getValue(1),eps);
	}

	public void testEnkfRestartOscillator() {
        generateObservations_oscillator();
		System.out.println("========================================================");
		System.out.println(" Test Enkf (oscillator)");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory factory = new OscillatorStochModelFactory();
        factory.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});
		//create and run filter
		StochVector.setSeed(103344);
        IAlgorithm enkf = new EnKF();
        enkf.initialize(testRunDataDir, new String[]{"<algorithm><ensembleSize>5</ensembleSize></algorithm>"});
        enkf.setStochComponents(obsGenerated,factory);
        enkf.prepare();
        enkf.next();enkf.next();
        //
        // save state
        //
        IVector x_orig = enkf.getState();
        IModelState state = enkf.saveInternalState();
        File mainModelFile = new File(testRunDataDir,"algorithm_restart_tempdir_185811180000/mainmodel_185811180000.zip");
        assertTrue(mainModelFile.exists());
        File stateFile = new File(testRunDataDir,"enkf_restart.zip");
        state.savePersistentState(stateFile);
        enkf.releaseInternalState(state);
        assertTrue(stateFile.exists());
        enkf.run();
        IVector x_orig_final = enkf.getState();
        //
        // create new algorithm for restoring state
        //
		IStochModelFactory factory2 = new OscillatorStochModelFactory();
        factory2.initialize(null, new String[]{""});
		IStochObserver obsGenerated2 = new CsvStochObserver();
        obsGenerated2.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});
		//create and run filter
		StochVector.setSeed(103344);
        IAlgorithm enkf2 = new EnKF();
        enkf2.initialize(testRunDataDir, new String[]{"<algorithm><ensembleSize>5</ensembleSize></algorithm>"});
        enkf2.setStochComponents(obsGenerated,factory);
        enkf2.prepare();
        enkf2.next();enkf2.next();
        //
        // restore state
        //
        IModelState state2 = enkf2.loadPersistentState(stateFile);
        enkf2.restoreInternalState(state2);
        IVector x_restart = enkf2.getState();
        enkf2.run();
        IVector x_restart_final = enkf2.getState();
		// state at final time
        double eps=0.0001;
        System.out.println("State at restart time. t=???");
		System.out.println("x_orig = "+x_orig);
		System.out.println("x_restart = "+x_restart);
		assertEquals("x[0]",x_orig.getValue(0),x_restart.getValue(0),eps);
        System.out.println("State at final time. t=???");
		System.out.println("x_orig_final = "+x_orig_final);
		System.out.println("x_restart_final = "+x_restart_final);
		//assertEquals("x[0]",x_orig_final.getValue(0),x_restart_final.getValue(0),eps); 
		//("x[1]",x_orig_final.getValue(1),x_restart_final.getValue(1),eps);
		//MVL TODO FIX THIS QUICKLY
	}

	
	private void generateObservations_oscillator() {
		System.out.println("========================================================");
		System.out.println("Generate observations, evaluate cost at 1st guess and opt.");
		System.out.println("Noise is added for this twinexperiment");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact = new OscillatorStochModelFactory();
        fact.initialize(null, new String[]{""});
		IStochModelInstance model = fact.getInstance(IStochModelFactory.OutputLevel.Suppress);
		model.setAutomaticNoiseGeneration(true);
		// Dummy observations
		IVector time   = Vector.range(0.0, 10.0, 1.0);
		System.out.println("obstimes = "+time);
		System.out.println("Should be obstimes = [0.0,1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]");
		int n         = time.getSize();
		IVector values = new Vector(n);
		IVector stdVal = new Vector(n); stdVal.setConstant(0.1);
		IVector indVal = new Vector(n); indVal.setConstant(0.0);
		IVector[] columns= new IVector[4];
		columns[0] = time;
		columns[1] = indVal;
		columns[2] = values;
		columns[3] = stdVal;
		String[] keys = {"time","index","value","std"};
		IStochObserver obs = new CsvStochObserver(columns,keys);
		// Now run model to generate data for these obs
		IObservationDescriptions descr = obs.getObservationDescriptions();
		model.announceObservedValues(descr);
		model.compute(model.getTimeHorizon().getEndTime());
		IVector prd = model.getObservationOperator().getObservedValues(descr);
		// create new observer with these generated values
		columns[2] = prd;
		CsvStochObserver obsGenerated = new CsvStochObserver(columns,keys);

		//write generated observations to file
		obsGenerated.toFile(testRunDataDir, "observations_oscillator_generated.csv");
		model.finish();
	    }

}
