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

import org.openda.algorithms.kalmanFilter.SequentialEnsembleSimulation;
import org.openda.algorithms.kalmanFilter.SequentialSimulation;
import org.openda.interfaces.*;
import org.openda.models.lorenz.LorenzStochModelFactory;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class SimulationTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SimulationTest.class,"algorithms");
        testRunDataDir = testData.getTestRunDataDir();
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
		IVector prd = model.getObservedValues(descr);
		// create new observer with these generated values
		columns[2] = prd;
		CsvStochObserver obsGenerated = new CsvStochObserver(columns,keys);

		//write generated observations to file
		obsGenerated.toFile(testRunDataDir, "observations_oscillator_generated.csv");
		model.finish();
	    }

	private void generateObservations_lorenz() {
		System.out.println("========================================================");
		System.out.println("Generate observations, evaluate cost at 1st guess and opt.");
		System.out.println("Noise is added for this twinexperiment");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact = new LorenzStochModelFactory();
        fact.initialize(null, new String[]{""});
		IStochModelInstance model = fact.getInstance(IStochModelFactory.OutputLevel.Suppress);
		model.setAutomaticNoiseGeneration(true);
		// Dummy observations
		IVector time   = Vector.range(0.0, 10.0, 0.5);
		System.out.println("obstimes = "+time);
		System.out.println("Should be obstimes = ");
		int n         = time.getSize();
		IVector values = new Vector(n);
		IVector stdVal = new Vector(n); stdVal.setConstant(1.0);
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
		IVector prd = model.getObservedValues(descr);
		// create new observer with these generated values
		columns[2] = prd;
		CsvStochObserver obsGenerated = new CsvStochObserver(columns,keys);

		//write generated observations to file
		obsGenerated.toFile(testRunDataDir, "observations_lorenz_generated.csv");
		model.finish();
	    }

	public void testSimulation_Oscillator() {
        generateObservations_oscillator();
		System.out.println("========================================================");
		System.out.println(" Test Simulation (oscillator, a single run)");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory factory = new OscillatorStochModelFactory();
        factory.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});
		//create and run filter
        IAlgorithm sim = new Simulation();
        sim.setStochComponents(obsGenerated,factory);
        sim.prepare();
        sim.run();
		// state at final time
        double eps=0.0001;
        IVector x = sim.getState();
		System.out.println("x = "+x);
		System.out.println("Should be x = [-0.2280086306647443,-0.017986342843738214]");
		assertEquals("x[0]",x.getValue(0),-0.2280086306647443,eps);

	}

	public void testSimulation_Lorenz() {
        generateObservations_lorenz();
		System.out.println("========================================================");
		System.out.println(" Test Simulation (lorenz, multiple runs)");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory factory = new LorenzStochModelFactory();
        factory.initialize(null, new String[]{""});
        IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir,new String[]{"observations_lorenz_generated.csv"});
		//create and run filter
        IAlgorithm sim = new Simulation();
        sim.setStochComponents(obsGenerated,factory);
        sim.prepare();
        sim.run();
		// state at final time
        double eps=0.0001;
        IVector x = sim.getState();
		System.out.println("x = "+x);
		System.out.println("Should be x = [2.5874767500762665,-1.8536755172824717,27.46377573890796]");
		assertEquals("x[0]",x.getValue(0),2.5874767500762665,eps);

	}

	public void testSequentialSimulation_Oscillator() {
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
        sim.setStochComponents(obsGenerated,factory);
        sim.prepare();
        sim.run();
		// state at final time
        double eps=0.0001;
        IVector x = sim.getState();
		System.out.println("x = "+x);
		System.out.println("Should be x = [-0.2280086306647443,-0.017986342843738214]");
		assertEquals("x[0]",x.getValue(0),-0.2280086306647443,eps);
	}

	public void testSequentialSimulation_Lorenz() {
        generateObservations_lorenz();
		System.out.println("========================================================");
		System.out.println(" Test SequentialSimulation (lorenz, multiple runs)");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory factory = new LorenzStochModelFactory();
        factory.initialize(null, new String[]{""});
        IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir,new String[]{"observations_lorenz_generated.csv"});
		//create and run filter
        IAlgorithm sim = new SequentialSimulation();
        sim.setStochComponents(obsGenerated,factory);
        sim.prepare();
        sim.run();
		// state at final time
        double eps=0.0001;
        IVector x = sim.getState();
		System.out.println("x = "+x);
		System.out.println("Should be x = [2.5874767500762665,-1.8536755172824717,27.46377573890796]");
		assertEquals("x[0]",x.getValue(0),2.5874767500762665,eps);
	}

	public void testSequentialEnsembleSimulation_Oscillator() {
        generateObservations_oscillator();
		StochVector.setSeed(103344);
		System.out.println("========================================================");
		System.out.println(" Test SequentialEnsembleSimulation (oscillator, a single run)");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory factory = new OscillatorStochModelFactory();
        factory.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});
		//create and run filter
        IAlgorithm sim = new SequentialEnsembleSimulation();
        sim.setStochComponents(obsGenerated,factory);
        sim.prepare();
        sim.run();
		// state at final time
        double eps=0.0001;
        IVector x = sim.getState();
		System.out.println("x = "+x);
		System.out.println("Should be x = [-0.15224461736321235,-0.10789768525973542]");
		assertEquals("x[0]",-0.15224461736321235,x.getValue(0),eps);
	}

}
