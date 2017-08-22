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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

public class CostTest extends TestCase {

	private File testRunDataDir;

	OpenDaTestSupport testData = new OpenDaTestSupport(CostTest.class, "algorithms");

	protected void setUp() throws IOException {
		testRunDataDir = testData.getTestRunDataDir();
		generateObservations();
	}

	private void generateObservations() {
		System.out.println("========================================================");
		System.out.println("Generate observations, evaluate cost at 1st guess and opt.");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact1 = new OscillatorStochModelFactory();
		fact1.initialize(null, new String[]{""});
		IStochModelInstance mod1 = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		mod1.setParameters(pTrue);
		// Dummy observations
		IVector time   = Vector.range(0.0, 10.0, 1.0);
		System.out.println("obstimes = "+time);
		System.out.println("Should be obstimes = [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]");
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
		IStochObserver obs1 = new CsvStochObserver(columns,keys);
		// Now run mod1 to generate data for these obs
		IObservationDescriptions descr1 = obs1.getObservationDescriptions();
		mod1.announceObservedValues(descr1);
		mod1.compute(mod1.getTimeHorizon().getEndTime());
		IVector prd = mod1.getObservedValues(descr1);
		// create new observer with these generated values
		columns[2] = prd;
		CsvStochObserver obsGenerated = new CsvStochObserver(columns,keys);
		//write generated observations to file
		obsGenerated.toFile(testRunDataDir, "observations_oscillator_generated.csv");
		mod1.finish();
	}

	public void testCostFunction() {
		System.out.println("========================================================");
		System.out.println("Read observations, evaluate cost for optimal parameters.");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact1a = new OscillatorStochModelFactory();
		fact1a.initialize(null, new String[]{""});
		IStochModelInstance mod1a = fact1a.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		mod1a.setParameters(pTrue);
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});

		// evaluate cost in optimum ; should be 0 (there is no noise)
		ICostFunction J = new SimulationKwadraticCostFunction(fact1a,obsGenerated);
		IVector p0 = new Vector(mod1a.getParameters());
		System.out.println("p0 = "+p0);
		System.out.println("Should be p0 = [8.5,1.7]");
		assertEquals("p0",p0.toString(),"[8.5,1.7]");
		double Jp0 = J.evaluate(p0,"initialization");
		System.out.println("Initial cost = "+Jp0);
		System.out.println("Should be Initial cost = 0.0");
		assertEquals("J(p0) = ",Jp0,0.0);
		mod1a.finish();
	}

	public void testClonedCostFunction() {
		System.out.println("========================================================");
		System.out.println("SimulationKwadraticCostFunction with cloning");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact1a = new OscillatorStochModelFactory();
		fact1a.initialize(null, new String[]{""});
		IStochModelInstance mod1a = fact1a.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});

		// evaluate cost in optimum ; should be 0 (there is no noise)
		ICostFunction J1 = new SimulationKwadraticCostFunction(fact1a,obsGenerated);
		ICostFunction J2 = J1.clone();

		IVector p1Init = mod1a.getParameters();
		IVector p1True = pTrue.clone();
		IVector p2Init = mod1a.getParameters();
		IVector p2True = pTrue.clone();
		double J1Init = J1.evaluate(p1Init,"initialization");
		double J2Init = J2.evaluate(p2Init,"initialization");
		double J1True = J1.evaluate(p1True,"initialization");
		double J2True = J2.evaluate(p2True,"initialization");
		System.out.println("J1(p1Init) = "+J1Init);
		System.out.println("Should be J1(p1Init) = 16.674326043666902");
		assertEquals("J1(p1Init) = ",16.674326043666902,J1Init);
		System.out.println("J1(p1True) = "+J1True);
		System.out.println("Should be J1(p1True) = 0.0");
		assertEquals("J1(p1True) = ",0.0,J1True);

		System.out.println("J2(p2Init) = "+J2Init);
		System.out.println("Should be J2(p2Init) = 16.674326043666902");
		assertEquals("J2(p2Init) = ",16.674326043666902,J2Init);
		System.out.println("J2(p2True) = "+J2True);
		System.out.println("Should be J2(p2True) = 0.0");
		assertEquals("J2(p2True) = ",0.0,J2True);
		mod1a.finish();
	}

	public void testParallelClonedCostFunction() {
		System.out.println("========================================================");
		System.out.println("SimulationKwadraticCostFunction with cloning and prepare.");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact1a = new OscillatorStochModelFactory();
		fact1a.initialize(null, new String[]{""});
		IStochModelInstance mod1a = fact1a.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});

		// evaluate cost in optimum ; should be 0 (there is no noise)
		SimulationKwadraticCostFunction J1 = new SimulationKwadraticCostFunction(fact1a,obsGenerated);
		SimulationKwadraticCostFunction J2 = (SimulationKwadraticCostFunction) J1.clone();
		J1.setTryParallel(true);
		J2.setTryParallel(true);

		IVector p1Init = mod1a.getParameters();
		IVector p1True = pTrue.clone();
		IVector p2Init = mod1a.getParameters();
		IVector p2True = pTrue.clone();
		J1.prepare(p1Init);
		J2.prepare(p2Init);
		double J1Init = J1.evaluate(p1Init,"initialization");
		double J2Init = J2.evaluate(p2Init,"initialization");
		J1.prepare(p1True);
		J2.prepare(p2True);
		double J1True = J1.evaluate(p1True,"initialization");
		double J2True = J2.evaluate(p2True,"initialization");
		System.out.println("J1(p1Init) = "+J1Init);
		System.out.println("Should be J1(p1Init) = 16.674326043666902");
		assertEquals("J1(p1Init) = ",16.674326043666902,J1Init);
		System.out.println("J1(p1True) = "+J1True);
		System.out.println("Should be J1(p1True) = 0.0");
		assertEquals("J1(p1True) = ",0.0,J1True);

		System.out.println("J2(p2Init) = "+J2Init);
		System.out.println("Should be J2(p2Init) = 16.674326043666902");
		assertEquals("J2(p2Init) = ",16.674326043666902,J2Init);
		System.out.println("J2(p2True) = "+J2True);
		System.out.println("Should be J2(p2True) = 0.0");
		assertEquals("J2(p2True) = ",0.0,J2True);
		mod1a.finish();
	}

	public void testSimpleCost() {
		System.out.println("========================================================");
		System.out.println("Basic tests for simpleCostFunction");
		System.out.println("========================================================");
		double delta=1e-6;
		//generate observations
		ICostFunctionWithGradient cost = new SimpleCostFunction();
		// default parameters for cost
		//double means[] = {1.0,1.0};
		//double width[] = {1.0,2.0};
		double value1 = cost.evaluate(new Vector("[1.0,1.0]"),"initialization");
		assertEquals(0.0, value1, delta);
		double value2 = cost.evaluate(new Vector("[2.0,2.0]"),"initialization");
		assertEquals(1.25, value2, delta);
		
		IVector grad1 = cost.evaluateGradient(new Vector("[1.0,1.0]"));
		assertEquals("[0.0,0.0]", grad1.toString());
		
		cost.prepare(new Vector("[2.0,2.0]"));
		IVector grad2 = cost.evaluateGradient(new Vector("[2.0,2.0]"));
		assertEquals("[2.0,0.5]", grad2.toString());
	}

}
