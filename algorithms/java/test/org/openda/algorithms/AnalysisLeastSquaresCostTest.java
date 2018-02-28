/* OpenDA v2.4.3 
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

import org.openda.algorithms.kalmanFilter.AnalysisLeastSquaresCost;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.*;

public class AnalysisLeastSquaresCostTest extends TestCase {

    private static OpenDaTestSupport testData= new OpenDaTestSupport(AnalysisLeastSquaresCostTest.class,"algorithms");
    private static File testRunDataDir=testData.getTestRunDataDir();

 

	public void testCreateCost() {
		generateObservations(); //first generate obs for this twin experiment
		
		System.out.println("========================================================");
		System.out.println("Generate cost function and do elementary tests");
		System.out.println("========================================================");
		IStochModelFactory factory = new OscillatorStochModelFactory();
		String args[] = new String[]{"<oscillatorConfig><simulationTimespan>[0.0,0.05,10.0]</simulationTimespan><parameters names=\"t_damp,omega\">[8.0,1.5708]</parameters><parameterUncertainty names=\"t_damp,omega\">[1.0,0.1257]</parameterUncertainty><systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise><initialState>[0.0,0.0]</initialState><initialStateUncertainty>[0.8,0.8]</initialStateUncertainty></oscillatorConfig>"};
        factory.initialize(null, args);
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir,new String[]{"observations_oscillator_generated.csv"});

		int n = 3; //ensemble size
		IVector states[] = new IVector[n];
		states[0] = new Vector("[0.0,0.0]");
		states[1] = new Vector("[1.0,0.0]");
		states[2] = new Vector("[0.0,1.0]");

		IStochModelInstance model = factory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IModelState savedState = model.saveInternalState();

		ITime obsTimes[] = obsGenerated.getTimes();
		IStochObserver obs = obsGenerated.createSelection(obsTimes[0]);

		AnalysisLeastSquaresCost f = new AnalysisLeastSquaresCost(model, states,
				obs, obs.getExpectations(), savedState);

		IVector p0 = new Vector("[0.0,0.0,0.0]");
		double f0 = f.evaluate(p0,"initialization");
		System.out.println("p0="+p0);
		System.out.println("f(p0) = "+f0);
		System.out.println("Should be close to f(p0) = 32.0");
		assertEquals("f(p0)",32.0,f0,1e-6);

		// matches obs, so only background cost (much lower cost)
		IVector p1 = new Vector("[-1.0,1.0,0.0]");
		p1.scale(Math.sqrt(2.0));
		double f1 = f.evaluate(p1,"initialization");
		System.out.println("p1="+p1);
		System.out.println("f(p1) = "+f1);
		System.out.println("Should be close to f(p1) = 4.0");
		assertEquals("f(p1)",4.0,f1,1e-6);

		model.finish();
	}
	
	public void generateObservations() {
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

		//get true state at final time for reference
		IVector xTrue = model.getState();
		System.out.println("xTrue = "+xTrue);
		model.finish();
	}


}
