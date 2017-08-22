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
import org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelFactory;
import org.openda.observers.GroupStochObserver;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.ConfigTree;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.Vector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

public class CalibrationTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
       	testData = new OpenDaTestSupport(CalibrationTest.class,"algorithms");
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
		Vector p0 = new Vector(mod1a.getParameters());
		System.out.println("p0 = "+p0);
		System.out.println("Should be p0 = [8.5,1.7]");
		assertEquals("p0",p0.toString(),"[8.5,1.7]");
		double Jp0 = J.evaluate(p0,"initialization");
		System.out.println("Initial cost = "+Jp0);
		System.out.println("Should be Initial cost = 0.0");
		assertEquals("J(p0) = ",Jp0,0.0);
		mod1a.finish();
	}

	public void testSimulation() {
		System.out.println("========================================================");
		System.out.println("Simulation of oscillatormodel");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		// Now start calibration through proper algorithm-class
		Simulation algorithm = new Simulation();
		algorithm.initialize(null, new String[]{""});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.prepare();
		algorithm.run();
		Vector par=new Vector(algorithm.getBestEstimate().getParameters());
		System.out.println("parameters = "+par);
		System.out.println("Should be parameters = [8.0,1.5707963267948966]");
		assertEquals("par",par.toString(),"[8.0,1.5707963267948966]");
	}

	public void testSimulationWithConfig() {
		System.out.println("========================================================");
		System.out.println("Simulation of oscillatormodel");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		// Now start calibration through proper algorithm-class
		// <SimulationConfig>
		//     <instance repeat="1" stochParameter="true" stochForcing="true" />
		//     <writeObservations filename="somefile.csv" addNoise="true" format="csv" />
		// </SimulationConfig>
		String simulationConfig="<SimulationConfig><instance repeat=\"2\" stochParameter=\"false\" stochForcing=\"true\" /><writeObservations filename=\"somefile.csv\" addNoise=\"true\" format=\"csv\" /></SimulationConfig>";
		Simulation algorithm = new Simulation();
		algorithm.initialize(testRunDataDir,new String[]{simulationConfig});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.prepare();
		algorithm.run();
		Vector par= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("parameters = "+par);
		System.out.println("Should be parameters = [8.0,1.5707963267948966]");
		assertEquals("par",par.toString(),"[8.0,1.5707963267948966]");
	}

	public void testSimplexCalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization with simplex method and oscillatormodel");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		Simplex algorithm = new Simplex();
		algorithm.initialize(null, new String[]{""});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.initStep=0.5;
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.421096801757812,1.6997346025516316]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.421096801757812,1.6997346025516316]");
	}

	public void testSimplexCalibrationWithConfig() {
		System.out.println("========================================================");
		System.out.println("Optimization with simplex method, oscillator and CONFIG");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// <SimplexConfig>
		//     <costFunction weakParameterConstraint="false" class="org.openda.algorithms.SimulationKwadraticCostFunction" />
		//     <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 />
		// </SimplexConfig>
		String simplexConfig="<SimplexConfig><costFunction weakParameterConstraint=\"true\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /></SimplexConfig>";
		Simplex algorithm = new Simplex();
		algorithm.initialize(testRunDataDir,new String[]{simplexConfig});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.119140625,1.694005663677871]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.119140625,1.694005663677871]");
	}

	public void testSCECalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization with SCE method and oscillatormodel");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		SCE algorithm = new SCE();
		algorithm.initialize(testRunDataDir, new String[]{"sceAlgorithm.xml"});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.initStep=0.5;
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal+", while pTrue = "+pTrue);
		assertEquals("pCal[0]",pCal.getValue(0),pTrue.getValue(0),1e-1);
		assertEquals("pCal[1]",pCal.getValue(1),pTrue.getValue(1),1e-1);
	}

	public void testPowellCalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization with Powell and oscillator model");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact3 = new OscillatorStochModelFactory();
		fact3.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		Powell algorithm = new Powell();
		algorithm.initialize(null, new String[]{""});
		algorithm.setStochComponents(obsGenerated, fact3);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.499697935789392,1.699991425861068]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.499697935789392,1.699991425861068]");
	}

	public void testPowellCalibrationWithConfig() {
		System.out.println("========================================================");
		System.out.println("Optimization with Powell and oscillator model WITH CONFIG");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact3 = new OscillatorStochModelFactory();
		fact3.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		String configString = "<PowellConfig><costFunction weakParameterConstraint=\"true\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><lineSearch type=\"brent\" maxIterations=\"20\" relTolerance=\"0.01\" maxRelStepSize=\"100.0\" ><brent startBracketValue=\"1.0\" /></lineSearch></PowellConfig>";
		Powell algorithm = new Powell();
		algorithm.initialize(testRunDataDir,new String[]{configString});
		algorithm.setStochComponents(obsGenerated, fact3);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.159255184931276,1.6971949325486975]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.159255184931276,1.6971949325486975]");
	}

	/**
	* configuration string for DuD method
	*/
	private String dudConfigString(boolean parallel, boolean withWeakConstraint, 
				boolean makeMatlabOut, boolean sparse) {
		String dudConfig;
		dudConfig = 	 "<DudConfig>"
				+"	<costFunction class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" ";
		String absTol;
		if (withWeakConstraint) {
			dudConfig += "weakParameterConstraint=\"true\" factor=\"0.5\" />";
			absTol = "0.005";
		} else {
			dudConfig += "weakParameterConstraint=\"off\" />";
			absTol = "0.01";
		}
		dudConfig +=	 "	<outerLoop maxIterations=\"10\" absTolerance=\"" + absTol + "\" relTolerance=\"0.01\" />"
				+"	<lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" >"
				+"		<backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" />"
				+"	</lineSearch>";
		if (makeMatlabOut) 	dudConfig += "	<matlabOutputFile>dud.m</matlabOutputFile>";
		if (parallel) 		dudConfig += "	<initialize tryParallel=\"true\" />";
		if (sparse)		dudConfig += 
				 "	<dependencies>"
				+"		<obs id=\"global.position\"> <depends_on> <par id=\"t_damp\"/> </depends_on> </obs>"
				+"		<obs id=\"global.position\"> <depends_on> <par id=\"omega\"/> </depends_on> </obs>"
				+"	</dependencies>";
		dudConfig += 	 "</DudConfig>";
		return dudConfig;
	}

	/**
	* Test for DUD : configuration from a string or a file
	*/
	private void testDudCalibration(boolean parallel, boolean withWeakConstraint, 
			boolean useFile, boolean sparse, boolean startNear, boolean noosObservations) {
		System.out.println("========================================================");
		System.out.println("Optimization with DUD and oscillator model");
		if (sparse) System.out.println("with sparse gradient");
		if (parallel) System.out.println("with parallel initialization");
		if (withWeakConstraint) System.out.println("with weak parameter constraint");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory fact1 = new OscillatorStochModelFactory();
		String startNearString;
		if (startNear) {
			startNearString = 
			 "<oscillatorConfig>"
		        +"	<parameters names=\"t_damp,omega\">[8.0,1.63]</parameters>"
		        +"</oscillatorConfig>";
		} else {
			startNearString = "";
		}
		fact1.initialize(null, new String[]{startNearString});

		IStochObserver obsGenerated;
		if (noosObservations) {
			obsGenerated = new NoosTimeSeriesStochObserver();
			obsGenerated.initialize(testRunDataDir, new String[]{
				"noosOscillatorWithNoFilter.xml"}); 
		} else {
			obsGenerated = new CsvStochObserver();
			obsGenerated.initialize(testRunDataDir, new String[]{
				"observations_oscillator_generated.csv"});
		}

		IVector pTrue = new Vector("[8.5,1.7]"); // iterations start at [8.0,1.5707963267948966]

		// Now start calibration through proper algorithm-class
		boolean makeMatlabOut=true;
		String dudConfig =  dudConfigString(parallel, withWeakConstraint, !makeMatlabOut, sparse);

		BaseDud algorithm;
		if (sparse) {
			algorithm = new SparseDud();
		} else {
			algorithm = new Dud();
		}
		if (useFile) {
			// initialize dud with configuration-file
			ConfigTree conf= new ConfigTree(dudConfig);
			String fileName = "dudConfigTest.xml";
			conf.toFile(testRunDataDir, fileName); //write this configuration to a file
			algorithm.initialize(testRunDataDir,new String[]{fileName});
		} else {
			// initialize dud with configuration-string
			algorithm.initialize(testRunDataDir,new String[]{dudConfig});
		}

		algorithm.setStochComponents(obsGenerated,fact1);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		String pCalOK = "Undefined";
		if (sparse && startNear) {
			if (withWeakConstraint) {
				pCalOK = "[8.168992076862322,1.6970972805886373]";
			} else {
				pCalOK = "[8.463734525046668,1.703690592973862]";
			}
		} else if (!sparse && !startNear) {
			if (withWeakConstraint) {
				pCalOK = "[8.08562425961299,1.6995677068771347]"; 
			} else {
				pCalOK = "[8.519506120120756,1.6995787072937993]"; 
			}
		}
		System.out.println("Should be pCal = " + pCalOK);
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),pCalOK);
		algorithm.finish();
	}

	/**
	* Test for DUD : configuration from a string containing xml
	*/
	public void testSimultaneousCalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization of two oscillator models / simultaneous calibration");
		System.out.println("========================================================");

		//groupModel
		String modIds[] = {"oscillator1","oscillator2"};
		IStochModelFactory models[] = new IStochModelFactory[2];
		IStochModelFactory fact1 = new OscillatorStochModelFactory();
		fact1.initialize(null, new String[]{""});
		models[0]=fact1;
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		models[1]=fact2;
		IStochModelFactory fact=new SimultaneousGroupStochModelFactory(models, modIds);

		//groupObserver
		ArrayList<String> obsIds = new ArrayList<String>();
		obsIds.add("oscillator1");
		obsIds.add("oscillator2");
		ArrayList<IStochObserver> obsParts = new ArrayList<IStochObserver>();
		IStochObserver obs1 = new CsvStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		obsParts.add(obs1);
		IStochObserver obs2 = new CsvStochObserver();
		obs2.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		obsParts.add(obs2);
		IStochObserver obs = new GroupStochObserver(obsParts,obsIds);
		
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		
		// Now start calibration through proper algorithm-class
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean makeMatlabOut= true;
		boolean sparse = true;
		String dudConfig = dudConfigString(!parallel, !withWeakConstraint, makeMatlabOut, !sparse);
		Dud algorithm = new Dud();
		algorithm.initialize(testRunDataDir,new String[]{dudConfig});
		algorithm.setStochComponents(obs,fact);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.519506120120624,1.6995787072937982]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.519506120120624,1.6995787072937982]");
		algorithm.finish();
	}



	/**
	* Test for DUD : configuration from a string containing xml
	*/
	public void testDudCalibration() {
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean useFile = true;
		boolean sparse = true;
		boolean startNear = true;
		boolean noosObservations = true;
		testDudCalibration(!parallel, !withWeakConstraint, !useFile, !sparse, !startNear, !noosObservations);
	} 
	
	/**
	* Test for DUD : configuration from a string containing xml 
	*/
	public void testDudCalibrationParallel() {
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean useFile = true;
		boolean sparse = true;
		boolean startNear = true;
		boolean noosObservations = true;
		testDudCalibration(parallel, !withWeakConstraint, !useFile, !sparse, !startNear, noosObservations);
	}
	
	
	/**
	* Test for DUD : configuration is read from a file
	*/
	public void testDudCalibrationWithWeakConstraint() {
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean useFile = true;
		boolean sparse = true;
		boolean startNear = true;
		boolean noosObservations = true;
		testDudCalibration(parallel, withWeakConstraint, !useFile, !sparse, !startNear, !noosObservations);
	}
	
	/**
	* Test for sparse DUD : configuration from a string containing xml
	*/
	public void testSparseDudCalibration() {
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean useFile = true;
		boolean sparse = true;
		boolean startNear = true;
		boolean noosObservations = true;
		testDudCalibration(!parallel, !withWeakConstraint, !useFile, sparse, startNear, noosObservations);
	}
	/**
	* Test for sparse DUD : configuration from a string containing xml
	*/
	public void testSparseDudCalibrationParallel() {
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean useFile = true;
		boolean sparse = true;
		boolean startNear = true;
		boolean noosObservations = true;
		testDudCalibration(parallel, !withWeakConstraint, useFile, sparse, startNear, noosObservations);
	}
	/**
	* Test for sparse DUD : configuration from a string containing xml
	*/
	public void testSparseDudCalibrationWithWeakConstraint() {
		boolean parallel = true;
		boolean withWeakConstraint = true;
		boolean useFile = true;
		boolean sparse = true;
		boolean startNear = true;
		boolean noosObservations = true;
		testDudCalibration(!parallel, withWeakConstraint, !useFile, sparse, startNear, noosObservations);
	}


	public void testGriddedFullSearchCalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization with GriddedFullSearch method and oscillatormodel");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		GriddedFullSearch algorithm = new GriddedFullSearch();
		algorithm.initialize(null, new String[]{""});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.6,1.6964600329384885]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.6,1.6964600329384885]");
		algorithm.finish();
	}

	public void testGriddedFullSearchCalibrationWithConfig() {
		System.out.println("========================================================");
		System.out.println("Optimization with GriddedFullSearch method, oscillator and CONFIG");
		System.out.println("========================================================");
		//set model and observations
		IStochModelFactory fact2 = new OscillatorStochModelFactory();
		fact2.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// <GriddedFullSearchConfig>
		//     <costFunction weakParameterConstraint="false" class="org.openda.algorithms.SimulationKwadraticCostFunction" />
		//     <gridRange min="[0.04,2.4]" max="[0.08,3.3]" step="[0.005,0.1]" />
		// </GriddedFullSearchConfig>
		
		String GriddedFullSearchConfig="<GriddedFullSearchConfig><costFunction weakParameterConstraint=\"false\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><gridRange min=\"[8.0,1.0]\" max=\"[9.0,2.0]\" step=\"[0.1,0.1]\" /></GriddedFullSearchConfig>";
		GriddedFullSearch algorithm = new GriddedFullSearch();
		algorithm.initialize(testRunDataDir,new String[]{GriddedFullSearchConfig});
		algorithm.setStochComponents(obsGenerated, fact2);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be pCal = [8.5,1.7000000000000002]");
		System.out.println("This is close to pTrue = "+pTrue);
		assertEquals("pCal",pCal.toString(),"[8.5,1.7000000000000002]");
		algorithm.finish();
	}




	public void testConjugateGradientCalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization with Conjugate Gradient and oscillator model");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory  fact3 = new OscillatorStochModelFactory();
		fact3.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		ConjugateGradient algorithm = new ConjugateGradient();
		algorithm.withPreconditioning = true;
		algorithm.initialize(null, new String[]{""});
		algorithm.setStochComponents(obsGenerated, fact3);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be close to "+pTrue);
		assertEquals("First variable ",pTrue.getValue(0),pCal.getValue(0),0.02);
		assertEquals("Second variable ",pTrue.getValue(1),pCal.getValue(1),0.01);
	}

	public void testBFGSCalibration() {
		System.out.println("========================================================");
		System.out.println("Optimization with BFGS and oscillator model");
		System.out.println("========================================================");
		//generate observations
		IStochModelFactory  fact3 = new OscillatorStochModelFactory();
		fact3.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"observations_oscillator_generated.csv"});
		IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
		// Now start calibration through proper algorithm-class
		BFGS algorithm = new BFGS();
		algorithm.withPreconditioning = true;
		algorithm.initialize(null, new String[]{""});
		algorithm.setStochComponents(obsGenerated, fact3);
		algorithm.prepare();
		algorithm.run();
		Vector pCal= new Vector(algorithm.getBestEstimate().getParameters().getValues());
		System.out.println("pCal = "+pCal);
		System.out.println("Should be close to "+pTrue);
		assertEquals("First variable ",pTrue.getValue(0),pCal.getValue(0),0.01);
		assertEquals("Second variable ",pTrue.getValue(1),pCal.getValue(1),0.01);
	}

	//TODO restart on stored iteration
	//public SimplexCoreOptimizer(SimpleCostFunction f, Vector pCurrent[]){
	//public SimplexCoreOptimizer(SimpleCostFunction f, Vector pCurrent[], double fCurrent[]){
	//public void setCurrentValues(Vector[] pars){
	//public void setCurrentValues(Vector pars[], double values[]){
}
