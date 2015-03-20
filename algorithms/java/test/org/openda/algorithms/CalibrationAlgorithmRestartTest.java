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
import org.openda.interfaces.IModelState;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.utils.CsvStochObserver;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Restart tests for various algorithms
 */
public class CalibrationAlgorithmRestartTest extends TestCase {

	private IVector pCalSparseDudExpected = new Vector("[8.463734525046668,1.703690592973862]");
	private IVector pCalDudExpected = new Vector("[8.519506120120756,1.6995787072937993]");
	private IVector pCalPowellExpected = new Vector("[8.499697935789392,1.699991425861068]");
	private IVector pCalSimplexExpected = new Vector("[8.369140625,1.701859645311845]");
	
	int restartFromIterationNumber = 2;
	
	private File testRunDataDir;
	File savedSparseDudStateFile;
	File savedDudStateFile;
	File savedPowellStateFile;
	File savedSimplexStateFile;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(CalibrationAlgorithmRestartTest.class, "algorithms");
		testRunDataDir = testData.getTestRunDataDir();
		savedSparseDudStateFile = new File(testRunDataDir, "savedSparseDudStateFile.xml");
		savedDudStateFile = new File(testRunDataDir, "savedDudStateFile.xml");
		savedPowellStateFile = new File(testRunDataDir, "savedPowellStateFile.xml");
		savedSimplexStateFile = new File(testRunDataDir, "savedSimplexStateFile.xml");
	}

/*******************************************************************************************************/
	public void testSparseDudOscillator() {
		
		System.out.println("Starting continuous run");
		performSparseDudContinuousRun();
		System.out.println("Continuous run done");
		performSparseDudRestartRunFirstPart();
		System.out.println("First part of run until restart done");
		performSparseDudRestartRunSecondPart();
		System.out.println("Second part starting from restart done");
		
	}

	public void testDudOscillator() {
		
		System.out.println("Starting continuous run");
		performDudContinuousRun();
		System.out.println("Continuous run done");
		performDudRestartRunFirstPart();
		System.out.println("First part of run until restart done");
		performDudRestartRunSecondPart();
		System.out.println("Second part starting from restart done");
		
	}
		
		
	public void testPowellOscillator() {
		
		performPowellContinuousRun();
		performPowellRestartRunFirstPart();
		performPowellRestartRunSecondPart();
		
	}
		
	public void testSimplexOscillator() {
		
		System.out.println("One continuous run");
		performSimplexContinuousRun();
		System.out.println("First two iterations");
		performSimplexRestartRunFirstPart();
		System.out.println("Third to last iteration");
		performSimplexRestartRunSecondPart();
		
	}


/*******************************************************************************************************/

	private void performSparseDudContinuousRun() {
		SparseDud algorithm = createSparseDudAlgorithm(true);
		algorithm.run();
		IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
		System.out.println("pCal="+pCal);
		System.out.println("should be pCal="+pCalSparseDudExpected);
		assertEquals("pCal SparseDud Continuous Run", pCalSparseDudExpected.toString(), pCal.toString());
		algorithm.finish();
    	}

	private void performSparseDudRestartRunFirstPart() {
		SparseDud algorithm = createSparseDudAlgorithm(true);
		for (int i = 0; i < restartFromIterationNumber; i++) {
			algorithm.next();
		}
		IModelState state=algorithm.saveInternalState();
		System.out.println("State before saving: "+state.toString());
		state.savePersistentState(savedSparseDudStateFile);
		algorithm.finish();
	}

	private void performSparseDudRestartRunSecondPart() {
		SparseDud algorithm = createSparseDudAlgorithm(false);
		algorithm.restoreInternalState(algorithm.loadPersistentState(savedSparseDudStateFile));
		algorithm.run();
		IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
		System.out.println("pCal="+pCal);
		System.out.println("should be pCal="+pCalSparseDudExpected);
		assertEquals("pCal SparseDud Restart Run", pCalSparseDudExpected.toString(), pCal.toString());
		algorithm.finish();
	}
		
	private SparseDud createSparseDudAlgorithm(boolean doPrepare) {
		IStochModelFactory stochModelFactory = new OscillatorStochModelFactory();
		stochModelFactory.initialize(null, new String[]{
			 "<oscillatorConfig>"
		        +"	<parameters names=\"t_damp,omega\">[8.0,1.63]</parameters>"
		        +"</oscillatorConfig>"});
		IStochObserver obsGenerated = new NoosTimeSeriesStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"noosOscillatorWithNoFilter.xml"});
		// Now start calibration through proper algorithm-class
		String dudConfig = 
			 "<DudConfig>"
			+"	<costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" />"
			+"	<outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" />"
			+"	<lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" >"
			+"		<backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" />"
			+"	</lineSearch>"

			+"	<matlabOutputFile>dud.m</matlabOutputFile>"

			+"	<dependencies>"
			+"		<obs id=\"global.position\"> <depends_on> <par id=\"t_damp\"/> </depends_on> </obs>"
			+"		<obs id=\"global.position\"> <depends_on> <par id=\"omega\"/> </depends_on> </obs>"
			+"	</dependencies>"
			+"</DudConfig>";
		SparseDud algorithm = new SparseDud();
		algorithm.initialize(testRunDataDir,new String[]{dudConfig});
		algorithm.setStochComponents(obsGenerated,stochModelFactory);
		if (doPrepare) algorithm.prepare();
		return algorithm;
	}

/*******************************************************************************************************/
	private void performDudContinuousRun() {
		Dud algorithm = createDudAlgorithm(true);
		algorithm.run();
		IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
		System.out.println("pCal="+pCal);
		System.out.println("should be pCal="+pCalDudExpected);
		assertEquals("pCal Dud Continuous Run", pCalDudExpected.toString(), pCal.toString());
		algorithm.finish();
	}
		
	private void performDudRestartRunFirstPart() {
		Dud algorithm = createDudAlgorithm(true);
		for (int i = 0; i < restartFromIterationNumber; i++) {
			algorithm.next();
		}
		IModelState state=algorithm.saveInternalState();
		System.out.println("State before saving: "+state.toString());
		state.savePersistentState(savedDudStateFile);
		algorithm.finish();
	}
		
	private void performDudRestartRunSecondPart() {
		Dud algorithm = createDudAlgorithm(false);
		algorithm.restoreInternalState(algorithm.loadPersistentState(savedDudStateFile));
		algorithm.run();
		IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
		System.out.println("pCal="+pCal);
		System.out.println("should be pCal="+pCalDudExpected);
		assertEquals("pCal Dud Restart Run", pCalDudExpected.toString(), pCal.toString());
		algorithm.finish();
	}
		
	private Dud createDudAlgorithm(boolean doPrepare) {
		IStochModelFactory stochModelFactory = new OscillatorStochModelFactory();
		stochModelFactory.initialize(null, new String[]{""});
		IStochObserver obsGenerated = new CsvStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"oscillatorCalibrationObservations.csv"});
		// Now start calibration through proper algorithm-class
		String dudConfig = "<DudConfig><costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" ><backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" /></lineSearch><matlabOutputFile>dud.m</matlabOutputFile></DudConfig>";
		Dud algorithm = new Dud();
		algorithm.initialize(testRunDataDir,new String[]{dudConfig});
		algorithm.setStochComponents(obsGenerated,stochModelFactory);
		if (doPrepare) algorithm.prepare();
		return algorithm;
	}

/*******************************************************************************************************/
    private void performPowellContinuousRun() {
        Powell algorithm = createPowellAlgorithm(true);
        algorithm.run();
        IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
        assertEquals("pCal Powell Continuous Run", pCalPowellExpected.toString(), pCal.toString());
		algorithm.finish();
	}

    private void performPowellRestartRunFirstPart() {
        Powell algorithm = createPowellAlgorithm(true);
        for (int i = 0; i < restartFromIterationNumber; i++) {
            algorithm.next();
        }
        IModelState state=algorithm.saveInternalState();
		System.out.println("State before saving: "+state.toString());
		state.savePersistentState(savedPowellStateFile);
		algorithm.finish();
	}

    private void performPowellRestartRunSecondPart() {
        Powell algorithm = createPowellAlgorithm(false);
        algorithm.restoreInternalState(algorithm.loadPersistentState(savedPowellStateFile));
        algorithm.run();
        IVector pCal= new Vector(algorithm.getBestEstimate().getParameters());
        assertEquals("pCal Powell Restart Run", pCalPowellExpected.toString(), pCal.toString());
		algorithm.finish();
	}

    private Powell createPowellAlgorithm(boolean doPrepare) {
        IStochModelFactory stochModelFactory = new OscillatorStochModelFactory();
        stochModelFactory.initialize(null, new String[]{""});
        IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir, new String[]{"oscillatorCalibrationObservations.csv"});
        // Now start calibration through proper algorithm-class
        String powellConfig = "<PowellConfig><costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" ><backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" /></lineSearch><matlabOutputFile>powell.m</matlabOutputFile></PowellConfig>";
        Powell algorithm = new Powell();
        algorithm.initialize(testRunDataDir,new String[]{powellConfig});
        algorithm.setStochComponents(obsGenerated,stochModelFactory);
        if (doPrepare) {
            algorithm.prepare();
        }
        return algorithm;
    }

/*******************************************************************************************************/
    private void performSimplexContinuousRun() {
        Simplex algorithm = createSimplexAlgorithm(true);
        algorithm.run();
        IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
        assertEquals("pCal Simplex Continuous Run", pCalSimplexExpected.toString(), pCal.toString());
		algorithm.finish();
	}

    private void performSimplexRestartRunFirstPart() {
        Simplex algorithm = createSimplexAlgorithm(true);
        for (int i = 0; i < restartFromIterationNumber; i++) {
            algorithm.next();
        }
		IModelState state=algorithm.saveInternalState();
		System.out.println("State before saving: "+state.toString());
		state.savePersistentState(savedSimplexStateFile);
		algorithm.finish();
	}

    private void performSimplexRestartRunSecondPart() {
        Simplex algorithm = createSimplexAlgorithm(false);
        algorithm.restoreInternalState(algorithm.loadPersistentState(savedSimplexStateFile));
        algorithm.run();
        IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
        assertEquals("pCal Simplex Restart Run", pCalSimplexExpected.toString(), pCal.toString());
		algorithm.finish();
	}

    private Simplex createSimplexAlgorithm(boolean doPrepare) {
        IStochModelFactory stochModelFactory = new OscillatorStochModelFactory();
        stochModelFactory.initialize(null, new String[]{""});
        IStochObserver obsGenerated = new CsvStochObserver();
        obsGenerated.initialize(testRunDataDir, new String[]{"oscillatorCalibrationObservations.csv"});
        // Now start calibration through proper algorithm-class
        String simplexConfig = "<SimplexConfig><costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"100\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" ><backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" /></lineSearch><matlabOutputFile>simplex.m</matlabOutputFile></SimplexConfig>";
        Simplex algorithm = new Simplex();
        algorithm.initialize(testRunDataDir,new String[]{simplexConfig});
        algorithm.setStochComponents(obsGenerated,stochModelFactory);
        if (doPrepare) {
            algorithm.prepare();
        }
        return algorithm;
    }
}
