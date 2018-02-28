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

import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.Vector;
import org.openda.utils.io.CalRestartSettings;

import java.io.File;
import java.util.ArrayList;
import java.util.Random;


/**
 * OpenDA Shuffled Complex Evolution (SCE) algorithm. This algorithm finds a minimum of a
 * cost function of several variables using the Shuffled Complex Evolution (SCE) algorithm
 * originally introduced by Duan et al. (1992). The implementation of this algorithm in OpenDA
 * is adopted from the SCE.m program of Brecht Donckels, BIOMATH, Universiteit Gent.
 */
public class SCE extends Instance implements IAlgorithm {

    IStochModelFactory stochModelFactory;
    ConfigTree sceConf;

    private IStochModelInstance bestEstimate = null;
    //config
    public double initStep=1.0; //scaling factor for initial perturbations of parameters
    //workspace
    private SimulationKwadraticCostFunction J=null;
    private SCECoreOptimizer sceOptimizer = null;

    private File workingDir=null;
    private String[] arguments=null;
    private IStochObserver stochObserver;
    private Random randomGenerator = new Random();

    public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        this.arguments = arguments;
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochModelFactory = stochModelFactory;
        this.stochObserver = stochObserver;
        //parse config
        //<SCEConfig>
        //    <costFunction class="org.openda.algorithms.SimulationKwadraticCostFunction" maxEvaluation="23"/>
        //    <outerLoop maxIterations="20" absTolerance="0.001" relTolerance="0.001" nComplex="5"/>
        //    <innerLoop numIteration = "10"/>
        //    <parameterRange min="[0.4,0.1]" max="[0.6,0.3]"/>
        //</SCEConfig>

        String configString = arguments[0];
        Results.putMessage("configstring = "+ configString);
        sceConf = new ConfigTree(workingDir, configString);
        
        // Create costFunction
        Results.putMessage("costFunction@class="+ sceConf.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
        if(sceConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
            this.J = new SimulationKwadraticCostFunction(stochModelFactory, stochObserver);
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
        // options for costFunctions
        J.addBackgroundTerm = sceConf.getAsBoolean("costFunction@weakParameterConstraint",false);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = sceConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);

        // Initialize core optimizer
        this.sceOptimizer = new SCECoreOptimizer(J);
        this.sceOptimizer.maxCostEvaluation = sceConf.getAsInt("costFunction@maxEvaluation",this.sceOptimizer.maxCostEvaluation);
		Results.putMessage("Maximum number of cost function evaluations = "+this.sceOptimizer.maxCostEvaluation);

    }

    public void prepare() {

        // Create initial population
        // Get initial parameters from model
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IVector initialParameters = stochModelInstance.getParameters();
        this.sceOptimizer.pInit = initialParameters.clone();
        this.sceOptimizer.nparam = initialParameters.getSize(); // centralValues.getSize(); // n
        this.sceOptimizer.nPointsComplex = 2 * this.sceOptimizer.nparam + 1; // m=2n+1
        this.sceOptimizer.nPointsSimplex = this.sceOptimizer.nparam + 1; // q=n+1

        // Parse and use parameter ranges (LB and UB).
        //     <parameterRange min="[0.04,2.4]" max="[0.08,3.3]"" />
		// If <parameterRange> is not specified, use <stdDev> around seed values. (PREFERRED METHOD)
		IVector initialParameterUncertainties = stochModelInstance.getParameterUncertainty().getStandardDeviations();
        String minRangeString = sceConf.getAsString("parameterRange@min", "");
		if (minRangeString.length() == 0) {
			this.sceOptimizer.LB = new Vector(initialParameters);
			this.sceOptimizer.LB.axpy(-1., initialParameterUncertainties);
		} else {
			this.sceOptimizer.LB = new Vector(minRangeString);
		}
		if (this.sceOptimizer.LB.getSize() != this.sceOptimizer.nparam) {
			throw new RuntimeException(this.getClass().getName()+": the size of parameterRange min is not equal to the number of calibration parameters.");
		}
		String maxRangeString = sceConf.getAsString("parameterRange@max", "");
		if (maxRangeString.length() == 0) {
			this.sceOptimizer.UB = new Vector(initialParameters);
			this.sceOptimizer.UB.axpy(1., initialParameterUncertainties);
		} else {
			this.sceOptimizer.UB = new Vector(maxRangeString);
		}
		if (this.sceOptimizer.UB.getSize() != this.sceOptimizer.nparam) {
			throw new RuntimeException(this.getClass().getName()+": the size of parameterRange max is not equal to the number of calibration parameters.");
		}

        // read number of complexes
        this.sceOptimizer.nComplexes = sceConf.getAsInt("outerLoop@nComplex",this.sceOptimizer.nComplexes);
        Results.putMessage("outerLoop@nComplex="+this.sceOptimizer.nComplexes);

        // set stopping thresholds:
        this.sceOptimizer.maxitSCE = sceConf.getAsInt("outerLoop@maxIterations", sceOptimizer.maxitSCE);
        Results.putMessage("outerLoop@maxIterations="+this.sceOptimizer.maxitSCE);
        this.sceOptimizer.absTolSCE = sceConf.getAsDouble("outerLoop@absTolerance",this.sceOptimizer.absTolSCE);
        Results.putMessage("outerLoop@absTolerance="+this.sceOptimizer.absTolSCE);
        this.sceOptimizer.relTolSCE = sceConf.getAsDouble("outerLoop@relTolerance",this.sceOptimizer.relTolSCE);
        Results.putMessage("outerLoop@relTolerance="+this.sceOptimizer.relTolSCE);
        this.sceOptimizer.numInnerLoop = sceConf.getAsInt("innerLoop@numIteration",this.sceOptimizer.numInnerLoop);
        Results.putMessage("innerLoop@numIteration="+this.sceOptimizer.numInnerLoop);
        // additional stopCriteria
        ConfigTree parts[] = sceConf.getSubTrees("stopCriteria/stopCriterion");
        if (parts != null) {
            String className = null;
            double threshold;
            Class javaClass;
            Object object = null;
            for (ConfigTree part : parts) {
                className = part.getAsString("@class", null);
                threshold = part.getAsDouble("@threshold", this.sceOptimizer.stopCritThresDefault);
                Results.putMessage(this, "stopCriteria/stopCriterion@class=" + className + ", @threshold=" + threshold);
                try {
                    javaClass = Class.forName(className);
                    try {
                        object = javaClass.newInstance();
                    } catch (InstantiationException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                    this.sceOptimizer.stopCriteria.add((IStopCriterion) object);
                    this.sceOptimizer.stopCriteriaThreshold.add(threshold);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
                }
            }
//            this.sceOptimizer.obsDescr = stochObserver.getObservationDescriptions();
            ITime selectionTimes = stochModelInstance.getTimeHorizon();
            this.sceOptimizer.obsDescr = this.stochObserver.createSelection(selectionTimes).getObservationDescriptions();
        }

        // Initialize population
        int nPoints = this.sceOptimizer.nPointsComplex * this.sceOptimizer.nComplexes; // s=m*p
        IVector rangeParam = this.sceOptimizer.UB.clone();
        rangeParam.axpy(-1.0,this.sceOptimizer.LB); // rangeParam = UB - LB
        IVector[] InitialPopulation = new IVector[nPoints]; // Array of parameter-vectors, representing s.
        InitialPopulation[0] = initialParameters.clone();
        for(int i=1;i<nPoints;i++){
            IVector p = initialParameters.clone();    // content of no use, take care that the InitialPopulation vectors
                                                      // are treevectors if the parameters vector is a tree vector
            for (int j=0;j<rangeParam.getSize();j++){
                double r = randomGenerator.nextDouble(); // uniform (0,1)
                double thisP = r * rangeParam.getValue(j) + this.sceOptimizer.LB.getValue(j);      // addedToParam = r * (UB - LB)
                p.setValue(j,thisP);
            }
            InitialPopulation[i] = p.clone(); // centralValues
        }

        // Initialize core optimizer
        this.sceOptimizer.initialize(InitialPopulation);
        this.bestEstimate = this.J.getBestModel();
		stochModelInstance.finish();
	}


    public void run() {        
        // Call Simplex optimizer
    	while(this.hasNext()){
    		this.next();
    	}
	}

	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	public boolean hasNext(){
		return this.sceOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.sceOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
		try {
			if (!this.hasNext() && this.bestEstimate != null && this.bestEstimate.getModelRunDir() != null) {
				Results.putMessage("Optimal results are in model run dir "+ this.bestEstimate.getModelRunDir().getAbsolutePath());
			}
		} catch (Exception e) {
			// no model run dir, no logging
		}
	}

    public IModelState saveInternalState() {
        CalRestartSettings restartSettings = new CalRestartSettings("simplex");
        restartSettings.setComment("saved state for simplex algorithm");
        restartSettings.setParameters(this.sceOptimizer.getCurrentValues());
        restartSettings.setCostValues(this.sceOptimizer.getCurrentCosts());
        return restartSettings;
    }

    public void restoreInternalState(IModelState savedInternalState) {
        if (!(savedInternalState instanceof CalRestartSettings)) {
            throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
        }
        CalRestartSettings restartSettings = (CalRestartSettings) savedInternalState;
        this.sceOptimizer.initialize(restartSettings.getParameters(),
                restartSettings.getCosts());
    }

	public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof CalRestartSettings)) {
			throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
		}
		// no action needed
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("simplex", algorithmStateFile);
	}

	public void finish() {
		// each created model instance has been finished individually (in prepare() and
		// in the cost function)
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
    }

	public IVector getState() {
		// TODO Auto-generated method stub
		return null;
	}
	
	
	public ITime getTimeHorizon() {
		throw new UnsupportedOperationException("method getTimeHorizon not implemented."+this.getClass().getName());
	}

	
	public ITime getCurrentTime() {
		return null;
	}

	
	public void compute(ITime targetTime) {
		throw new UnsupportedOperationException("method compute not implemented."+this.getClass().getName());
	}

}
