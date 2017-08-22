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

import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Instance;
import org.openda.utils.Results;

import java.io.File;


/**
 * OpenDA GLUE algorithm
 * This class wraps the generic optimization routines in GriddedFullSearchCoreOptimizer for use as a tool
 * for the calibration of parameters
 */
public class GLUE extends Instance implements IAlgorithm {

    IStochModelFactory stochModelFactory;
    ConfigTree glueConf;

    private IStochModelInstance bestEstimate = null;
    private IStochObserver stochObserver = null;
    private IVector pInit=null;

    private RMSECostFunction J=null;
    private GLUECoreOptimizer glueOptimizer;
    private int ensembleSize = 20;

    
    public void initialize(File workingDir, String[] arguments) {
        String configString = arguments[0];
        glueConf = new ConfigTree(workingDir, configString);
        Results.putMessage("configstring = "+ configString);
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochModelFactory = stochModelFactory;
        this.stochObserver = stochObserver;

//      TO DO: modify the following code for creating likelihoodFunction for GLUE
        // Create costFunction
//        Results.putMessage("costFunction@class="+ glueConf.getAsString("costFunction@class","RMSECostFunction"));
//        if(glueConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
//            this.J = new SimulationKwadraticCostFunction(stochModelFactory, stochObserver);
//        }else{
//        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
//        }
//        // options for costFunctions
//        J.addBackgroundTerm = glueConf.getAsBoolean("costFunction@weakParameterConstraint",false);
//        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
//        J.factor = glueConf.getAsDouble("costFunction@factor",J.factor);
//        Results.putMessage("costFunction@factor="+J.factor);
        Results.putMessage("likelihoodFunction@class="+ glueConf.getAsString("likelihoodFunction@class","RMSECostFunction"));
        if(glueConf.getAsString("costFunction@class","RMSECostFunction").contains("RMSECostFunction")){
            this.J = new RMSECostFunction(stochModelFactory, stochObserver,
                    glueConf.getAsBoolean("doComputeCosts", false));
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
    }

    public void prepare() {
        //create grid
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        // initial uncertainty for parameters is used to start the optimization
//        IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();
        ITreeVector resultAsTreeVector = (ITreeVector)stochModelInstance.getParameters();
        for (String subTreeVectorId : resultAsTreeVector.getSubTreeVectorIds()) {
            System.out.println(subTreeVectorId);
        }

        this.ensembleSize = glueConf.getAsInt("ensembleSize",this.ensembleSize);
        this.glueOptimizer = new GLUECoreOptimizer(J,this.ensembleSize);
//        ConfigTree parts[] = glueConf.getSubTrees("listOfParameters/parameter");
//        if (parts != null) {
//            String[] parameterId = new String[parts.length];
//            String paramSource;
//            this.glueOptimizer.typeParameterUncertainty = new int[parts.length];
//            int i=0;
//            for (ConfigTree part : parts) {
//                parameterId[i] = part.getAsString("@id",null);
//                if (!resultAsTreeVector.getSubTreeVectorIds().contains((String)parameterId[i])){
//                    throw new RuntimeException(this.getClass().getName()+": parameter id "+parameterId[i]+" does not match any parameters.")
//                }
//                paramSource = part.getAsString("@source","");
//                if (paramSource.isEmpty() || paramSource.toLowerCase().contains("uncertainitem")){
//                    this.glueOptimizer.typeParameterUncertainty[i] = 0;
//                } else {
//                    this.glueOptimizer.typeParameterUncertainty[i] = 1;
//                }
//
//                i++;
//            }
//        }

//        // additional stopCriteria
//        ConfigTree parts[] = glueConf.getSubTrees("stopCriteria/stopCriterion");
//        if (parts != null) {
//            String className = null;
//            double threshold;
//            Class javaClass;
//            Object object = null;
//            for (ConfigTree part : parts) {
//                className = part.getAsString("@class", null);
//                threshold = part.getAsDouble("@threshold", this.glueOptimizer.stopCritThresDefault);
//                Results.putMessage(this, "stopCriteria/stopCriterion@class=" + className + ", @threshold=" + threshold);
//                try {
//                    javaClass = Class.forName(className);
//                    try {
//                        object = javaClass.newInstance();
//                    } catch (InstantiationException e) {
//                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
//                    } catch (IllegalAccessException e) {
//                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
//                    }
//                    this.glueOptimizer.stopCriteria.add((IStopCriterion) object);
//                    this.glueOptimizer.stopCriteriaThreshold.add(threshold);
//                } catch (ClassNotFoundException e) {
//                    throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
//                }
//            }
//            ITime selectionTimes = stochModelInstance.getTimeHorizon();
//            this.glueOptimizer.obsDescr = this.stochObserver.createSelection(selectionTimes).getObservationDescriptions();
//        }
		stochModelInstance.finish(); // removed unused additional model (work-1)
	}

    /**
     * Main routine for GriddedFullSearch calibration
     * Here the calibration problem is converted to an optimization problem and
     * the gfsCoreOptimizer is then started.
     */
    public void run() {        
    	while(this.hasNext()){
    		this.next();
    	}
    }

	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	public boolean hasNext(){
		return this.glueOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.glueOptimizer.next();
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
        throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.restoreInternalState(): Not implemented yet.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.restoreInternalState(): Not implemented yet.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.releaseInternalState(): Not implemented yet.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.loadPersistentState(): Not implemented yet.");
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
    }

	public void finish() {
		// each created model instance has been finished individually (in prepare() and
		// in the cost function)
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
