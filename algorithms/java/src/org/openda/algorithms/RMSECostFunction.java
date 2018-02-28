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
import org.openda.utils.*;

import java.util.ArrayList;
import java.util.List;

public class RMSECostFunction implements ICostFunction{

	//fields for this class
	IStochModelFactory modFac;
    private List<IObservationSpaceFilter> obsSpaceFilters = new ArrayList<IObservationSpaceFilter>();
    IStochObserver obs;
	IStochObserver obsSelection = null;
	double factor=1.0; //scale costfunction with this factor
	boolean addBackgroundTerm=false; // add term Jb(p) = (p-p0)'*inv(B)*(p-p0)
	boolean useAnnounceObs=true;     // make one single run for collecting obs
	public boolean tryParallel=false;      // try to run multiple models in parallel
    public boolean biasRemoval=false;


	// for saving results
	static int nextEvaluation=1;
	int numberEvaluation = 0; //make numbers unique over instances
	IVector pMin = null;     // best parameters sofar
	IVector predMin = null;  // best predictions
	double fMin = Double.MAX_VALUE;
	private java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	private java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	private IVector lastPredicted=null;
	private IStochModelInstance bestModel=null;
	IStochVector parameterUncertainty=null;
	IStochVector observationUncertainty=null;

	// for saving data between prepare and evaluate calls
	private boolean prepared = false;
	private IVector preparePars= null;
	private IObservationDescriptions prepareDescr=null;
	private IStochModelInstance prepareMod=null;
    private boolean obsFilterHasNotBeenRun = true;
    private boolean doComputeCosts = true;

    public RMSECostFunction(IStochModelFactory modFac, IStochObserver obs){
		this.modFac = modFac;
		this.obs    = obs;
		//TODO creates additional model!!
		IStochModelInstance stochModelInstance = modFac.getInstance(IStochModelFactory.OutputLevel.Suppress);
		this.parameterUncertainty = stochModelInstance.getParameterUncertainty();
		stochModelInstance.finish(); // removed unused additional model (work-0)
		nextEvaluation=1;
		numberEvaluation = 0; //make numbers unique over instances
	}

    public RMSECostFunction(IStochModelFactory modFac, IStochObserver stochObserver,
                            boolean doComputeCosts){
        this(modFac, stochObserver);
        this.doComputeCosts = doComputeCosts;
		nextEvaluation=1;
		numberEvaluation = 0; //make numbers unique over instances
    }

    public RMSECostFunction(IStochModelFactory modFac, IStochObserver stochObserver,
                            List<IObservationSpaceFilter> obsSpaceFilters){
        this(modFac, stochObserver);
        this.obsSpaceFilters = obsSpaceFilters;
		nextEvaluation=1;
		numberEvaluation = 0; //make numbers unique over instances
    }

	public RMSECostFunction(
            IStochModelFactory modFac,
            IStochObserver obs,
            IStochVector parameterUncertainty,
            IStochObserver obsSelection,
            double factor,
            boolean addBackgroundTerm,
            boolean useAnnounceObs,
            IStochVector observationUncertainty){
		this.modFac = modFac;
		this.obs    = obs;
		this.parameterUncertainty = parameterUncertainty;
		this.obsSelection = obsSelection;
		this.factor = factor;
		if(addBackgroundTerm){
			throw new UnsupportedOperationException("RMSECostFunction: addBackgroudTerm is not supported yet");
		}
		this.addBackgroundTerm = addBackgroundTerm;
		this.useAnnounceObs = useAnnounceObs;
		this.observationUncertainty = observationUncertainty;
		nextEvaluation=1;
		numberEvaluation = 0; //make numbers unique over instances
	}


	/**
	 * @param p parameters to use for next evaluation
	 */
	public void prepare(IVector p) {
		//checks
		if(this.prepared){
			throw new RuntimeException(this.getClass().getName()+": Preparing twice.");
		}
		//start preparations
		this.numberEvaluation = RMSECostFunction.nextEvaluation;
		RMSECostFunction.nextEvaluation++;
    	Results.putMessage("========================================================================");
        Results.putMessage("prepare no "+ this.numberEvaluation);
        System.out.println("prepare no "+ this.numberEvaluation);
	    IStochModelInstance stochModelInstance =modFac.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setParameters(p);
        this.preparePars=p;

    	// select observations within interval and announce
    	if(this.obsSelection==null){ // delayed initialization because this requires a model
    		ITime selectionTimes = stochModelInstance.getTimeHorizon();
    		this.obsSelection = this.obs.createSelection(selectionTimes);
    		if(this.obsSelection.getCount()==0){
                throw new RuntimeException("No observations are falling within simulation span:"+selectionTimes.toString());
    		}
	    	// get observations as Vectors
	    	IVector obsMean = this.obsSelection.getExpectations();
	    	IVector obsStd = this.obsSelection.getStandardDeviations();
    		this.observationUncertainty = new StochVector(obsMean, obsStd);
    	}

	    /*
	     *  Jo observation term
	     *  Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
	     */
	    if(this.useAnnounceObs){ // evaluate cost with one model-run
	    	IObservationDescriptions descr = this.obsSelection.getObservationDescriptions();
	    	this.prepareDescr = descr;
	    	stochModelInstance.announceObservedValues(descr);
	    	// run model
            ITime targetTime = stochModelInstance.getTimeHorizon().getEndTime();
            try {
                stochModelInstance.compute(targetTime);
            } catch (RuntimeException e) {
                handleRuntimeException(e);
            }
            this.prepareMod = stochModelInstance;
	    }else{ //without stochModelInstance.announceObs, by splitting run in parts
	    	throw(new RuntimeException("Simulation without announceObs is not implemented yet"));
	    }

	    this.prepared=true;
	}

//	/**
//	 * Evaluate the costfunction for some parameters
//	 * @param p : Vector with parameters
//	 * @return cost as double
//	 */
//	public double evaluate(IVector p){
//		if(! this.prepared){
//			this.prepare(p);
//		}
//		//check parameters
//		IVector diff = p.clone();
//		diff.axpy(-1.0, this.preparePars);
//		if(diff.norm2()>1.0E-8){
//			throw new RuntimeException("Evaluate is called with different parameters than prepare was.\n"
//					+"p_prepare="+this.preparePars+"\n"
//					+"p_evaluate="+p);
//		}
//		//compute cost
//	    double totalCost=0.0;
//    	Results.putMessage("========================================================================");
//        Results.putMessage("evaluate no. "+ this.numberEvaluation);
//	    IStochModelInstance mod=this.prepareMod;
//
//	    /*
//	     *  Jo observation term
//	     *  Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
//	     */
//	    if(this.useAnnounceObs){ // evaluate cost with one model-run
//	    	// get results as Vectors
//	    	IObservationDescriptions descr = this.prepareDescr;
//	    	IVector prd      = mod.getObservedValues(descr);
//	    	this.lastPredicted=prd;
//            // observation filtering
//            if (this.obsSpaceFilters.size()>0 & obsFilterHasNotBeenRun){
//                ObservationSpace thisObsAndPred = new ObservationSpace();
//                thisObsAndPred.observer = this.obsSelection;
//                thisObsAndPred.predictedValues = this.lastPredicted;
//                for (IObservationSpaceFilter obsSpaceFilter : this.obsSpaceFilters) {
//                    thisObsAndPred = obsSpaceFilter.applyFilter(thisObsAndPred);
//                }
//                this.lastPredicted = thisObsAndPred.predictedValues;
//                prd = this.lastPredicted;
//                this.obsSelection = thisObsAndPred.observer;
//                descr = this.obsSelection.getObservationDescriptions();
//                obsFilterHasNotBeenRun = false;
//            }
//            if (this.biasRemoval){
//                // remove bias by adjusting lastPredicted:
//                this.lastPredicted = removeBias(this.obsSelection,this.lastPredicted);
//                prd = this.lastPredicted;
//            }
//
//	    	// residuals obs-prd
//            IVector obsMean = this.obsSelection.getExpectations();
//            IVector obsStd  = this.obsSelection.getStandardDeviations();
//            this.observationUncertainty = new StochVector(obsMean, obsStd);
//	    	IVector residuals= obsMean.clone();
//	    	residuals.axpy(-1.0, prd); //TODO fails for periodic variables
//            Results.putValue("observed", obsMean, IResultWriter.MessageType.Instance);
//            Results.putValue("evaluatedParameters", p, IResultWriter.MessageType.Instance);
//            Results.putValue("predicted", prd, IResultWriter.MessageType.Instance);
//            Results.putValue("residuals", residuals, IResultWriter.MessageType.Instance);
//	    	// output statistics
//	    	Statistics stats = new Statistics(residuals, descr);
//            Results.putMessage(stats.toString());
//
//            if (doComputeCosts) {
//                // Jo = sqrt{ 1/K * sum_k factor[((obs_k - prd_k))^2]}
//                IVector temp = residuals.clone();
//                double obsCost = Math.sqrt(Math.pow(temp.norm2(),2.0)/((double)temp.getSize()));
//                obsCost *= this.factor;
//                totalCost += obsCost;
//                Results.putValue("costObserved", obsCost, IResultWriter.MessageType.Instance);
//                Results.putValue("cost", totalCost, IResultWriter.MessageType.Instance);
//            }
//        }else{ //without mod.announceObs, by splitting run in parts
//	    	throw(new RuntimeException("Simulation without announceObs is not implemented yet"));
//	    }
//
//	    this.allPars.add(p.clone());
//	    this.allCosts.add(totalCost);
//
//        Results.putProgression("SimulationKwadraticCostFunction: evaluation "
//                + this.numberEvaluation + " : cost = " + PrintNumber.printNumber(totalCost));
//        Results.putIterationReport(mod, this.numberEvaluation, totalCost, p);
//
//        if (doComputeCosts) {
//            if (totalCost < this.fMin) {
//                this.fMin = totalCost;
//                this.pMin = p.clone();
//                this.predMin = this.lastPredicted.clone();
//                if (this.bestModel != null && this.bestModel != mod) {
//                    this.bestModel.finish();
//                }
//                this.bestModel = mod;
//            } else {
//                mod.finish();
//            }
//        } else {
//			mod.finish();
//		}
//
//        this.prepared=false; //clear for next evaluation
//
//	    return totalCost;
//	}

    /**
     * Evaluate the costfunction for some parameters
     * @param p : Vector with parameters
     * @param context : String of information on from where evaluate is called
     * @return cost as double
     */
	public double evaluate(IVector p, String context) {
        if(! this.prepared){
            this.prepare(p);
        }
        //check parameters
        IVector diff = p.clone();
        diff.axpy(-1.0, this.preparePars);
        if(diff.norm2()>1.0E-8){
            throw new RuntimeException("Evaluate is called with different parameters than prepare was.\n"
                    +"p_prepare="+this.preparePars+"\n"
                    +"p_evaluate="+p);
        }
        //compute cost
        double totalCost=0.0;
        Results.putMessage("========================================================================");
        Results.putMessage("evaluate no. "+ this.numberEvaluation);
        IStochModelInstance mod=this.prepareMod;

        /*
         *  Jo observation term
         *  Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
         */
        if(this.useAnnounceObs){ // evaluate cost with one model-run
            // get results as Vectors
            IObservationDescriptions descr = this.prepareDescr;
            IVector prd      = mod.getObservedValues(descr);
            this.lastPredicted=prd;
            // observation filtering
            if (this.obsSpaceFilters.size()>0 & obsFilterHasNotBeenRun){
                ObservationSpace thisObsAndPred = new ObservationSpace();
                thisObsAndPred.observer = this.obsSelection;
                thisObsAndPred.predictedValues = this.lastPredicted;
                for (IObservationSpaceFilter obsSpaceFilter : this.obsSpaceFilters) {
                    thisObsAndPred = obsSpaceFilter.applyFilter(thisObsAndPred);
                }
                this.lastPredicted = thisObsAndPred.predictedValues;
                prd = this.lastPredicted;
                this.obsSelection = thisObsAndPred.observer;
                descr = this.obsSelection.getObservationDescriptions();
                obsFilterHasNotBeenRun = false;
            }
            if (this.biasRemoval){
                // remove bias by adjusting lastPredicted:
                this.lastPredicted = removeBias(this.obsSelection,this.lastPredicted);
                prd = this.lastPredicted;
            }

            // residuals obs-prd
            IVector obsMean = this.obsSelection.getExpectations();
            IVector obsStd  = this.obsSelection.getStandardDeviations();
            this.observationUncertainty = new StochVector(obsMean, obsStd);
            IVector residuals= obsMean.clone();
            residuals.axpy(-1.0, prd); //TODO fails for periodic variables
            Results.putValue("predicted", prd, prd.getSize(), context, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
            Results.putValue("observed", obsMean, obsMean.getSize(), context, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
            Results.putValue("residuals", residuals, residuals.getSize(), context, IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);
            Results.putValue("evaluatedParameters", p, 1, "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
            // output statistics
            Statistics stats = new Statistics(residuals, descr);
            Results.putMessage(stats.toString());

            if (doComputeCosts) {
                // Jo = sqrt{ 1/K * sum_k factor[((obs_k - prd_k))^2]}
                IVector temp = residuals.clone();
                double obsCost = Math.sqrt(Math.pow(temp.norm2(),2.0)/((double)temp.getSize()));
                obsCost *= this.factor;
                totalCost += obsCost;
                Results.putValue("costObserved", obsCost, 1, context, IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
                Results.putValue("costTotal", totalCost, 1, context, IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
            }
        }else{ //without mod.announceObs, by splitting run in parts
            throw(new RuntimeException("Simulation without announceObs is not implemented yet"));
        }

        this.allPars.add(p.clone());
        this.allCosts.add(totalCost);

        Results.putProgression("SimulationKwadraticCostFunction: evaluation "
                + this.numberEvaluation + " : cost = " + PrintNumber.printNumber(totalCost));
        Results.putIterationReport(mod, this.numberEvaluation, totalCost, p);

        if (doComputeCosts) {
            if (totalCost < this.fMin) {
                this.fMin = totalCost;
                this.pMin = p.clone();
                this.predMin = this.lastPredicted.clone();
                if (this.bestModel != null && this.bestModel != mod) {
                    this.bestModel.finish();
                }
                this.bestModel = mod;
            } else {
                mod.finish();
            }
        } else {
            mod.finish();
        }

        this.prepared=false; //clear for next evaluation

        return totalCost;
	}

	private IVector removeBias(IStochObserver obsSelection, IVector lastPredicted) {
        IVector prd = lastPredicted.clone();
        IObservationDescriptions descr = obsSelection.getObservationDescriptions();
        List<IPrevExchangeItem> items;
        try{
            items = (List<IPrevExchangeItem>) descr.getExchangeItems();
        }catch (Exception e) {
            throw new RuntimeException("No observation data is available.");
        }
        int firstIndex=0;
        int lastIndex;
        for(IPrevExchangeItem item : items){
            String id = item.getId();
            double times[] = item.getTimes();
            if (times == null) {
                throw new RuntimeException("KwadraticCostFunction: no data on "+id+" stations-pair is found.");
            }
            // get obs data for this location:
            double obsData[] = item.getValuesAsDoubles();
            int nData = obsData.length;
            lastIndex = firstIndex + nData;
            // compute bias difference between prd and obs:
            int j=0;
            double bias=0.0;
            for (int i=firstIndex; i<lastIndex; i++){
                bias += 1/((double)nData)*(obsData[j]-prd.getValue(i));
                j++;
            }
            // add bias difference to prd:
            double newPrd;
            for (int i=firstIndex; i<lastIndex; i++){
                newPrd = prd.getValue(i) + bias;
                prd.setValue(i,newPrd);
            }
            firstIndex = lastIndex;
        }
        return prd;
    }

    public IStochObserver runObservationFilters(IVector p){
		if(! this.prepared){
			this.prepare(p);
		}
		//check parameters
		IVector diff = p.clone();
		diff.axpy(-1.0, this.preparePars);
		if(diff.norm2()>1.0E-8){
			throw new RuntimeException("Evaluate is called with different parameters than prepare was.\n"
					+"p_prepare="+this.preparePars+"\n"
					+"p_evaluate="+p);
		}
	    IStochModelInstance mod=this.prepareMod;
	    if(this.useAnnounceObs){ // evaluate cost with one model-run
	    	// get results as Vectors
	    	IObservationDescriptions descr = this.prepareDescr;
	    	IVector prd      = mod.getObservedValues(descr);
	    	this.lastPredicted=prd;
            // observation filtering
            if (this.obsSpaceFilters.size()>0 & obsFilterHasNotBeenRun){
                ObservationSpace thisObsAndPred = new ObservationSpace();
                thisObsAndPred.observer = this.obsSelection;
                thisObsAndPred.predictedValues = this.lastPredicted;
                for (IObservationSpaceFilter obsSpaceFilter : this.obsSpaceFilters) {
                    thisObsAndPred = obsSpaceFilter.applyFilter(thisObsAndPred);
                }
                this.lastPredicted = thisObsAndPred.predictedValues;
                prd = this.lastPredicted;
                this.obsSelection = thisObsAndPred.observer;
                descr = this.obsSelection.getObservationDescriptions();
                this.prepareDescr = descr;
                obsFilterHasNotBeenRun = false;
            }
        }
        return this.obsSelection;
	}

	public IStochModelInstance getBestModel(){
        return this.bestModel;
	}

	/**
	 * Get all the costValues evaluated so far
	 * @return costvalue for each evaluation
	 */
	public IVector getCosts(){
		int n = this.allCosts.size();
		Vector result = new Vector(n);
		for(int i=0;i<n;i++){
			result.setValue(i, this.allCosts.get(i));
		}
		return result;
	}

	/**
	 * Get the parameters used for each evaluation
	 * @return array of parameters, each as a Vector
	 */
	public IVector[] getParameters(){
		int n = this.allCosts.size();
		IVector[] result = new IVector[n];
		for(int i=0;i<n;i++){
			result[i] = this.allPars.get(i);
		}
		return result;
	}

	/**
	 * Get optimal costs
	 * @return optimal cost as double
	 */
	public double getOptimalCost(){
        return this.fMin;
	}

	/**
	 * Get parameters leading to optimal cost
	 * @return optimal parameters as Vector
	 */
	public IVector getOptimalParameters(){
		if(this.pMin==null){
		    throw new RuntimeException("No costs were computed, thus no optimal parameters exist.");
		}
        return this.pMin.clone();
	}


	/**
	 *  Output info about this costfunction
	 */
	public void writeResults(){
        if (doComputeCosts) {
            Results.putMessage("===================================================================");
            Results.putMessage("RMSECostfunction: optimal results");
            Results.putMessage("    number of evaluations: " + this.allCosts.size());
            Results.putMessage("    all cost values:");
            Results.putMessage("        " + this.getCosts().printString(""));
            Results.putMessage("    all parameter values");
            Matrix allParsMatrix = new Matrix(this.getParameters());
            Results.putMessage("        " + allParsMatrix.printString());
            Results.putMessage("    number of observations: " + this.lastPredicted.getSize());
            Results.putMessage("    best cost:");
            Results.putMessage("        cost = " + PrintNumber.printNumber(this.getOptimalCost()));
            Results.putMessage("    best parameters:");
            Results.putMessage("        " + this.getOptimalParameters().printString("        "));
            Results.putMessage("===================================================================");
            Results.putIterationReport(null, -1, this.getOptimalCost(), this.getOptimalParameters());
        } else {
            Results.putMessage("===================================================================");
            Results.putMessage("RMSECostfunction: no costs computed");
            Results.putMessage("===================================================================");
        }
    }

    private void handleRuntimeException(Exception e) {
        Results.putMessage("NOTE:");
        Results.putMessage("   Model failed: " + e.getMessage());
    }

	public double getMultiplicationFactor() {
		return this.factor;
	}

	public ICostFunction clone(){
		RMSECostFunction result = new RMSECostFunction(
				modFac, obs, parameterUncertainty, obsSelection, factor, addBackgroundTerm,
				useAnnounceObs, observationUncertainty);
		return result;
	}

	//
	public boolean getTryParallel() {
		return tryParallel;
	}

	//
	public void setTryParallel(boolean tryParallel) {
		this.tryParallel=tryParallel;
	}

}
