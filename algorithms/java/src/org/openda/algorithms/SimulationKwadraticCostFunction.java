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
import org.openda.interfaces.*;
import org.openda.utils.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class SimulationKwadraticCostFunction implements LeastSquaresCostFunction{

	//fields for this class
	public IStochModelFactory modFac;
    private List<IObservationSpaceFilter> obsSpaceFilters = new ArrayList<IObservationSpaceFilter>();
	public IStochObserver obs;
	private IStochObserver obsSelection = null;
	public double factor=0.5; //scale costfunction with this factor
	public boolean addBackgroundTerm=false; // add term Jb(p) = (p-p0)'*inv(B)*(p-p0)
	private boolean useAnnounceObs=true;     // make one single run for collecting obs
	public boolean tryParallel=false;      // try to run multiple models in parallel
    public boolean biasRemoval=false;
    public boolean stdRemoval=false;


	// for saving results
	private static int nextEvaluation=1;
	private int numberEvaluation = 0; //make numbers unique over instances
	private IVector pMin = null;     // best parameters sofar
	private IVector predMin = null;  // best predictions
	private double fMin = Double.MAX_VALUE;
	private java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	private java.util.Vector<IVector> allPreds = new java.util.Vector<IVector>();
	private java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	private IVector lastPredicted=null;
	private IStochModelInstance bestModel=null;
	private IStochVector parameterUncertainty=null;
	private IStochVector observationUncertainty=null;

	// for saving data between prepare and evaluate calls
	private boolean prepared = false;
	private IVector preparePars= null;
	private IObservationDescriptions prepareDescr=null;
	private IStochModelInstance prepareMod=null;
    private boolean obsFilterHasNotBeenRun = true;

	private String debugFilePathPrefix = null;

	public SimulationKwadraticCostFunction(IStochModelFactory modFac, IStochObserver obs){
		this.modFac = modFac;
		this.obs    = obs;
		//TODO creates additional model!!
		IStochModelInstance stochModelInstance = modFac.getInstance(IStochModelFactory.OutputLevel.Suppress);
		this.parameterUncertainty = stochModelInstance.getParameterUncertainty();
		stochModelInstance.finish();
		numberEvaluation = 0;
		nextEvaluation=1;
		Results.putMessage("CostFunction = SimulationKwadraticCostFunction");
	}

    public SimulationKwadraticCostFunction(IStochModelFactory modFac, IStochObserver stochObserver, List<IObservationSpaceFilter> obsSpaceFilters){
        this(modFac, stochObserver);
        this.obsSpaceFilters = obsSpaceFilters;
    }

	public SimulationKwadraticCostFunction() {
	}

	public void setListObservationFilter(List<IObservationSpaceFilter> obsSpaceFilters) {
        this.obsSpaceFilters = obsSpaceFilters;
    }

	public SimulationKwadraticCostFunction(
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
		this.addBackgroundTerm = addBackgroundTerm;
		this.useAnnounceObs = useAnnounceObs;
		this.observationUncertainty = observationUncertainty;
	}

	public void setDebugFilePathPrefix(String debugFilePathPrefix) {
		this.debugFilePathPrefix = debugFilePathPrefix;
	}

	/**
	 * @param p parameters to use for next evaluation
	 */
	public void prepare(IVector p) {
		//checks
		if(this.prepared){
			throw new RuntimeException("SimulationKwadraticCostFunction: Preparing twice.");
		}
		//start preparations
		this.numberEvaluation = SimulationKwadraticCostFunction.nextEvaluation;
		SimulationKwadraticCostFunction.nextEvaluation++;
    	Results.putMessage("========================================================================");
        Results.putMessage("prepare no "+ this.numberEvaluation);
        System.out.println("prepare no "+ this.numberEvaluation);
	    IStochModelInstance stochModelInstance =modFac.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setParameters(p);
		Results.putValue("evaluatedParameters", p, 1, "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
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
                throw e;
            }
            this.prepareMod = stochModelInstance;
	    }else{ //without stochModelInstance.announceObs, by splitting run in parts
	    	throw(new RuntimeException("Simulation without announceObs is not implemented yet"));
	    }

	    this.prepared=true;
	}

	public IVector getState(int iDomain) {
		return this.prepareMod.getState(iDomain);
	}

	public IVector getState(){
		return this.prepareMod.getState();
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
//            Results.putValue("predicted", prd, IResultWriter.MessageType.Instance);
//			logDebugInfo(mod.toString(), this.obsSelection, prd.getValues());
//			Results.putValue("predicted", prd, IResultWriter.MessageType.Instance);
//            Results.putValue("observed", obsMean, IResultWriter.MessageType.Instance);
//            Results.putValue("residuals", residuals, IResultWriter.MessageType.Instance);
//            Results.putValue("obs_stdev", obsStd, IResultWriter.MessageType.Instance);
//	    	// output statistics
//	    	Statistics stats = new Statistics(residuals, descr);
//            Results.putMessage(stats.toString());
//
//	    	// Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
//	    	IVector temp     = residuals.clone();
//	    	temp.pointwiseDivide(obsStd);
//	    	double obsCost = Math.pow(temp.norm2(),2.0);
//	    	obsCost *= this.factor;
////            obsCost *= (1/(double)this.obs.getCount()); // normalized cost over nObs
//	    	totalCost += obsCost;
//	    	Results.putValue("costObserved", obsCost, IResultWriter.MessageType.Instance);
//	    	Results.putValue("cost", totalCost, IResultWriter.MessageType.Instance);
//	    }else{ //without mod.announceObs, by splitting run in parts
//	    	throw(new RuntimeException("Simulation without announceObs is not implemented yet"));
//	    }
//
//        /*
//         *  Jb background term
//         *  Jb = Sum_i factor *((p(i)-means(i)/width(i))^2
//         */
//	    if(this.addBackgroundTerm){
//	    	ISqrtCovariance sqrtPar = this.parameterUncertainty.getSqrtCovariance();
//	    	// check for zero standard deviations
//	    	IVector stdPar=this.parameterUncertainty.getStandardDeviations();
//	    	for(int i=0;i<stdPar.getSize();i++){
//	    		double sigma = stdPar.getValue(i);
//	    		if(sigma<1e-8){
//	    			Results.putProgression("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
//	    			throw new RuntimeException("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
//	    		}
//	    	}
//
//	    	IVector avgPar = this.parameterUncertainty.getExpectations();
//	    	IVector deltaPar = p.clone();
//	    	deltaPar.axpy(-1.0, avgPar);
//	    	IVector transPar = new Vector(sqrtPar.getNumberOfColumns());
//	    	sqrtPar.rightSolve(deltaPar, transPar);
//	    	double backgroundCost = Math.pow(transPar.norm2(),2.0);
//	    	backgroundCost *= this.factor;
//	    	totalCost += backgroundCost;
//	    	Results.putValue("costWeakConstraintPenalty", backgroundCost, IResultWriter.MessageType.Instance);
//	    }
//
//	    this.allPars.add(p.clone());
//	    this.allPreds.add(this.lastPredicted);
//	    this.allCosts.add(totalCost);
//	    if(totalCost<this.fMin){
//	    	this.fMin      = totalCost;
//	    	this.pMin      = p.clone();
//	    	this.predMin   = this.lastPredicted.clone();
//         if (this.bestModel != null && this.bestModel != mod) {
//                this.bestModel.finish();
//            }
//	    	this.bestModel = mod;
//            } else {
//            mod.finish();
//	}
//        Results.putProgression("SimulationKwadraticCostFunction: evaluation "
//        		+ this.numberEvaluation +" : cost = " + PrintNumber.printNumber(totalCost));
//        Results.putIterationReport(mod, this.numberEvaluation, totalCost, p);
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
	    	IVector prd      = mod.getObservationOperator().getObservedValues(descr);
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
            if (this.stdRemoval){
                // remove temporal variation of the residual by adjusting lastPredicted:
                this.lastPredicted = removeStd(this.obsSelection,this.lastPredicted);
                prd = this.lastPredicted;
            }

	    	// residuals obs-prd
            IVector obsMean = this.obsSelection.getExpectations();
            IVector obsStd  = this.obsSelection.getStandardDeviations();
            this.observationUncertainty = new StochVector(obsMean, obsStd);
	    	IVector residuals= obsMean.clone();
	    	residuals.axpy(-1.0, prd); //TODO fails for periodic variables
			String modelInstanceName = mod.toString(); // TODO: add getName() to IModelInstance
			logDebugInfo(modelInstanceName, this.obsSelection, prd.getValues());
			Results.putValue("predicted", prd, prd.getSize(), context, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
            Results.putValue("observed", obsMean, obsMean.getSize(), context, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
            Results.putValue("residuals", residuals, residuals.getSize(), context, IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);
            Results.putValue("obs_stdev", obsStd, obsStd.getSize(), context, IResultWriter.OutputLevel.All, IResultWriter.MessageType.Instance);
	    	// output statistics
	    	Statistics stats = new Statistics(residuals, descr);
            Results.putMessage(stats.toString());

	    	// Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
	    	IVector temp     = residuals.clone();
	    	temp.pointwiseDivide(obsStd);
	    	double obsCost = Math.pow(temp.norm2(),2.0);
	    	obsCost *= this.factor;
//            obsCost *= (1/(double)this.obs.getCount()); // normalized cost over nObs
	    	totalCost += obsCost;
	    	Results.putValue("costObserved", obsCost, 1, context, IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
	    }else{ //without mod.announceObs, by splitting run in parts
	    	throw(new RuntimeException("Simulation without announceObs is not implemented yet"));
	    }

        /*
         *  Jb background term
         *  Jb = Sum_i factor *((p(i)-means(i)/width(i))^2
         */
	    if(this.addBackgroundTerm){
	    	ISqrtCovariance sqrtPar = this.parameterUncertainty.getSqrtCovariance();
	    	// check for zero standard deviations
	    	IVector stdPar=this.parameterUncertainty.getStandardDeviations();
	    	for(int i=0;i<stdPar.getSize();i++){
	    		double sigma = stdPar.getValue(i);
	    		if(sigma<1e-8){
	    			Results.putProgression("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
	    			throw new RuntimeException("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
	    		}
	    	}

	    	IVector avgPar = this.parameterUncertainty.getExpectations();
	    	IVector deltaPar = p.clone();
	    	deltaPar.axpy(-1.0, avgPar);
	    	IVector transPar = new Vector(sqrtPar.getNumberOfColumns());
	    	sqrtPar.rightSolve(deltaPar, transPar);
	    	double backgroundCost = Math.pow(transPar.norm2(),2.0);
	    	backgroundCost *= this.factor;
	    	totalCost += backgroundCost;
            Results.putValue("costWeakConstraintPenalty", backgroundCost, 1, context, IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
	    }
		Results.putValue("costTotal", totalCost, 1, context, IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);

	    this.allPars.add(p.clone());
	    this.allPreds.add(this.lastPredicted);
	    this.allCosts.add(totalCost);
	    if(totalCost<this.fMin){
	    	this.fMin      = totalCost;
	    	this.pMin      = p.clone();
	    	this.predMin   = this.lastPredicted.clone();
         if (this.bestModel != null && this.bestModel != mod) {
                this.bestModel.finish();
            }
	    	this.bestModel = mod;
            } else {
            mod.finish();
	}
        Results.putProgression("SimulationKwadraticCostFunction: evaluation "
        		+ this.numberEvaluation +" : cost = " + PrintNumber.printNumber(totalCost));
        Results.putIterationReport(mod, this.numberEvaluation, totalCost, p);
        this.prepared=false; //clear for next evaluation

	    return totalCost;
	}

	private void logDebugInfo(String modelInstanceName, IStochObserver observationSelection,
							  double[] predictions) {
		if (debugFilePathPrefix != null) {
			String debugLogFileName = debugFilePathPrefix + "_" + numberEvaluation + ".csv";
			if (!debugLogFileName.toLowerCase().endsWith(".csv")) {
				debugLogFileName += ".csv";
			}
			File file = new File(debugLogFileName);
			BufferedWriter debugFileWriter;
			try {
				debugFileWriter = new BufferedWriter(new FileWriter(file));
			} catch (IOException e) {
				throw new RuntimeException("Could not open log file for writing: " + file.getAbsolutePath() +
						"(" + e.getMessage() + ")");
			}
			try {
				// Add model instance identifier to evaluation
				String message = "Evaluation:," + numberEvaluation + "," + modelInstanceName;
				debugFileWriter.write(message + "\n");
				List<IPrevExchangeItem> observationItems =
						observationSelection.getObservationDescriptions().getExchangeItems();
				debugFileWriter.write("obs.id's,size,startIndex\n");
				List<Double> obsValues = new ArrayList<Double>();
				List<Double> obsTimes = new ArrayList<Double>();
				int startIndex = 1;
				for	(IPrevExchangeItem exchangeItem : observationItems) {
					double[] eiValues = exchangeItem.getValuesAsDoubles();
					double[] eiTimes = exchangeItem.getTimes();
					debugFileWriter.write(exchangeItem.getId() + "," + eiValues.length + "," + startIndex + "\n");
					startIndex += eiValues.length;
					for (double eiValue : eiValues) {
						obsValues.add(eiValue);
					}
					for (double eiTime : eiTimes) {
						obsTimes.add(eiTime);
					}
				}
				if (!(obsValues.size()== predictions.length)) {
					throw new RuntimeException("Inconstent #values: " +
							obsValues.size() + "!=" + predictions.length );
				}
				debugFileWriter.write("index, MJD, Obs, Pred\n");
				for (int i = 0 ; i < predictions.length; i++) {
					debugFileWriter.write((i+1) + "," + obsTimes.get(i) +
							"," + obsValues.get(i) + "," + predictions[i] + "\n");
				}
				debugFileWriter.flush();
				debugFileWriter.close();
			} catch (IOException e) {
				throw new RuntimeException("Could write to log file: " + e.getMessage());
			}
		}
	}

	/**
	 * Correct for unknown bias errors in the model or observations. The temporal 
	 * average of the residuals is added to the model predictions. This will remove
	 * these errors in subsequent computation of the residuals.
	 * @param obsSelection observations
	 * @param lastPredicted model predictions before correction
	 * @return
	 */
	private IVector removeBias(IStochObserver obsSelection, IVector lastPredicted) {
        IVector prd = lastPredicted.clone();
        IObservationDescriptions descr = obsSelection.getObservationDescriptions();
        List<IPrevExchangeItem> items;
        try{
            items = descr.getExchangeItems();
        }catch (Exception e) {
            throw new RuntimeException("No observation data is available through the exchangeItems.");
        }
        int firstIndex=0;
        int lastIndex;
        for(IPrevExchangeItem item : items){
            String id = item.getId();
            double times[] = item.getTimes();
            if (times == null) {
                throw new RuntimeException("KwadraticCostFunction: no data on "+id+" stations-pair is found.");
            }
            //TODO this will only work for timeseries
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

	/**
	 * Remove temporal variation of residuals. This will lead to estimation of the average residuals
	 * only. 
	 * @param obsSelection observations
	 * @param lastPredicted model predictions before correction
	 * @return corrected model predictions
	 */
	private IVector removeStd(IStochObserver obsSelection, IVector lastPredicted) {
        IVector prd = lastPredicted.clone();
        IObservationDescriptions descr = obsSelection.getObservationDescriptions();
        List<IPrevExchangeItem> items;
        try{
            items = descr.getExchangeItems();
        }catch (Exception e) {
            throw new RuntimeException("No observation data is available through exchangeItems.");
        }
        int firstIndex=0;
        int lastIndex;
        for(IPrevExchangeItem item : items){
            String id = item.getId();
            double times[] = item.getTimes();
            if (times == null) {
                throw new RuntimeException("KwadraticCostFunction: no data on "+id+" stations-pair is found.");
            }
            //TODO This will only work for timeseries
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
            // set prd = -bias + obs :
            double newPrd;
            j=0;
            for (int i=firstIndex; i<lastIndex; i++){
                newPrd = obsData[j] - bias;
                prd.setValue(i,newPrd);
                j++;
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
	    	this.lastPredicted=mod.getObservationOperator().getObservedValues(descr);
            // observation filtering
            if (this.obsSpaceFilters.size()>0 & obsFilterHasNotBeenRun){
                ObservationSpace thisObsAndPred = new ObservationSpace();
                thisObsAndPred.observer = this.obsSelection;
                thisObsAndPred.predictedValues = this.lastPredicted;
                for (IObservationSpaceFilter obsSpaceFilter : this.obsSpaceFilters) {
                    thisObsAndPred = obsSpaceFilter.applyFilter(thisObsAndPred);
                }
                this.lastPredicted = thisObsAndPred.predictedValues;
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
		Results.putMessage("===================================================================");
		Results.putMessage("SimulationKwadraticCostfunction: optimal results");
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
	}


	/*
	 * extensions for least-squares
	 *
	 *
	 */

    /**
     * Get mean and covariance for the parameters
     * @return StochVector with description of uncertainty for the parameters
     */
    public IStochVector getParameterUncertainty(){
        return this.parameterUncertainty;
    }

    /**
     * Get predictions corresponding to the last call to evaluate so far
     * @return Vector with predictions
     */
    public IVector getLastPredictions(){
        return this.lastPredicted.clone();
    }

    /**
     * Get uncertainty for the observations
     * @return StochVector with observation uncertainties
     */
    public IStochVector getObservationUncertainty(){
    	IStochVector result = this.observationUncertainty;
    	if(result==null){
    		throw new RuntimeException("SimulationKwadraticCostFunction.getObservationUncertainty: call prepare or evaluate first.");
    	}
    	return result;
    }

    /**
     * Use background term for cost function, i.e. add Jb = (p-p0)'/B(p-p0)
     * @param onIsTrue to turn backgroundterm on(true) of off(false)
     */
    public void setBackgroundTerm(boolean onIsTrue){
    	this.addBackgroundTerm = onIsTrue;
    }

    /**
     * Test if background term is in use for this cost function
     * @return True if background term is in use, false otherwise
     */
    public boolean doAddBackgroundTerm(){
    	return this.addBackgroundTerm;
    }

    /**
     * Get predictions corresponding to the lowest cost value evaluated so far
     * @return Vector with predictions
     */
    public IVector getOptimalPredictions(){
    	return this.predMin.clone();
    }

    /**
     * Get predictions for each function evaluation until now
     * @return Array of Vectors one for each evaluation predictions
     */
    public IVector[] getAllPredictions(){
		int n = this.allCosts.size();
		IVector[] result = new IVector[n];
		for(int i=0;i<n;i++){
			result[i] = this.allPreds.get(i);
		}
		return result;
    }

    private void handleRuntimeException(Exception e) {
        Results.putMessage("NOTE:");
        Results.putMessage("   Model failed: " + e.getMessage());
    }

	public double getMultiplicationFactor() {
		return this.factor;
	}

	public LeastSquaresCostFunction clone(){
		SimulationKwadraticCostFunction result = new SimulationKwadraticCostFunction(
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
