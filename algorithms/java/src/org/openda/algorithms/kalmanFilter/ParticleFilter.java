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
package org.openda.algorithms.kalmanFilter;
import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.io.File;
import java.util.Random;


/**
 *
 * @author Martin Verlaan / Albrecht Weerts
 * Residual resampling filter.
 */
public class ParticleFilter extends AbstractSequentialEnsembleAlgorithm {

	boolean useRandomSeed = false;
	long fixedSeed = 1234567890;
	Random randomGenerator = null;

	
	public void initialize(File workingDir, String[] arguments) {
		super.initialize(workingDir,arguments);
		
		/*
		 * parse config
		 */
		Results.putMessage("configstring = " + this.configString);
		ConfigTree conf = new ConfigTree(workingDir, configString);
		this.ensembleSize = conf.getAsInt("ensembleSize",this.ensembleSize);
		Results.putMessage("this.ensembleSize="+this.ensembleSize);
		String samplingMethod = conf.getAsString("samplingMethod","residual resampling");
		if(!samplingMethod.toLowerCase().startsWith(("residual"))){
			throw new RuntimeException("Only 'residual resampling' is supported for now.");
		}
		
		// initializeAndRun random number generator
		if(this.useRandomSeed){ 
			this.fixedSeed = System.currentTimeMillis();
		}else{
			this.randomGenerator = new Random(this.fixedSeed);
		}
	}

	
	public void analysis(IStochObserver stochObserver, IVector obsValues, IVector predMainModel, 
			IStochModelInstance mainModel, ITime analysisTime){
		IVector[] xi   = new IVector[this.ensembleSize];
		IVector[] pred = new IVector[this.ensembleSize];

		double[] weightsAllObservations = new double[this.ensembleSize];
		IModelState[] savedState = new IModelState[this.ensembleSize];
		for(int i=0;i<this.ensembleSize;i++){
			// get state
			xi[i] = this.ensemble[i].getState();
			// save state
			savedState[i] = this.ensemble[i].saveInternalState();
			// collect predictions
			pred[i] = this.ensemble[i].getObservedValues(stochObserver.getObservationDescriptions());
		}

		// compute output statistics
		IVector xiAvg = ensembleAverage(xi);
		IVector xiStd = ensembleStd(xi);
		this.setCurrentState(xiAvg.clone());
        Results.putValue("std_x_f", xiStd , xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		IVector predAvg = ensembleAverage(pred);
        Results.putValue("pred_f", predAvg, predAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
		IVector obsVal  = stochObserver.getExpectations();


		//
		// Update weights each member/particle
		//
		double[] normalizedWeightsAllObservations = new double[this.ensembleSize] ;

		double sumWeightsAllObservations = 0.0;
		for(int i=0;i<this.ensembleSize;i++){
			weightsAllObservations[i] = stochObserver.evaluatePDF(pred[i]);
			sumWeightsAllObservations += weightsAllObservations[i];
		}
		for(int i=0;i<this.ensembleSize;i++){
			normalizedWeightsAllObservations[i] = weightsAllObservations[i] / sumWeightsAllObservations;
		}
        Vector vecNormalizedWeightsAllObservations = new Vector(normalizedWeightsAllObservations);
        Results.putValue("weights", vecNormalizedWeightsAllObservations, vecNormalizedWeightsAllObservations.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);

		// index gives for each new member from which old member it should copy
		int[] index = residualResampling(normalizedWeightsAllObservations); // residual resampling

		// apply index
		for(int i=0;i<this.ensembleSize;i++){
			this.ensemble[i].restoreInternalState(savedState[index[i]]);
		}
		// releas saved states
		for(int i=0;i<this.ensembleSize;i++){
			this.ensemble[i].releaseInternalState(savedState[i]);
		}

		//collect output for this analysis
		for(int i=0;i<this.ensembleSize;i++){
			// get state
			xi[i] = this.ensemble[i].getState();
			// collect predictions
			pred[i] = this.ensemble[i].getObservedValues(stochObserver.getObservationDescriptions());
		}
		xiAvg = ensembleAverage(xi);
		// adjust mainModel (adjust mainModel to mean analyzed state)
		IVector deltaMain = xiAvg.clone();
		deltaMain.axpy(-1.0,mainModel.getState());
		mainModel.axpyOnState(1.0, deltaMain);
		
		xiStd = ensembleStd(xi);
		this.setCurrentState(xiAvg.clone());
		xiStd = ensembleStd(xi);
        Results.putValue("stdx_a", xiStd, xiStd.getSize() , "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		predAvg = ensembleAverage(pred);
        Results.putValue("pred_a", predAvg, predAvg.getSize() , "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
	}


	/*
	 * 
	 * Resampling routines
	 * 
	 */

	/**
	 * Resample particles with given weights. Weights should be normalized (sum=1) on entry.
	 * This is based on residual resampling.
	 * @param normalizedWeights array with normalized weights
	 * @return index array pointing to father of each particle in next generation. Eg. {1,1,2}
	 * indicates that particle 1 gets 2 children, particle 2 gets 1 and particle 3 none.
	 */
	private int[] residualResampling(double[] normalizedWeights){
		int[] result = new int[normalizedWeights.length];

		double[] residualWeights = new double[ensembleSize];
		int[] nSons = new int[ensembleSize];

		//
		// In first round probabilies greater than k/ensembleSize all get k sons
		//
		int nFirst = 0;
		for (int i = 0; i < ensembleSize; i++) {
			nSons[i] = ((Double) Math.floor(ensembleSize *normalizedWeights[i])).intValue(); //this are selected deterministically
			nFirst  += nSons[i]; //this is removed from probabilities
			residualWeights[i] = normalizedWeights[i] - ((double) nSons[i]/(double) ensembleSize);
		}


		//
		// remaining sons are sampled
		// 
		int nNext = ensembleSize-nFirst;
		if(nNext>0){
			double cumulativeSum =0.0;
			double[] cumSumResidualWeights = new double[ensembleSize];
			for (int i = 0; i < ensembleSize; i++) { // normalize to sum=1 again for residualWeights
				residualWeights[i] = residualWeights[i]*ensembleSize/nNext;
				//      		random selection is easier from cumulative probabilities cumsum({0.25,0.25,0.5}) -> {0.25,0.50,1.00}
				cumSumResidualWeights[i] = cumulativeSum + residualWeights[i]; 
				cumulativeSum += residualWeights[i];
			}
			for(int i=0;i<nNext;i++){
				nSons[drawFromDiscreteDistribution(cumSumResidualWeights)]++; //add one son to selected particle 
			}
		}

		//
		// Create index array
		//
		int currentSonIndex = 0;
		for (int j = 0; j < ensembleSize; j++) {
			if (nSons[j] > 0) {
				for (int i = 0; i < nSons[j]; i++) {
					result[currentSonIndex++] = j;
				}
			}
		}

        Vector vecResult = new Vector(result);
        Results.putValue("resamplingIndices", vecResult, vecResult.getSize(), "resampling step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);

		return result;
	}


	private int drawFromDiscreteDistribution(double[] cumulativeProbabilities){
		int result=0;
		double uniformRandomSample = this.randomGenerator.nextDouble(); //uniform(0,1)
		for(int i=0;i<cumulativeProbabilities.length;i++){
			if (uniformRandomSample<cumulativeProbabilities[i]){
				result = i;
				break; //no need to look further
			}
		}
		return result;
	}

}
