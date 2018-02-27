/* MOD_V1.0
* Copyright (c) 2013 OpenDA Association
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
package org.openda.models.biasAwareObservations;
/**
 * Model for modelling bias in the observed values
 *
 * @author Nils van Velzen (TU-Delft/VORtech)
 *
 */

import org.openda.localization.LocalizationDomainsSimpleModel;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.interfaces.*;
import org.openda.observers.ObserverUtils;
import org.openda.utils.StochVector;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;


public class BiasAwareObservationsModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated {
	private IStochModelInstance childModel=null;  		//Handle to underlying child model
	private Vector bias=null;                     		//Vector modelling the bias in the observations
	private IStochVector biasNoise=null;          		//Stochastic vector for generating noise
	private boolean automaticNoiseGeneration=false;		//Flag indicating whether to generate noise or not
	private boolean checkObservationID=false;           //Match observations using their ID or just assume it matched
	private HashMap<String, Integer> obsIDIndex; 		//Index of each observation in bias vector;
	private boolean localization=true;                  //Use pure localization on biases
	private boolean explicitDeclaration=false;          //Explicit declaration of observations to consider
	                                            //Not handling/skipping observations is allowed

	private ILocalizationDomains localizationDomains = null;

	private int getObservationIndex(String observationID){
		if (obsIDIndex==null){
			obsIDIndex=new HashMap<String, Integer>();
		}

		Integer index = obsIDIndex.get(observationID);
		if (index == null){
			if (obsIDIndex.size()>=this.bias.getSize()){
			   	if(this.explicitDeclaration){
					return -1;
			   	}
				else {
					throw new RuntimeException("The specified augmented state for modelling the bias "+
						"is not large enough to hold all the observations. The specified length is "+this.bias.getSize());
				}
			}
			index=obsIDIndex.size();
			System.out.println(this.getClass().getCanonicalName()+": First usage of observation with ID "+observationID);
			System.out.println(this.getClass().getCanonicalName()+": Location of bias term in augmented state is "+index);
			obsIDIndex.put(observationID, index);
		}
		return index;
	}

	private int[] getObservationIndices(IObservationDescriptions observationDescriptions){

		ObserverUtils obsUtils = new ObserverUtils(observationDescriptions);
		String[] obsIds = obsUtils.getObsIds();
        int nObs = obsIds.length;
		if (nObs == 0) return null;

		int [] indices = new int[nObs];
		for (int iObs=0; iObs<nObs; iObs++){
			indices[iObs] = getObservationIndex(obsIds[iObs]);
		}
		return indices;
	}

	public BiasAwareObservationsModelInstance(IStochModelInstance child, double std, HashMap<String, Double> obsIDstd, int maxBias, boolean checkObservationID, boolean localization){
		this.childModel         = child;
		this.localization       = localization;
		this.checkObservationID = checkObservationID;

		int nState = maxBias;
		if (obsIDstd != null){
		    nState=Math.max(nState,obsIDstd.size());
		}

		// Create bias augmented state
		this.bias               = new Vector(nState);
		bias.setConstant(0.0);

		// Create uncertainty model
		Vector stdVec=bias.clone();
		stdVec.setConstant(std);

		// For those observation ID's we have explicitly specified a standard deviation we
		// -Add the id to the administration (allocate index in augmented state)
		// -Set the value of the standard deviation
		if (obsIDstd!=null){
			Iterator <String> keyIterator = obsIDstd.keySet().iterator();
			while(keyIterator.hasNext()){
				this.explicitDeclaration=true; //We have explicitly declared observations
				String key=keyIterator.next();
				double value = obsIDstd.get(key);
				int index = getObservationIndex(key);
				stdVec.setValue(index,value);
			}
		}
		// Create stochastic vector for adding noise
		biasNoise=new StochVector(this.bias.clone(),stdVec);

		this.localizationDomains = new LocalizationDomainsSimpleModel();
	}

	public IVector getState(int iDomain) {
		TreeVector x0 =new TreeVector("state","state",childModel.getState(iDomain));
		TreeVector x1 =new TreeVector("observationsBias","observationsBias",bias.clone());

		TreeVector x =new TreeVector("BiasAwareObservationsModel","BiasAwareObservationsModel");
		x.addChild(x0);
		x.addChild(x1);
		return x;
	}

	public IVector getState() {

		TreeVector x0 =new TreeVector("state","state",childModel.getState());
		TreeVector x1 =new TreeVector("observationsBias","observationsBias",bias.clone());

	    TreeVector x =new TreeVector("BiasAwareObservationsModel","BiasAwareObservationsModel");
		x.addChild(x0);
		x.addChild(x1);
		return x;
	}

	public void axpyOnState(double alpha, IVector vector) {

		ITreeVector x= (ITreeVector) vector;

		childModel.axpyOnState(alpha, x.getSubTreeVector("state"));
		bias.axpy(alpha, x.getSubTreeVector("observationsBias"));
	}

	public void axpyOnState(double alpha, IVector change, int iDomain) {
		// TODO Auto-generated method stub

	}


	public IVector getParameters() {
		return childModel.getParameters();
	}

	public void setParameters(IVector parameters) {
		childModel.setParameters(parameters);
	}

	public void axpyOnParameters(double alpha, IVector vector) {
		childModel.axpyOnParameters(alpha, vector);
	}

	public IStochVector getStateUncertainty() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	public IStochVector getParameterUncertainty() {
		return childModel.getParameterUncertainty();
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		return new IStochVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	public boolean isWhiteNoiseStationary() {
		return false;  //To change body of implemented methods use File | Settings | File Templates.
	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		return new ITime[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	public void setAutomaticNoiseGeneration(boolean value) {
        childModel.setAutomaticNoiseGeneration(value);
        this.automaticNoiseGeneration=value;
	}

	public IObservationOperator getObservationOperator(){
		return new ObservationOperatorDeprecatedModel(this);
	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {

		// Get the predictions from the child model
		IVector HxMinBias = childModel.getObservationOperator().getObservedValues(observationDescriptions);

		//We need to find out which obs corresponds to each index:
		if (checkObservationID){
			int[] indices=getObservationIndices(observationDescriptions);
			int count=0;
			for (int iObs=0;iObs<indices.length;iObs++){
				// Observations that are not in our augmented state we have a negative index
				// We just skip these values
				if (indices[iObs]>=0){
					count++;
					double hx=HxMinBias.getValue(iObs);
					double biasValue=this.bias.getValue(indices[iObs]);
					HxMinBias.setValue(iObs,hx+biasValue);
				}
			}
			if (count==0){
				System.out.println("Warning none of the observations are handled by the bias correction algorithm");
			}

		}
		else {
			HxMinBias.axpy(1.0, this.bias.clone());
		}
		return HxMinBias;
	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		childModel.announceObservedValues(observationDescriptions);
	}

	public IVector getStateScaling() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	public ITime getTimeHorizon() {
		return childModel.getTimeHorizon();
	}

	public ITime getCurrentTime() {
		return childModel.getCurrentTime();
	}

	public void compute(ITime targetTime) {
		ITime currentTime=null;
		if (automaticNoiseGeneration){currentTime=childModel.getCurrentTime();}

		childModel.compute(targetTime);

		if (automaticNoiseGeneration){
			//Compute size of time step
			double simulationInDays=targetTime.getMJD()-currentTime.getMJD();
			bias.axpy(simulationInDays, biasNoise.createRealization());
		}
	}


	public ILocalizationDomains getLocalizationDomains(){
		return this.localizationDomains;
	}


	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		return getObservedLocalization(observationDescriptions, distance, -1);
	}


	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain){
		IVector[] modelLocalization;
		if (iDomain<0) {
			modelLocalization = childModel.getObservedLocalization(observationDescriptions, distance);
		}
		else {
			modelLocalization = childModel.getObservedLocalization(observationDescriptions, distance, iDomain);
		}
		TreeVector [] x = new TreeVector[modelLocalization.length];
        int[] indices=null;
		if (this.checkObservationID){
			indices = this.getObservationIndices(observationDescriptions);
		}
		for (int iObs=0; iObs<modelLocalization.length; iObs++){
			TreeVector x0 =new TreeVector("state","state",modelLocalization[iObs]);
			Vector localizeObs=bias.clone();
			if (this.localization) {
				localizeObs.setConstant(0.0);
				int index=iObs;
				if (this.checkObservationID){ index = indices[iObs];}
				if (index>=0){
					localizeObs.setValue(index,1.0);
				}
			}
			else {
				localizeObs.setConstant(1.0);
			}
			TreeVector x1 =new TreeVector("observationsBias","observationsBias",localizeObs);
			x[iObs] =new TreeVector("BiasAwareObservationsModel","BiasAwareObservationsModel");
			x[iObs].addChild(x0);
			x[iObs].addChild(x1);
		}
		return x;
	}

	
	public IModelState saveInternalState() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void restoreInternalState(IModelState savedInternalState) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void releaseInternalState(IModelState savedInternalState) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IModelState loadPersistentState(File persistentStateFile) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public File getModelRunDir() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public String[] getExchangeItemIDs() {
		return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void finish() {
        childModel.finish();
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void initialize(File workingDir, String[] arguments) {
       childModel.initialize(workingDir,arguments);
	}

	
	public IInstance getParent() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}
}
