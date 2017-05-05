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

package org.openda.costa;

import org.openda.localization.LocalizationDomainsSimpleModel;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.interfaces.*;
import org.openda.utils.DistributedCounter;
import org.openda.utils.StochVector;
import org.openda.utils.performance.OdaTiming;

import java.io.File;

public class CtaOpenDaModel extends CtaObject implements IStochModelInstance, IStochModelInstanceDeprecated {

    // In case of parallel runs we use the Distributed counter to generate unique IDs
	static DistributedCounter lastGlobInstanceNr = new DistributedCounter();
	private ILocalizationDomains localizationDomains;
	int InstanceNr;
	String ModelID;
	OdaTiming timerCreate     = null;
	OdaTiming timerAnnounce   = null;
	OdaTiming timerAxpyParam  = null;
	OdaTiming timerAxpyState  = null;
	OdaTiming timerAxpyNoise  = null;
	OdaTiming timerCompute    = null;
	OdaTiming timerGetObs     = null;
	OdaTiming timerGetParUnc  = null;
	OdaTiming timerGetObsLoc  = null;
	OdaTiming timerGetState   = null;
	OdaTiming timerGetParam   = null;
	OdaTiming timerGetScaling = null;

	public CtaOpenDaModel(String ModelCls, String Filename){

		/* Set modelID and instance Nr */
		synchronized(lastGlobInstanceNr){
		  lastGlobInstanceNr.inc();
		  InstanceNr = lastGlobInstanceNr.val();
		  ModelID="Native Model:"+InstanceNr;
		}
		if ( timerCreate == null){
		   timerCreate = new OdaTiming(ModelID);
		}
		timerCreate.start();


        // Create the model
        if (! new File(Filename).exists()){
            throw new RuntimeException("File:"+Filename+" does not exist");
        }

        this.ctaHandle=ctaCreate(ModelCls, Filename);

		localizationDomains = new LocalizationDomainsSimpleModel();

		timerCreate.stop();
	}

	public void announceObservedValues(
			IObservationDescriptions observationDescriptions) {

		if ( timerAnnounce == null){
		   timerAnnounce = new OdaTiming(ModelID);
		}
		timerAnnounce.start();

		if (observationDescriptions instanceof CtaObservationDescriptions ){
			ctaAnnounceObservedValues(observationDescriptions);
		}
		else {
			if(observationDescriptions.getTimes()!= null && observationDescriptions.getTimes().length>1){
				timerAnnounce.stop();
				throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.announceObservedValues not implemented yet for other types");
			}
		}
		timerAnnounce.stop();
	}


	public void axpyOnParameters(double alpha, IVector vector) {
	   if ( timerAxpyParam == null){
		   timerAxpyParam = new OdaTiming(ModelID);
		}
		timerAxpyParam.start();


		if (vector instanceof CtaTreeVector ){
			ctaAxpyOnParameters(alpha, vector);
		}
		else {
	 		timerAxpyParam.stop();
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.axpyOnParameters not implemented yet for other types");
		}
		timerAxpyParam.stop();
	}


	public void axpyOnState(double alpha, IVector vector) {
		ctaAxpyOnStateDomain(alpha, vector, -1);
	}


	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		if ( timerAxpyState == null){
			timerAxpyState = new OdaTiming(ModelID);
		}
		timerAxpyState.start();

		// TODO Auto-generated method stub
		if (vector instanceof CtaTreeVector ){
			ctaAxpyOnStateDomain(alpha, vector, iDomain);
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.axpyOnState not implemented yet for other types");
		}
		timerAxpyState.stop();
	}



	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		if ( timerAxpyNoise == null){
			timerAxpyNoise = new OdaTiming(ModelID);
		}
		timerAxpyNoise.start();
	    timerAxpyNoise.stop();
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.axpyOnWhiteNoise(): Not implemented yet.");
	}

	public void compute(ITime targetTime) {
		if ( timerCompute == null){
			timerCompute = new OdaTiming(ModelID);
		}
		timerCompute.start();


       // Convert time to avoid problems
        CtaTime ctaTarget= new CtaTime();
        ctaTarget.setMJD(targetTime.getMJD());
        // Do timestep(s)
        ctaCompute(ctaTarget);
		timerCompute.stop();
    }


	public String[] getExchangeItemIDs() {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getExchangeItemIDs(): Not implemented yet.");
	}

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        if (role == IPrevExchangeItem.Role.InOut) {
            return getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getExchangeItemIDs(): Role selection not implemented yet.");
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.costa.CtaOpenDaModel.getDataObjectExchangeItem(): Not implemented yet.");
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getExchangeItem(): Not implemented yet.");
    }

	/**
	 * Get the operator that can calculate model values corresponding to a number of observations
	 * This returns the operator that calculates what the observations would look like,
	 * if reality would be equal to the current stoch model state.
	 *
	 * @return Observation operator
	 */
	public IObservationOperator getObservationOperator(){
		return new ObservationOperatorDeprecatedModel(this);
	}

	public IVector getObservedValues(
			IObservationDescriptions observationDescriptions) {

		if ( timerGetObs == null){
			timerGetObs = new OdaTiming(ModelID);
		}
		timerGetObs.start();

        int n=observationDescriptions.getObservationCount();
        CtaVector values = new CtaVector(n);

        // If observation description is already native we can directly pass it to the native implementation
        if (observationDescriptions instanceof CtaObservationDescriptions ){
            ctaGetObservedValues(observationDescriptions, values);

		}
		else {
           // First create a native wrapper for a generic observationdescription and then call the native
           // implementation
           CtaObservationDescriptions ObsDescr= new CtaObservationDescriptions(observationDescriptions);
           ctaGetObservedValues(ObsDescr, values);
           // delete wrapper
           ObsDescr.ctaFree();
		}
		timerGetObs.stop();
        return values;
    }


	public IStochVector getParameterUncertainty() {
        if ( timerGetParUnc == null){
			timerGetParUnc = new OdaTiming(ModelID);
		}
		timerGetParUnc.start();

		IVector params=this.getParameters();
        IVector std=params.clone();
        int npar=params.getSize();
        for (int i=0; i<npar; i++){
           std.setValue(i,1.0);
        }
        StochVector paramUncertainty= new StochVector(params,std);
        timerGetParUnc.stop();
        return paramUncertainty;
    }

	public IVector getParameters(){
		IVector retVec;
		if ( timerGetParam == null){
			timerGetParam = new OdaTiming(ModelID);
		}
		timerGetParam.start();
		retVec=ctaGetParameters();
		timerGetParam.stop();
		return retVec;
	}

	public IVector getState(int iDomain){
		IVector retVec;
		if ( timerGetState == null){
			timerGetState = new OdaTiming(ModelID);
		}
		timerGetState.start();
		if (iDomain>=0) {
			retVec = ctaGetStateDomain(iDomain);
		}
		else {
			retVec = ctaGetState();
		}
		timerGetState.stop();
		return retVec;
	}

	public IVector getState(){
		return getState(-1);
	}


	public IVector getStateScaling(){
		IVector retVec;
		if ( timerGetScaling == null){
			timerGetScaling = new OdaTiming(ModelID);
		}
		timerGetScaling.start();
		retVec=ctaGetStateScaling();
		timerGetScaling.stop();
		return retVec;
	}


	public ILocalizationDomains getLocalizationDomains(){
		return this.localizationDomains;
	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {

		return getObservedLocalization(observationDescriptions, distance, -1);
	}



	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain){
		if ( timerGetObsLoc == null){
			timerGetObsLoc = new OdaTiming(ModelID);
		}
		timerGetObsLoc.start();

		boolean javaObsDescr= !(observationDescriptions instanceof CtaObservationDescriptions);
		CtaObservationDescriptions ObsDescr=null;
		int[] ctaHandles;

		IVector rho[]= null;
		if (observationDescriptions.getObservationCount()>0){
			System.out.print("DEBUG in getObservedLocalization het aantal obs is:"+observationDescriptions.getObservationCount()+"\n");

			if (javaObsDescr){
				// First create a native wrapper for a generic observationdescription and then call the native
				// implementation
				ObsDescr= new CtaObservationDescriptions(observationDescriptions);
				if (iDomain>=0){
					ctaHandles = ctaGetObservedLocalizationDomain(ObsDescr, distance, iDomain);
				} else {
					ctaHandles = ctaGetObservedLocalization(ObsDescr, distance);
				}
				// delete wrapper
				ObsDescr.ctaFree();
			}
			else {
				if (iDomain>=0){
					ctaHandles = ctaGetObservedLocalizationDomain(observationDescriptions, distance, iDomain);
				} else {
					ctaHandles = ctaGetObservedLocalization(observationDescriptions, distance);
				}
			}
			if (ctaHandles == null){
				System.out.print("DEBUG in getObservedLocalization NULL array terug gekregen!\n");
			}
			rho	= new IVector[ctaHandles.length];
			for (int iObs=0; iObs<ctaHandles.length; iObs++){
				rho[iObs]=new CtaTreeVector(ctaHandles[iObs]);
			}
		}
		timerGetObsLoc.stop();
		return rho;
	}


	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int[] selector){
		return getObservedLocalization(observationDescriptions,distance,-1);
	}

	public IVector[] getStateScaling(
			IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getStateScaling(): Not implemented yet.");
	}

	public IStochVector getStateUncertainty() {
		// Getting state uncertainty from native ctaModel not (yet) implemented.
		// Return null (uncertainty specified)
        return null;
    }


	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getWhiteNoise(): Not implemented yet.");
	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getWhiteNoiseTimes(): Not implemented yet.");
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getWhiteNoiseUncertainty(): Not implemented yet.");
	}

	public boolean isWhiteNoiseStationary() {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.isWhiteNoiseStationary(): Not implemented yet.");
	}

	public void releaseInternalState(IModelState savedInternalState){
		if (savedInternalState instanceof CtaModelState){
			String IDstate = ((CtaModelState) savedInternalState).getID();
			ctaReleaseInternalState(IDstate);
		}
		else {
			throw new UnsupportedOperationException("savedInternalState is not an instance of CtaModelState");
		}
	}


	public IModelState loadPersistentState(File persistentStateFile) {
		String FileName = persistentStateFile.getAbsolutePath();
	    return ctaLoadPersistentState(FileName);
	}

    public IModelState saveInternalState(){
		return ctaSaveInternalState();
	}



	public void restoreInternalState(IModelState savedInternalState){
		if (savedInternalState instanceof CtaModelState){
			String IDstate = ((CtaModelState) savedInternalState).getID();
			ctaRestoreInternalState(IDstate);
		}
		else {
			throw new UnsupportedOperationException("savedInternalState is not an instance of CtaModelState");
		}
	}


	public void setParameters(IVector parameters) {
		if (parameters instanceof CtaTreeVector ){
 			ctaSetParameters(parameters);
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.setParameters not implemented yet for other types");
		}
	}


	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.setWhiteNoise(): Not implemented yet.");
	}

    public File getModelRunDir() {
        return null;
    }

    public void finish() {
        System.out.print("Finish is called for native model (handle="+this.ctaHandle+ ").\nFree function is called for this model\n");
		this.free();
		// no action needed (yet)
	}

    public void initialize(File workingDir, String[] arguments) {
        // no action needed (handled by model factory)
    }


	public int getNumDomains(double distance){
		return ctaGetNumDomains(distance);
	}

    public int[] getObservationSelector(IObservationDescriptions observationDescriptions, double distance, int iDomain){
		return ctaGetObservationSelector(observationDescriptions, distance, iDomain);
	}


    // Native implementations
	public native ITime getCurrentTime();
	public native ITime getTimeHorizon();
	public native void setAutomaticNoiseGeneration(boolean value);

	private native int ctaCreate(String ModelCls, String filename);
	private native void ctaAnnounceObservedValues(IObservationDescriptions observationDescriptions);
	private native void ctaAxpyOnParameters(double alpha, IVector vector);
	private native void ctaAxpyOnStateDomain(double alpha, IVector vector, int iDomain);
	private native void ctaCompute(ITime targetTime);
	private native void ctaGetObservedValues(IObservationDescriptions observationDescriptions, CtaVector values);
	private native IVector ctaGetState();
	private native IVector ctaGetStateDomain(int iDomain);
	private native IVector ctaGetParameters();
	private native IVector ctaGetStateScaling();
    private native void ctaReleaseInternalState(String IDstate);
    private native IModelState ctaLoadPersistentState(String persistentStateFile);
	private native IModelState ctaSaveInternalState();
	private native void ctaRestoreInternalState(String IDstate);
	private native int[] ctaGetObservedLocalization(IObservationDescriptions observationDescriptions, double distance);
	private native int[] ctaGetObservedLocalizationDomain(IObservationDescriptions observationDescriptions, double distance, int iDomain);
	private native void ctaSetParameters(IVector parameters);
	private native int ctaGetNumDomains(double distance);
	private native int[] ctaGetObservationSelector(IObservationDescriptions observationDescriptions, double distance, int iDomain);

}
