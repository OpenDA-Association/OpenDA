/* MOD_V1.0
* Copyright (c) 2011 OpenDA Association
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

package org.openda.models.rmiModel;

/**
 * Model implementation for RMI stoachastic models running on a remote Java virtual machine
 *
 * @author Nils van Velzen (VORtech)
 *
 */

import org.openda.interfaces.*;
import org.openda.observers.SerializableObservationDescriptions;
import org.openda.utils.DistributedCounter;
import org.openda.utils.performance.OdaGlobSettings;
import org.openda.utils.performance.OdaTiming;

import java.io.File;
import java.io.Serializable;

public class RmiClientStochModelInstance implements IStochModelInstance{

	// Timing of methods in this class
	static DistributedCounter lastGlobInstanceNr = new DistributedCounter();
	static boolean cashModelState=false;

	int InstanceNr;
	IVector cashedModelState;      //The state is cashed
	boolean cashedStateIsUpdated;  //Flag indicating whether the state has been updated
	boolean stateNotUpdated;       //

	String ModelID;
	//OdaTiming timerCreate     = null;
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
	OdaTiming timerSetParam   = null;
	OdaTiming timerGetScaling = null;
	OdaTiming timerTest       = null;


    /* Host that holds the real instance */
    String host=null;
    IRmiIStochModel rmiModel=null;

    public RmiClientStochModelInstance(IRmiIStochModel rmiModelIn) {
		/* Set modelID and instance Nr */
		synchronized(lastGlobInstanceNr){
		  lastGlobInstanceNr.inc();
		  InstanceNr = lastGlobInstanceNr.val();
		  ModelID="RMI Model:"+InstanceNr;
		}
        this.rmiModel=rmiModelIn;
		this.cashedModelState=null;
		this.cashedStateIsUpdated=false;
		this.stateNotUpdated=false;
		/* Handle global settings */
		try {
			this.rmiModel.setGlobalSettingsOnServer(OdaGlobSettings.getTimePrecision(),
					                                OdaGlobSettings.getVectorPrecisionFloat(),
													OdaGlobSettings.getProductionRun());
		      	  	}
		catch (Exception e) {
			System.err.println("Client exception: " + e.toString());
		    e.printStackTrace();
		    throw new RuntimeException("Error calling remote method");
		}
	}


    public IVector getState() {
		if ( timerGetState == null){ timerGetState = new OdaTiming(ModelID);}
		timerGetState.start();

        IVector retVal=null;
        if (stateNotUpdated && cashedModelState!=null && cashModelState){
		   System.out.println("Debug:return cashed state");
		   retVal=cashedModelState;
		}
		else {
			System.out.println("Debug:return state from model");
	         try {
    	       retVal=rmiModel.getState();
				cashedModelState=rmiModel.getState();
      	  	}
        	catch (Exception e) {
           		System.err.println("Client exception: " + e.toString());
           		e.printStackTrace();
           		throw new RuntimeException("Error calling remote method");
        	}
		}
		timerGetState.stop();
        return retVal;
    }


    public void axpyOnState(double alpha, IVector vector) {
		if ( timerAxpyState == null){ timerAxpyState = new OdaTiming(ModelID);}
		timerAxpyState.start();

       try {
           rmiModel.axpyOnState(alpha, vector);
		   stateNotUpdated=false;
        }
        catch (Exception e) {
           System.err.println("Client exception: " + e.toString());
           e.printStackTrace();
           throw new RuntimeException("Error calling remote method");
        }

		timerAxpyState.stop();
    }


    public IVector getParameters() {
		if ( timerGetParam == null){ timerGetParam = new OdaTiming(ModelID);}
		timerGetParam.start();

        IVector retVal=null;
        try {
            retVal=rmiModel.getParameters();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }

		 timerGetParam.stop();

         return retVal;
    }


    public void setParameters(IVector parameters) {
		if ( timerSetParam  == null){ timerSetParam = new OdaTiming(ModelID);}
		timerSetParam.start();

        try {
            rmiModel.setParameters(parameters);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }

		timerSetParam.stop();
    }

    public void axpyOnParameters(double alpha, IVector vector) {
		if ( timerAxpyParam  == null){ timerAxpyParam = new OdaTiming(ModelID);}
		timerAxpyParam.start();

        try {
            rmiModel.axpyOnParameters(alpha,vector);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }
		timerAxpyParam.stop();
    }

    public IStochVector getStateUncertainty() {

        IStochVector retVal=null;
        try {
            retVal=rmiModel.getStateUncertainty();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public IStochVector getParameterUncertainty() {
        IStochVector retVal=null;
        try {
            retVal=rmiModel.getParameterUncertainty();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
        IStochVector retVal[]=null;
        try {
            retVal=rmiModel.getWhiteNoiseUncertainty(time);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public boolean isWhiteNoiseStationary() {
        boolean retVal=false;
        try {
            retVal=rmiModel.isWhiteNoiseStationary();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
        ITime retVal[]=null;
        try {
            retVal=rmiModel.getWhiteNoiseTimes(timeSpan);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public IVector[] getWhiteNoise(ITime timeSpan) {
        IVector retVal[]=null;
        try {
            retVal=rmiModel.getWhiteNoise(timeSpan);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public void setWhiteNoise(IVector[] whiteNoise) {
        try {
            rmiModel.setWhiteNoise(whiteNoise);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
    }

    public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
        try {
            rmiModel.axpyOnWhiteNoise(alpha,vector);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
    }

    public void setAutomaticNoiseGeneration(boolean value) {
         try {
            rmiModel.setAutomaticNoiseGeneration(value);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }    }

    public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		if ( timerGetObs == null){ timerGetObs = new OdaTiming(ModelID);}
		timerGetObs.start();

        IVector retVal=null;
		IObservationDescriptions serObservationDescriptions =
			makeSerializableObservationDescriptions(observationDescriptions);
        try {
            retVal=rmiModel.getObservedValues(serObservationDescriptions);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }

		timerGetObs.stop();

        return retVal;
    }

    public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		if ( timerAnnounce == null){ timerAnnounce = new OdaTiming(ModelID);}
		timerAnnounce.start();

		IObservationDescriptions serObservationDescriptions =
			makeSerializableObservationDescriptions(observationDescriptions);
         try {
            rmiModel.announceObservedValues(serObservationDescriptions);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
		timerAnnounce.stop();
    }

    public IVector getStateScaling() {
		if ( timerGetScaling == null){ timerGetScaling = new OdaTiming(ModelID);}
		timerGetScaling.start();

        IVector retVal=null;
        try {
            retVal=rmiModel.getStateScaling();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
		timerGetScaling.stop();

        return retVal;
   }

    public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		if ( timerGetScaling == null){ timerGetScaling = new OdaTiming(ModelID);}
		timerGetScaling.start();

        IVector retVal[]=null;
		IObservationDescriptions serObservationDescriptions =
			makeSerializableObservationDescriptions(observationDescriptions);
        try {
            retVal=rmiModel.getStateScaling(serObservationDescriptions);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        timerGetScaling.stop();
        return retVal;
    }

    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        IPrevExchangeItem retVal=null;
        try {
            retVal=rmiModel.getExchangeItem(exchangeItemID);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public ITime getTimeHorizon() {
        ITime retVal=null;
        try {
            retVal=rmiModel.getTimeHorizon();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public ITime getCurrentTime() {
        ITime retVal=null;
        try {
            retVal=rmiModel.getCurrentTime();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public void compute(ITime targetTime) {
		if ( timerCompute == null){ timerCompute = new OdaTiming(ModelID);}
		timerCompute.start();

         try {
            rmiModel.compute(targetTime);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }

        timerCompute.stop();
    }

    public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
        IVector retVal[]=null;
		IObservationDescriptions serObservationDescriptions =
			makeSerializableObservationDescriptions(observationDescriptions);
        try {
            retVal=rmiModel.getObservedLocalization(serObservationDescriptions, distance);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public IModelState saveInternalState() {
        IModelState retVal=null;
        try {
            retVal=rmiModel.saveInternalState();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public void restoreInternalState(IModelState savedInternalState) {
        try {
            rmiModel.restoreInternalState(savedInternalState);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
    }

    public void releaseInternalState(IModelState savedInternalState) {
         try {
            rmiModel.releaseInternalState(savedInternalState);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
    }

    public IModelState loadPersistentState(File persistentStateFile) {
         IModelState retVal=null;
          try {
            retVal=rmiModel.loadPersistentState(persistentStateFile);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

    public File getModelRunDir() {
         File retVal=null;
         try {
            retVal=rmiModel.getModelRunDir();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }
         return retVal;
    }

    public String[] getExchangeItemIDs() {
         String retVal[]=null;
         try {
            retVal=rmiModel.getExchangeItemIDs();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }
         return retVal;
    }

    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
         String retVal[]=null;
         try {
            retVal=rmiModel.getExchangeItemIDs(role);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }
         return retVal;
    }

    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
         IExchangeItem retVal=null;
         try {
            retVal=rmiModel.getDataObjectExchangeItem(exchangeItemID);
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }
         return retVal;
    }

    public void finish() {
         try {
            rmiModel.finish();
         }
         catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error calling remote method");
         }
    }

    public void initialize(File workingDir, String[] arguments) {
        try {
           rmiModel.initialize(workingDir, arguments);
        }
        catch (Exception e) {
           System.err.println("Client exception: " + e.toString());
           e.printStackTrace();
           throw new RuntimeException("Error calling remote method");
        }
    }

    public IInstance getParent() {
        IInstance retVal=null;
        try {
           retVal=rmiModel.getParent();
        }
        catch (Exception e) {
           System.err.println("Client exception: " + e.toString());
           e.printStackTrace();
           throw new RuntimeException("Error calling remote method");
        }
        return retVal;
    }

	private IObservationDescriptions makeSerializableObservationDescriptions(IObservationDescriptions observationDescriptions){
		IObservationDescriptions serObservationDescriptions;

        /* Copy content of the observationDescriptions is not serializable
         * otherwise we can use the input observationDescriptions */
        if (observationDescriptions instanceof Serializable){
			 serObservationDescriptions=observationDescriptions;
		}
		else {
			serObservationDescriptions = new SerializableObservationDescriptions(observationDescriptions);
		}
		return serObservationDescriptions;
	}
}
