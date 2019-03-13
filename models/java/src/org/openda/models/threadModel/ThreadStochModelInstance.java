/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association
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
package org.openda.models.threadModel;

import org.openda.interfaces.*;

import java.io.File;
import java.lang.Thread;
import java.util.Vector;

/**
 * Model implementation of the (interface) model that handles parallelization of model computations using threads.
 *
 * @author Nils van Velzen (VORtech)
 *
 */
public class ThreadStochModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated{

    /* In a single configuration it possible that various models make use
       of various instances of this class for different model(factories).
       The static variables should therefore support this.
       Each model factory of has it's own ID (an index in the static vectors)
      */


    static Vector<ThreadStochModelAdmin> threadAdmins                 = null;  /* Thread administration for all factories that are
                                                            handled by model                                   */
    ThreadStochModelCompute threadModelCompute   = null ;           /* Thread of the compute method (of this instance)  */
	ThreadStochModelGetState threadModelGetState = null ;           /* Thread of the getState method (of this instance) */
    ThreadStochModelAxpyOnState threadStochModelAxpyOnState = null; /* Thread of the getState method (of this instance) */
	int threadID                                 = -1;    /* Thead ID of the compute method of this instance in
                                                             the thread administrsion                           */
    IStochModelInstance threadModel              = null;  /* The instance of the model that is threaded using
                                                             this (interface) model                             */

    int factoryID                                = -1;    /* The factory ID in the thread adminstration of
                                                             factory that created this (threadModel) instance   */

	IVector cashedState = null; /* The state of this model instance (already cashed) */

	boolean cashState=false;
	boolean nonBlockingAxpy =false;
	long sleepTime;
	private ThreadStochModelFinish threadModelFinish;

	public ThreadStochModelInstance(IStochModelInstance model, int factoryID, int maxThreads,
									boolean cashState, boolean nonBlockingAxpy, long sleepTime){

        this.factoryID=factoryID;
		this.cashState=cashState;
		this.nonBlockingAxpy = nonBlockingAxpy;

        // Allocate/increase the static administration arrays
        if (threadAdmins==null){
            threadAdmins = new Vector<ThreadStochModelAdmin>();
            ThreadStochModelAdmin admin = new ThreadStochModelAdmin(maxThreads, sleepTime);
            threadAdmins.add(admin);
        }
        else
        if (factoryID>=threadAdmins.size()){
            threadAdmins.setSize(factoryID+1);
            ThreadStochModelAdmin admin = new ThreadStochModelAdmin(maxThreads, sleepTime);
            threadAdmins.set(factoryID,admin);
        }
        this.threadModel        = model;
        this.threadModelCompute = null;
		this.cashedState        = null;
        this.threadID           = -1;
		this.sleepTime          = sleepTime;

		System.out.println("Creating model instance of ThreadStochModelInstance");
		System.out.println("factoryID      = "+this.factoryID);
		System.out.println("cashState      = "+this.cashState);
		System.out.println("maxThreads     = "+maxThreads);
		System.out.println("sleepTime (ms) = "+this.sleepTime);


    }

	public IVector getState(int iDomain) {
		return this.getState();
	}

	public IVector getState() {
		IVector retVec=null;
        block();
		if (this.cashedState!=null){
			retVec=cashedState;
			cashedState=null;
		}
		else {
           retVec=threadModel.getState();
		}
		return retVec;
    }

    public void axpyOnState(double alpha, IVector vector) {
        block();
		if (nonBlockingAxpy) {
			threadStochModelAxpyOnState = new ThreadStochModelAxpyOnState(this.threadModel,alpha, vector);
			threadStochModelAxpyOnState.run();
		}
		else {
			threadModel.axpyOnState(alpha, vector);
		}
    }

	public void axpyOnState(double alpha, IVector change, int iDomain) {
		// TODO Auto-generated method stub

	}


	public IVector getParameters() {
        block();
        return threadModel.getParameters();
    }

    
    public void setParameters(IVector parameters) {
        block();
        threadModel.setParameters(parameters);
    }

    
    public void axpyOnParameters(double alpha, IVector vector) {
        block();
        threadModel.axpyOnParameters(alpha, vector);
    }

    
    public IStochVector getStateUncertainty() {
        block();
        return threadModel.getStateUncertainty();
    }

    
    public IStochVector getParameterUncertainty() {
        block();
        return threadModel.getParameterUncertainty();
    }

    
    public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
        block();
        return threadModel.getWhiteNoiseUncertainty(time);
    }

    
    public boolean isWhiteNoiseStationary() {
        block();
        return threadModel.isWhiteNoiseStationary();
    }

    
    public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
        block();
        return threadModel.getWhiteNoiseTimes(timeSpan);
    }

    
    public IVector[] getWhiteNoise(ITime timeSpan) {
        block();
        return threadModel.getWhiteNoise(timeSpan);
    }

    
    public void setWhiteNoise(IVector[] whiteNoise) {
        block();
        threadModel.setWhiteNoise(whiteNoise);
    }

    
    public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
        block();
        threadModel.axpyOnWhiteNoise(alpha, vector);
    }

    
    public void setAutomaticNoiseGeneration(boolean value) {
        block();
        threadModel.setAutomaticNoiseGeneration(value);
    }


	public IObservationOperator getObservationOperator(){
		block();
		return threadModel.getObservationOperator();
	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
        block();
        return threadModel.getObservationOperator().getObservedValues(observationDescriptions);
    }

    
    public void announceObservedValues(IObservationDescriptions observationDescriptions) {
        block();
        threadModel.announceObservedValues(observationDescriptions);
    }

    
    public IVector getStateScaling() {
        block();
        return threadModel.getStateScaling();
    }

    
    public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
        block();
        return threadModel.getStateScaling(observationDescriptions);
    }

    
    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        block();
        return threadModel.getExchangeItem(exchangeItemID);
    }

    public ITime getTimeHorizon() {
        block();
        return threadModel.getTimeHorizon();
    }

    
    public ITime getCurrentTime() {
        block();
        return threadModel.getCurrentTime();
    }

    
    public void compute(ITime targetTime) {
		block();
        ThreadStochModelAdmin admin = (ThreadStochModelAdmin) threadAdmins.get(factoryID);
		threadID=admin.waitUntilFreeThread();
		threadModelCompute = new ThreadStochModelCompute(threadModel, targetTime);
		threadModelCompute.start();
		admin.setThread(threadID, (Thread) threadModelCompute);

		// Start Thread for the getState method
		if (cashState){
			this.threadModelGetState = new ThreadStochModelGetState(threadModel, threadModelCompute);
			this.threadModelGetState.start();
		}
    }


	public ILocalizationDomains getLocalizationDomains(){
		block();
		return threadModel.getLocalizationDomains();
	}


	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		block();
		return threadModel.getObservedLocalization(observationDescriptions, distance);
	}


	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		block();
        return threadModel.getObservedLocalization(observationDescriptions, distance, iDomain);
    }

    
    public IModelState saveInternalState() {
        block();
        return threadModel.saveInternalState();
    }

    
    public void restoreInternalState(IModelState savedInternalState) {
        block();
        threadModel.restoreInternalState(savedInternalState);
    }

    
    public void releaseInternalState(IModelState savedInternalState) {
        block();
        threadModel.releaseInternalState(savedInternalState);
    }

    
    public IModelState loadPersistentState(File persistentStateFile) {
        block();
        return threadModel.loadPersistentState(persistentStateFile);
    }

    
    public File getModelRunDir() {
        block();
        return threadModel.getModelRunDir();
    }

    
    public String[] getExchangeItemIDs() {
        block();
        return threadModel.getExchangeItemIDs();
    }

    
    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
        block();
        return threadModel.getExchangeItemIDs(role);
    }

    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        return threadModel.getDataObjectExchangeItem(exchangeItemID);
    }

    
    public void finish() {
        block();
		System.out.println("Calling finish of ThreadStochModelInstance");
		ThreadStochModelAdmin admin = threadAdmins.get(factoryID);
		threadID = admin.waitUntilFreeThread();
		threadModelFinish = new ThreadStochModelFinish(threadModel, threadID);
		threadModelFinish.start();
		admin.setThread(threadID, threadModelFinish);
    }

    
    public void initialize(File workingDir, String[] arguments) {
        block();
        threadModel.initialize(workingDir, arguments);
    }

    
    public IInstance getParent() {
        block();
        return threadModel.getParent();
    }

    private void block(){
        if (threadModelCompute != null){
			ThreadStochModelAdmin admin = (ThreadStochModelAdmin) threadAdmins.get(factoryID);
			threadModelCompute =  (ThreadStochModelCompute) admin.waitForThread(threadID, threadModelCompute);
        }
		if (this.threadModelGetState != null){
			// Wait for the thread to finish
			System.out.println("Waiting for the parallel getState Method to be done");
			try {
				this.threadModelGetState.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			this.cashedState = threadModelGetState.getModelState();
			this.threadModelGetState=null;
		}
		if (this.threadStochModelAxpyOnState !=null){
			// Wait for the thread to finish
			System.out.println("Waiting for the parallel getState Method to be done");
			try {
				this.threadStochModelAxpyOnState.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
    }
}
