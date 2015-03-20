/* MOD_V1.0
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

package org.openda.models.rmiModel;

/**
 * Interface to RMI stoachastic model running on a remote Java virtual machine
 *
 * @author Nils van Velzen (VORtech)
 *
 */

import org.openda.interfaces.*;

import java.io.File;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IRmiIStochModel extends Remote {

	void setGlobalSettingsOnServer(double timePrecision, boolean VectorPrecisionIsFloat, boolean productionRun) throws RemoteException;
     // The methods from IStochModelInstance but now with RemoteException
    IVector getState() throws RemoteException;
    void axpyOnState(double alpha, IVector vector) throws RemoteException;
    IVector getParameters() throws RemoteException;
    void setParameters(IVector parameters) throws RemoteException;
    void axpyOnParameters(double alpha, IVector vector) throws RemoteException;
    IStochVector getStateUncertainty() throws RemoteException;
    IStochVector getParameterUncertainty() throws RemoteException;
    IStochVector[] getWhiteNoiseUncertainty(ITime time) throws RemoteException;
    boolean isWhiteNoiseStationary() throws RemoteException;
    ITime[] getWhiteNoiseTimes(ITime timeSpan) throws RemoteException;
    IVector[] getWhiteNoise(ITime timeSpan) throws RemoteException;
    void setWhiteNoise(IVector whiteNoise[]) throws RemoteException;
    void axpyOnWhiteNoise(double alpha, IVector vector[]) throws RemoteException;
    void setAutomaticNoiseGeneration(boolean value) throws RemoteException;
    IVector getObservedValues(IObservationDescriptions observationDescriptions) throws RemoteException;
    void announceObservedValues(IObservationDescriptions observationDescriptions) throws RemoteException;
    IVector getStateScaling() throws RemoteException;
    IVector[] getStateScaling(IObservationDescriptions observationDescriptions) throws RemoteException;


    // The methods from IModelInstance but now with RemoteException
	IPrevExchangeItem getExchangeItem(String exchangeItemID) throws RemoteException;
    ITime getTimeHorizon() throws RemoteException;
    ITime getCurrentTime() throws RemoteException;
    void compute(ITime targetTime) throws RemoteException;
    IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) throws RemoteException;
    IModelState saveInternalState() throws RemoteException;
    void restoreInternalState(IModelState savedInternalState) throws RemoteException;
    void releaseInternalState(IModelState savedInternalState) throws RemoteException;
	IModelState loadPersistentState(File persistentStateFile) throws RemoteException;
    File getModelRunDir() throws RemoteException;
	void finish() throws RemoteException;

    // The methods from IDataObjec but now with RemoteException
    String [] getExchangeItemIDs() throws RemoteException;
    String [] getExchangeItemIDs(IExchangeItem.Role role) throws RemoteException;
    IExchangeItem getDataObjectExchangeItem(String exchangeItemID) throws RemoteException;
    //void finish();

    // The methods from IConfigurable but now with RemoteException
    public void initialize(File workingDir, String[] arguments) throws RemoteException;

    // The methods from IInstance but now with RemoteException
    public IInstance getParent() throws RemoteException;


    /* We need to squeeze in the factory stuff as well since we can only
      communicate with the Client using the methods in this interface
    */

    /**
     * Initialize the model factory on the remote stochastic model
     * @return The stochastic Model instance
	 * @param workingDir
     * @param arguments
     */
    void factoryInitialize(File workingDir, String[] arguments) throws RemoteException;

     /**
     * Create an instance of a remote stochastic Model
     * @return The string that must be used to look-up the remote instance with
      *        java.rmi.registry.Registry.lookup
	 * @param outputLevel Amount of output that the new instance should produce.
     */
    String getInstance(IStochModelFactory.OutputLevel outputLevel) throws RemoteException;
}

