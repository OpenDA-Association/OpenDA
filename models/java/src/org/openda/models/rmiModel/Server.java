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
 * Implementation of the Server that contains the instances of a Stochastic Model that is accessed using
 * RMI from a remote (Client) Java virtual machine
 *
 * @author Nils van Velzen (VORtech)
 *
 */

import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.DistributedCounter;
import org.openda.utils.ObjectSupport;
import org.openda.utils.OpenDaComponentConfig;
import org.openda.utils.performance.OdaGlobSettings;

import java.io.File;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;

public class Server implements IRmiIStochModel{

    static IStochModelFactory stochModelFactory;
    static int nModels=0;
    static ArrayList allStochModels = null; //Static list to avoid garbage collector to remove instnaces
    IStochModelInstance stochModel;
	static String FactoryBindingID="IRmiIStochModel";

    public static void main(String args[]) {


        if (args.length == 0){
           System.out.print("No initial binding ID is specified we will use the default\n");
        }
        else if (args.length == 1){
           FactoryBindingID=args[0];
        }
		else if (args.length == 3){
			FactoryBindingID=args[0];
			int offset    = Integer.valueOf(args[1]).intValue();
			int increment = Integer.valueOf(args[2]).intValue();
            if (increment<1){
				throw new RuntimeException("3rd argument (increment="+increment+") should be >0. Value is ");
			}
			if (offset<0){
				throw new RuntimeException("2nd argument (offset="+offset+") should be >=0. Value is ");
			}
			if (offset>=increment){
				throw new RuntimeException("2nd argument (offset="+offset+") should be <increment (="+increment+") (3rd argument)");
			}
            System.out.print("Initialized DistributedCounter offset    = "+offset+"\n");
			System.out.print("Initialized DistributedCounter increment = "+increment+"\n");

			DistributedCounter.offset    =  offset;
			DistributedCounter.increment =  increment;

		}
        else {
            System.out.print("OpenDA RMI server\n");
            System.out.print("usage: java  -Djava.rmi.server.codebase=file:///$OPENDADIR/ org.openda.models.rmiModel.Server [ID] [offset] [increment]\n");
            System.out.print("ID initial binding ID for model factory\n");
			System.out.print("offset number of this server in group of servers used\n");
			System.out.print("increment total number of servers used\n");
            System.exit(1);
        }
        System.out.print("we will use the binding ID :"+FactoryBindingID+"\n");

	    try {
            allStochModels = new ArrayList();

	        Server obj = new Server();
	        IRmiIStochModel stub = (IRmiIStochModel) UnicastRemoteObject.exportObject(obj, 0);

	        // Bind the remote object's stub in the registry
	        Registry registry = LocateRegistry.getRegistry();
	        registry.bind(FactoryBindingID, stub);

	        System.err.println("Server ready");
	    } catch (Exception e) {
	        System.err.println("Server exception: " + e.toString());
	        e.printStackTrace();
	    }
    }

	public void setGlobalSettingsOnServer(double timePrecision, boolean VectorPrecisionIsFloat, boolean productionRun){
		System.out.println("Setting global settings on Server");
		System.out.println("TimePrecision          ="+timePrecision);
		System.out.println("VectorPrecisionIsFloat ="+VectorPrecisionIsFloat);
		System.out.println("ProductionRun          ="+productionRun);
		OdaGlobSettings.setTimePrecision(timePrecision);
		OdaGlobSettings.setVectorPrecisionFloat(VectorPrecisionIsFloat);
		OdaGlobSettings.setProductionRun(productionRun);
	}

    public IVector getState() {
//		IVector stateModel=stochModel.getState();
//        return new Vector(stateModel.getValues());
       return stochModel.getState();
    }

    public void axpyOnState(double alpha, IVector vector) {
        stochModel.axpyOnState(alpha, vector);
    }

    public IVector getParameters() {
        return stochModel.getParameters();
    }

    public void setParameters(IVector parameters) {
        stochModel.setParameters(parameters);
    }

    public void axpyOnParameters(double alpha, IVector vector) {
        stochModel.axpyOnParameters(alpha, vector);
    }

    public IStochVector getStateUncertainty() {
        return stochModel.getStateUncertainty();
    }

    public IStochVector getParameterUncertainty() {
        return stochModel.getParameterUncertainty();
    }

    public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
        return stochModel.getWhiteNoiseUncertainty(time);
    }

    public boolean isWhiteNoiseStationary() {
        return stochModel.isWhiteNoiseStationary();
    }

    public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
        return stochModel.getWhiteNoiseTimes(timeSpan);
    }

    public IVector[] getWhiteNoise(ITime timeSpan) {
        return stochModel.getWhiteNoise(timeSpan);
    }

    public void setWhiteNoise(IVector[] whiteNoise) {
        stochModel.setWhiteNoise(whiteNoise);
    }

    public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
        stochModel.axpyOnWhiteNoise(alpha, vector);
    }

    public void setAutomaticNoiseGeneration(boolean value) {
        stochModel.setAutomaticNoiseGeneration(value);
    }

    public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
        return stochModel.getObservedValues(observationDescriptions);
    }

    public void announceObservedValues(IObservationDescriptions observationDescriptions) {
        stochModel.announceObservedValues(observationDescriptions);
    }

    public IVector getStateScaling() {
        return stochModel.getStateScaling();
    }

    public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
        return getStateScaling(observationDescriptions);
    }

    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        return stochModel.getExchangeItem(exchangeItemID);
    }

    public ITime getTimeHorizon() {
        return stochModel.getTimeHorizon();
    }

    public ITime getCurrentTime() {
        return stochModel.getCurrentTime();
    }

    public void compute(ITime targetTime) {
        stochModel.compute(targetTime);
    }

    public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
        return stochModel.getObservedLocalization(observationDescriptions, distance);
    }

    public IModelState saveInternalState() {
        return stochModel.saveInternalState();
    }

    public void restoreInternalState(IModelState savedInternalState) {
        stochModel.restoreInternalState(savedInternalState);
    }

    public void releaseInternalState(IModelState savedInternalState) {
        stochModel.releaseInternalState(savedInternalState);
    }

    public IModelState loadPersistentState(File persistentStateFile) {
        return stochModel.loadPersistentState(persistentStateFile);
    }

    public File getModelRunDir() {
        return stochModel.getModelRunDir();
    }

    public String[] getExchangeItemIDs() {
        return stochModel.getExchangeItemIDs();
    }

    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
        return stochModel.getExchangeItemIDs(role);
    }

    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        return stochModel.getDataObjectExchangeItem(exchangeItemID);
    }

    public void finish() {
        stochModel.finish();
    }

    public void initialize(File workingDir, String[] arguments) {
        stochModel.initialize(workingDir, arguments);
    }

    public IInstance getParent() {
        return stochModel.getParent();
    }

    public void factoryInitialize(File workingDir, String[] arguments) {
        String className          =null; // Class name of the model that is created on the server
        String workingDirModelStr =null; // Working dir (of input files) of the model on the server
        File workingDirModel;
        String configFile         =null; // name of the model configuration file for the model

        boolean configIsFile;


        // Check the arguments and print some info on the arguments
        System.out.print("Debug: RMI factory Initialize arguments are:\n");
        System.out.print("workdir :"+workingDir+"\n");
        for (int i=0; i<arguments.length; i++){
            System.out.print("argument "+i+" :"+arguments[i]);
        }
        if (arguments.length!=1){
            System.err.print("Expecting only a single argument (workdir)\n");
            System.exit(-1);
        }

        // Open and parse the model configuration file
        ConfigTree conf = new ConfigTree(workingDir, arguments[0]);

        // Get the name of the model factory from the configuration tree
        className = conf.getAsString("stochModelFactory@className","");
        if (className==""){
           throw new RuntimeException("The className is empty. Please specify it correctly in:"+arguments[0]);
        }
        // Get the name of the model configuration file from th configuration tree
        workingDirModelStr = conf.getAsString("stochModelFactory/workingDirectory",".");
        workingDirModel = new File(workingDirModelStr);

        // Get the name of the model factory from the configuration tree
        configFile = conf.getAsString("stochModelFactory/configFile","");

        // Setup the configuration */
        configIsFile=true;
        OpenDaComponentConfig configuration= new OpenDaComponentConfig(workingDirModel, className, new String[]{configFile}, configIsFile);

        // Lookup and create the modelFactory
        IConfigurable factoryInstance = ObjectSupport.createConfigurable("Stoch Model Factory: ", configuration);
        if (!(factoryInstance instanceof IStochModelFactory)) {
            throw new RuntimeException(factoryInstance.getClass().getName() +
                    " is not implementing the StochModelFactory interface");
        }
        stochModelFactory = (IStochModelFactory) factoryInstance;

       /* Initialize this modelfactory */
       factoryInstance.initialize(workingDirModel, new String[]{configFile});
    }

    private void setModel(IStochModelInstance stochModelInstance){
        stochModel=stochModelInstance;
    }

    public String getInstance(IStochModelFactory.OutputLevel outputLevel)  {
        String instanceID=null;

	    try {
            nModels++;
            instanceID= FactoryBindingID+"_IRmiIStochModel_"+nModels;

            // Create a new model instance
            IStochModelInstance newStochModel = stochModelFactory.getInstance(outputLevel);
            // Create a new server instance and set the new model for this instance
            Server obj = new Server();
            obj.setModel(newStochModel);

   	        IRmiIStochModel stub = (IRmiIStochModel) UnicastRemoteObject.exportObject(obj, 0);

            // Register this insntance such that it will not be deleted.
            allStochModels.add(stub);

           // Bind the remote object's stub in the registry
	        Registry registry = LocateRegistry.getRegistry();
	        registry.bind(instanceID, stub);

	        System.err.println("Server has created model "+instanceID);
	    } catch (Exception e) {
	        System.err.println("Server exception when creating new model: " + e.toString());
	        e.printStackTrace();
	    }
        return instanceID;
    }
}
