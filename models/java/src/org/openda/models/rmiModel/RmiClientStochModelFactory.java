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

package org.openda.models.rmiModel;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.utils.ConfigTree;

import javax.management.RuntimeErrorException;
import java.io.File;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

/**
 * Model factory for RMI stoachastic models running on a remote Java virtual machine
 *
 * @author Nils van Velzen (VORtech)
 *
 */
public class RmiClientStochModelFactory implements IStochModelFactory {

    protected File workingDir    = null;
	protected String[] arguments = null;
    IRmiIStochModel[] rmiStaticMainModel; // instance to the remote model factory on each server
    Registry[] registry;                  // Registry (connection to all servers)
	int numServers=0;                     // Total number of servers we are using
	int lastCreatedOnServer=-1;           // Server (index) on which we have created the last instance

    public IStochModelInstance getInstance(OutputLevel outputLevel) {

        IRmiIStochModel rmiStochModel=null;

		// Determine which server we will use for the creation
		lastCreatedOnServer=(lastCreatedOnServer+1)%numServers;

        /* Create the model remotely and get the ID of the model in the registry */
        try{
           String registryID=rmiStaticMainModel[lastCreatedOnServer].getInstance(outputLevel);
           rmiStochModel = (IRmiIStochModel) registry[lastCreatedOnServer].lookup(registryID);
        }
        catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
            throw new RuntimeException("Error when we try to lookup model at Server");
        }
        return new RmiClientStochModelInstance(rmiStochModel);
    }

    public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new UnsupportedOperationException("org.openda.models.rmiModel.RmiClientStochModelFactory.getPostprocessorInstance(): Not implemented yet.");
	}

	public void finish() {
		// TODO: shut down?
	}

	public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        this.arguments = arguments;
		String[] hosts=null;
		String hostsCSV;
		int numHosts=0;

        String factoryIDCSV    = null;


		/* Get information from the configuration in order to connect to the correct server */
	   // Check the arguments and print some info on the arguments

		if (arguments.length!=1){
			throw new RuntimeException("Expecting only a single argument (configuration file)\n");
		}

		// Open and parse the model configuration file
		ConfigTree conf = new ConfigTree(workingDir, arguments[0]);

		// Get the factory IDs (related to the servers) to which I have to connect
		factoryIDCSV = conf.getAsString("factoryID","IRmiIStochModel");
		String[] factoryIDs = factoryIDCSV.split (",");
		numServers=factoryIDs.length;

		// Get the location (address) of machines hosting the servers
		hostsCSV = conf.getAsString("serverAddress","");

		// Set up a list of hosts for each server
		// Check whether the number of hosts corresponds to the number of servers
		// Note a single host and multiple servers is allowed (for easy configuration)
		hosts=new String[numServers];
		if (hostsCSV==""){
			numHosts=1;
			// Set all hosts to null (this mean local host in the getRegistry method)
			for (int iHost=0; iHost<numServers; iHost++){hosts[iHost]=null;}
		}
		else {
			numHosts=hostsCSV.split(",").length;
			if (numHosts>2 && numHosts!=numServers){
				String errStr="The number of hosts and servers does not correspond in your configuration input.\n"+
							  "We only support 1 host name and multiple servers or as many host names as servers\n"+
							  "You have selected "+numHosts+" hosts and "+numServers+" servers\n"+
							  "Please check and correct your input file\n";
				throw new RuntimeException(errStr);
			}
			if (numHosts==1){
			   for (int iHost=0; iHost<numServers; iHost++){hosts[iHost]=hostsCSV;}
			}
			else{
				hosts=hostsCSV.split(",");
			}
		}

		/* First make a binding to the static main instance on the Server */
		registry =new Registry[numServers];
		rmiStaticMainModel = new IRmiIStochModel[numServers];

		for (int iServer=0; iServer<numServers; iServer++){
			try{
				registry[iServer] = LocateRegistry.getRegistry(hosts[iServer]);
				rmiStaticMainModel[iServer] = (IRmiIStochModel) registry[iServer].lookup(factoryIDs[iServer]);
				/* Now initialize the remote factory such that we can create models on the Server */
				rmiStaticMainModel[iServer].factoryInitialize(workingDir, arguments);
			}
			catch (Exception e) {
				// For some reason we cannot connect to the server
				// Produce an error that will hopfully help the user
				System.err.println("Client exception: " + e.toString());
				e.printStackTrace();
				String thisHost=hosts[iServer];
				if (thisHost==null){thisHost="LOCAL HOST (=null)";}
				String errStr="We could not connect to the server\n"+
							  "FactoryID     = "+factoryIDs[iServer]+"\n"+
							  "serverAddress = "+thisHost;
				throw new RuntimeException(errStr);
		 }



        }

    }
}
