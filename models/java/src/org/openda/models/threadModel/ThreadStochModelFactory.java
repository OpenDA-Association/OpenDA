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

import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.ObjectSupport;
import org.openda.utils.OpenDaComponentConfig;

import java.io.File;

/**
 * Model factory an (interface) model that handles parallelization of model computations using threads.
 *
 * @author Nils van Velzen (VORtech)
 *
 */

public class ThreadStochModelFactory implements IStochModelFactory, ITimeHorizonConsumer {

    private static int lastParallelGroupID = -1;
    int [] parallelGroupID;
    int lastAssignedGroup                  = -1;
    int numThreadGroups                    =  1;
    int maxThreads                         = -1;
    protected File workingDir;
    protected String[] arguments;
    IStochModelFactory stochModelFactory;
    boolean cashState                      = false;
    boolean nonBlockingAxpy                = false;
    long sleepTime                         = 10;



    public IStochModelInstance getInstance(OutputLevel outputLevel) {
        IStochModelInstance childModelInstance = stochModelFactory.getInstance(outputLevel);
        this.lastAssignedGroup ++;
        int assingToGroup = this.lastAssignedGroup%numThreadGroups;
        IStochModelInstance newThreadModel = new ThreadStochModelInstance(childModelInstance, parallelGroupID[assingToGroup], maxThreads, cashState, nonBlockingAxpy, sleepTime);
        return newThreadModel;
    }

    public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void initialize(File workingDir, String[] arguments) {

        String className;
        String workingDirModelStr;
        File workingDirModel;
        String configFile;
        boolean configIsFile;


        this.workingDir = workingDir;
        this.arguments = arguments;

        // Open and parse the model configuration file
        ConfigTree conf = new ConfigTree(workingDir, arguments[0]);

        // Get the name of the model factory from the configuration tree
        className = conf.getAsString("stochModelFactory@className","");
        if ("".equals(className)){
           throw new RuntimeException("The className is empty. Please specify it correctly in:"+arguments[0]);
        }
        // Get the name of the model configuration file from th configuration tree
        workingDirModelStr = conf.getAsString("stochModelFactory/workingDirectory",".");
        if((new File(workingDirModelStr)).isAbsolute()){ 
    		workingDirModel = new File(workingDirModelStr);        	
        }else{
        	workingDirModel = new File(workingDir,workingDirModelStr);
        }

        // Get the name of the model factory from the configuration tree
        configFile = conf.getAsString("stochModelFactory/configFile","");

        // Get the number of thread groups
        numThreadGroups = conf.getAsInt("numThreadsGroups",1);

        // get max number of threads per group
        maxThreads = conf.getAsInt("maxThreads",4);

		// Get the sleep time between checking running threads (ms)
		sleepTime = (long) conf.getAsInt("sleepTime",10);

		// Get flag whether want to cash the state
		cashState = conf.getAsBoolean("cashState",false);

		// Get flag whether want to cash the state
		nonBlockingAxpy = conf.getAsBoolean("nonBlockingAxpy",false);

        // Setup the configuration */
        configIsFile=true;
        OpenDaComponentConfig configuration= new OpenDaComponentConfig(workingDirModel, className, new String[]{configFile}, configIsFile);

        // Deal with the parallel group ID's
		this.parallelGroupID = new int[numThreadGroups];
        for (int i=0; i<numThreadGroups; i++) {
            lastParallelGroupID++;
            parallelGroupID[i] = lastParallelGroupID;
        }

        // Lookup and create the modelFactory
        IConfigurable factoryInstance = ObjectSupport.createConfigurable("Stoch Model Factory: ", configuration);
        if (!(factoryInstance instanceof IStochModelFactory)) {
            throw new RuntimeException(factoryInstance.getClass().getName() +
                    " is not implementing the StochModelFactory interface");
        }
        stochModelFactory = (IStochModelFactory) factoryInstance;

		System.out.println("ThreadStochModelFactory configuration:");
		System.out.println("maxThreads      ="+maxThreads);
		System.out.println("numThreadGroups ="+numThreadGroups);
		System.out.println("cashState       ="+cashState);
		System.out.println("nonBlockingAxpy ="+nonBlockingAxpy);
		System.out.println("nonBlockingAxpy ="+sleepTime);


	}
	public void setTimeHorizon(ITime timeHorizon) {
		if (this.stochModelFactory instanceof ITimeHorizonConsumer) {
			((ITimeHorizonConsumer)this.stochModelFactory).setTimeHorizon(timeHorizon);
		}
	}

	public void finish() {
		if (stochModelFactory !=  null) {
			stochModelFactory.finish();
		}
	}
}
