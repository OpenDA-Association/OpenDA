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
package org.openda.models.biasAwareObservations;

import org.openda.interfaces.IConfigurable;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.utils.ConfigTree;
import org.openda.utils.ObjectSupport;
import org.openda.utils.OpenDaComponentConfig;

import java.io.File;
import java.util.HashMap;

/**
 * Model for modelling bias in the observed values
 *
 * @author Nils van Velzen (TU-Delft/VORtech)
 *
 */

public class BiasAwareObservationsModelFactory implements IStochModelFactory {

	IStochModelFactory stochModelFactory = null;
	double std;
	HashMap<String, Double> obsIDstd= new HashMap<String, Double>();
	int maxBias;
	boolean checkObservationID;
	boolean localization;


	
	public IStochModelInstance getInstance(OutputLevel outputLevel) {

		return new BiasAwareObservationsModelInstance(stochModelFactory.getInstance(outputLevel),this.std,
				      this.obsIDstd, this.maxBias, this.checkObservationID, this.localization);
	}

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void finish() {
		// no action needed (yet)
	}

	
	public void initialize(File workingDir, String[] arguments) {
		String className;
		String workingDirModelStr;
		File workingDirModel;
		String configFile;
		boolean configIsFile;


		// Open and parse the model configuration file
		ConfigTree conf = new ConfigTree(workingDir, arguments[0]);

		// Get the name of the model factory from the configuration tree
		className = conf.getAsString("stochModelFactory@className","");
		if (className.equals("")){
			throw new RuntimeException("The className is empty. Please specify it correctly in:"+arguments[0]);
		}
		// Get the name of the model configuration file from the configuration tree
		workingDirModelStr = conf.getAsString("stochModelFactory/workingDirectory",".");
		if((new File(workingDirModelStr)).isAbsolute()){
			workingDirModel = new File(workingDirModelStr);
		}else{
			workingDirModel = new File(workingDir,workingDirModelStr);
		}

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

		// Check existence of state
		if (conf.getSubTrees("state/observation")==null){
			throw new RuntimeException("Mandatory XML element state has not been specified");
		}

		this.std = conf.getAsDouble("state@standard_deviation",Double.MIN_VALUE);
		this.maxBias = conf.getAsInt("state@maxSize",0);
		this.checkObservationID = conf.getAsBoolean("state@checkObservationID",true);
		this.localization = conf.getAsBoolean("state@localization",true);

		ConfigTree[] subTrees =conf.getSubTrees("state/observation");
		if (subTrees != null){
			for (int iSub=0; iSub<subTrees.length; iSub++){
				String id=subTrees[iSub].getAsString("@id","");
				if (id.equals("")){
					throw new RuntimeException("Parsing properties of specified observation "+iSub+1+". The attribute id is mandatory");
				}
				double stdObs=subTrees[iSub].getAsDouble("@standard_deviation",Double.MIN_VALUE);
				if (stdObs==Double.MIN_VALUE){
					throw new RuntimeException("Parsing properties of specified observation "+iSub+1+". The attribute standard_deviation is mandatory");
				}
				obsIDstd.put(id,stdObs);
			}
		}
		if (!checkObservationID && obsIDstd.size()>0){
			throw new RuntimeException("You cannot set checkObservationID=\"false\" and configure individual observations at the same time");
		}

		if (maxBias<=0 && obsIDstd.size() <=0){
			throw new RuntimeException("The augmented state for modelling bias has zero length");
		}
		if (maxBias>obsIDstd.size() && std==Double.MIN_VALUE){
			throw new RuntimeException("You have not specified a default standard deviation (attribute: <state standard_deviation=\"..\"..)");
		}

	}
}
