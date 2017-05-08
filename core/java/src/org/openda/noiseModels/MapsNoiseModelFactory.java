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
package org.openda.noiseModels;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.interfaces.ITime;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * Module for modeling spatially and temporally correlated noise in 2d.
 * A gaussian spatial correlation is assumed and exponential temporal correlation.
 * The probablities are all Gaussian.
 * 
 * @author verlaanm
 *
 */
public class MapsNoiseModelFactory implements IStochModelFactory, ITimeHorizonConsumer {
	protected File workingDir=null;
	protected String[] arguments=null;
	protected ITime timeHorizon = null;
	private Map<String, SpatialCorrelationCovariance> eiCovarianceMap;

	
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		this.arguments = arguments;
		this.eiCovarianceMap = new HashMap<>();
	}

	public void setTimeHorizon(ITime timeHorizon) {
		this.timeHorizon = timeHorizon;
	}

	
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		MapsNoiseModelInstance result = new MapsNoiseModelInstance(this.timeHorizon);
		result.outputLevel = outputLevel;
		result.setEiCovarianceMap(eiCovarianceMap);
		result.initialize(this.workingDir, this.arguments);
		return result;
	}

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoiseModelFactory.getPostprocessorInstance(): Not implemented yet.");
	}

	
	public void finish() {
		// no action needed (yet)
	}

	public String toString(){
    	String result = "MapsNoiseModelFactory("+this.getClass().getName()+"){";
    	if(this.workingDir!=null){
    		result+="\n   "+workingDir.getAbsolutePath()+","+arguments[0]+"}";
    	}
    	result+="}";
    	return result;
    }
}
