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
package org.openda.noiseModels;
import java.io.File;

import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.interfaces.ITime;

public class TimeSeriesNoiseModelFactory implements IStochModelFactory, ITimeHorizonConsumer {
	protected File workingDir=null;
	protected String[] arguments=null;
	private ITime timeHorizon = null;

	
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		this.arguments = arguments;
	}

	public void setTimeHorizon(ITime timeHorizon) {
		this.timeHorizon = timeHorizon;
	}

	
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		TimeSeriesNoiseModelInstance result = new TimeSeriesNoiseModelInstance(this.timeHorizon);
		result.outputLevel = outputLevel;
		result.initialize(this.workingDir, this.arguments);
		return result;
	}

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelFactory.getPostprocessorInstance(): Not implemented yet.");
	}

	
	public void finish() {
		// no action needed (yet)
	}

	public String toString(){
    	String result = "TimeSeriesNoiseModelFactory("+this.getClass().getName()+"){";
    	if(this.workingDir!=null){
    		result+="\n   "+workingDir.getAbsolutePath()+","+arguments[0]+"}";
    	}
    	result+="}";
    	return result;
    }
}
