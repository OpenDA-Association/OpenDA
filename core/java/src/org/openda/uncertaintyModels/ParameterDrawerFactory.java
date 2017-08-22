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
package org.openda.uncertaintyModels;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

public class ParameterDrawerFactory implements IStochModelFactory {

	protected File workingDir=null;
	protected String[] arguments=null;

	
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		this.arguments = arguments;
	}

	
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		ParameterDrawer result = new ParameterDrawer();
		result.initialize(this.workingDir, this.arguments);
		return result;
	}

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new UnsupportedOperationException("org.openda.noiseModels.ParameterDrawerFactory.getPostprocessorInstance(): Not implemented yet.");
	}

	
	public void finish() {
		// no action needed (yet)
	}

	public String toString(){
    	String result = this.getClass().getName()+"{";
    	if(this.workingDir!=null){
    		result+="\n   "+workingDir.getAbsolutePath();
    	}
		if (this.arguments != null) {
			for (String argument : arguments) {
				result += "," + argument;
			}
		}
		result+="}";
    	return result;
    }

}
