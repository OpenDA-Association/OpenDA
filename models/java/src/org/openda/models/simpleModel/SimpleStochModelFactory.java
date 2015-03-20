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
package org.openda.models.simpleModel;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;

import java.io.File;

/* ================================================================
 * OpenDa interfaces
 * ================================================================
 *
 * Project Info:  http://www.openda.org
 *
 * ----------------------------------------------------------------
 */




/**
 * Factory for creating abstract simpleStochModel instances. The simpleStochModel implements serveral
 * methods of the StochModel interface with simple default implementations. This is intended for
 * test models.
 */
public abstract class SimpleStochModelFactory implements IStochModelFactory {
	protected File workingDir=null;
	protected String[] arguments=null;
	
	
    /**
     * Initialize the factory. Here we only store the workingDir and configString and 
     * no work is done.
     * @param workingDir The working directory for the Stoch Model Factory
     * @param arguments One string, either with xml or with the filename of an xml-file
     * For a description of the format, see the simpleStochModelInstance
     */
    public void initialize(File workingDir, String[] arguments) {
       this.workingDir = workingDir;
       this.arguments = arguments;
    }

    /**
     * Abstract method to create a new instance. Each model that uses this class should override
     * with something like
     * return new MymodelStochModelInstance(this.workingDir, this.configString);
     * here.
	 * @param outputLevel
	 */
    public abstract IStochModelInstance getInstance(OutputLevel outputLevel);
        // override with something like:
    	//return new MymodelStochModelInstance(this.workingDir, this.configString);
    
    public String toString(){
    	String result = "SimpleStochModelFactory("+this.getClass().getName()+"){";
    	if(this.workingDir!=null){
    		result+="\n   "+workingDir.getAbsolutePath()+","+arguments[0]+"}";
    	}
    	result+="}";
    	return result;
    }

    
}
