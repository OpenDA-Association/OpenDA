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
package org.openda.models.smootherModel;
import org.openda.blackbox.wrapper.BBStochModelFactory;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 8/20/12
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class smootherModelFactory implements IStochModelFactory {

    IStochModelFactory bbfac=null;
	
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		return new smootherModelInstance(bbfac.getInstance(outputLevel));
	}

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void finish() {
		// no action needed (yet)
	}

	
	public void initialize(File workingDir, String[] arguments) {
       bbfac= new BBStochModelFactory();
	   bbfac.initialize(workingDir,arguments);
	}
}
