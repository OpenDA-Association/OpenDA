/* MOD_V1.0
* Copyright (c) 2015 OpenDA Association
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
package org.openda.models.circularAdvection;

import org.openda.exchange.StringExchangeItem;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.interfaces.IVector;

import java.io.File;

/**
 * Created by strube on 8-6-15.
 */
public class CircularAdvectionFactory implements IStochModelFactory {

    int instanceCounter;
	File workingDir;
	String referenceFileName;

	@Override
	public IStochModelInstance getInstance(OutputLevel outputLevel) {

		CircularAdvectionInstance newModel=new CircularAdvectionInstance();
		String arguments[] = new String[2];
		arguments[0]= ""+this.instanceCounter;
		arguments[1] = this.referenceFileName;
		newModel.initialize(this.workingDir, arguments);
		this.instanceCounter++;
		return newModel;

	}

	@Override
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		return null;
	}

	@Override
	public void finish() {

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		this.instanceCounter=0;
		this.workingDir=workingDir;
		this.referenceFileName = arguments[0];
	}
}
