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
package org.openda.algorithms.kalmanFilter;
import java.io.File;

import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

public class SequentialSimulation extends AbstractSequentialAlgorithm {

	public void initialize(File workingDir, String[] arguments) {
		super.initialize(workingDir, arguments);
		
	}
	
	@Override
	public void prepare() {
		// TODO Auto-generated method stub

	}

	@Override
	public void forecast(IStochObserver observations, ITime targetTime) {
		// TODO Auto-generated method stub

	}

	@Override
	public void analysis(IStochObserver observations, IVector obsValues, IVector predictions,
			IStochModelInstance mainModel, ITime analysisTime) {
		// This algorithm does not assimilate any data
	}

}
