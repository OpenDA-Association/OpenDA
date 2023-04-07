/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.externalsocket;
import org.openda.algorithms.Dud;
import org.openda.blackbox.wrapper.BBStochModelInstance;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelInstance;

public class ExternalSocketDudAlgorithm extends Dud {

	private IStochModelInstance currentBestEstimate;
	private double[] currentBestStdValues;

	@Override
	public void next() {
		super.next();
		if (this.hasNext()) {
			if (this.bestEstimate != currentBestEstimate) {
				currentBestStdValues = optimizer.getStdValues();
				currentBestEstimate = this.bestEstimate;
			}
			return;
		}
		if (this.bestEstimate == null) return;
		if (!(bestEstimate instanceof BBStochModelInstance)) return;
		IModelInstance bestModel = ((BBStochModelInstance) bestEstimate).getModel();
		if (!(bestModel instanceof ExternalSocketModelInstance)) return;
		((ExternalSocketModelInstance) bestModel).sendFinalParameters(currentBestStdValues);
	}


}
