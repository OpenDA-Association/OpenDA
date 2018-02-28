/* OpenDA v2.4.3 
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
package org.openda.model_nemo;
import org.openda.blackbox.wrapper.BBModelFactory;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IStochModelFactory;

/**
 * We only need to override the getInstance method such that we will get a model implementing
 * the extension for handling observations and localization.
 */
public class NemoModelFactory extends BBModelFactory {

	public BBModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		int newInstanceNumber=instanceNumber.val();
		instanceNumber.inc();
		return new NemoModelInstance(this.bbModelConfig, newInstanceNumber, this.timeHorizon);
	}
}
