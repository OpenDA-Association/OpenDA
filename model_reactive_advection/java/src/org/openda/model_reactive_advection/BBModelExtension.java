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
package org.openda.model_reactive_advection;

import org.openda.blackbox.config.BBModelConfig;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IModelExtensions;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

/**
 * @author Nils van Velzen
 *         This class extends the black box model builder such that the user can
 *         implement observation handling and localization
 */
public class BBModelExtension extends BBModelInstance implements IModelExtensions {
	/**
	 * Create new blackbox model instance. You should not need to call this routine manually, but use the
	 * getInstance method from the factory instead.
	 *
	 * @param bbModelConfig  configuration, typically parsed from an input file.
	 * @param instanceNumber number for this copy of the model. Starts at 0 and does not reuse numbers
	 *                       even when a model has been cleaned from memory.
	 * @param timeHorizon    feed this timeHorizon from outside to the model. If it is null then use the
	 */
	public BBModelExtension(BBModelConfig bbModelConfig, int instanceNumber, ITime timeHorizon) {
		super(bbModelConfig, instanceNumber, timeHorizon);
	}

	public IVector[] getObservedLocalization(String stateExchangeItemID, IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("This method is not yet implemented");
		//return new IVector[0];
	}

	/**
	 * Tell model that it can expect to be asked for model values corresponding to the observations
	 * described. The model can make arrangement to save these values. The method compute run over a long
	 * interval at once, not stopping at each time with observations. This is meant to increase the performance
	 * especially of calibration algorithms.
	 *
	 * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
	 */
	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("Not yet implemented");
	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("This method is not yet implemented");
		//return null;
	}
}




