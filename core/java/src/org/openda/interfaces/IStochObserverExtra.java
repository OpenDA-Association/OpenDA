/* OpenDA v2.4 
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

package org.openda.interfaces;

/***
 *  Extended interface for (temporarely) handling localization (Cohn) combined with assimilating the
 *  observations one at a time.
 *  NOTE: the weights in the observer SHOULD corresponds to those in the model.
 *  This is directly why this setup is far from ideal, will probably be removed in the future when
 *  we reimplement localization in OpenDA!
 */
public interface IStochObserverExtra {

	/**
	 * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
	 * The selection criterium is in the form of an SQLite query.
	 * @param ObsAssim    Obervations we are assimilating
	 * @param distance    localization distance
	 * @return            localization weights for all observations in this observer for each observation in ObsAssim
	 */
	public IVector[] getObservedLocalizationAtLocalizationLocations(IObservationDescriptions ObsAssim, double distance);

}
