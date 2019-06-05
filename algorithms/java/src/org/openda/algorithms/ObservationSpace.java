/*
 * Copyright (c) 2019 OpenDA Association
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
package org.openda.algorithms;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;

/**
 * The ObservationSpace collects together the essential data for combining observations with model output.
 * This collection facilitates the exchange of these closely linked items. 
 * One important use of this 'struct' is filtering the observations.
 *   
 * @author verlaanm
 *
 */
public class ObservationSpace {
	public IStochObserver observer=null;
	public IVector predictedValues=null;
}
