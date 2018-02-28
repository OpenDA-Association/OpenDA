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
package org.openda.interfaces;

/**
 * This interface has been introduced for the BmiModelInstance, the only
 * implementer of this interface.
 * It is used to indicate that the model should return output (i.e. the
 * buffered state items buffered per time step), instead of the 'regular'
 * input/output state item that is adjusted by the filter.
 * A state variable and a related buffered output state item have the
 * same exchange item id, which originates the problem.
 * So a final solution has to be found in configuring the items in the
 * BmiModelInstance in a different way (see ODA-624).
 */
public interface IOutputModeSetter {
	public void setInOutputMode(boolean inOutputMode);
}
