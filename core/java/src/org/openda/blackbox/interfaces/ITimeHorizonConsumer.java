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
package org.openda.blackbox.interfaces;
import org.openda.interfaces.ITime;

/**
 * Optional consumer interface that can be implemented by a (stoch)ModelFactory if it
 * needs the timeHorizon to be set by OpenDA. The setTimeHorizon is called
 * after the initialize method of the (stoch)ModelFactory has been called.
 */
public interface ITimeHorizonConsumer {
	public void setTimeHorizon(ITime timeHorizon);
}
