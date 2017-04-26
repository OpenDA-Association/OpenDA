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
package org.openda.interfaces;

/**
 * Interface for objects that describe the geometry (spatial coordinates) of an exchangeItem.
 */
//TODO this empty interface is not useful, so add some methods, e.g. getCellCount(), getXCoordinate(int index), getYCoordinate(int index), getGeometryType(), etc.
public interface IGeometryInfo {

	/**
	 * Return distance to the point given for each element in the ExchangeItem.
	 * Typically the point originates from the position of an observation.
	 * @param x
	 * @param y
	 * @param z
	 * @return distance (by preference in meters, but at least consistently for all exchangeitems in an experiment)
	 */
	IArray distanceToPoint(double x, double y, double z);
}
