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

package org.openda.exchange;

import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;

import java.util.Arrays;

/**
 * An irregular grid is just a list with grid cell centers.
 *
 * @author Arno Kockx
 */
public class IrregularGridGeometryInfo implements IGeometryInfo {
	private final int cellCount;
	private final double[] xCoordinates;
	private final double[] yCoordinates;

	public IrregularGridGeometryInfo(int cellCount, double[] xCoordinates, double[] yCoordinates) {
		this.cellCount = cellCount;
		this.xCoordinates = xCoordinates;
		this.yCoordinates = yCoordinates;
	}

	/**
	 * @return vector with the x coordinate for each grid cell.
	 */
	public IVector getXCoordinates() {
		return this.xCoordinates == null ? null : new Vector(this.xCoordinates);
	}

	/**
	 * @return vector with the y coordinate for each grid cell.
	 */
	public IVector getYCoordinates() {
		return this.yCoordinates == null ? null : new Vector(this.yCoordinates);
	}

	public int getCellCount() {
		return this.cellCount;
	}

	
	public String toString(){
		return "{" + getClass().getSimpleName() + " cellCount=" + this.cellCount + "}";
	}

	
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass() || o.hashCode() != hashCode()) {
			return false;
		}

		IrregularGridGeometryInfo that = (IrregularGridGeometryInfo) o;
		if (this.cellCount != that.cellCount) return false;
		if (!Arrays.equals(this.xCoordinates, that.xCoordinates)) return false;
		if (!Arrays.equals(this.yCoordinates, that.yCoordinates)) return false;
		return true;
	}

	
	public int hashCode() {
		int h = this.cellCount;
		h = 31 * h + Arrays.hashCode(this.xCoordinates);
		h = 31 * h + Arrays.hashCode(this.yCoordinates);

		return h;
	}
}
