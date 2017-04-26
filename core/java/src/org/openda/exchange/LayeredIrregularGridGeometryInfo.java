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

package org.openda.exchange;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;

import java.util.Arrays;

/**
 * A layered irregular grid is just a list with grid cell centers for one or more layers.
 * The xCoordinates and yCoordinates are given for one layer only and are the same for each layer.
 * Furthermore there is one zCoordinate for each layer.
 *
 * @author Arno Kockx
 */
public class LayeredIrregularGridGeometryInfo implements IGeometryInfo {
	private final int cellCount;
	private final int layerCount;
	private final double[] xCoordinates;
	private final double[] yCoordinates;
	private final double[] zCoordinates;

	public LayeredIrregularGridGeometryInfo(int cellCount, int layerCount, double[] xCoordinates, double[] yCoordinates, double[] zCoordinates) {
		if (zCoordinates == null) throw new IllegalArgumentException("zCoordinates == null");
		if (zCoordinates.length != layerCount) throw new IllegalArgumentException("zCoordinates.length != layerCount");

		this.cellCount = cellCount;
		this.layerCount = layerCount;
		this.xCoordinates = xCoordinates;
		this.yCoordinates = yCoordinates;
		this.zCoordinates = zCoordinates;
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

	/**
	 * @return vector with the z coordinate for each layer.
	 */
	public IVector getZCoordinates() {
		return new Vector(this.zCoordinates);
	}

	public int getCellCount() {
		return this.cellCount;
	}

	public int getLayerCount() {
		return this.layerCount;
	}

	public String toString(){
		return "{" + getClass().getSimpleName() + " cellCount=" + this.cellCount + " layerCount=" + this.layerCount + "}";
	}

	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass() || o.hashCode() != hashCode()) {
			return false;
		}

		LayeredIrregularGridGeometryInfo that = (LayeredIrregularGridGeometryInfo) o;
		if (this.cellCount != that.cellCount) return false;
		if (this.layerCount != that.layerCount) return false;
		if (!Arrays.equals(this.xCoordinates, that.xCoordinates)) return false;
		if (!Arrays.equals(this.yCoordinates, that.yCoordinates)) return false;
		if (!Arrays.equals(this.zCoordinates, that.zCoordinates)) return false;
		return true;
	}

	public int hashCode() {
		int h = this.cellCount;
		h = 31 * h + this.layerCount;
		h = 31 * h + Arrays.hashCode(this.xCoordinates);
		h = 31 * h + Arrays.hashCode(this.yCoordinates);
		h = 31 * h + Arrays.hashCode(this.zCoordinates);

		return h;
	}

	@Override
	public IArray distanceToPoint(double x, double y, double z) {
		throw new RuntimeException("org.openda.exchange.LayeredIrregularGridGeometryInfo.distanceToPoint() not implemented yet");
	}
}
