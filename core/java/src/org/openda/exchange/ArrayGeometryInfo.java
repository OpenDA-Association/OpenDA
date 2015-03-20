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
import java.util.Arrays;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Array;
import org.openda.utils.Vector;

/**
 * Geometry info for spatial gridded data, that is for values stored in an IArray.
 * Horizontal coordinates are preferably in WGS84 latitude and longitude in degrees.
 * 
 * @author verlaanm
 *
 */
//TODO rename to LatLonGridGeometryInfo or 2DGridGeometryInfo? AK
public class ArrayGeometryInfo implements IArrayGeometryInfo {

	private IArray latitudeCoordinateValues=null;
	private IArray longitudeCoordinateValues=null;
	private IArray heightCoordinateValues=null;

    /**
     * latitudeValueIndices contains for each dimension in the latitudeCoordinateValues array
     * the index of the corresponding dimension in the values array.
     * Not clear why this is needed.
     */
	private int[] latitudeValueIndices=null;
    /**
     * longitudeValueIndices contains for each dimension in the longitudeCoordinateValues array
     * the index of the corresponding dimension in the values array.
     * Not clear why this is needed.
     */
	private int[] longitudeValueIndices=null;

	private int[] heightValueIndices=null;
	private IQuantityInfo latitudeQuantityInfo=null;
	private IQuantityInfo longitudeQuantityInfo=null;
	private IQuantityInfo heightQuantityInfo=null;
	private int[] activeCellMask=null;

	public ArrayGeometryInfo(){
		//empty constructor - use setInfo to set values
	}
	
	public ArrayGeometryInfo(IArray latitudeArray, int[] latitudeValueIndices, IQuantityInfo latitudeQuantityInfo,
			IArray longitudeArray, int[] longitudeValueIndices, IQuantityInfo longitudeQuantityInfo,
			IArray heightArray, int[] heightValueIndices, IQuantityInfo heightQuantityInfo) {
		this(latitudeArray, latitudeValueIndices, latitudeQuantityInfo,
				longitudeArray, longitudeValueIndices, longitudeQuantityInfo,
				heightArray, heightValueIndices, heightQuantityInfo, null);
	}

	public ArrayGeometryInfo(IArray latitudeArray, int[] latitudeValueIndices, IQuantityInfo latitudeQuantityInfo,
			IArray longitudeArray, int[] longitudeValueIndices, IQuantityInfo longitudeQuantityInfo,
			IArray heightArray, int[] heightValueIndices, IQuantityInfo heightQuantityInfo, int[] activeCellMask) {
		this.latitudeCoordinateValues = latitudeArray;
		this.longitudeCoordinateValues = longitudeArray;
		this.heightCoordinateValues = heightArray;
		this.latitudeValueIndices = latitudeValueIndices;
		this.longitudeValueIndices = longitudeValueIndices;
		this.heightValueIndices = heightValueIndices;
		this.latitudeQuantityInfo = latitudeQuantityInfo;
		this.longitudeQuantityInfo = longitudeQuantityInfo;
		this.heightQuantityInfo = heightQuantityInfo;
		this.activeCellMask = activeCellMask;
	}

	public void setInfo(IArray latitudeArray, int[] latitudeValueIndices, IQuantityInfo latitudeQuantityInfo,
			IArray longitudeArray, int[] longitudeValueIndices, IQuantityInfo longitudeQuantityInfo,
			IArray heightArray, int[] heightValueIndices, IQuantityInfo heightQuantityInfo) {
		this.latitudeCoordinateValues = latitudeArray;
		this.longitudeCoordinateValues = longitudeArray;
		this.heightCoordinateValues = heightArray;
		this.latitudeValueIndices = latitudeValueIndices;
		this.longitudeValueIndices = longitudeValueIndices;
		this.heightValueIndices = heightValueIndices;
		this.latitudeQuantityInfo = latitudeQuantityInfo;
		this.longitudeQuantityInfo = longitudeQuantityInfo;
		this.heightQuantityInfo = heightQuantityInfo;
	}

	/**
	 * Degrees north of the equator. 
	 * Horizontal coordinates are preferably in WGS84.
	 * The latitude grid may form a multidimensional array.
	 * @return latitude
	 */
	public IArray getLatitudeArray(){
		return latitudeCoordinateValues;
	}

	/**
	 * Degrees east of Greenwich 0 meridian. 
	 * Horizontal coordinates are preferably in WGS84.
	 * The latitude grid may form a multidimensional array.
	 * @return longitude
	 */
	public IArray getLongitudeArray(){
		return longitudeCoordinateValues;
	}

	/**
	 * Pointer or pointers to latitude index in values array.
	 * See ArrayBasedExchangeItem for details
	 * @return pointers to array dimension(s)
	 */
	public int[] getLatitudeValueIndices(){
		return latitudeValueIndices;
	}

	/**
	 * Pointer or pointers to longitude index in values array.
	 * See ArrayBasedExchangeItem for details
	 * @return pointers to array dimension(s)
	 */
	public int[] getLongitudeValueIndices(){
		return longitudeValueIndices;
	}

	/**
	 * Returns information about the latitude coordinate values.
	 *
	 * @return quantity info
	 */
	public IQuantityInfo getLatitudeQuantityInfo() {
		return latitudeQuantityInfo;
	}

	/**
	 * Returns information about the longitude coordinate values.
	 *
	 * @return quantity info
	 */
	public IQuantityInfo getLongitudeQuantityInfo() {
		return longitudeQuantityInfo;
	}
	
	public String toString(){
		String result="{ArrayGeometryInfo \n";
		result+="{longitude ";
		if(this.longitudeQuantityInfo!=null){ result+=" "+this.longitudeQuantityInfo; }
		if(this.longitudeCoordinateValues!=null){ result+=" "+this.longitudeCoordinateValues.toString(); }
		result+="}\n";
		result+="{latitude ";
		if(this.latitudeQuantityInfo!=null){ result+=" "+this.latitudeQuantityInfo; }
		if(this.latitudeCoordinateValues!=null){ result+=" "+this.latitudeCoordinateValues.toString(); }
		result+="}}";
		return result;
	}

	@Override
	public IArray getHeightArray() {
		return this.heightCoordinateValues;
	}

	@Override
	public int[] getHeightValueIndices() {
		return heightValueIndices;
	}

	@Override
	public IQuantityInfo getHeightQuantityInfo() {
		return heightQuantityInfo;
	}

	@Override
	public int[] getActiveCellMask() {
		return this.activeCellMask;
	}

	/**
	 * @return vector with the x coordinate for each grid cell.
	 */
	public IVector getXCoordinates() {
		//first create curvilinearGeometryInfo so that there is a separate pair of x,y coordinates for each grid cell.
		return new Vector(this.toCurvilinearGeometryInfo().getLongitudeArray().getValuesAsDoubles());
	}

	/**
	 * @return vector with the y coordinate for each grid cell.
	 */
	public IVector getYCoordinates() {
		//first create curvilinearGeometryInfo so that there is a separate pair of x,y coordinates for each grid cell.
		return new Vector(this.toCurvilinearGeometryInfo().getLatitudeArray().getValuesAsDoubles());
	}

	public int getCellCount() {
		if (this.latitudeCoordinateValues == null) throw new RuntimeException("GeometryInfo has no y coordinates.");
		if (this.longitudeCoordinateValues == null) throw new RuntimeException("GeometryInfo has no x coordinates.");

		if (this.latitudeCoordinateValues.getNumberOfDimensions() == 2 && this.longitudeCoordinateValues.getNumberOfDimensions() == 2) {//if curvilinear grid.
			return this.longitudeCoordinateValues.length();
		}

		if (this.latitudeCoordinateValues.getNumberOfDimensions() == 1 && this.longitudeCoordinateValues.getNumberOfDimensions() == 1) {//if rectangular grid.
			//each row has a separate y coordinate.
			int rowCount = this.latitudeCoordinateValues.length();
			//each column has a separate x coordinate.
			int columnCount = this.longitudeCoordinateValues.length();
			return rowCount*columnCount;
		}

		throw new UnsupportedOperationException(getClass() + ".getGridCellCount() only implemented for rectangular and curvilinear grid geometries.");
	}

	/**
	 * This can be used to convert a rectangular grid to a curvilinear grid,
	 * i.e. convert the x and y axes from 1-dimensional to 2-dimensional
	 * so that there is a separate x and y coordinate for each grid cell.
	 * In other words generate 2d coordinate variables from the given
	 * 1d coordinate variables.
	 * This can be useful when a list with the x and y coordinate of each
	 * grid cell is needed.
	 *
	 * @return curvilinear version of this ArrayGeometryInfo, or this object if it is already curvilinear.
	 */
	private ArrayGeometryInfo toCurvilinearGeometryInfo() {
		if (this.latitudeCoordinateValues == null) {
			throw new RuntimeException("GeometryInfo has no y coordinates.");
		}
		if (this.longitudeCoordinateValues == null) {
			throw new RuntimeException("GeometryInfo has no x coordinates.");
		}

		if (this.latitudeCoordinateValues.getNumberOfDimensions() == 2 && this.longitudeCoordinateValues.getNumberOfDimensions() == 2) {//if curvilinear grid.
			return this;
		}

		if (this.latitudeCoordinateValues.getNumberOfDimensions() == 1 && this.longitudeCoordinateValues.getNumberOfDimensions() == 1) {//if rectangular grid.
			//each row has a separate y coordinate.
			double[] rowYCoordinates = this.latitudeCoordinateValues.getValuesAsDoubles();
			int rowCount = rowYCoordinates.length;
			//each column has a separate x coordinate.
			double[] columnXCoordinates = this.longitudeCoordinateValues.getValuesAsDoubles();
			int columnCount = columnXCoordinates.length;

			//generate grid cell coordinates in the same order as the values in the exchangeItem.
			double[] gridCellYCoordinates = new double[rowCount * columnCount];
			double[] gridCellXCoordinates = new double[rowCount * columnCount];
			int[] dimensions;
			boolean rowMajor = this.latitudeValueIndices[0] < this.longitudeValueIndices[0];//if value array in exchangeItem has dimensions (y, x).
			if (rowMajor) {//if value array in exchangeItem has dimensions (y, x).
				int index = 0;
				for (int row = 0; row < rowCount; row++) {
					for (int column = 0; column < columnCount; column++) {
						gridCellYCoordinates[index] = rowYCoordinates[row];
						gridCellXCoordinates[index] = columnXCoordinates[column];
						index++;
					}
				}
				dimensions = new int[]{rowCount, columnCount};
			} else {//if value array in exchangeItem has dimensions (x, y).
				int index = 0;
				for (int column = 0; column < columnCount; column++) {
					for (int row = 0; row < rowCount; row++) {
						gridCellYCoordinates[index] = rowYCoordinates[row];
						gridCellXCoordinates[index] = columnXCoordinates[column];
						index++;
					}
				}
				dimensions = new int[]{columnCount, rowCount};
			}

			Array allYCoordinates = new Array(gridCellYCoordinates, dimensions, false);
			Array allXCoordinates = new Array(gridCellXCoordinates, dimensions, false);
			int[] valueIndices = new int[]{0, 1};
			return new ArrayGeometryInfo(allYCoordinates, valueIndices, this.latitudeQuantityInfo,
					allXCoordinates, valueIndices, this.longitudeQuantityInfo,
					this.heightCoordinateValues, this.heightValueIndices, this.heightQuantityInfo, this.activeCellMask);
		}

		throw new UnsupportedOperationException(getClass() + ".toCurvilinearGeometryInfo() only implemented for rectangular and curvilinear grid geometries.");
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass() || o.hashCode() != hashCode()) {
			return false;
		}
		ArrayGeometryInfo that = (ArrayGeometryInfo) o;

		if (this.latitudeCoordinateValues == null) {
			if (that.latitudeCoordinateValues != null) return false;
		} else {//if this.latitudeCoordinateValues != null.
			if (!this.latitudeCoordinateValues.equals(that.latitudeCoordinateValues)) return false;
		}
		if (this.longitudeCoordinateValues == null) {
			if (that.longitudeCoordinateValues != null) return false;
		} else {//if this.longitudeCoordinateValues != null.
			if (!this.longitudeCoordinateValues.equals(that.longitudeCoordinateValues)) return false;
		}
		if (this.heightCoordinateValues == null) {
			if (that.heightCoordinateValues != null) return false;
		} else {//if this.heightCoordinateValues != null.
			if (!this.heightCoordinateValues.equals(that.heightCoordinateValues)) return false;
		}

		if (!Arrays.equals(this.latitudeValueIndices, that.latitudeValueIndices)) {
			return false;
		}
		if (!Arrays.equals(this.longitudeValueIndices, that.longitudeValueIndices)) {
			return false;
		}
		if (!Arrays.equals(this.heightValueIndices, that.heightValueIndices)) {
			return false;
		}

		if (this.latitudeQuantityInfo == null) {
			if (that.latitudeQuantityInfo != null) return false;
		} else {//if this.latitudeQuantityInfo != null.
			if (!this.latitudeQuantityInfo.equals(that.latitudeQuantityInfo)) return false;
		}
		if (this.longitudeQuantityInfo == null) {
			if (that.longitudeQuantityInfo != null) return false;
		} else {//if this.longitudeQuantityInfo != null.
			if (!this.longitudeQuantityInfo.equals(that.longitudeQuantityInfo)) return false;
		}
		if (this.heightQuantityInfo == null) {
			if (that.heightQuantityInfo != null) return false;
		} else {//if this.heightQuantityInfo != null.
			if (!this.heightQuantityInfo.equals(that.heightQuantityInfo)) return false;
		}

		if (!Arrays.equals(this.activeCellMask, that.activeCellMask)) {
			return false;
		}

		return true;
	}

	@Override
	public int hashCode() {
		int h = this.latitudeCoordinateValues == null ? 0 : this.latitudeCoordinateValues.hashCode();
		h = 31 * h + (this.longitudeCoordinateValues == null ? 0 : this.longitudeCoordinateValues.hashCode());
		h = 31 * h + (this.heightCoordinateValues == null ? 0 : this.heightCoordinateValues.hashCode());
		h = 31 * h + Arrays.hashCode(this.latitudeValueIndices);
		h = 31 * h + Arrays.hashCode(this.longitudeValueIndices);
		h = 31 * h + Arrays.hashCode(this.heightValueIndices);
		h = 31 * h + (this.latitudeQuantityInfo == null ? 0 : this.latitudeQuantityInfo.hashCode());
		h = 31 * h + (this.longitudeQuantityInfo == null ? 0 : this.longitudeQuantityInfo.hashCode());
		h = 31 * h + (this.heightQuantityInfo == null ? 0 : this.heightQuantityInfo.hashCode());
		h = 31 * h + Arrays.hashCode(this.activeCellMask);
		return h;
	}
}
