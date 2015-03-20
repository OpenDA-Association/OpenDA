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

package org.openda.utils.geometry;

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.IrregularGridGeometryInfo;
import org.openda.interfaces.*;
import org.openda.utils.Vector;

import java.util.Arrays;

/**
 * @author Arno Kockx
 */
public class GeometryUtils {
	private static final double p1_4 = 1.0/4.0;
	private static final double p1_2 = 1.0/2.0;
	private static final double p2_3 = 2.0/3.0;
	private static final double p5_8 = 5.0/8.0;
	private static final double p5_3 = 5.0/3.0;
	private static final double p1_12 = 1.0/12.0;

	private GeometryUtils() {
	}

	/**
	 * Returns the number of grid cells for the given exchangeItem and timeIndex. If item is not a grid, then that is the same as a grid with only one cell.
	 */
	public static int getGridCellCount(IPrevExchangeItem prevExchangeItem, int timeIndex) {
		//get exchangeItem.
		if (!(prevExchangeItem instanceof IExchangeItem)) {
			return 1;
		}
		IExchangeItem exchangeItem = (IExchangeItem) prevExchangeItem;

		//get geometryInfo.
		IGeometryInfo geometryInfo;
		if (exchangeItem instanceof IGridTimeSeriesExchangeItem && timeIndex != -1) {
			geometryInfo = ((IGridTimeSeriesExchangeItem) exchangeItem).getGeometryInfoForSingleTimeIndex(timeIndex);
		} else {
			geometryInfo = exchangeItem.getGeometryInfo();
		}
		if (geometryInfo == null) return 1;

		//get geometry.
		if (!(geometryInfo instanceof ArrayGeometryInfo || geometryInfo instanceof IrregularGridGeometryInfo)) {
			return 1;
		}
		return (geometryInfo instanceof ArrayGeometryInfo) ? ((ArrayGeometryInfo) geometryInfo).getCellCount() : ((IrregularGridGeometryInfo) geometryInfo).getCellCount();
	}

	/**
	 * Interpolates the given model values from the given model grid to the given observation locations.
	 * This method only works for WGS84 lat,lon coordinates in degrees.
	 *
	 * @param observationXCoordinates x coordinates of observation points.
	 * @param observationYCoordinates y coordinates of observation points.
	 * @param modelGeometryInfo model grid, must be a rectangular grid (ArrayGeometryInfo).
	 * @param modelValues values on the model grid.
	 * @return interpolated values.
	 */
	public static Vector getObservedValuesBilinearInterpolation(IVector observationXCoordinates, IVector observationYCoordinates, IGeometryInfo modelGeometryInfo, double[] modelValues) {
		if (observationXCoordinates == null) throw new IllegalArgumentException("observationXCoordinates == null");
		if (observationYCoordinates == null) throw new IllegalArgumentException("observationYCoordinates == null");
		if (observationXCoordinates.getSize() != observationYCoordinates.getSize()) throw new IllegalArgumentException("observationXCoordinates.getSize() != observationYCoordinates.getSize()");
		if (modelGeometryInfo == null) throw new IllegalArgumentException("modelGeometryInfo == null");
		if (modelValues == null) throw new IllegalArgumentException("modelValues == null");

		int observationLocationCount = observationXCoordinates.getSize();

		//interpolate values.
		double[] observedModelValues = new double[observationLocationCount];
		for (int observationLocationIndex = 0; observationLocationIndex < observationLocationCount; observationLocationIndex++) {
			//this code assumes that the lat,lon coordinates for the observations and for the model are always in the same range, e.g. [-90,90][-180, 180].
			double observationY = observationYCoordinates.getValue(observationLocationIndex);
			double observationX = observationXCoordinates.getValue(observationLocationIndex);
			observedModelValues[observationLocationIndex] = bilinearInterpolateValue(observationX, observationY, modelGeometryInfo, modelValues);
		}

		return new Vector(observedModelValues);
	}

	private static double bilinearInterpolateValue(double destinationX, double destinationY, IGeometryInfo sourceGeometryInfo, double[] sourceValues) {
		//get source coordinates.
		if (!(sourceGeometryInfo instanceof ArrayGeometryInfo)) {
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for geometryInfo of type "
					+ ArrayGeometryInfo.class.getName() + " not for geometryInfo of type " + sourceGeometryInfo.getClass().getName());
		}
		ArrayGeometryInfo sourceArrayGeometryInfo = ((ArrayGeometryInfo) sourceGeometryInfo);
		IArray latitudeArray = sourceArrayGeometryInfo.getLatitudeArray();
		IArray longitudeArray = sourceArrayGeometryInfo.getLongitudeArray();
		if (latitudeArray == null || latitudeArray.getNumberOfDimensions() != 1 || longitudeArray == null || longitudeArray.getNumberOfDimensions() != 1) {//if not rectangular grid.
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for rectangular source grid.");
		}
		double[] rowCenterYCoordinates = latitudeArray.getValuesAsDoubles();
		double[] columnCenterXCoordinates = longitudeArray.getValuesAsDoubles();
		if (rowCenterYCoordinates == null || rowCenterYCoordinates.length <= 1 || columnCenterXCoordinates == null || columnCenterXCoordinates.length <= 1) {//if not a grid.
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for source grid.");
		}
		if (!isSortedAscending(rowCenterYCoordinates) || !isSortedAscending(columnCenterXCoordinates)) {
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for grids with coordinates that are sorted ascending.");
		}

		int[] latitudeValueIndices = sourceArrayGeometryInfo.getLatitudeValueIndices();
		int[] longitudeValueIndices = sourceArrayGeometryInfo.getLongitudeValueIndices();
		if (latitudeValueIndices == null || latitudeValueIndices.length < 1 || longitudeValueIndices == null || longitudeValueIndices.length < 1) {
			throw new IllegalArgumentException("sourceGeometryInfo.latitudeValueIndices or sourceGeometryInfo.longitudeValueIndices not set." +
					" This is needed to determine whether the coordinates in sourceGeometryInfo are stored in rowMajor or columnMajor order.");
		}
		boolean rowMajor = latitudeValueIndices[0] < longitudeValueIndices[0];//if value array in exchangeItem has dimensions (y, x).
		int rowCount = rowCenterYCoordinates.length;
		int columnCount = columnCenterXCoordinates.length;

		//this code assumes that coordinates are sorted ascending.
		int rowIndexAtOrSouthOfY = getRowIndexAtOrSouthOfY(rowCenterYCoordinates, destinationY);
		int columnIndexAtOrWestOfX = getColumnIndexAtOrWestOfX(columnCenterXCoordinates, destinationX);
		int rowIndexNorthOfY = rowIndexAtOrSouthOfY + 1;
		int columnIndexEastOfX = columnIndexAtOrWestOfX + 1;
		//note: this code currently cannot handle destination x,y coordinates that are exactly on the north or east edge of the grid.
		if (rowIndexAtOrSouthOfY < 0 || columnIndexAtOrWestOfX < 0 || rowIndexNorthOfY >= rowCount || columnIndexEastOfX >= columnCount) {//if outside grid.
			return Double.NaN;
		}

		//get values and coordinates of surrounding points.
		double northWestX = columnCenterXCoordinates[columnIndexAtOrWestOfX];
		double northWestY = rowCenterYCoordinates[rowIndexNorthOfY];
		int northWestCellIndex = getCellIndex(rowIndexNorthOfY, columnIndexAtOrWestOfX, rowMajor, rowCount, columnCount);
		double northWestValue = sourceValues[northWestCellIndex];

		double southWestX = columnCenterXCoordinates[columnIndexAtOrWestOfX];
		double southWestY = rowCenterYCoordinates[rowIndexAtOrSouthOfY];
		int southWestCellIndex = getCellIndex(rowIndexAtOrSouthOfY, columnIndexAtOrWestOfX, rowMajor, rowCount, columnCount);
		double southWestValue = sourceValues[southWestCellIndex];

		double southEastX = columnCenterXCoordinates[columnIndexEastOfX];
		int southEastCellIndex = getCellIndex(rowIndexAtOrSouthOfY, columnIndexEastOfX, rowMajor, rowCount, columnCount);
		double southEastValue = sourceValues[southEastCellIndex];

		double northEastX = columnCenterXCoordinates[columnIndexEastOfX];
		int northEastCellIndex = getCellIndex(rowIndexNorthOfY, columnIndexEastOfX, rowMajor, rowCount, columnCount);
		double northEastValue = sourceValues[northEastCellIndex];

		//interpolate.
		double df = (northEastValue - northWestValue);
		double dx = (northEastX - northWestX);
		double deltaX = destinationX - northWestX;
		double northValue = northWestValue + deltaX * df/dx;

		df = (southEastValue - southWestValue);
		dx = (southEastX - southWestX);
		deltaX = destinationX - southWestX;
		double southValue = southWestValue + deltaX * df/dx;

		df = (northValue - southValue);
		double dy = (northWestY - southWestY);
		double deltaY = destinationY - southWestY;
		return southValue + deltaY * df/dy;
	}

	private static int getCellIndex(int row, int column, boolean rowMajor, int rowCount, int columnCount) {
		if (rowMajor) {//if value array in exchangeItem has dimensions (y, x).
			return row*columnCount + column;
		} else {//if value array in exchangeItem has dimensions (x, y).
			return column*rowCount + row;
		}
	}

	private static int getRowIndexAtOrSouthOfY(double[] rowCenterYCoordinates, double y) {
		return getClosestIndexEqualToOrSmallerThanGivenCoordinate(rowCenterYCoordinates, y);
	}

	private static int getColumnIndexAtOrWestOfX(double[] columnCenterXCoordinates, double x) {
		return getClosestIndexEqualToOrSmallerThanGivenCoordinate(columnCenterXCoordinates, x);
	}

	private static int getClosestIndexEqualToOrSmallerThanGivenCoordinate(double[] coordinates, double coordinate) {
		int i = Arrays.binarySearch(coordinates, coordinate);
		if (i >= 0) return i;
		int insertionIndex = -i - 1;
		if (insertionIndex <= 0 || insertionIndex >= coordinates.length) {//if outside grid.
			return -1;
		}
		return insertionIndex - 1;
	}

	private static boolean isSortedAscending(double[] array) {
		if (array.length <= 1) return true;

		double lastValue = array[0];
		for (int i = 1; i < array.length; i++) {
			double v = array[i];
			if (v < lastValue) return false;
			lastValue = v;
		}

		return true;
	}

	/**
	 * Returns the localization weights for each of the given observation locations.
	 * This method only works for WGS84 lat,lon coordinates in degrees.
	 *
	 * @param observationXCoordinates x coordinates of observation points.
	 * @param observationYCoordinates y coordinates of observation points.
	 * @param stateGeometryInfo model grid, must be a rectangular or curvilinear grid (ArrayGeometryInfo).
	 * @param cohnDistanceInMeters characteristic distance for Cohn's formula
	 * @return weight vector for each observation location.
	 *         The size of the returned array equals the number of given observation points.
	 *         The size of each vector in the returned array equals the number of grid cells in the given state geometry info.
	 */
	public static IVector[] getLocalizationWeights(IVector observationXCoordinates, IVector observationYCoordinates, IGeometryInfo stateGeometryInfo, double cohnDistanceInMeters) {
		if (observationXCoordinates == null) throw new IllegalArgumentException("observationXCoordinates == null");
		if (observationYCoordinates == null) throw new IllegalArgumentException("observationYCoordinates == null");
		if (observationXCoordinates.getSize() != observationYCoordinates.getSize()) throw new IllegalArgumentException("observationXCoordinates.getSize() != observationYCoordinates.getSize()");
		if (stateGeometryInfo == null) throw new IllegalArgumentException("stateGeometryInfo == null");

		//get state coordinates.
		if (!(stateGeometryInfo instanceof ArrayGeometryInfo)) {
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".getLocalizationWeights() only implemented for geometryInfo of type "
					+ ArrayGeometryInfo.class.getName() + " not for geometryInfo of type " + stateGeometryInfo.getClass().getName());
		}
		ArrayGeometryInfo stateArrayGeometryInfo = ((ArrayGeometryInfo) stateGeometryInfo);
		//this code assumes that the coordinates are stored in the same order as the values in the state exchangeItem.
		IVector stateYCoordinates = stateArrayGeometryInfo.getYCoordinates();
		IVector stateXCoordinates = stateArrayGeometryInfo.getXCoordinates();
		int stateCellCount = stateXCoordinates.getSize();

		//compute weights for each observation location.
		int observationLocationCount = observationXCoordinates.getSize();
		IVector[] weightVectors = new IVector[observationLocationCount];
		for (int observationLocationIndex = 0; observationLocationIndex < observationLocationCount; observationLocationIndex++) {
			double observationX = observationXCoordinates.getValue(observationLocationIndex);
			double observationY = observationYCoordinates.getValue(observationLocationIndex);

			//calculate weights.
			double[] weights = new double[stateCellCount];
			//loop over state grid cells.
			for (int stateCellIndex = 0; stateCellIndex < stateCellCount; stateCellIndex++) {
				//this code assumes that the lat,lon coordinates for the observations and for the state are always in the same range, e.g. [-90,90][-180, 180].
				//weights do not have to be normalised, i.e. sum of weights does not have to be 1.
				weights[stateCellIndex] = calculateCohnWeightLatLonCoordinates(observationY, observationX,
						stateYCoordinates.getValue(stateCellIndex), stateXCoordinates.getValue(stateCellIndex), cohnDistanceInMeters);
			}
			weightVectors[observationLocationIndex] = new Vector(weights);
		}

		return weightVectors;
	}

	/**
	 * This calculation is not the most accurate, but it takes less computation time.
	 *
	 * @param lat1 latitude of point 1
	 * @param lon1 longitude of point 1
	 * @param lat2 latitude of point 2
	 * @param lon2 longitude of point 2
	 * @return distance in meters
	 */
	static double distanceInMeters(double lat1, double lon1, double lat2, double lon2) {
		//this calculation is not the most accurate, but it takes less computation time.
		double dy = 60. * (lat2 - lat1) * 1852.27;
		double phi = (lat2 + lat1) * 0.017453 / 2;
		double dx = 60. * (lon2 - lon1) * 1852.27 * Math.cos(phi);

		return Math.hypot(dx, dy);
	}

	/**
	 * Calculates localization weight between the two given points according to Cohn's formula.
	 * This method only works for WGS84 lat,lon coordinates in degrees.
	 *
	 * @param lat1 latitude of point 1
	 * @param lon1 longitude of point 1
	 * @param lat2 latitude of point 2
	 * @param lon2 longitude of point 2
	 * @param cohnDistanceInMeters characteristic distance for Cohn's formula
	 * @return weight
	 */
	private static double calculateCohnWeightLatLonCoordinates(double lat1, double lon1, double lat2, double lon2, double cohnDistanceInMeters) {
		//compute distance on a sphere.
		double distanceInMeters = distanceInMeters(lat1, lon1, lat2, lon2);
		return calculateCohnWeight(distanceInMeters, cohnDistanceInMeters);
	}

	/**
	 * Calculates localization weight between the two given points according to Cohn's formula.
	 * This method only works for cartesian coordinates in meters.
	 *
	 * @param x1 x coordinate of point 1
	 * @param y1 y coordinate of point 1
	 * @param x2 x coordinate of point 2
	 * @param y2 y coordinate of point 2
	 * @param cohnDistanceInMeters characteristic distance for Cohn's formula
	 * @return weight
	 */
	@SuppressWarnings("UnusedDeclaration")
	private static double calculateCohnWeightCartesianCoordinates(double x1, double y1, double x2, double y2, double cohnDistanceInMeters) {
		if (cohnDistanceInMeters <= 0) {
			throw new IllegalArgumentException("cohnDistanceInMeters <= 0");
		}

		//compute distance in each direction.
		double deltaXAbsolute = Math.abs(x2 - x1);
		double deltaYAbsolute = Math.abs(y2 - y1);
		double thresholdDistanceInMeters = 2*cohnDistanceInMeters;
		if (deltaXAbsolute > thresholdDistanceInMeters || deltaYAbsolute > thresholdDistanceInMeters) {
			//weights outside radius of 2*cohnDistance are 0.
			return 0;
		}

		//compute Euclidian distance.
		//TODO cache distance for each combination of (slightly rounded) deltaXAbsolute and deltaYAbsolute to increase performance. AK
		double distanceInMeters = Math.hypot(deltaXAbsolute, deltaYAbsolute);
		return calculateCohnWeight(distanceInMeters, cohnDistanceInMeters);
	}

	/**
	 * Calculates localization weight between two points according to Cohn's formula.
	 *
	 * @param r absolute distance between two points in meters
	 * @param cohnDistance characteristic distance for Cohn's formula in meters
	 * @return weight
	 */
	static double calculateCohnWeight(double r, double cohnDistance) {
		if (r < 0) {
			throw new IllegalArgumentException("r < 0");
		}
		if (cohnDistance <= 0) {
			throw new IllegalArgumentException("cohnDistance <= 0");
		}

		if (r > 2*cohnDistance) {
			//weights outside radius of 2*cohnDistance are 0.
			return 0;
		}

		//TODO cache weight for each combination of (slightly rounded) r and cohnDistance to increase performance. AK
		double z1 = r/cohnDistance;
		double z2 = z1*z1;
		double z3 = z2*z1;
		double z4 = z3*z1;
		double z5 = z4*z1;

		if (r < cohnDistance) {
			//weight = -1/4 * (r/cohnDistance)^5 + 1/2 * (r/cohnDistance)^4 + 5/8 * (r/cohnDistance)^3 - 5/3 * (r/cohnDistance)^2 + 1
			//using z := r/cohnDistance
			//weight = -1/4 * z^5 + 1/2 * z^4 + 5/8 * z^3 - 5/3 * z^2 + 1
			return -p1_4*z5 + p1_2*z4 + p5_8*z3 - p5_3*z2 + 1;

		} else {//if cohnDistance <= z <= 2 * cohnDistance = thresholdDistance
			//weight = 1/12 * (r/cohnDistance)^5 - 1/2 * (r/cohnDistance)^4 + 5/8 * (r/cohnDistance)^3 + 5/3 * (r/cohnDistance)^2 - 5 * (r/cohnDistance) + 4 - 2/3 * (cohnDistance/r)
			//using z := r/cohnDistance
			//weight = 1/12 * z^5 - 1/2 * z^4 + 5/8 * z^3 + 5/3 * z^2 - 5*z + 4 - 2/3 * 1/z
			return p1_12*z5 - p1_2*z4 + p5_8*z3 + p5_3*z2 - 5*z1 + 4 - p2_3/z1;
		}
	}
}
