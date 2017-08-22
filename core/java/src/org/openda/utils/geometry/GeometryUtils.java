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

package org.openda.utils.geometry;

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.IrregularGridGeometryInfo;
import org.openda.exchange.LayeredIrregularGridGeometryInfo;
import org.openda.exchange.PointGeometryInfo;
import org.openda.interfaces.*;
import org.openda.utils.Vector;

import java.util.Arrays;

/**
 * @author Arno Kockx
 */
public class GeometryUtils {
	private static final double METERS_PER_DEGREE = 60. * 1852.27;

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
			//IPrevExchangeItem has no geometryInfo, so can never be a grid.
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

		//get cellCount.
		return getGridCellCount(geometryInfo);
	}

	/**
	 * Returns the number of grid cells for the given exchangeItem. If item is not a grid, then that is the same as a grid with only one cell.
	 */
	public static int getGridCellCount(IPrevExchangeItem prevExchangeItem) {
		//get exchangeItem.
		if (!(prevExchangeItem instanceof IExchangeItem)) {
			//IPrevExchangeItem has no geometryInfo, so can never be a grid.
			return 1;
		}
		IExchangeItem exchangeItem = (IExchangeItem) prevExchangeItem;

		return getGridCellCount(exchangeItem.getGeometryInfo());
	}

	//TODO add getCellCount to IGeometryInfo interface. AK
	public static int getGridCellCount(IGeometryInfo geometryInfo) {
		if (isScalar(geometryInfo)) return 1;

		//if grid.
		if (geometryInfo instanceof ArrayGeometryInfo) return ((ArrayGeometryInfo) geometryInfo).getCellCount();
		if (geometryInfo instanceof IrregularGridGeometryInfo) return ((IrregularGridGeometryInfo) geometryInfo).getCellCount();
		return ((LayeredIrregularGridGeometryInfo) geometryInfo).getCellCount();
	}

	public static boolean isScalar(IPrevExchangeItem prevExchangeItem) {
		//get exchangeItem.
		if (!(prevExchangeItem instanceof IExchangeItem)) {
			//IPrevExchangeItem has no geometryInfo, so can never be a grid.
			return true;
		}
		IExchangeItem exchangeItem = (IExchangeItem) prevExchangeItem;

		return isScalar(exchangeItem.getGeometryInfo());
	}

	public static boolean isScalar(IGeometryInfo geometryInfo) {
		if (geometryInfo == null || geometryInfo instanceof PointGeometryInfo) {//if scalar.
			return true;
		}
		if (geometryInfo instanceof ArrayGeometryInfo || geometryInfo instanceof IrregularGridGeometryInfo || geometryInfo instanceof LayeredIrregularGridGeometryInfo) {//if grid.
			return false;
		}

		throw new RuntimeException("Unknown grid geometryInfo type: " + geometryInfo.getClass().getSimpleName());
	}

	//TODO add getYCoordinates to IGeometryInfo interface. AK
	public static IVector getYCoordinates(IGeometryInfo geometryInfo) {
		if (geometryInfo instanceof ArrayGeometryInfo) return ((ArrayGeometryInfo) geometryInfo).toCurvilinearGeometryInfo().getCellYCoordinates();
		if (geometryInfo instanceof IrregularGridGeometryInfo) return ((IrregularGridGeometryInfo) geometryInfo).getYCoordinates();
		if (geometryInfo instanceof LayeredIrregularGridGeometryInfo) return ((LayeredIrregularGridGeometryInfo) geometryInfo).getYCoordinates();

		throw new RuntimeException("Unknown grid geometryInfo type: " + geometryInfo.getClass().getSimpleName());
	}

	//TODO add getXCoordinates to IGeometryInfo interface. AK
	public static IVector getXCoordinates(IGeometryInfo geometryInfo) {
		if (geometryInfo instanceof ArrayGeometryInfo) return ((ArrayGeometryInfo) geometryInfo).toCurvilinearGeometryInfo().getCellXCoordinates();
		if (geometryInfo instanceof IrregularGridGeometryInfo) return ((IrregularGridGeometryInfo) geometryInfo).getXCoordinates();
		if (geometryInfo instanceof LayeredIrregularGridGeometryInfo) return ((LayeredIrregularGridGeometryInfo) geometryInfo).getXCoordinates();

		throw new RuntimeException("Unknown grid geometryInfo type: " + geometryInfo.getClass().getSimpleName());
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
	public static IVector getObservedValuesBilinearInterpolation(IVector observationXCoordinates, IVector observationYCoordinates, IGeometryInfo modelGeometryInfo, double[] modelValues) {
		if (observationXCoordinates == null) throw new IllegalArgumentException("observationXCoordinates == null");
		if (observationYCoordinates == null) throw new IllegalArgumentException("observationYCoordinates == null");
		if (observationXCoordinates.getSize() != observationYCoordinates.getSize()) throw new IllegalArgumentException("observationXCoordinates.getSize() != observationYCoordinates.getSize()");
		if (modelGeometryInfo == null) throw new IllegalArgumentException("modelGeometryInfo == null");
		if (modelValues == null) throw new IllegalArgumentException("modelValues == null");

		int observationLocationCount = observationXCoordinates.getSize();

		//interpolate values.
		IVector observedModelValues = new Vector(observationLocationCount);
		for (int observationLocationIndex = 0; observationLocationIndex < observationLocationCount; observationLocationIndex++) {
			//this code assumes that the lat,lon coordinates for the observations and for the model are always in the same range, e.g. [-90,90][-180, 180].
			double observationY = observationYCoordinates.getValue(observationLocationIndex);
			double observationX = observationXCoordinates.getValue(observationLocationIndex);
			observedModelValues.setValue(observationLocationIndex, bilinearInterpolateValue(observationX, observationY, modelGeometryInfo, modelValues));
		}
		return observedModelValues;
	}

	private static double bilinearInterpolateValue(double destinationX, double destinationY, IGeometryInfo sourceGeometryInfo, double[] sourceValues) {
		//get source coordinates.
		if (!(sourceGeometryInfo instanceof ArrayGeometryInfo)) {
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for geometryInfo of type "
					+ ArrayGeometryInfo.class.getName() + " not for geometryInfo of type " + sourceGeometryInfo.getClass().getName());
		}
		ArrayGeometryInfo sourceArrayGeometryInfo = ((ArrayGeometryInfo) sourceGeometryInfo);
		if (!sourceArrayGeometryInfo.isRectangular()) throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for a rectangular source grid.");

		double[] rowCenterYCoordinates = sourceArrayGeometryInfo.getRowYCoordinates().getValues();
		double[] columnCenterXCoordinates = sourceArrayGeometryInfo.getColumnXCoordinates().getValues();
		if (rowCenterYCoordinates == null || rowCenterYCoordinates.length <= 1 || columnCenterXCoordinates == null || columnCenterXCoordinates.length <= 1) {//if not a grid.
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for a source grid with at least two rows and at least two columns.");
		}
		if (!isSortedAscending(rowCenterYCoordinates) || !isSortedAscending(columnCenterXCoordinates)) {
			throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".bilinearInterpolateValue() only implemented for a source grid with coordinates that are sorted ascending.");
		}

		boolean rowMajor = isRowMajor(sourceArrayGeometryInfo);
		int rowCount = rowCenterYCoordinates.length;
		int columnCount = columnCenterXCoordinates.length;

		//this code assumes that coordinates are sorted ascending.
		int rowIndexAtOrSouthOfY = getRowIndexOfCellCenterAtOrSouthOfY(rowCenterYCoordinates, destinationY);
		int columnIndexAtOrWestOfX = getColumnIndexOfCellCenterAtOrWestOfX(columnCenterXCoordinates, destinationX);
		int rowIndexNorthOfY = rowIndexAtOrSouthOfY + 1;
		int columnIndexEastOfX = columnIndexAtOrWestOfX + 1;
		//note: this code currently cannot handle destination x,y coordinates that are exactly on the north or east edge of the grid.
		//note: this code currently cannot handle destination x,y coordinates that are in the outer edge of the grid between the edge of the grid and the outermost cell centers.
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

		if (Double.isNaN(northWestValue) || Double.isNaN(southWestValue) || Double.isNaN(southEastValue) || Double.isNaN(northEastValue)) {
			//if one of the surrounding values is missing, then cannot interpolate. Return the value of the grid cell that contains the destination location.
			return snapToContainingGridCell(northWestX, northEastX, southWestY, northWestY, northWestValue, southWestValue, southEastValue, northEastValue, destinationX, destinationY);
		}

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

	private static int getRowIndexOfCellCenterAtOrSouthOfY(double[] rowCenterYCoordinates, double y) {
		return getIndexOfClosestCoordinateEqualToOrSmallerThanGivenCoordinate(rowCenterYCoordinates, y);
	}

	private static int getColumnIndexOfCellCenterAtOrWestOfX(double[] columnCenterXCoordinates, double x) {
		return getIndexOfClosestCoordinateEqualToOrSmallerThanGivenCoordinate(columnCenterXCoordinates, x);
	}

	/**
	 * The given coordinate is "floored" to the nearest smaller coordinate value, then the index of the floored coordinate value is returned.
	 */
	private static int getIndexOfClosestCoordinateEqualToOrSmallerThanGivenCoordinate(double[] coordinates, double coordinate) {
		int i = Arrays.binarySearch(coordinates, coordinate);
		if (i >= 0) return i;
		int insertionIndex = -i - 1;
		if (insertionIndex <= 0) {//if outside grid.
			return -1;
		}
		if (insertionIndex >= coordinates.length) {//if outside grid.
			return coordinates.length;
		}
		return insertionIndex - 1;
	}

	private static int getClosestIndexEqualToOrLargerThanGivenCoordinate(double[] coordinates, double coordinate) {
		int i = Arrays.binarySearch(coordinates, coordinate);
		if (i >= 0) return i;
		int insertionIndex = -i - 1;
		if (insertionIndex <= 0) {//if outside grid.
			return -1;
		}
		if (insertionIndex >= coordinates.length) {//if outside grid.
			return coordinates.length;
		}
		return insertionIndex;
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

	private static boolean isRowMajor(ArrayGeometryInfo sourceArrayGeometryInfo) {
		int[] latitudeValueIndices = sourceArrayGeometryInfo.getLatitudeValueIndices();
		int[] longitudeValueIndices = sourceArrayGeometryInfo.getLongitudeValueIndices();
		if (latitudeValueIndices == null || latitudeValueIndices.length < 1 || longitudeValueIndices == null || longitudeValueIndices.length < 1) {
			throw new IllegalArgumentException("sourceGeometryInfo.latitudeValueIndices or sourceGeometryInfo.longitudeValueIndices not set." +
					" This is needed to determine whether the coordinates in sourceGeometryInfo are stored in rowMajor or columnMajor order.");
		}
		return latitudeValueIndices[0] < longitudeValueIndices[0];
	}

	private static double snapToContainingGridCell(double northWestX, double northEastX, double southWestY, double northWestY,
			double northWestValue, double southWestValue, double southEastValue, double northEastValue, double destinationX, double destinationY) {

		//this code assumes that the edges of the grid cells are exactly in the middle between two adjacent grid cell centers for each grid cell.
		double cellBoundaryX = (northWestX + northEastX) / 2;
		double cellBoundaryY = (southWestY + northWestY) / 2;

		//find out in which quadrant the destination location lies, with some tolerance to account for rounding errors in the coordinates.
		//tolerance of 0.0001 degrees = 11 meters.
		final double tolerance = 1e-4;
		boolean westHalf = destinationX < cellBoundaryX - tolerance;
		boolean eastHalf = destinationX > cellBoundaryX + tolerance;
		boolean southHalf = destinationY < cellBoundaryY - tolerance;
		boolean northHalf = destinationY > cellBoundaryY + tolerance;

		//if quadrant is certain, return its value.
		if (northHalf && westHalf) return northWestValue;
		if (southHalf && westHalf) return southWestValue;
		if (southHalf && eastHalf) return southEastValue;
		if (northHalf && eastHalf) return northEastValue;

		//if quadrant is uncertain (due to the tolerance), but half is certain.
		//within the half choose a quadrant that has a non-missing value.
		if (westHalf) return Double.isNaN(southWestValue) ? northWestValue : southWestValue;
		if (eastHalf) return Double.isNaN(southEastValue) ? northEastValue : southEastValue;
		if (southHalf) return Double.isNaN(southWestValue) ? southEastValue : southWestValue;
		if (northHalf) return Double.isNaN(northWestValue) ? northEastValue : northWestValue;

		//if quadrant is uncertain and half is uncertain (due to the tolerance).
		//choose a quadrant that has a non-missing value.
		if (!Double.isNaN(northWestValue)) return northWestValue;
		if (!Double.isNaN(southWestValue)) return southWestValue;
		if (!Double.isNaN(southEastValue)) return southEastValue;
		if (!Double.isNaN(northEastValue)) return northEastValue;

		//if all surrounding values are missing.
		return Double.NaN;
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
		boolean rectangularStateGeometry = stateArrayGeometryInfo.isRectangular();
		boolean curvilinearStateGeometry = stateArrayGeometryInfo.isCurvilinear();

		//compute weights for each observation location.
		int observationLocationCount = observationXCoordinates.getSize();
		IVector[] weightVectors = new IVector[observationLocationCount];
		for (int observationLocationIndex = 0; observationLocationIndex < observationLocationCount; observationLocationIndex++) {
			double observationX = observationXCoordinates.getValue(observationLocationIndex);
			double observationY = observationYCoordinates.getValue(observationLocationIndex);

			//calculate weights.
			IVector weights;
			if (rectangularStateGeometry) {
				weights = getLocalizationWeightsRectangularStateGeometry(observationX, observationY, stateArrayGeometryInfo, cohnDistanceInMeters);
			} else if (curvilinearStateGeometry) {
				weights = getLocalizationWeightsCurvilinearStateGeometry(observationX, observationY, stateArrayGeometryInfo, cohnDistanceInMeters);
			} else {
				throw new UnsupportedOperationException(GeometryUtils.class.getName() + ".getLocalizationWeights() only implemented for state geometries that are rectangular or curvilinear.");
			}
			weightVectors[observationLocationIndex] = weights;
		}

		return weightVectors;
	}

	private static IVector getLocalizationWeightsRectangularStateGeometry(double observationX, double observationY, ArrayGeometryInfo rectangularStateGeometryInfo, double cohnDistanceInMeters) {
		//each row has a separate y coordinate.
		IVector stateRowYCoordinates = rectangularStateGeometryInfo.getRowYCoordinates();
		int rowCount = stateRowYCoordinates.getSize();
		//each column has a separate x coordinate.
		IVector stateColumnXCoordinates = rectangularStateGeometryInfo.getColumnXCoordinates();
		int columnCount = stateColumnXCoordinates.getSize();
		boolean rowMajor = isRowMajor(rectangularStateGeometryInfo);

		//first create a vector with only 0 values, then only fill in the values that are not 0.
		IVector weights = new Vector(rowCount*columnCount);

		//determine bounding latitudes of region of influence of the given observation. All weights outside this region are 0 anyway.
		//always use all columns, because determining the bounding longitudes would be difficult for three reasons:
		//1. thresholdLongitudeDifferenceInDegrees depends on latitude.
		//2. region of influence can wrap around the pole(s).
		//3. region of influence can wrap around the dateline.
		double thresholdLatitudeDifferenceInDegrees = 2*cohnDistanceInMeters / METERS_PER_DEGREE;
		//minLat and maxLat are both inclusive.
		double minLat = observationY - thresholdLatitudeDifferenceInDegrees;
		double maxLat = observationY + thresholdLatitudeDifferenceInDegrees;
		//this code assumes that the coordinates are sorted ascending.
		//minRow and maxRow are both inclusive.
		int minRow = getClosestIndexEqualToOrLargerThanGivenCoordinate(stateRowYCoordinates.getValues(), minLat);
		if (minRow >= rowCount) {//if region of influence completely outside grid.
			//return all 0 weights.
			return weights;
		}
		if (minRow < 0) minRow = 0;
		int maxRow = getIndexOfClosestCoordinateEqualToOrSmallerThanGivenCoordinate(stateRowYCoordinates.getValues(), maxLat);
		if (maxRow < 0) {//if region of influence completely outside grid.
			//return all 0 weights.
			return weights;
		}
		if (maxRow >= rowCount) maxRow = rowCount - 1;

		//loop over state grid cells.
		for (int row = minRow; row <= maxRow; row++) {
			for (int column = 0; column < columnCount; column++) {
				int stateCellIndex = getCellIndex(row, column, rowMajor, rowCount, columnCount);
				//this code assumes that the lat,lon coordinates for the observations and for the state are always in the same range, e.g. [-90,90][-180, 180].
				//weights do not have to be normalised, i.e. sum of weights does not have to be 1.
				weights.setValue(stateCellIndex, calculateCohnWeightLatLonCoordinates(observationY, observationX,
						stateRowYCoordinates.getValue(row), stateColumnXCoordinates.getValue(column), cohnDistanceInMeters));
			}
		}

		return weights;
	}

	private static IVector getLocalizationWeightsCurvilinearStateGeometry(double observationX, double observationY, ArrayGeometryInfo curvilinearStateGeometryInfo, double cohnDistanceInMeters) {
		//get x,y coordinates for each grid cell.
		IVector stateCellYCoordinates = curvilinearStateGeometryInfo.getCellYCoordinates();
		IVector stateCellXCoordinates = curvilinearStateGeometryInfo.getCellXCoordinates();
		int stateCellCount = curvilinearStateGeometryInfo.getCellCount();

		//loop over state grid cells.
		IVector weights = new Vector(stateCellCount);
		double thresholdDistanceInDegrees = 2*cohnDistanceInMeters / METERS_PER_DEGREE;
		for (int stateCellIndex = 0; stateCellIndex < stateCellCount; stateCellIndex++) {
			//this code assumes that the lat,lon coordinates for the observations and for the state are always in the same range, e.g. [-90,90][-180, 180].
			//weights do not have to be normalised, i.e. sum of weights does not have to be 1.
			weights.setValue(stateCellIndex, calculateCohnWeightLatLonCoordinates(observationY, observationX,
					stateCellYCoordinates.getValue(stateCellIndex), stateCellXCoordinates.getValue(stateCellIndex), cohnDistanceInMeters, thresholdDistanceInDegrees));
		}
		return weights;
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
		double dy = (lat2 - lat1) * METERS_PER_DEGREE;
		double phi = (lat2 + lat1) * 0.017453 / 2;
		double dx = (lon2 - lon1) * METERS_PER_DEGREE * Math.cos(phi);

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
		if (cohnDistanceInMeters <= 0) throw new IllegalArgumentException("cohnDistanceInMeters <= 0");

		//compute distance on a sphere.
		double distanceInMeters = distanceInMeters(lat1, lon1, lat2, lon2);
		return calculateCohnWeight(distanceInMeters, cohnDistanceInMeters);
	}

	/**
	 * Calculates localization weight between the two given points according to Cohn's formula.
	 * This method only works for WGS84 lat,lon coordinates in degrees.
	 *
	 * @param lat1 latitude of point 1
	 * @param lon1 longitude of point 1
	 * @param lat2 latitude of point 2
	 * @param lon2 longitude of point 2
	 * @param cohnDistanceInMeters characteristic distance for Cohn's formula in meters
	 * @param thresholdDistanceInDegrees 2 * characteristic distance for Cohn's formula in degrees
	 * @return weight
	 */
	private static double calculateCohnWeightLatLonCoordinates(double lat1, double lon1, double lat2, double lon2, double cohnDistanceInMeters, double thresholdDistanceInDegrees) {
		if (cohnDistanceInMeters <= 0) throw new IllegalArgumentException("cohnDistanceInMeters <= 0");

		//check distance in latitude direction.
		//Note: check on distance in longitude direction would be more complicated, because shortest distance between two points on the earth can wrap around the dateline and/or poles.
		double deltaLatitudeAbsolute = Math.abs(lat2 - lat1);
		if (deltaLatitudeAbsolute > thresholdDistanceInDegrees) {
			//weights outside radius of 2*cohnDistance are 0.
			return 0;
		}

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
		if (cohnDistanceInMeters <= 0) throw new IllegalArgumentException("cohnDistanceInMeters <= 0");

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
