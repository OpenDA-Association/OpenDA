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

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Exchange item representing values for a grid time series
 * without the missing values. This can only read data (not write).
 *
 * @author Arno Kockx
 */
public class NonMissingStochObserverGridTimeSeriesExchangeItem implements IGridTimeSeriesExchangeItem {
	//wrapped item.
	private final IGridTimeSeriesExchangeItem exchangeItem;

	/**
	 * This exchangeItem wraps the given exchangeItem and ignores any missing values while reading.
	 * This assumes that missing values are always Double.NaN.
	 */
	public NonMissingStochObserverGridTimeSeriesExchangeItem(IGridTimeSeriesExchangeItem exchangeItem) {
		this.exchangeItem = exchangeItem;
	}

	public String getId() {
		return this.exchangeItem.getId();
	}

	public String getDescription() {
		return this.exchangeItem.getDescription();
	}

	public Role getRole() {
		return this.exchangeItem.getRole();
	}

	public ITimeInfo getTimeInfo() {
		return this.exchangeItem.getTimeInfo();
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public double[] getTimes() {
		return this.exchangeItem.getTimes();
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public void setTimes(double[] times) {
		throw new RuntimeException(getClass().getSimpleName() + ": setTimes not allowed for observation data.");
	}

	public IQuantityInfo getQuantityInfo() {
		return this.exchangeItem.getQuantityInfo();
	}

	public IGeometryInfo getGeometryInfo() {
		throw new IllegalStateException("geometryInfo depends on time. Call method getGeometryInfoForSingleTimeIndex instead.");
	}

	/**
	 * Returns a geometryInfo that contains only the grid cells for which there are non-missing values for the given timeIndex.
	 */
	public IGeometryInfo getGeometryInfoForSingleTimeIndex(int timeIndex) {
		//get values.
		double[] values = getAllValuesForSingleTimeIndex(timeIndex);
		if (!BBUtils.containsNaN(values)) {
			return this.exchangeItem.getGeometryInfoForSingleTimeIndex(timeIndex);
		}

		//get geometryInfo.
		IGeometryInfo geometryInfo = this.exchangeItem.getGeometryInfoForSingleTimeIndex(timeIndex);
		if (!(geometryInfo instanceof ArrayGeometryInfo || geometryInfo instanceof IrregularGridGeometryInfo)) {
			throw new RuntimeException("No coordinates available for exchangeItem " + this.exchangeItem.getId());
		}

		//get coordinates.
		//this code assumes that the coordinates are stored in the same order as the values in the exchangeItem.
		//need one coordinate for each grid cell.
		IVector yCoordinates = (geometryInfo instanceof ArrayGeometryInfo) ? ((ArrayGeometryInfo) geometryInfo).toCurvilinearGeometryInfo().getCellYCoordinates()
				: ((IrregularGridGeometryInfo) geometryInfo).getYCoordinates();
		//this code assumes that the coordinates are stored in the same order as the values in the exchangeItem.
		//need one coordinate for each grid cell.
		IVector xCoordinates = (geometryInfo instanceof ArrayGeometryInfo) ? ((ArrayGeometryInfo) geometryInfo).toCurvilinearGeometryInfo().getCellXCoordinates()
				: ((IrregularGridGeometryInfo) geometryInfo).getXCoordinates();

		//filter out cells with missing values.
		List<Double> nonMissingXCoordinatesList = new ArrayList<Double>(values.length);
		List<Double> nonMissingYCoordinatesList = new ArrayList<Double>(values.length);
		for (int cellIndex = 0; cellIndex < values.length; cellIndex++) {
			if (Double.isNaN(values[cellIndex])) {
				continue;
			}
			nonMissingXCoordinatesList.add(xCoordinates.getValue(cellIndex));
			nonMissingYCoordinatesList.add(yCoordinates.getValue(cellIndex));
		}
		double[] nonMissingXCoordinates = BBUtils.unbox(nonMissingXCoordinatesList.toArray(new Double[nonMissingXCoordinatesList.size()]));
		double[] nonMissingYCoordinates = BBUtils.unbox(nonMissingYCoordinatesList.toArray(new Double[nonMissingYCoordinatesList.size()]));

		return new IrregularGridGeometryInfo(nonMissingXCoordinates.length, nonMissingXCoordinates, nonMissingYCoordinates);
	}

	public ValueType getValuesType() {
		return this.exchangeItem.getValuesType();
	}

	public Class<?> getValueType() {
		return this.exchangeItem.getValueType();
	}

	public Object getValues() {
		throw new IllegalStateException("values depend on time. Call method getValuesAsDoublesForSingleTimeIndex instead.");
	}

	public double[] getValuesAsDoubles() {
		throw new IllegalStateException("values depend on time. Call method getValuesAsDoublesForSingleTimeIndex instead.");
	}

	public double[] getValuesAsDoublesForSingleTimeIndex(int timeIndex) {
		double[] values = getAllValuesForSingleTimeIndex(timeIndex);
		if (!BBUtils.containsNaN(values)) {
			return values;
		}

		//filter out missing values.
		List<Double> nonMissingValuesList = new ArrayList<Double>(values.length);
		for (int cellIndex = 0; cellIndex < values.length; cellIndex++) {
			double value = values[cellIndex];
			if (Double.isNaN(value)) {
				continue;
			}
			nonMissingValuesList.add(value);
		}
		return BBUtils.unbox(nonMissingValuesList.toArray(new Double[nonMissingValuesList.size()]));
	}

	private double[] getAllValuesForSingleTimeIndex(int timeIndex) {
		return this.exchangeItem.getValuesAsDoublesForSingleTimeIndex(timeIndex);
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new RuntimeException(getClass().getSimpleName() + ": axpyOnValues not allowed for observation data.");
	}

	public void axpyOnValuesForSingleTimeIndex(int timeIndex, double alpha, double[] axpyValues) {
		throw new RuntimeException(getClass().getSimpleName() + ": axpyOnValuesForSingleTimeIndex not allowed for observation data.");
	}

	public void multiplyValues(double[] multiplicationFactors) {
		throw new RuntimeException(getClass().getSimpleName() + ": multiplyValues not allowed for observation data.");
	}

	public void multiplyValuesForSingleTimeIndex(int timeIndex, double[] multiplicationFactors) {
		throw new RuntimeException(getClass().getSimpleName() + ": multiplyValuesForSingleTimeIndex not allowed for observation data.");
	}

	public void setValues(Object values) {
		throw new RuntimeException(getClass().getSimpleName() + ": setValues not allowed for observation data.");
	}

	public void setValuesAsDoubles(double[] values) {
		throw new RuntimeException(getClass().getSimpleName() + ": setValuesAsDoubles not allowed for observation data.");
	}

    public void setValuesAsDoublesForSingleTimeIndex(int timeIndex, double[] values) {
		throw new RuntimeException(getClass().getSimpleName() + ": setValuesAsDoublesForSingleTimeIndex not allowed for observation data.");
    }

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException(getClass().getSimpleName() + ": copyValuesFromItem not allowed for observation data.");
	}
}
