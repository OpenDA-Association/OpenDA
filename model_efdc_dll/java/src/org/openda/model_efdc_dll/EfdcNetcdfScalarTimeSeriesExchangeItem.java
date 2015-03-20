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

package org.openda.model_efdc_dll;

import java.util.ArrayList;
import java.util.List;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;

/**
 * Exchange item representing values for a time series for a single location
 * that are stored in a netcdf file and can be read from there (but not written).
 *
 * @author Arno Kockx
 */
@SuppressWarnings("serial")
public class EfdcNetcdfScalarTimeSeriesExchangeItem implements IExchangeItem {

	private final int locationDimensionIndex;
	private final int locationIndex;
	private final String id;
	private final Role role;
	/**
	* ITimeInfo that stores all times.
	*/
	private final ITimeInfo allTimesInfo;
	private final IQuantityInfo quantityInfo;
	private final EfdcNetcdfDataObject netcdfDataObject;

	/**
	 * ITimeInfo that stores only the times for which there are non-missing values.
	 * This is needed because the algorithms cannot cope with missing values.
	 * This object needs to be updated whenever any missing values are changed to
	 * non-missing values or vice versa.
	 */
	private ITimeInfo timesWithNonMissingValuesInfo;

	public EfdcNetcdfScalarTimeSeriesExchangeItem(int locationDimensionIndex, int locationIndex,
			String locationId, String parameterId, Role role, ITimeInfo allTimesInfo, EfdcNetcdfDataObject netcdfDataObject) {
		this.locationDimensionIndex = locationDimensionIndex;
		this.locationIndex = locationIndex;
		//id = "locationId.parameterId"
		this.id = locationId + "." + parameterId;
		this.role = role;
		this.allTimesInfo = allTimesInfo;
		this.quantityInfo = new QuantityInfo(parameterId, "unknown");
		this.netcdfDataObject = netcdfDataObject;

		//determine times for which there are non-missing values for this scalar time series.
		//This is needed because the algorithms cannot cope with missing values.
		double[] allTimes = this.allTimesInfo.getTimes();
		double[] allValues = getAllValues();
		List<Double> timesWithNonMissingValues = new ArrayList<Double>();
		for (int n = 0; n < allValues.length; n++) {
			if (!Double.isNaN(allValues[n])) {
				timesWithNonMissingValues.add(allTimes[n]);
			}
		}
		this.timesWithNonMissingValuesInfo = new TimeInfo(BBUtils.unbox(
				timesWithNonMissingValues.toArray(new Double[timesWithNonMissingValues.size()])));
	}

	public String getId() {
		return this.id;
	}

	public String getDescription() {
		return null;
	}

	public Role getRole() {
		return this.role;
	}

	/**
	 * Returns all times for which there are non-missing values for this scalar time series.
	 */
	public ITimeInfo getTimeInfo() {
		return this.timesWithNonMissingValuesInfo;
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public double[] getTimes() {
		return getTimeInfo().getTimes();
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setTimes not implemented.");
	}

	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	/**
	 * Returns all non-missing values for this scalar time series.
	 */
	public Object getValues() {
		double[] values = getValuesAsDoubles();
		IVector vector = new Vector(values);
		return vector;
	}

	/**
	 * Returns all non-missing values for this scalar time series.
	 */
	public double[] getValuesAsDoubles() {
		double[] allValues = getAllValues();
		int numberOfTimesWithNonMissingValues = this.timesWithNonMissingValuesInfo.getTimes().length;
		if (numberOfTimesWithNonMissingValues == allValues.length) {//if all values are non-missing.
			return allValues;
		}

		//filter only the non-missing values.
		double[] values = new double[numberOfTimesWithNonMissingValues];
		int index = 0;
		for (int n = 0; n < allValues.length; n++) {
			if (!Double.isNaN(allValues[n])) {
				values[index++] = allValues[n];
			}
		}
		return values;
	}

	/**
	 * Returns all values for this scalar time series (also missing values).
	 */
	private double[] getAllValues() {
		return this.netcdfDataObject.readDataForExchangeItemForSingleLocation(this, this.locationDimensionIndex, this.locationIndex);
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": axpyOnValues not implemented.");
	}

	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": multiplyValues not implemented.");
	}

	public void setValues(Object vector) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setValues not implemented.");
	}

	public void setValuesAsDoubles(double[] values) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setValuesAsDoubles not implemented.");
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": copyValuesFromItem not implemented.");
	}
}
