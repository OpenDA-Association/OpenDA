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

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IVector;

/**
 * Exchange item representing values for a grid time series
 * that are stored in a netcdf file and can be written there (but not read).
 *
 * @author Arno Kockx
 */
@SuppressWarnings("serial")
public class EfdcNetcdfGridTimeSeriesExchangeItem implements IExchangeItem {

	private final ITimeInfo timeInfo;
	private final String id;
	private final Role role;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;
	private final EfdcNetcdfDataObject netcdfDataObject;
	private final int timeDimensionIndex;

	public EfdcNetcdfGridTimeSeriesExchangeItem(String id, Role role, ITimeInfo timeInfo, IQuantityInfo quantityInfo,
			IGeometryInfo geometryInfo, EfdcNetcdfDataObject netcdfDataObject, int timeDimensionIndex) {
		this.id = id;
		this.role = role;
		this.timeInfo = timeInfo;
		this.quantityInfo = quantityInfo;
		this.geometryInfo = geometryInfo;
		this.netcdfDataObject = netcdfDataObject;
		this.timeDimensionIndex = timeDimensionIndex;
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

	public ITimeInfo getTimeInfo() {
		return this.timeInfo;
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public double[] getTimes() {
		//delegate to new getTimeInfo method.
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
		return this.geometryInfo;
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	public Object getValues() {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": getValues not implemented.");
	}

	public double[] getValuesAsDoubles() {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": getValuesAsDoubles not implemented.");
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

	/**
	 * From the given sourceItem copies only the values for the times
	 * that are both present in the given sourceItem and in this exchangeItem.
	 *
	 * Currently this method only works for source exchangeItems that store values for only one time.
	 */
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null
				|| sourceItem.getTimeInfo().getTimes().length != 1) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because it contains no time info or stores values for more than one time.");
		}
		if (sourceItem.getValuesType() != ValueType.IVectorType) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because its value type is not " + ValueType.IVectorType);
		}

		//get index of sourceTime in this exchangeItem.
		double sourceTime = sourceItem.getTimeInfo().getTimes()[0];
		double[] times = this.getTimeInfo().getTimes();
		int sourceTimeIndex = TimeUtils.findMatchingTimeIndex(times, sourceTime, 1e-5);
		if (sourceTimeIndex == -1) {//if this exchangeItem does not contain sourceTime.
			//no data to copy.
			return;
		}

		//write values for current time.
		double[] valuesForSourceTime = ((IVector) sourceItem.getValues()).getValues();
		this.netcdfDataObject.writeDataForExchangeItemForSingleTime(this, this.timeDimensionIndex, sourceTimeIndex, valuesForSourceTime);
	}
}
