/* OpenDA v2.4.3 
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

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;

/**
 * Exchange item representing values for a grid time series
 * that are stored in a netcdf file.
 *
 * @author Arno Kockx
 */
public class NetcdfGridTimeSeriesExchangeItem implements IGridTimeSeriesExchangeItem {
	//TODO remove hacks for writing per timeStep by using NetcdfGridExchangeItemWriter. AK

	private final ITimeInfo timeInfo;
	private final String id;
	private final int realizationDimensionIndex;
	private final int realizationIndex;
	private final Role role;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;
	private final NetcdfDataObject netcdfDataObject;
	private final int timeDimensionIndex;
	private final int dimensionIndexToFlipForReadData;

	/**
	 * @param id
	 * @param role
	 * @param timeInfo
	 * @param quantityInfo
	 * @param geometryInfo
	 * @param netcdfDataObject
	 * @param timeDimensionIndex
	 * @param dimensionIndexToFlipForReadData index of dimension to flip for read data. If this is -1, then nothing is flipped.
	 */
	public NetcdfGridTimeSeriesExchangeItem(String id, int realizationDimensionIndex, int realizationIndex, Role role, ITimeInfo timeInfo, IQuantityInfo quantityInfo,
			IGeometryInfo geometryInfo, NetcdfDataObject netcdfDataObject, int timeDimensionIndex,
			int dimensionIndexToFlipForReadData) {
		this.id = id;
		this.realizationDimensionIndex = realizationDimensionIndex;
		this.realizationIndex = realizationIndex;
		this.role = role;
		this.timeInfo = timeInfo;
		this.quantityInfo = quantityInfo;
		this.geometryInfo = geometryInfo;
		this.netcdfDataObject = netcdfDataObject;
		this.timeDimensionIndex = timeDimensionIndex;
		this.dimensionIndexToFlipForReadData = dimensionIndexToFlipForReadData;
	}

	public NetcdfGridTimeSeriesExchangeItem(String id, Role role, ITimeInfo timeInfo, IQuantityInfo quantityInfo,
			IGeometryInfo geometryInfo, NetcdfDataObject netcdfDataObject, int timeDimensionIndex, int dimensionIndexToFlipForReadData) {
		this(id, -1, -1, role, timeInfo, quantityInfo, geometryInfo, netcdfDataObject, timeDimensionIndex, dimensionIndexToFlipForReadData);
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
		//within one NetCDF file the grid geometry is constant in time.
		return this.geometryInfo;
	}

	public IGeometryInfo getGeometryInfoForSingleTimeIndex(int timeIndex) {
		//within one NetCDF file the grid geometry is constant in time.
		return this.geometryInfo;
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	public Object getValues() {
		throw new IllegalStateException("values depend on time. Call method getValuesAsDoublesForSingleTimeIndex instead.");
	}

	public double[] getValuesAsDoubles() {
		throw new IllegalStateException("values depend on time. Call method getValuesAsDoublesForSingleTimeIndex instead.");
	}

	public double[] getValuesAsDoublesForSingleTimeIndex(int timeIndex) {
		if (this.realizationDimensionIndex == -1) {
			return this.netcdfDataObject.readDataForExchangeItemFor2DGridForSingleTime(this, this.timeDimensionIndex, timeIndex,
					this.dimensionIndexToFlipForReadData);
		} else {//if ensemble exchange item.
			return this.netcdfDataObject.readDataForExchangeItemFor2DGridForSingleTimeAndRealization(this, this.realizationDimensionIndex, this.realizationIndex, this.timeDimensionIndex, timeIndex,
					this.dimensionIndexToFlipForReadData);
		}

	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new IllegalStateException("values depend on time. Call method axpyOnValuesForSingleTimeIndex instead.");
	}

	public void axpyOnValuesForSingleTimeIndex(int timeIndex, double alpha, double[] axpyValues) {
		double[] values = getValuesAsDoublesForSingleTimeIndex(timeIndex);
		if (axpyValues.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": axpyValues.length (" + axpyValues.length
					+ ") must equal cellCount (" + values.length + ") for variable " + this.id + '.');
		}

		for (int n = 0; n < values.length; n++) {
			values[n] += alpha * axpyValues[n];
		}
		setValuesAsDoublesForSingleTimeIndex(timeIndex, values);
	}

	public void multiplyValues(double[] multiplicationFactors) {
		throw new IllegalStateException("values depend on time. Call method multiplyValuesForSingleTimeIndex instead.");
	}

	public void multiplyValuesForSingleTimeIndex(int timeIndex, double[] multiplicationFactors) {
		double[] values = getValuesAsDoublesForSingleTimeIndex(timeIndex);
		if (multiplicationFactors.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": multiplicationFactors.length (" + multiplicationFactors.length
					+ ") must equal cellCount (" + values.length + ") for variable " + this.id + '.');
		}

		for (int n = 0; n < values.length; n++) {
			values[n] *= multiplicationFactors[n];
		}
		setValuesAsDoublesForSingleTimeIndex(timeIndex, values);
	}

	public void setValues(Object vector) {
		throw new IllegalStateException("values depend on time. Call method setValuesAsDoublesForSingleTimeIndex instead.");
	}

	public void setValuesAsDoubles(double[] values) {
		throw new IllegalStateException("values depend on time. Call method setValuesAsDoublesForSingleTimeIndex instead.");
	}

    public void setValuesAsDoublesForSingleTimeIndex(int timeIndex, double[] values) {
        //write all values for given timeIndex.
		this.netcdfDataObject.makeSureFileHasBeenCreated();
        this.netcdfDataObject.writeDataForExchangeItemForSingleTime(this, this.timeDimensionIndex, timeIndex, values);
    }

	public void setValuesAsDoublesForSingleTime(double sourceTime, double[] valuesForSourceTime) {
		//get index of sourceTime in this exchangeItem.
		double[] times = this.getTimeInfo().getTimes();
		int timeIndexOfSourceTime = TimeUtils.findMatchingTimeIndex(times, sourceTime, 1e-5);
		if (timeIndexOfSourceTime == -1) {//if this exchangeItem does not contain sourceTime.
			//no data to write.
			return;
		}
		setValuesAsDoublesForSingleTimeIndex(timeIndexOfSourceTime, valuesForSourceTime);
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

        //get active values for current time.
        double[] valuesForSourceTime = ((IVector) sourceItem.getValues()).getValues();

        //add missing values for non-active grid cells.
        IGeometryInfo sourceGeometryInfo = sourceItem.getGeometryInfo();
		valuesForSourceTime = NetcdfUtils.addMissingValuesForNonActiveGridCells(sourceGeometryInfo, valuesForSourceTime);

        double sourceTime = sourceItem.getTimeInfo().getTimes()[0];
		setValuesAsDoublesForSingleTime(sourceTime, valuesForSourceTime);
	}
}
