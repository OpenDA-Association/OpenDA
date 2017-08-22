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

package org.openda.model_wflow;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITime;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;

/**
 * Exchange item representing a 2D map with values for the wflow model.
 *
 * @author Arno Kockx
 */
public class Wflow2DMapStateExchangeItem implements IExchangeItem {
	private final String variableName;
    private final IPrevExchangeItem.Role role;
	private final ITime timeHorizon;
	private final WflowPythonToJavaAdapter adapter;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;

	private final int[] mask;
	private final int activeGridCellCount;

	/**
	 * @param variableName the name of the variable as used by the wflow model.
	 * @param role
	 * @param timeHorizon
	 * @param adapter
	 */
	public Wflow2DMapStateExchangeItem(String variableName, IPrevExchangeItem.Role role,
			IQuantityInfo quantityInfo, ArrayGeometryInfo geometryInfo, ITime timeHorizon, WflowPythonToJavaAdapter adapter) {
		this.variableName = variableName;
		this.role = role;
		this.quantityInfo = quantityInfo;
		this.geometryInfo = geometryInfo;
		this.timeHorizon = timeHorizon;
		this.adapter = adapter;

		//init mask.
		this.mask = geometryInfo.getActiveCellMask();
		this.activeGridCellCount = mask.length - BBUtils.count(mask, 0);
	}

	public String getId() {
		return this.variableName;
	}

	public String getDescription() {
		return null;
	}

	public Role getRole() {
		return this.role;
	}

	public ITimeInfo getTimeInfo() {
		return new TimeInfo(getTimes());
	}

	public double[] getTimes() {
		//return current time, since the wflow model only stores the current values in memory.
		return new double[]{this.adapter.getCurrentTime(this.timeHorizon).getMJD()};
	}

	public void setTimes(double[] times) {
		throw new RuntimeException(this.getClass().getName() + ": setting time stamps not supported for wflow model.");
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	public Object getValues() {
		double[] values = getValuesAsDoubles();
		IVector vector = new Vector(values);
//		return new TreeVector(getId(), vector, rowCount, columnCount);
		return vector;
	}

	/**
	 * Returns only the current values, since the wflow model only stores the current values in memory.
	 * The values of inactive grid cells are converted to Double.NaN, because the algorithms cannot cope with inactive grid cells.
	 */
	public double[] getValuesAsDoubles() {
		return getConvertedValues();
	}

	/**
	 * Returns only the current values, since the wflow model only stores the current values in memory.
	 * The values of inactive grid cells are converted to Double.NaN, because the algorithms cannot cope with inactive grid cells.
	 */
	private double[] getConvertedValues() {
		double[] allValues = getAllValues();
    	if (this.activeGridCellCount == this.mask.length) {//if all cells are active.
    		return allValues;
    	}

		//change the values of the inactive grid cells to Double.NaN.
		double[] convertedValues = new double[allValues.length];
		for (int n = 0; n < convertedValues.length; n++) {
			if (this.mask[n] == 0) {
				convertedValues[n] = Double.NaN;
			} else {
				convertedValues[n] = allValues[n];
			}
		}
		return convertedValues;
	}

	/**
	 * Returns only the current values, since the wflow model only stores the current values in memory.
	 */
	private double[] getAllValues() {
		double[] allValues = this.adapter.getMapAsList(this.variableName);
    	if (allValues.length != this.mask.length) {
    		throw new IllegalStateException(this.getClass().getName() + ": allValues.length (" + allValues.length
    				+ ") must equal mask.length (" + this.mask.length + ") for variable " + this.variableName + ".");
        }
    	return allValues;
	}

	/**
	 * Only changes the current values, since the wflow model only stores the current values in memory.
	 * Only changes the values of the active grid cells.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues) {
    	if (axpyValues.length != this.mask.length) {
    		throw new IllegalStateException(this.getClass().getName() + ": axpyValues.length (" + axpyValues.length
    				+ ") must equal gridCellCount (" + this.mask.length + ") for variable " + this.variableName + ".");
        }

    	double[] allValues = getAllValues();
        for (int n = 0; n < allValues.length; n++) {
    		if (this.mask[n] != 0) {
    			allValues[n] += alpha * axpyValues[n];
			}
        }
        setAllValues(allValues);
    }

	/**
	 * Only changes the current values, since the wflow model only stores the current values in memory.
	 * Only changes the values of the active grid cells.
	 */
	public void multiplyValues(double[] multiplicationFactors) {
    	if (multiplicationFactors.length != this.mask.length) {
    		throw new IllegalStateException(this.getClass().getName() + ": multiplicationFactors.length (" + multiplicationFactors.length
    				+ ") must equal gridCellCount (" + this.mask.length + ") for variable " + this.variableName + ".");
        }

    	double[] allValues = getAllValues();
        for (int n = 0; n < allValues.length; n++) {
    		if (this.mask[n] != 0) {
    			allValues[n] *= multiplicationFactors[n];
			}
        }
		setAllValues(allValues);
	}

	public void setValues(Object vector) {
        if (!(vector instanceof IVector)) {
    		throw new IllegalArgumentException(this.getClass().getName() + ": supply values as an IVector not as " + vector.getClass().getName());
        }

        setValuesAsDoubles(((IVector) vector).getValues());
	}

	/**
	 * Sets only the current values, since the wflow model only stores the current values in memory.
	 * Sets the values of all grid cells, also of the inactive grid cells. This is not a problem,
	 * because the wflow model ignores the values of the inactive grid cells anyway.
	 */
	public void setValuesAsDoubles(double[] values) {
		setAllValues(values);
	}

	/**
	 * Sets only the current values, since the wflow model only stores the current values in memory.
	 * Sets the values of all grid cells, also of the inactive grid cells. This is not a problem,
	 * because the wflow model ignores the values of the inactive grid cells anyway.
	 */
	private void setAllValues(double[] allValues) {
    	if (allValues.length != this.mask.length) {
    		throw new IllegalStateException(this.getClass().getName() + ": allValues.length (" + allValues.length
    				+ ") must equal mask.length (" + this.mask.length + ") for variable " + this.variableName + ".");
        }

    	this.adapter.setValues(this.variableName, allValues);
    }

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (!(sourceItem instanceof NetcdfGridTimeSeriesExchangeItem)) {
			throw new UnsupportedOperationException(getClass().getName() + ": can only copy data from an exchangeItem of type "
					+ NetcdfGridTimeSeriesExchangeItem.class.getSimpleName());
		}
		if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because it contains no time info.");
		}
		if (sourceItem.getValuesType() != ValueType.IVectorType) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because its value type is not " + ValueType.IVectorType);
		}

		//get index of wantedTime in sourceItem.
		double[] sourceTimes = sourceItem.getTimeInfo().getTimes();
		double wantedTime = this.getTimeInfo().getTimes()[0];
		if (this.role == Role.Input) {
			//for a computation timeStep from t1 to t2 the wflow model needs as input the input data from t2.
			//Therefore if role is input, then this exchangeItem should only store the values
			//for the next timeStep after current time.
			//copy values for next timeStep from sourceItem.
			wantedTime += this.timeHorizon.getStepMJD();
		}
		int wantedTimeIndex = TimeUtils.findMatchingTimeIndex(sourceTimes, wantedTime, 1e-5);
		if (wantedTimeIndex == -1) {//if sourceItem does not contain wantedTime.
			//no data to copy.
			return;
		}

		//get values for wanted time.
		double[] valuesForWantedTime = ((NetcdfGridTimeSeriesExchangeItem) sourceItem).getValuesAsDoublesForSingleTimeIndex(wantedTimeIndex);
		this.setAllValues(valuesForWantedTime);
	}

	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public IGeometryInfo getGeometryInfo() {
		return this.geometryInfo;
	}
}
