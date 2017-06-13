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
package org.openda.model_efdc_dll;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.IrregularGridGeometryInfo;
import org.openda.exchange.LayeredIrregularGridGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.*;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

/**
 * Exchange item representing a 2D grid with values for a single time
 * that are stored in the dll version of the EFDC model.
 *
 * @author Arno Kockx
 */
@SuppressWarnings("serial")
public class EfdcGridExchangeItem implements IExchangeItem {

	private final String id;
	/**
	 * Integer that corresponds to a certain parameter within the efdc model.
	 */
	private final int parameterNumber;
	private final Role role;
	private final EfdcDLL modelDll;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;

	public EfdcGridExchangeItem(int parameterNumber, String parameterId, Role role, EfdcDLL modelDll) {
		//id = "locationId.parameterId"
		this.id = BBUtils.getIdForGrid(parameterId);
		this.parameterNumber = parameterNumber;
		this.role = role;
		this.modelDll = modelDll;
		this.quantityInfo = new QuantityInfo(parameterId, "unknown");

		int layerCount = getLayerCount();
		if (layerCount <= 1) {
			this.geometryInfo = new IrregularGridGeometryInfo(getCellCount(), null, null);
		} else {//if multiple layers.
			//convert relative layer thicknesses to relative z-coordinates.
			//in EFDC the layers do not have z coordinates, because for each grid cell the layers have a different absolute thickness (because the total water depth in the river varies)
			//and a different absolute height (because the river goes downstream). The layers only have relative thicknesses (without a unit) that sum to a total of 1 by definition.
			double[] relativeThicknesses = this.modelDll.getLayerDepths();
			double[] zCoordinates = new double[relativeThicknesses.length];
			//in EFDC the layers start counting at the bottom, so the first layer is the lowest layer.
			double z = 0;
			for (int n = 0; n < zCoordinates.length; n++) {
				zCoordinates[n] = z;
				z += relativeThicknesses[n];
			}
			this.geometryInfo = new LayeredIrregularGridGeometryInfo(getCellCount(), layerCount, null, null, zCoordinates);
		}
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
		//return current time, since the efdc model only stores the current values in memory.
		return new TimeInfo(new double[]{this.modelDll.getCurrentTime()});
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

	private int getCellCount() {
		return this.modelDll.getCellCount(this.parameterNumber);
	}

	private int getLayerCount() {
		return this.modelDll.getLayerCount(this.parameterNumber);
	}

	/**
	 * Returns only the current values, since the efdc model only stores the current values in memory.
	 */
	public Object getValues() {
		double[] values = getValuesAsDoubles();
		int layerCount = getLayerCount();
		if (layerCount <= 1) {
			return new Vector(values);
		} else {//if multiple layers.
			return new TreeVector(Integer.toString(this.parameterNumber), new Vector(values), layerCount, getCellCount());
		}
	}

	/**
	 * Returns only the current values, since the efdc model only stores the current values in memory.
	 */
	public double[] getValuesAsDoubles() {
		return this.modelDll.getValues(this.parameterNumber);
	}

	/**
	 * Only changes the current values, since the efdc model only stores the current values in memory.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues) {
		double[] values = getValuesAsDoubles();
		if (axpyValues.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": axpyValues.length (" + axpyValues.length
					+ ") must equal valueCount (" + values.length + ") for variable " + this.id + ".");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] += alpha * axpyValues[n];
		}
		setValuesAsDoubles(values);
	}

	/**
	 * Only changes the current values, since the efdc model only stores the current values in memory.
	 */
	public void multiplyValues(double[] multiplicationFactors) {
		double[] values = getValuesAsDoubles();
		if (multiplicationFactors.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": multiplicationFactors.length (" + multiplicationFactors.length
					+ ") must equal valueCount (" + values.length + ") for variable " + this.id + ".");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] *= multiplicationFactors[n];
		}
		setValuesAsDoubles(values);
	}

	/**
	 * Only sets the current values, since the efdc model only stores the current values in memory.
	 */
	public void setValues(Object vector) {
		if (!(vector instanceof IVector)) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": supply values as an IVector not as " + vector.getClass().getSimpleName());
		}

		setValuesAsDoubles(((IVector) vector).getValues());
	}

	/**
	 * Only sets the current values, since the efdc model only stores the current values in memory.
	 */
	public void setValuesAsDoubles(double[] values) {
		this.modelDll.setValues(this.parameterNumber, values);
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": copyValuesFromItem not implemented.");
	}
}
