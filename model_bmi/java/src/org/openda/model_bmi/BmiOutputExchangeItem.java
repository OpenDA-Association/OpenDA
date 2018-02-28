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

package org.openda.model_bmi;

import bmi.BMIModelException;
import bmi.BMI;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Array;
import org.openda.utils.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Exchange item representing a 2D map with values for BMI models
 *
 * @author Arno Kockx
 * @author Niels Drost
 */
public class BmiOutputExchangeItem implements IExchangeItem {

	private static Logger LOGGER = LoggerFactory.getLogger(BmiOutputExchangeItem.class);

	private static final long serialVersionUID = 1L;
	private final String variableName;
	private final IPrevExchangeItem.Role role;
	private final String type;
	private final BMI model;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;
	private double modelMissingValue;

	/**
	 * @param variableName
	 *            the name of the variable as used by a BMI model
	 * @param role
	 * @param model
	 * @throws BMIModelException
	 */
	public BmiOutputExchangeItem(String variableName, IPrevExchangeItem.Role role, BMI model, double modelMissingValue) throws BMIModelException {
		this.modelMissingValue = modelMissingValue;
		if (variableName == null) throw new IllegalArgumentException("variableName == null");
		if (role == null) throw new IllegalArgumentException("role == null");
		if (model == null) throw new IllegalArgumentException("model == null");

		this.variableName = variableName;
		this.role = role;
		this.model = model;

		this.quantityInfo = new QuantityInfo(variableName, model.getVarUnits(variableName));

		this.geometryInfo = createGeometryInfo();

		//currently only "float32" and "float64" are supported
		this.type  = model.getVarType(variableName);
	}

	private IGeometryInfo createGeometryInfo() {
		// lower-left corner
		try {
			double[] origin = this.model.getGridOrigin(variableName);
			double[] spacing = this.model.getGridSpacing(variableName);
			int[] shape = this.model.getGridShape(variableName);

			// data in grid lower-to-higher latitudes (south to north)
			double[] latitudes = new double[shape[0]];
			for (int n = 0; n < latitudes.length; n++) {
				// calculate latitude at center of each cell
				latitudes[n] = origin[0] + (spacing[0] / 2) + (n * spacing[0]);
			}
			IArray latitudeArray = new Array(latitudes);

			double[] longitudes = new double[shape[1]];
			for (int n = 0; n < longitudes.length; n++) {
				longitudes[n] = origin[1] + (spacing[1] / 2) + (n * spacing[1]);
			}
			IArray longitudeArray = new Array(longitudes);

			// latitudeValueIndex(es) = dimensionIndex(es) of latitudeArray
			// dimension(s) in the grid data array.
			int[] latitudeValueIndices = new int[] { 0 };
			// longitudeValueIndex(es) = dimensionIndex(es) of longitudeArray
			// dimension(s) in the grid data array.
			int[] longitudeValueIndices = new int[] { 1 };

			IQuantityInfo latitudeQuantityInfo = new QuantityInfo("y coordinate according to model coordinate system",
					"meter");
			IQuantityInfo longitudeQuantityInfo = new QuantityInfo("x coordinate according to model coordinate system",
					"meter");
			//here create a rectangular grid geometryInfo, otherwise GeometryUtils.getObservedValuesBilinearInterpolation does not work and GeometryUtils.getLocalizationWeights works slow.
			return new ArrayGeometryInfo(latitudeArray, latitudeValueIndices, latitudeQuantityInfo, longitudeArray,
					longitudeValueIndices, longitudeQuantityInfo, null, null, null, null);
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}

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
		// return current time, since BMI models only store the current values
		// in memory.
		try {
			return new double[] { TimeUtils.udUnitsTimeToMjd(model.getCurrentTime(), model.getTimeUnits()) };
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
	}

	public void setTimes(double[] times) {
		throw new RuntimeException(this.getClass().getName() + ": setting time stamps not supported for BMI model.");
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	public Object getValues() {
		// TODO to use native code (to improve performance) for vector and
		// matrix calculations in algorithm, for that need to return a CtaVector
		// here. AK
		return new Vector(getValuesAsDoubles());
	}

	/**
	 * Returns only the current values, since the model only stores the current
	 * values in memory.
	 */
	public double[] getValuesAsDoubles() {
		double[] allModelValues;
		try {
			if ("float32".equals(type)) {
				allModelValues = BBUtils.toDoubleArray(model.getFloat(variableName));
			} else if ("float64".equals(type)) {
				allModelValues = model.getDouble(variableName);
			} else {
				throw new BMIModelException("unsupported variable data type: " + type + " currently only float and double types supported");
			}
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
		double[] checkedValues = new double[allModelValues.length];
		for (int i = 0; i < allModelValues.length; i++) {
			checkedValues[i] = Double.compare(allModelValues[i], modelMissingValue) == 0 ? Double.NaN : allModelValues[i];
		}
		return checkedValues;
	}

	/**
	 * Only changes the current values, since the model only stores the current
	 * values in memory. Only changes the values of the active grid cells.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues) {
		double[] allValues = getValuesAsDoubles();

		for (int n = 0; n < allValues.length; n++) {
			// for all NaNs results in NaN
			allValues[n] += alpha * axpyValues[n];
		}
		setValuesAsDoubles(allValues);
	}

	/**
	 * Only changes the current values, since the model only stores the current
	 * values in memory. Only changes the values of the active grid cells.
	 */
	public void multiplyValues(double[] multiplicationFactors) {

		double[] allValues = getValuesAsDoubles();
		for (int n = 0; n < allValues.length; n++) {
			allValues[n] *= multiplicationFactors[n];
		}
		setValuesAsDoubles(allValues);
	}

	public void setValues(Object vector) {
		if (!(vector instanceof IVector)) {
			throw new IllegalArgumentException(this.getClass().getName() + ": supply values as an IVector not as "
					+ vector.getClass().getName());
		}

		setValuesAsDoubles(((IVector) vector).getValues());
	}

	/**
	 * Sets only the current values, since BMI models only store the current
	 * values in memory. Sets the values of all grid cells, also of the
	 * inactive/dry/dead grid cells. This is not a problem, since BMI models
	 * should ignore the values of the inactive grid cells anyway.
	 */
	public void setValuesAsDoubles(double[] values) {
		LOGGER.info("Setting " + values.length + " values in variable " + variableName);
		double[] checkedValues = new double[values.length];
		for (int i = 0; i < values.length; i++) {
			if (Double.isNaN(values[i]))
			{
				checkedValues[i] = modelMissingValue;
			}
			else
				checkedValues[i] = values[i];
		}
		try {
			if ("float32".equals(type)) {
				model.setFloat(variableName, BBUtils.toFloatArray(checkedValues));
			} else if ("float64".equals(type)) {
				model.setDouble(variableName, checkedValues);
			} else {
				throw new BMIModelException("unsupported variable data type: " + type + " currently only float and double types supported");
			}
			
			
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException(getClass().getName() + ".copyValuesFromItem() not implemented");
	}

	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public IGeometryInfo getGeometryInfo() {
		return this.geometryInfo;
	}
}
