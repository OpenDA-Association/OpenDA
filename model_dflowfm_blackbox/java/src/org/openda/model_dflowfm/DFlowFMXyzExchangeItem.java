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
package org.openda.model_dflowfm;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IVector;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

import org.openda.exchange.ExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.interfaces.IExchangeItem.ValueType;
import org.openda.utils.Vector;


@SuppressWarnings("serial")
public class DFlowFMXyzExchangeItem implements IExchangeItem {

	/**
	 * Implementations of the exangeItems of a DflowFM forcings file
	 * *
	 * The file contains lines of the form:
	 * <p/>
	 * x_coor  y_coor  factor
	 * <p/>
	 * The parameters that can be calibrated is factor. Potentially, there can be as
	 * many exchangeItems as lines in the file, but lines containing the same factor
	 * are supposed to belong to one calibration parameter.
	 */
	private String id;              // Unique ID of the parameter
	private String unitId;          // A factor does not have a unit
	private ValueType valuetype;    // The value of the exchange item is always a double
	File xyzFile;          // File handle
	ArrayList<Integer> lineNum = new ArrayList<Integer>();
	double[] values = new double[1];
	IVector vector;

	private String description = null;

	public DFlowFMXyzExchangeItem(String id, File xyzFile, double value, int lineNum) {

		this.id = id;
		this.unitId = "-";
		this.xyzFile = xyzFile;
		this.values[0] = value;
		this.vector = new Vector(this.values);
		this.lineNum.add(lineNum);
	}

	public String getId() {
		return this.id;
	}

	public String getDescription() {
		return description;
	}

	public void addValue(int linenum, double value) {
		this.values[0] = value;
		IVector myVector = new Vector(this.values);
		this.vector = Vector.concatenate(this.vector, myVector);
		this.lineNum.add(linenum);
	}

	public String getUnitId() {
		return this.unitId;
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
		//return this.valuetype;
	}

	public Role getRole() {
		return IExchangeItem.Role.InOut;
	}

	public Object getValues() {
		return this.vector;
	}

	public void setValues(Object values) {
		vector = (Vector) values;
		//System.out.println("XyzExchangeItem" + vector);

		int index = 0;
		for (Integer line : this.lineNum) {
			DFlowFMXyzUtils.writeValueToFile(this.xyzFile, line, vector.getValue(index));
			index++;
		}
	}

	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (this.valuetype != sourceItem.getValuesType()) {
			throw new RuntimeException("Incompatible value types in copy action from " + sourceItem.getId() +
					" to " + getId() + "(" + sourceItem.getValueType().toString() + "/=" + this.valuetype.toString());
		}
		setValues(sourceItem.getValues());
	}

	public IQuantityInfo getQuantityInfo() {
		return null;
	}

	public ITimeInfo getTimeInfo() {
		return null;
	}

	/*
	 * =======================================================================
	 *  The following methods are due to the old interface and will be removed
	 * later.
	 * =======================================================================
	 */


	@Deprecated
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("setTimes method make no sense for a DoubleExchangeItem");
	}

	@SuppressWarnings("rawtypes")

	@Deprecated
	public Class getValueType() {
		return double.class;
	}


	@Deprecated
	public double[] getTimes() {
		return null;
	}

	@Deprecated
	public double[] getValuesAsDoubles() {
		return vector.getValues();
	}


	@Deprecated
	public void axpyOnValues(double alpha, double[] axpyValues) {
		//if(axpyValues.length==1){
		vector.axpy(alpha, new Vector(axpyValues));
		//}else{
		//	throw new RuntimeException("\"Length of axpy value array must be 1.");
		//}
	}


	@Deprecated
	public void multiplyValues(double[] multiplicationFactors) {
		vector.pointwiseMultiply(new Vector(multiplicationFactors));
	}


	@Deprecated
	public void setValuesAsDoubles(double[] values) {
		vector.setValues(values);
		this.setValues(vector);
		//if (values.length !=1){
		//	throw new UnsupportedOperationException("Length of value array must be 1");
		//}
		//this.value = values[0];
		//System.out.println("XyzExchangeItem setValuesAsDoubles:" + values[0] );
		//System.out.println("XyzExchangeItem " + this.xyzFile);
		//
		//for (int line : this.lineNum) {
		//	DFlowFMXyzUtils.writeValueToFile(this.xyzFile, line, this.value);
		//}
	}

}
