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
package org.openda.model_dflowfm;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.io.File;
import java.util.ArrayList;

/**
 * @deprecated
 * This class is replaced by the generic DFlowFMXynExchangeItem class
 */
@SuppressWarnings("serial")
public class DFlowFMFrictionCoefExchangeItem implements IExchangeItem {

	/**
	 * Implementations of the exangeItems of a DflowFM forcings file
	 * *
	 * The file contains lines of the form:
	 *
	 *   x_coor  y_coor  factor
	 *
	 * The parameters that can be calibrated is factor. Potentially, there can be as
	 * many exchangeItems as lines in the file, but lines containing the same factor
	 * are supposed to belong to one calibration parameter.
	 *
	 */
	private String id;              // Unique ID of the parameter
	private String unitId;          // A factor does not have a unit
	private ValueType valuetype;    // The value of the exchange item is always a double
	private double value;           // The value of the exchange item
	File frictioncoefFile;          // File handle
	ArrayList<Integer> lineNum = new ArrayList<Integer>();
	                                // list of line numbers that belong to the same parameter
	private String description=null;

	public DFlowFMFrictionCoefExchangeItem(String id, File frictioncoefFile, int lineNum){

		this.id          = id;
		this.unitId      = "-";
		this.valuetype   = ValueType.doubleType;
		this.frictioncoefFile = frictioncoefFile;
		this.lineNum.add(lineNum);

		// Here we need to read the values from the input file that will be stored in this.value[]
		this.value= DflowFMFrictionCoefficientUtils.readValueFromFile(this.frictioncoefFile, lineNum);
        // TODO, decision Arthur van Dam: value read from file or set to 1.0?
		this.value = 1.0;
	}

	public String getId() {
		return this.id;
	}

	public String getDescription() {
		return description;
	}

	public void addlineNum(int linenum){
		this.lineNum.add(linenum);
	}

	public String getUnitId() {
		return this.unitId;
	}

	public ValueType getValuesType() {
		return this.valuetype;
	}

	public Role getRole() {
		return IExchangeItem.Role.InOut;
	}

	public Object getValues() {
		return this.value;
	}


	public void setValues(Object values) {
		if(values instanceof Double){
			this.value = (Double) values;
			for (int line : this.lineNum) {
				DflowFMFrictionCoefficientUtils.writeValueToFile(this.frictioncoefFile, line, this.value);
			}
		}else{
			throw new UnsupportedOperationException("Can only set values for a Double object");
		}
	}

	public IGeometryInfo getGeometryInfo(){
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
	 * The getTimes/setTimes methods are part of the old IPrevExchangeItem
	 * interface and will be removed later.
	 * =======================================================================
	 */


	@Deprecated public void setTimes(double[] times) {
		throw new UnsupportedOperationException("setTimes method make no sense for a DoubleExchangeItem");
	}

	@SuppressWarnings("rawtypes")

	public Class getValueType() {
		return double.class;
	}


	@Deprecated public double[] getTimes() {
		return null;
	}

	public double[] getValuesAsDoubles() {
		return new double[]{this.value};
	}


	public void axpyOnValues(double alpha, double[] axpyValues) {
		if(axpyValues.length==1){
			this.value += alpha*axpyValues[0];
		}else{
			throw new RuntimeException("\"Length of axpy value array must be 1.");
		}
	}


	public void multiplyValues(double[] multiplicationFactors) {
		if(multiplicationFactors.length==1){
			this.value *= multiplicationFactors[0];
		}else{
			throw new RuntimeException("Length of multiplication factors array must be 1.");
		}
	}


	public void setValuesAsDoubles(double[] values) {
		if (values.length !=1){
			throw new UnsupportedOperationException("Length of value array must be 1");
		}
		this.value = values[0];
		for (int line : this.lineNum) {
			DflowFMFrictionCoefficientUtils.writeValueToFile(this.frictioncoefFile, line, this.value);
		}
	}

}
