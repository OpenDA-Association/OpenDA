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

import org.openda.interfaces.*;

/**
 * Exchange item for reading and writing simulation time information to the MDU file of DFLowFM.
 */
@SuppressWarnings("serial")
public class DFlowFMTimeInfoExchangeItem implements IExchangeItem {
	private DFlowFMTimeInfo DFlowFMTimeInfo;
	private String id;

	public DFlowFMTimeInfoExchangeItem(String id, DFlowFMTimeInfo DFlowFMTimeInfo) {
		this.id = id;
		this.DFlowFMTimeInfo = DFlowFMTimeInfo;
	}

	public String getId() {
		return id;
	}

	public String getDescription() {
		return null; // no description
	}

	@SuppressWarnings("rawtypes")
	public Class getValueType() {
		return double.class;
	}

	public ValueType getValuesType() {
		return ValueType.doubleType;
	}

	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	public Object getValues() {
		double time;
		if (this.id.equals("start_time")) {
			time = DFlowFMTimeInfo.getDblTStartSimulation();
		} else if (this.id.equals("end_time")) {
			time = DFlowFMTimeInfo.getDblTStopSimulation();
		} else {
			throw new RuntimeException("DFlowFMTimeInfo exchangeItemId "+this.id+" is not supported. Possible ids are start_time and stop_time.");
		}
		return time;
	}

	public double[] getValuesAsDoubles() {
		return new double[]{(Double)getValues()};
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.axpyOnValues(): Not implemented yet.");
	}

	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.multiplyValues(): Not implemented yet.");
	}

	public void setValues(Object values) {
		if (this.id.equals("start_time")) {
			DFlowFMTimeInfo.setDblTStartSimulation((Double)values);
		} else {
			DFlowFMTimeInfo.setDblTStopSimulation((Double)values);
		}
	}

	public void setValuesAsDoubles(double[] values) {
		if (this.id.equals("start_time")) {
			DFlowFMTimeInfo.setDblTStartSimulation(values[0]);
		} else {
			DFlowFMTimeInfo.setDblTStopSimulation(values[0]);
		}
	}

	public double[] getTimes() {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.getTimes(): Not implemented yet.");
	}

	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.setTimes(): Not implemented yet.");
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.copyValuesFromItem(): Not implemented yet.");
	}

	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.getTimeInfo(): Not implemented yet.");
	}

	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.model_DFlowFM.DFlowFMTimeInfoExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	public IGeometryInfo getGeometryInfo() {
		return null;
	}
}
