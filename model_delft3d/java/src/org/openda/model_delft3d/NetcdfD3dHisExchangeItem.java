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
package org.openda.model_delft3d;
import org.openda.exchange.ExchangeItem;
import org.openda.interfaces.*;

/**
 * Created by hummel on 13-Apr-16.
 */
public class NetcdfD3dHisExchangeItem extends ExchangeItem implements IExchangeItem {

	private final ITimeInfo timeInfo;
	private String varName;
	private int stationIndex;
	private int layerIndex;
	private NetcdfD3dHisDataObject netcdfD3dHisDataObject;

	public NetcdfD3dHisExchangeItem(String varName, String stationName, int stationIndex, int layerIndex,
									NetcdfD3dHisDataObject netcdfD3dHisDataObject, ITimeInfo timeInfo) {
		super(stationName + "." + varName + ".lay-" + layerIndex);
		this.varName = varName;
		this.stationIndex = stationIndex;
		this.layerIndex = layerIndex;
		this.netcdfD3dHisDataObject = netcdfD3dHisDataObject;
		this.timeInfo = timeInfo;

	}

	@Override
	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	@Override
	public Class getValueType() {
		return double[].class;
	}

	@Override
	public Object getValues() {
		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItem.getValues() not implemented yet");
	}

	@Override
	public double[] getValuesAsDoubles() {
		return netcdfD3dHisDataObject.getExchangeItemValues(varName, stationIndex, layerIndex);
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItem.axpyOnValues() not implemented yet");
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItem.multiplyValues() not implemented yet");
	}

	@Override
	public void setValues(Object values) {
		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItem.setValues() not implemented yet");
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItem.setValuesAsDoubles() not implemented yet");
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
}
