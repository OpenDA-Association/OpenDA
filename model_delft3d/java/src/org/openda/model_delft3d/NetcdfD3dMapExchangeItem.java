package org.openda.model_delft3d;

import org.openda.exchange.ExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.ITimeInfo;

/**
 * Created by hummel on 13-Apr-16.
 */
public class NetcdfD3dMapExchangeItem extends ExchangeItem implements IExchangeItem {

	private final ITimeInfo timeInfo;
	private String varName;
//	private int stationIndex;
//	private int layerIndex;
	private NetcdfD3dMapDataObject netcdfD3dMapDataObject;

	public NetcdfD3dMapExchangeItem(String varName, NetcdfD3dMapDataObject netcdfD3dMapDataObject, ITimeInfo timeInfo) {
		super(varName);
//		super(stationName + "." + varName + ".lay-" + layerIndex);
		this.varName = varName;
//		this.stationIndex = stationIndex;
//		this.layerIndex = layerIndex;
		this.netcdfD3dMapDataObject = netcdfD3dMapDataObject;
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

//	@Override
//	public Object getValues() {
//		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dMapExchangeItem.getValues() not implemented yet");
//	}

	@Override
	public Object getValues() {
		return netcdfD3dMapDataObject.getExchangeItemValues(varName);
	}

	@Override
	public double[] getValuesAsDoubles() {
		throw new RuntimeException("org.openda.exchange.dataobjects.NetcdfD3dMapExchangeItem cannot return double[], only Arrays. Use getValues() instead");
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
