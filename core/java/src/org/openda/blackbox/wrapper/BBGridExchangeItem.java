package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IGridTimeSeriesExchangeItem;

import java.io.File;
import java.util.HashMap;

public class BBGridExchangeItem extends BBExchangeItem implements IGridTimeSeriesExchangeItem {

	private final IGridTimeSeriesExchangeItem gridExchangeItem;

	public BBGridExchangeItem(String id, BBStochModelVectorConfig vectorConfig,
							  IGridTimeSeriesExchangeItem ioObjectExchangeItem,
							  HashMap<String, SelectorInterface> selectors, File configRootDir) {
		super(id,vectorConfig, ioObjectExchangeItem, selectors, configRootDir);

		gridExchangeItem = ioObjectExchangeItem;
	}

	@Override
	public IGeometryInfo getGeometryInfoForSingleTimeIndex(int timeIndex) {
		return gridExchangeItem.getGeometryInfoForSingleTimeIndex(timeIndex);
	}

	@Override
	public double[] getValuesAsDoublesForSingleTimeIndex(int timeIndex) {
		return gridExchangeItem.getValuesAsDoublesForSingleTimeIndex(timeIndex);

	}

	@Override
	public void setValuesAsDoublesForSingleTimeIndex(int timeIndex, double[] values) {
		gridExchangeItem.setValuesAsDoublesForSingleTimeIndex(timeIndex, values);
	}

	@Override
	public void axpyOnValuesForSingleTimeIndex(int timeIndex, double alpha, double[] axpyValues) {
		gridExchangeItem.axpyOnValuesForSingleTimeIndex(timeIndex, alpha, axpyValues);

	}

	@Override
	public void multiplyValuesForSingleTimeIndex(int timeIndex, double[] multiplicationFactors) {
		gridExchangeItem.multiplyValuesForSingleTimeIndex(timeIndex, multiplicationFactors);
	}
}
