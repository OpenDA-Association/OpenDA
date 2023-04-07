/*
* Copyright (c) 2023 OpenDA Association 
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
