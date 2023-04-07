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
package org.openda.model_dflowfm;
import org.openda.exchange.AbstractDataObject;
import org.openda.utils.generalJavaUtils.StringUtilities;

import java.io.File;

public class DFlowFMPartitionedRestartFilesWrapper extends AbstractDataObject {

	public static final String NUMBER_OF_PARTITIONS = "numberOfPartitions";
	public static final String RUN_ID = "runId";
	private int numberOfPartitions;
	private String runId;
	private DFlowFMRestartFileWrapper[] dFlowFMRestartFileWrapperArray;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		for (String argument : arguments) {
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			if (keyValue == null) continue;
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case NUMBER_OF_PARTITIONS:
					this.numberOfPartitions = Integer.parseInt(value);
					continue;
				case RUN_ID:
					runId = value;
					continue;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only " + NUMBER_OF_PARTITIONS + " and " + RUN_ID + " as key=value pair");
			}
		}

		dFlowFMRestartFileWrapperArray = new DFlowFMRestartFileWrapper[numberOfPartitions];
		for (int i = 0; i < numberOfPartitions; i++) {
			String partition = StringUtilities.padLeft(String.valueOf(i), 4, '0');
			String fileName = String.format("%s_%s_00000000_000000_rst.nc", runId, partition);
			DFlowFMRestartFileWrapper dFlowFMRestartFileWrapper = new DFlowFMRestartFileWrapper();
			dFlowFMRestartFileWrapper.initialize(workingDir, new String[]{fileName, "exchangeItemIdPostFix=_" + partition});
			String[] exchangeItemIDs = dFlowFMRestartFileWrapper.getExchangeItemIDs();
			for (String exchangeItemID : exchangeItemIDs) {
				DFlowFMExchangeItem dataObjectExchangeItem = (DFlowFMExchangeItem) dFlowFMRestartFileWrapper.getDataObjectExchangeItem(exchangeItemID);
				exchangeItems.put(exchangeItemID, dataObjectExchangeItem);
			}
			dFlowFMRestartFileWrapperArray[i] = dFlowFMRestartFileWrapper;
		}

	}

	@Override
	public void finish() {
		for (DFlowFMRestartFileWrapper dFlowFMRestartFileWrapper : dFlowFMRestartFileWrapperArray) {
			dFlowFMRestartFileWrapper.finish();
		}
	}
}
