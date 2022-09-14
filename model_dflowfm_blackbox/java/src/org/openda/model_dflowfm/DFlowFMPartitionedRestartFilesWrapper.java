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
