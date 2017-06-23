package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.generalJavaUtils.StringUtilities;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Created by pelgrim on 21-Jun-17.
 */
public class DFlowFMCalibrationFactorFile implements IDataObject {

	public static final String CAL_FACTOR = "CalFactor";
	public static final String DISCHARGE = "DISCHARGE";
	public static final String WATERLEVEL = "WATERLEVEL";
	public static final String[] PREFIXES = new String[]{"", "q", "h"};
	private File targetFile;
	private String[] linesArray;

	private LinkedHashMap<String, DFlowFMCalibrationFactorExchangeItem> exchangeItems = new LinkedHashMap<>();

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		for (DFlowFMCalibrationFactorExchangeItem exchangeItem : exchangeItems.values()) {
			int lineNumber = exchangeItem.getLineNumber();
			String line = linesArray[lineNumber];
			String[] split = line.split(" ");
			split[split.length - 1] = String.valueOf(exchangeItem.getValuesAsDoubles()[0]);
			linesArray[lineNumber] = StringUtilities.joinStringArrayUsingSeparator(split, " ");
		}
		AsciiFileUtils.writeLines(targetFile, Arrays.asList(linesArray));
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 1) throw new RuntimeException("Supply at least 1 argument as source file and optionally a second as target file");
		if (arguments.length > 2) throw new RuntimeException("Supply not more than 2 arguments, the first as source file and optionally a second as target file");
		File sourceFile = new File(workingDir, arguments[0]);
		targetFile = arguments.length == 1 ? sourceFile : new File(workingDir, arguments[1]);
		List<String> stringList = AsciiFileUtils.readLines(sourceFile);
		linesArray = new String[stringList.size()];
		stringList.toArray(linesArray);
		int dependecy = 0;
		for (int i = 0; i < linesArray.length; i++) {
			String line = linesArray[i];
			String trimmedLine = line.trim();
			if (trimmedLine.isEmpty()) continue;
			if (trimmedLine.startsWith("#")) continue;
			String[] split = StringUtilities.split(trimmedLine, ' ', '"');
			if (split.length == 2) {
				String id = CAL_FACTOR + '-' + split[0];
				exchangeItems.put(id, new DFlowFMCalibrationFactorExchangeItem(id, Double.valueOf(split[1]), i));
				continue;
			}
			if (split.length == 3) {
				String id = CAL_FACTOR + '-' + split[0] + '-' + PREFIXES[dependecy] + split[1];
				exchangeItems.put(id, new DFlowFMCalibrationFactorExchangeItem(id, Double.valueOf(split[2]), i));
				continue;
			}
			if (split.length >= 4) {
				if (split[1].equals(DISCHARGE)) {
					dependecy = 1;
					continue;
				}
				if (split[1].equals(WATERLEVEL)) {
					dependecy = 2;
					continue;
				}
			}
		}
	}
}
