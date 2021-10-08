package org.openda.model_wflow;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.io.AsciiFileUtils;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class WflowJuliaForcingAndTomlFile extends AbstractDataObject {
	public static final String STARTTIME = "starttime";
	public static final String ENDTIME = "endtime";
	public static final String START_TIME_ID = "startTime";
	public static final String END_TIME_ID = "endTime";
	private int startTimeLineIndex;
	private int endTimeLineIndex;
	private List<String> tomlLines;
	public static final String PATTERN = "yyyy-MM-dd'T'HH:mm:ss";
	private File tomlFile;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		File forcingFile = new File(workingDir, arguments[0]);
		tomlFile = new File(workingDir, arguments[1]);

		getStartEndTimesFromForcingFile(forcingFile);

		tomlLines = AsciiFileUtils.readLines(tomlFile);

		getStartEndTimeLineIndices();

	}

	private void getStartEndTimeLineIndices() {
		boolean startTimeFound = false;
		boolean endTimeFound = false;
		for (int i = 0, size = tomlLines.size(); i < size; i++) {
			String line = tomlLines.get(i);
			String[] split = line.split("=");
			if (split.length != 2) continue;
			if (!startTimeFound && split[0].trim().equals(STARTTIME)) {
				startTimeLineIndex = i;
				startTimeFound = true;
				if (endTimeFound) break;
				continue;
			}
			if (!endTimeFound && split[0].trim().equals(ENDTIME)) {
				endTimeLineIndex = i;
				endTimeFound = true;
				if (!startTimeFound) continue;
				break;
			}
		}
	}

	private void getStartEndTimesFromForcingFile(File forcingFile) {
		NetcdfFileWriter netcdfFileWriter = null;
		try {
			try {
				netcdfFileWriter = NetcdfFileWriter.openExisting(forcingFile.getAbsolutePath());
				Variable timeVariable = netcdfFileWriter.findVariable("time");
				if (timeVariable == null) throw new RuntimeException("No variable named time found in " + forcingFile);
				double[] allMjdTimes = NetcdfUtils.readTimes(timeVariable);
				exchangeItems.put(START_TIME_ID, new DoubleExchangeItem(START_TIME_ID, allMjdTimes[0]));
				exchangeItems.put(END_TIME_ID, new DoubleExchangeItem(END_TIME_ID, allMjdTimes[allMjdTimes.length - 1]));
			} finally {
				if (netcdfFileWriter != null) netcdfFileWriter.close();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void finish() {
		double startTime = exchangeItems.get(START_TIME_ID).getValuesAsDoubles()[0];
		String startTimeString = TimeUtils.mjdToString(startTime, PATTERN);
		String oldStartTimeString = tomlLines.set(startTimeLineIndex, STARTTIME + " = " + startTimeString);
		assert oldStartTimeString.contains(STARTTIME);
		double endTime = exchangeItems.get(END_TIME_ID).getValuesAsDoubles()[0];
		String endTimeString = TimeUtils.mjdToString(endTime, PATTERN);
		String oldEndTimeString = tomlLines.set(endTimeLineIndex, ENDTIME + " = " + endTimeString);
		assert oldEndTimeString.contains(ENDTIME);
		AsciiFileUtils.writeLines(tomlFile, tomlLines);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return this.exchangeItems.get(exchangeItemID);
	}
}
