/* MOD_V2.0
 * Copyright (c) 2013 OpenDA Association
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

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import java.io.*;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.openda.utils.generalJavaUtils.StringUtilities;

/**
 * Reading and writing information to [time] block of MDU-file
 */

public class DFlowFMTimeInfo implements IDataObject {

	public static final String USE_RST_FOR_RESTART = "useRstForRestart";
	public static final String NUMBER_OF_PARTITIONS = "numberOfPartitions";
	private IExchangeItem[] exchangeItems;

	private DFlowFMMduInputFile mduOptions;
	private File mduFile;
	private final String[] DFlowFMTimeInfoId = new String[]{"start_time", "end_time"};
	private final int NTimeInfoIds = DFlowFMTimeInfoId.length;
	private static final Map<String, String> timeFormat;
	private static final String SECTION_TIME = "time";
	private static final String START_OFFSET = "TStart";
	private static final String STOP_OFFSET = "TStop";
	private static final String START_DATETIME = "StartDatetime";
	private static final String STOP_DATETIME = "StopDatetime";
	private static final String REPLACE_MINUTES_AND_SECONDS = "0000";
	private static final String REPLACE_SECONDS = "00";
	private boolean useRstForRestart = false;
	private int numberOfPartitions = 0;

	static {
		timeFormat = new HashMap<>();
		timeFormat.put("S", "%.2f");
		timeFormat.put("M", "%.6f");
		timeFormat.put("H", "%.8f");
	}
	private DFlowFMMduInputFile[] dFlowFMMduInputFiles;

	private void initialize(File workingDir, String fileName, String[] arguments) {
		for (String argument : arguments) {
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case USE_RST_FOR_RESTART:
					this.useRstForRestart = Boolean.parseBoolean(value);
					continue;
				case NUMBER_OF_PARTITIONS:
					numberOfPartitions = Integer.parseInt(value);
					continue;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only " + USE_RST_FOR_RESTART + " as key=value pair");
			}
		}
		String mainFileName = numberOfPartitions == 0 ? fileName : fileName.substring(0, fileName.length() - 4) + "_0000" + fileName.substring(fileName.length() - 4);
		this.mduFile = new File(workingDir, mainFileName);
		mduOptions = new DFlowFMMduInputFile(workingDir, mainFileName);
		exchangeItems = new DFlowFMTimeInfoExchangeItem[NTimeInfoIds];
		for (int n = 0 ; n < NTimeInfoIds ; n++) {
			exchangeItems[n] = new DFlowFMTimeInfoExchangeItem(DFlowFMTimeInfoId[n], this);
		}
		if (numberOfPartitions == 0) return;
		dFlowFMMduInputFiles = new DFlowFMMduInputFile[numberOfPartitions - 1];
		for (int i = 1; i < numberOfPartitions; i++) {
			String partionedString = StringUtilities.padLeft(String.valueOf(i), 4, '0');
			String partitionedFileName = fileName.substring(0, fileName.length() - 4) + "_" + partionedString + fileName.substring(fileName.length() - 4);
			dFlowFMMduInputFiles[i - 1] = new DFlowFMMduInputFile(workingDir, partitionedFileName);
		}
	}

	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}

	public String[] getExchangeItemIDs() {
		String[] exchangeItemIDs = new String[exchangeItems.length];
		for (int n = 0; n < exchangeItems.length; n++) {
			exchangeItemIDs[n] = exchangeItems[n].getId();
		}
		return exchangeItemIDs;
	}

	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int index = NTimeInfoIds;
		int n = 0;
		while (n < NTimeInfoIds) {
			if (exchangeItemID.equals(DFlowFMTimeInfoId[n])) {
				index = n;
				n = NTimeInfoIds;
			}
			n++;
		}
		if (index == NTimeInfoIds) {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		}
		return exchangeItems[index];
	}

	public IExchangeItem[] getExchangeItems() {
		return exchangeItems;
	}

	private double getDoubleTime(String offset, String datetime) {
		if (mduOptions.contains(SECTION_TIME, offset)) {
			String time = mduOptions.get(SECTION_TIME, offset);
			double doubleTime = Double.parseDouble(time);
			long timeStepMillis = mduOptions.getTimeStepMillis();
			long referenceDateInMillis = mduOptions.getReferenceDateInMillis();
			long timeMillis = (long) (doubleTime * timeStepMillis + referenceDateInMillis) * 1000 / 1000;
			return TimeUtils.date2Mjd(new Date(timeMillis));
		}

		if (mduOptions.contains(SECTION_TIME, datetime)) {
			try {
				String time = mduOptions.get(SECTION_TIME, datetime);
				String timeUnit = mduOptions.get(SECTION_TIME, "Tunit");
				if ("H".equalsIgnoreCase(timeUnit)) return TimeUtils.date2Mjd(time.substring(0, time.length() - REPLACE_MINUTES_AND_SECONDS.length()) + REPLACE_MINUTES_AND_SECONDS);
				if ("M".equalsIgnoreCase(timeUnit)) return TimeUtils.date2Mjd(time.substring(0, time.length() - REPLACE_SECONDS.length()) + REPLACE_SECONDS);
				return TimeUtils.date2Mjd(time);
			} catch (Exception e) {
				throw new RuntimeException(String.format("Error parsing %s " + mduOptions.get(SECTION_TIME, datetime), datetime));
			}
		}
		throw new RuntimeException(String.format("Neither %s nor %s are specified in %s", offset, datetime, SECTION_TIME));
	}

	double getDblTStartSimulation() {
		return getDoubleTime(START_OFFSET, START_DATETIME);
	}

	void setDblTStartSimulation(double dblTStart) {
		// Determine TStart since RefDate, RefDate should not be changed.
		double dblTref = mduOptions.getReferenceDateInMjd();
		Double tStartInMduUnit = (dblTStart - dblTref) / mduOptions.getTimeToMjdFactor();
		if (Math.abs(dblTStart - dblTref) > 0.007) {
			// assumption: run is started if TStart 'equals' Reference Date;
			// run is restarted if Tstart is more than 1 minute larger than RefDate
			// 1 minute = 1/(24*60) = 6.94e-4 MJD
			setRestartOptions(dblTStart);
		}
		// round to 1 decimal place
		// tStartInMduUnit = Math.round(tStartInMduUnit*10)/10.0d;
		String TStart = String.format(Locale.US, timeFormat.get(mduOptions.get(SECTION_TIME, "Tunit")), tStartInMduUnit);
		mduOptions.put(SECTION_TIME, START_OFFSET, TStart);
		if (numberOfPartitions == 0) return;
		for (int i = 1; i < numberOfPartitions; i++) {
			dFlowFMMduInputFiles[i - 1].put("time", "TStart", TStart);
		}
	}

	double getDblTStopSimulation() {
		return getDoubleTime(STOP_OFFSET, STOP_DATETIME);
	}

	void setDblTStopSimulation(double dblTStop) {
		// Determine TStop since RefDate, RefDate should not be changed.
		double dblTref = mduOptions.getReferenceDateInMjd();
		double tStopInMduUnit = (dblTStop - dblTref) / mduOptions.getTimeToMjdFactor();
		// round to 1 decimal place
		tStopInMduUnit = Math.round(tStopInMduUnit * 10) / 10.0d;
		//TStop = tStopInMduUnit.toString();
		String TStop = String.format(Locale.US, timeFormat.get(mduOptions.get(SECTION_TIME, "Tunit")), tStopInMduUnit);
		mduOptions.put(SECTION_TIME, STOP_OFFSET, TStop);
		if (numberOfPartitions == 0) return;
		for (int i = 1; i < numberOfPartitions; i++) {
			dFlowFMMduInputFiles[i - 1].put("time", "TStop", TStop);
		}
	}

	private void setRestartOptions(double dblTime) {
		String RestartDateTime = TimeUtils.mjdToString(dblTime);
		String mduFileName = mduFile.getName();
		if (useRstForRestart) return;
		String mapfile = mduFileName.replace(".mdu", "_map.nc");

		mduOptions.put("restart", "RestartFile", mapfile);
		mduOptions.put("restart", "RestartDateTime", RestartDateTime + "00");
	}

	public void finish() {
		mduOptions.store();
		if (numberOfPartitions == 0) return;
		for (int i = 1; i < numberOfPartitions; i++) {
			dFlowFMMduInputFiles[i - 1].store();
		}
	}
}
