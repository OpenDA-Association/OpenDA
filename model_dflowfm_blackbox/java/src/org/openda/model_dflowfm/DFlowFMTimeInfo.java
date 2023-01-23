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
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.openda.utils.Results;

/**
 * Reading and writing information to [time] block of MDU-file
 */

public class DFlowFMTimeInfo implements IDataObject {

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

	static {
		timeFormat = new HashMap<>();
		timeFormat.put("S", "%.2f");
		timeFormat.put("M", "%.6f");
		timeFormat.put("H", "%.8f");
	}

	private void initialize(File workingDir, String fileName, String[] arguments) {
		if (arguments.length > 0) Results.putMessage("Initialize DFlowTimeInfo: additional arguments not used");
		this.mduFile = new File(workingDir, fileName);
	    mduOptions = new DFlowFMMduInputFile(workingDir, fileName);
		exchangeItems = new DFlowFMTimeInfoExchangeItem[NTimeInfoIds];
		for (int n = 0 ; n < NTimeInfoIds ; n++) {
			exchangeItems[n] = new DFlowFMTimeInfoExchangeItem(DFlowFMTimeInfoId[n], this);
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
			return Double.parseDouble(time) * mduOptions.getTimeToMjdFactor() + mduOptions.getReferenceDateInMjd();
		}

		if (mduOptions.contains(SECTION_TIME, datetime)) {
			String time = mduOptions.get(SECTION_TIME, datetime);
			String timeUnit = mduOptions.get(SECTION_TIME, "Tunit");
			if ("H".equalsIgnoreCase(timeUnit)) {
				time = time.substring(0, time.length() - REPLACE_MINUTES_AND_SECONDS.length()) + REPLACE_MINUTES_AND_SECONDS;
			}

			if ("M".equalsIgnoreCase(timeUnit)) {
				time = time.substring(0, time.length() - REPLACE_SECONDS.length()) + REPLACE_SECONDS;
			}

			try {
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
	}

	private void setRestartOptions(double dblTime) {
		String RestartDateTime = TimeUtils.mjdToString(dblTime);
		String mapfile = mduFile.getName().replace(".mdu","_map.nc");

		mduOptions.put("restart","RestartFile",mapfile);
		mduOptions.put("restart","RestartDateTime",RestartDateTime+"00");
	}

	public void finish() {
		mduOptions.store();
	}
}
