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
import org.openda.interfaces.IPrevExchangeItem;
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
	private String TStart = null;
	private String TStop = null;
	private String[] DFlowFMTimeInfoId = new String[]{"start_time","end_time"};
	private int NTimeInfoIds = DFlowFMTimeInfoId.length;
	private static Map<String, String> timeFormat;



	static {
		timeFormat= new HashMap<String, String>();
		timeFormat.put("S", "%.2f" );
		timeFormat.put("M", "%.6f" );
		timeFormat.put("H", "%.8f" );
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

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int index = NTimeInfoIds;
		int n = 0;
		while(n < NTimeInfoIds) {
			if (exchangeItemID.equals(DFlowFMTimeInfoId[n])) {
				index = n;
				n = NTimeInfoIds;
        	}
			n++;
		}
        if (index == NTimeInfoIds ) {throw new RuntimeException("unknown exchange item: " + exchangeItemID);}
		return exchangeItems[index];
	}

	public IExchangeItem[] getExchangeItems() {
		return exchangeItems;
	}

	double getDblTStartSimulation() {
		if (TStart==null) {
			readTimeInfo();
		}
		double dblTStart;
	    dblTStart = Double.parseDouble(TStart) * mduOptions.getTimeToMjdFactor();
	    dblTStart = dblTStart +  mduOptions.getReferenceDateInMjd();	
	    return dblTStart;
	}

	void setDblTStartSimulation(double dblTStart){
		// Determine TStart since RefDate, RefDate should not be changed.
		double dblTref = mduOptions.getReferenceDateInMjd();
		Double tStartInMduUnit = (dblTStart-dblTref)/ mduOptions.getTimeToMjdFactor();
		if (Math.abs(dblTStart - dblTref) > 0.007){
			// assumption: run is started if TStart 'equals' Reference Date;
			// run is restarted if Tstart is more than 1 minute larger than RefDate
			// 1 minute = 1/(24*60) = 6.94e-4 MJD
			setRestartOptions(dblTStart);
		}
		// round to 1 decimal place
		// tStartInMduUnit = Math.round(tStartInMduUnit*10)/10.0d;
		TStart =String.format(Locale.US, timeFormat.get( mduOptions.get("time","Tunit")), tStartInMduUnit);
		mduOptions.put("time", "TStart", TStart);
	}

	double getDblTStopSimulation() {
		if (TStop==null) {
			readTimeInfo();
		}
		double dblTStop;
	    dblTStop = Double.parseDouble(TStop) * mduOptions.getTimeToMjdFactor();
	    dblTStop = dblTStop +  mduOptions.getReferenceDateInMjd();	
	    return dblTStop;
	}

	void setDblTStopSimulation(double dblTStop){
		// Determine TStop since RefDate, RefDate should not be changed.
		double dblTref = mduOptions.getReferenceDateInMjd();
		Double tStopInMduUnit = (dblTStop-dblTref)/ mduOptions.getTimeToMjdFactor();
		// round to 1 decimal place
	    tStopInMduUnit = Math.round(tStopInMduUnit*10)/10.0d;
		//TStop = tStopInMduUnit.toString();
		TStop =String.format(Locale.US, timeFormat.get( mduOptions.get("time","Tunit")), tStopInMduUnit);
		mduOptions.put("time","TStop",TStop);
	}

	private void setRestartOptions(double dblTime) {
		String RestartDateTime = TimeUtils.mjdToString(dblTime);
		String mapfile = mduFile.getName().replace(".mdu","_map.nc");

		mduOptions.put("restart","RestartFile",mapfile);
		mduOptions.put("restart","RestartDateTime",RestartDateTime+"00");
	}

	private void readTimeInfo() throws RuntimeException {
		TStart = mduOptions.get("time","TStart");
		TStop  = mduOptions.get("time","TStop");
	}

	public void finish() {
		mduOptions.store();
	}
}
