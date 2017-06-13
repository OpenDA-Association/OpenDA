/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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

package org.openda.model_hspf;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

/**
 * The startTime and endTime in a UCI (User Control Input) file for the HSPF (Hydrological Simulation Program - FORTRAN) model
 * will be replaced with the values of the exchangeItems of this DataObject.
 *
 * The dateFormat for dates in the UCI file is yyyy/MM/dd HH:mm, e.g.:
 * "  START       2004/01/01 00:00  END    2004/01/10 00:00"
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class UciDataObject implements IDataObject {

	/**
	 * The timeZone that is used by the model.
	 * This is required to convert the times of the data values
	 * to/from the timeZone that is used by the model.
	 * Default is GMT.
	 */
	private TimeZone timeZone = TimeZone.getTimeZone("GMT");

	private IExchangeItem startTimeExchangeItem = null;
	private IExchangeItem endTimeExchangeItem = null;
	private File uciFile = null;
	private DateFormat dateFormat = null;
	/**
	 * This is the startTime of the runPeriod for the HSPF model in hours relative to the startTime of the OpenDA runPeriod.
	 * This can be used to extend the runPeriod for the HSPF model (the runPeriod as it will be put in the uci file)
	 * with respect to the OpenDA runPeriod. This is useful when there is no restart file (warm state) available for the forecast.
	 * In other words the extended period will allow the model to get to a warm state before the forecast starts.
	 *
	 * This is 0 by default.
	 */
	private double startTimeExtension = 0;

	/**
	 * @param workingDir the working directory.
	 * @param arguments the first argument should be the name of the file containing the data for this DataObject (relative to the working directory).
	 *                  the second argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
	 *                  the third and fourth arguments should be the ids of the startTime and endTime exchangeItems respectively,
	 *                  the (optional) fifth argument should be the startTimeExtension in hours relative to the startTime of the OpenDA runPeriod.
	 */
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException("No fileName argument specified for " + getClass().getSimpleName()
					+ ". The first argument should be the name of the file containing the data for this DataObject (relative to the working directory).");
		}
		uciFile = new File(workingDir, arguments[0]);

		//get timeZone.
		if (arguments.length < 2) {
			throw new IllegalArgumentException("No timeZone argument specified for " + getClass().getSimpleName()
					+ ". The second argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12).");
		}
		try {
			double timeZoneOffsetInHours = Double.parseDouble(arguments[1]);
			timeZone = TimeUtils.createTimeZoneFromDouble(timeZoneOffsetInHours);
		} catch (Exception e) {
			throw new IllegalArgumentException("Cannot parse second argument '" + arguments[1] + "' for " + getClass().getSimpleName()
					+ ". The second argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12).", e);
		}

		//create exchange items.
		if (arguments.length < 4) {
			throw new IllegalArgumentException("No exchange item ids arguments specified for " + getClass().getSimpleName()
					+ ". The third and fourth arguments should be the ids of the startTime and endTime exchangeItems respectively.");
		}
		startTimeExchangeItem = new DoubleExchangeItem(arguments[2], 0);
		endTimeExchangeItem = new DoubleExchangeItem(arguments[3], 0);

		//get optional startTimeExtension.
		if (arguments.length > 4) {
			try {
				startTimeExtension = Double.parseDouble(arguments[4]);
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse fifth argument '" + arguments[4] + "' for " + getClass().getSimpleName()
						+ ". The (optional) fifth argument should be the startTimeExtension in hours relative to the startTime of the OpenDA runPeriod.", e);
			}
		}

		//The dateFormat for dates in the UCI file is yyyy/MM/dd HH:mm, e.g.:
		//"  START       2004/01/01 00:00  END    2004/01/10 00:00"
		dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm");
		dateFormat.setTimeZone(timeZone);
	}

	public String[] getExchangeItemIDs() {
		return new String[]{startTimeExchangeItem.getId(), endTimeExchangeItem.getId()};
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		if (role == IPrevExchangeItem.Role.Input || role == IPrevExchangeItem.Role.InOut) {
			return new String[]{startTimeExchangeItem.getId(), endTimeExchangeItem.getId()};
		}
		return null;
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (startTimeExchangeItem.getId().equals(exchangeItemID)) {
			return startTimeExchangeItem;
		}
		if (endTimeExchangeItem.getId().equals(exchangeItemID)) {
			return endTimeExchangeItem;
		}

		//if not found.
		return null;
	}

	public void finish() {
		writeDefinitionFile();
	}

	/**
	 * The uciFile is read and the start and end time are replaced.
	 */
	private void writeDefinitionFile() {
		if (startTimeExchangeItem == null || endTimeExchangeItem == null) {
			throw new IllegalStateException(getClass().getSimpleName() + " not initialized yet.");
		}
		if (!uciFile.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Uci file '" + uciFile.getAbsolutePath() + "' does not exist.");
		}
		Results.putMessage(getClass().getSimpleName() + ": replacing start and end time in uci file " + uciFile.getAbsolutePath());

		//read file.
		List<String> lines = AsciiFileUtils.readLines(uciFile);

		//get start and end times.
		double startTime = (Double) startTimeExchangeItem.getValues();
		double endTime = (Double) endTimeExchangeItem.getValues();
		String startTimeString = UciUtils.getStartTimeString(startTime, dateFormat, timeZone, startTimeExtension);
		String endTimeString = UciUtils.getEndTimeString(endTime, dateFormat, timeZone);

		//replace start and end time.
		for (int n = 0; n < lines.size(); n++) {
			String line = lines.get(n);
			if (line.trim().toUpperCase().startsWith("START")) {
				//e.g. "  START       2004/01/01 00:00  END    2004/01/10 00:00"
				String newLine = "  START       " + startTimeString + "  END    " + endTimeString;
				lines.set(n, newLine);
				Results.putMessage(getClass().getSimpleName() + ": replaced line " + (n + 1) + " with start and end time '" + newLine + "' in file " + uciFile.getAbsolutePath());
			}
		}

		//write file.
		AsciiFileUtils.writeLines(uciFile, lines);
	}
}
