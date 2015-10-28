/* MOD_V2.0
 * Copyright (c) 2012 OpenDA Association
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

import org.openda.utils.Time;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * Util methods for reading/writing data from/to a uci file.
 *
 * @author Arno Kockx
 */
public class UciUtils {

	/**
	 * The dateFormat for dates in the UCI file is yyyy/MM/dd HH:mm, e.g.:
	 * "  START       2004/01/01 00:00  END    2004/01/10 00:00"
	 *
	 * @param startTimeDouble
	 * @param dateFormat
	 * @param timeZone
	 * @param startTimeExtension
	 * @return String startTime formatted for uci file.
	 */
	public static String getStartTimeString(double startTimeDouble, DateFormat dateFormat, TimeZone timeZone, double startTimeExtension) {
		long startTime = Time.mjdToMillies(startTimeDouble);

		//add startTimeExtension.
		startTime += startTimeExtension *3600*1000;

		Calendar calendar = Calendar.getInstance();
		calendar.setTimeZone(timeZone);
		calendar.setTimeInMillis(startTime);
		return dateFormat.format(calendar.getTime());
	}

	/**
	 * The dateFormat for dates in the UCI file is yyyy/MM/dd HH:mm, e.g.:
	 * "  START       2004/01/01 00:00  END    2004/01/10 00:00"
	 *
	 * @param endTimeDouble
	 * @param dateFormat
	 * @param timeZone
	 * @return String startTime formatted for uci file.
	 */
	public static String getEndTimeString(double endTimeDouble, DateFormat dateFormat, TimeZone timeZone) {
		//Note: the HSPF model does not write output values at the stopTime of the run.
		//In other words the HSPF model considers the run startTime to be inclusive
		//and the run stopTime to be exclusive.
		//To workaround this problem just increase the model run period in the run_info.xml
		//file by a little bit, to get output for the stopTime of the run.
		long stopTime = Time.mjdToMillies(endTimeDouble);
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeZone(timeZone);
		calendar.setTimeInMillis(stopTime);
		//Note: for some reason the HSPF model runs a day too long when the endTime of the run period
		//is at midnight in the timeZone of the model. This is probably a bug in the HSPF model.
		//To workaround this problem here subtract one day from the endTime if the endTime is at midnight in the
		//timeZone of the model.
		if (calendar.get(Calendar.HOUR_OF_DAY) == 0 && calendar.get(Calendar.MINUTE) == 0) {
			//if at midnight in timeZone of the model in this.dateFormat.
			calendar.add(Calendar.DAY_OF_MONTH, -1);
		}
		return dateFormat.format(calendar.getTime());
	}
}
