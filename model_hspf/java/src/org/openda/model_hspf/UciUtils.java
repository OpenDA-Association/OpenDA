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

import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Time;

import java.text.DateFormat;
import java.util.*;

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

	/**
	 * Splits the given string into parts that are 10 characters long.
	 * If the last part is shorter than 10 characters, then the last part will be ignored.
	 */
	public static String[] splitEvery10Characters(String string) {
		if (string == null || string.isEmpty()) return null;

		List<String> parts = new ArrayList<>();
		while (string.length() >= 10) {
			String part = string.substring(0, 10);
			parts.add(part);
			string = string.substring(10);
		}

		return parts.toArray(new String[parts.size()]);
	}

	public static List<String> readParameterIds(String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;
		int lastValueColumnIndex = columns.length - 1;

		List<String> parameterIds = new ArrayList<>();
		for (int k = firstValueColumnIndex; k <= lastValueColumnIndex; k++) {
			String parameterId = columns[k].trim();
			parameterIds.add(parameterId);
		}
		return parameterIds;
	}

	public static void validateParameterIds(List<String> parameterIds, String moduleName, String tableType) {
		for (String parameterId : parameterIds) {
			//if parameterId contains spaces, slashes or parentheses, then it is invalid, probably due to a units row that is positioned incorrectly in the uci file.
			if (parameterId.isEmpty() || parameterId.contains(" ") || parameterId.contains("/") || parameterId.contains("\\") || parameterId.contains("(") || parameterId.contains(")")) {
				throw new IllegalArgumentException("Invalid parameterId '" + parameterId + "' found in " + moduleName + " init table '" + tableType
						+ "' in uci state file. Please correct the format and position of the parameter and unit rows in the " + moduleName + " init table '" + tableType + "' in the uci state file.");
			}
		}
	}

	public static List<Double> readValues(String moduleName, String tableType, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;
		int lastValueColumnIndex = columns.length - 1;

		List<Double> values = new ArrayList<>();
		for (int k = firstValueColumnIndex; k <= lastValueColumnIndex; k++) {
			String valueString = columns[k].trim();
			try {
				values.add(Double.parseDouble(valueString));
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse double '" + valueString + "' in " + moduleName + " init table '" + tableType + "' in uci state file.", e);
			}
		}
		return values;
	}

	public static int readFirstLocationNumber(String moduleName, String tableType, String firstColumn) {
		String part1 = firstColumn.substring(0, 5).trim();
		try {
			return Integer.parseInt(part1);
		} catch (NumberFormatException e) {
			throw new IllegalArgumentException("Cannot parse integer '" + part1 + "' in " + moduleName + " init table '" + tableType + "' in uci state file.", e);
		}
	}

	public static int readLastLocationNumber(String moduleName, String tableType, String firstColumn, int firstLocationNumber) {
		String part2 = firstColumn.substring(5, 10).trim();
		if (part2.isEmpty()) {
			return firstLocationNumber;
		} else {
			try {
				return Integer.parseInt(part2);
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse integer '" + part2 + "' in " + moduleName + " init table '" + tableType + "' in uci state file.", e);
			}
		}
	}

	/**
	 * @param moduleName
	 * @param tableType
	 * @param locationIdPrefix
	 * @param firstLocationNumber inclusive.
	 * @param lastLocationNumber inclusive.
	 * @param parameterIds
	 * @param values
	 * @param stateTime
	 * @param uniqueLocationNumbers
	 * @param exchangeItems
	 */
	public static void createExchangeItems(String moduleName, String tableType, String locationIdPrefix, int firstLocationNumber, int lastLocationNumber,
			List<String> parameterIds, List<Double> values, double stateTime, Set<Integer> uniqueLocationNumbers, Map<String, IExchangeItem> exchangeItems) {

		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in " + moduleName + " init table '" + tableType + "' in uci state file.");
		if (values.size() != parameterIds.size()) {
			throw new IllegalArgumentException("Number of values (" + values.size() + ") not equal to number of parameters (" + parameterIds.size() + ") in " + moduleName + " init table '" + tableType + "' in uci state file.");
		}

		for (int locationNumber = firstLocationNumber; locationNumber <= lastLocationNumber; locationNumber++) {
			uniqueLocationNumbers.add(locationNumber);
			String locationId = locationIdPrefix + String.valueOf(locationNumber);

			for (int n = 0; n < values.size(); n++) {
				String parameterId = parameterIds.get(n);
				String id = locationId + "." + parameterId;

				DoubleExchangeItem newItem = new DoubleExchangeItem(id, IPrevExchangeItem.Role.InOut, values.get(n));
				newItem.setTime(stateTime);
				IExchangeItem previous = exchangeItems.put(id, newItem);
				if (previous != null) throw new IllegalArgumentException("Multiple exchange items with id '" + id + "' found in uci state file.");
			}
		}
	}

	/**
	 * Continue reading until the next line that starts with "END".
	 */
	public static void skipToEnd(Iterator<String> inputIterator) {
		while (inputIterator.hasNext()) {
			String line = inputIterator.next();
			if (line.trim().toUpperCase().startsWith("END")) break;
		}
	}

	public static String writeValuesRow(String locationIdPrefix, int locationNumber, List<String> parameterIds, Map<String, IExchangeItem> exchangeItems) {
		//use Locale.US so that always uses points as decimal symbols.
		Formatter valuesRow = new Formatter(Locale.US);

		//write first column.
		//format locationNumber:
		//1$ means first argument.
		//5 means minimum width of 5 characters (left-padded with spaces).
		//d means integer.
		valuesRow.format("%1$5d     ", locationNumber);

		//write other columns.
		String locationId = locationIdPrefix + String.valueOf(locationNumber);
		for (String parameterId : parameterIds) {
			String id = locationId + "." + parameterId;
			IExchangeItem item = exchangeItems.get(id);
			if (item == null) throw new IllegalStateException("Exchange item with id '" + id + "' not initialized during reading of uci state file.");

			double value = (double) item.getValues();
			//format value:
			//1$ means first argument.
			//10 means minimum width of 10 characters (left-padded with spaces).
			//.4 means 4 significant digits. This cannot be larger, since otherwise the scientific notation would not fit within the column width of 10 characters.
			//G means floating point, or scientific notation if it would not fit otherwise.
			valuesRow.format("%1$10.4G", value);
		}

		return valuesRow.toString();
	}
}
