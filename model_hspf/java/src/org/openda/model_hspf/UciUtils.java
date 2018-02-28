/* OpenDA v2.4.3 
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
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Time;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;

/**
 * Util methods for reading/writing data from/to a uci file.
 *
 * @author Arno Kockx
 */
public class UciUtils {
	public static final String RCHRES_MODULE_NAME = "RCHRES";
	public static final String PERLND_MODULE_NAME = "PERLND";
	public static final String IMPLND_MODULE_NAME = "IMPLND";

	public static final String RCHRES_LOCATION_ID_PREFIX = "RCH";
	public static final String PERLND_LOCATION_ID_PREFIX = "P";
	public static final String IMPLND_LOCATION_ID_PREFIX = "IMP";


	public static String getLocationIdPrefix(String moduleName) {
		if (RCHRES_MODULE_NAME.equals(moduleName)) return RCHRES_LOCATION_ID_PREFIX;
		if (PERLND_MODULE_NAME.equals(moduleName)) return PERLND_LOCATION_ID_PREFIX;
		if (IMPLND_MODULE_NAME.equals(moduleName)) return IMPLND_LOCATION_ID_PREFIX;

		throw new IllegalArgumentException("Unknown moduleName " + moduleName);
	}

	public static boolean firstHeaderRowContainsParameterIds(String tableName) {
		//init tables from RCHRES.
		return tableName.equals("HYDR-INIT")
				|| tableName.equals("HEAT-INIT")
				|| tableName.equals("BED-INIT")
				|| tableName.equals("OX-INIT")
				|| tableName.equals("NUT-DINIT")
				|| tableName.equals("PLNK-INIT")
				|| tableName.equals("BENAL-INIT")
				|| tableName.equals("PH-INIT")

				//init tables from IMPLND.
				|| tableName.equals("IWT-INIT");
	}

	public static boolean secondHeaderRowContainsParameterIds(String tableName) {
		//init tables from both PERLND and IMPLND.
		return tableName.equals("QUAL-INPUT")
				|| tableName.equals("SNOW-INIT1")
				|| tableName.equals("SNOW-INIT2")

				//init tables from PERLND.
				|| tableName.equals("PWAT-STATE1")
				|| tableName.equals("SED-STOR")
				|| tableName.equals("PSTEMP-TEMPS")
				|| tableName.equals("PWT-TEMPS")
				|| tableName.equals("PWT-GASES")
				|| tableName.equals("MST-TOPSTOR")
				|| tableName.equals("MST-TOPFLX")
				|| tableName.equals("MST-SUBSTOR")
				|| tableName.equals("MST-SUBFLX")
				|| tableName.equals("PEST-STOR1")
				|| tableName.equals("PEST-STOR2")
				|| tableName.equals("NIT-STOR1")
				|| tableName.equals("NIT-STOR2")
				|| tableName.equals("PHOS-STOR1")
				|| tableName.equals("PHOS-STOR2")
				|| tableName.equals("TRAC-TOPSTOR")
				|| tableName.equals("TRAC-SUBSTOR")

				//init tables from IMPLND.
				|| tableName.equals("IWAT-STATE1")
				|| tableName.equals("SLD-STOR")

				//init tables from RCHRES.
				|| tableName.equals("SSED-INIT")
				|| tableName.equals("NUT-ADSINIT");
	}

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
	 * Splits the given string into parts.
	 * The first part will be 10 characters. The other parts will have the given lengthInCharacters.
	 * If the last part is shorter than lengthInCharacters, then the last part will be thrown away.
	 */
	public static String[] splitAfter10ThenEveryNCharacters(String string, int lengthInCharacters) {
		if (string == null || string.isEmpty() || string.length() < 10) return null;

		List<String> parts = new ArrayList<>();

		String part = string.substring(0, 10);
		parts.add(part);
		string = string.substring(10);

		while (string.length() >= lengthInCharacters) {
			part = string.substring(0, lengthInCharacters);
			parts.add(part);
			string = string.substring(lengthInCharacters);
		}

		return parts.toArray(new String[parts.size()]);
	}

	/**
	 * Splits the given string into three parts. The first two parts are respectively 10 and 12 characters long.
	 */
	public static String[] splitAfter10And22Characters(String string) {
		if (string == null || string.isEmpty() || string.length() < 22) return null;

		String part1 = string.substring(0, 10);
		String part2 = string.substring(10, 22);
		String part3 = string.substring(22);

		return new String[]{part1, part2, part3};
	}

	public static boolean isSecondHeaderRow(String firstColumn) {
		//in example .uci files "# -  #" is incorrectly written as "x -  x" or "x  - x".
		return firstColumn.contains("#") || firstColumn.toUpperCase().contains("X");
	}

	public static boolean isValuesRow(String firstColumn) {
		//if contains at least one digit.
		return firstColumn.matches(".*\\d.*");
	}

	public static List<String> readParameterIds(String moduleName, String tableName, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;

		//for RCHRES HYDR-INIT only read second column (VOL), because for the other columns it is not clear which parameters should be used for the exchange items.
		//for RCHRES BED-INIT only read second column (BEDDEP), because otherwise parameters SAND, SILT and CLAY would not be unique (these could be either
		//initial bed sediment composition fractions from BED-INIT or initial suspended sediment concentrations from SSED-INIT).
		int lastValueColumnIndex;
		if (tableName.equals("HYDR-INIT") || tableName.equals("BED-INIT")) {
			lastValueColumnIndex = 1;
		} else {
			lastValueColumnIndex = columns.length - 1;
		}

		List<String> parameterIds = new ArrayList<>();
		for (int k = firstValueColumnIndex; k <= lastValueColumnIndex; k++) {
			String parameterId = columns[k].trim();
			parameterIds.add(parameterId);
		}

		UciUtils.validateParameterIds(moduleName, tableName, parameterIds);
		return parameterIds;
	}

	private static void validateParameterIds(String moduleName, String tableName, List<String> parameterIds) {
		for (String parameterId : parameterIds) {
			//if parameterId contains spaces, slashes or parentheses, then it is invalid, probably due to a units row that is positioned incorrectly in the uci file.
			if (parameterId.isEmpty() || parameterId.contains(" ") || parameterId.contains("/") || parameterId.contains("\\") || parameterId.contains("(") || parameterId.contains(")")) {
				throw new IllegalArgumentException("Invalid parameterId '" + parameterId + "' found in " + moduleName + " init table '" + tableName
						+ "' in uci state file. Please correct the format and position of the parameter and unit rows in the " + moduleName + " init table '" + tableName + "' in the uci state file.");
			}
		}
	}

	/**
	 * @param locationNumberAdditionalInfoMap can be null.
	 */
	public static void readValuesRow(String moduleName, String tableName, String[] columns, String inputLine, String locationIdPrefix, List<String> parameterIds, double stateTime,
			Set<Integer> uniqueLocationNumbers, Map<String, IExchangeItem> exchangeItems, Map<Integer, String> locationNumberAdditionalInfoMap) {

		int firstLocationNumber = readFirstLocationNumber(moduleName, tableName, columns[0]);
		int lastLocationNumber = readLastLocationNumber(moduleName, tableName, columns[0], firstLocationNumber);
		List<Double> values = readValues(moduleName, tableName, columns);
		createExchangeItems(moduleName, tableName, locationIdPrefix, firstLocationNumber, lastLocationNumber, parameterIds, values, stateTime, uniqueLocationNumbers, exchangeItems);

		if (locationNumberAdditionalInfoMap != null) {
			//for RCHRES HYDR-INIT only the second column (VOL) is used, but the additional columns also need to be stored, because these are needed during writing.
			//for RCHRES BED-INIT only the second column (BEDDEP) is used, but the additional columns also need to be stored, because these are needed during writing.
			if (tableName.equals("HYDR-INIT") || tableName.equals("BED-INIT")) {
				String additionalColumns = inputLine.substring(20);
				for (int locationNumber = firstLocationNumber; locationNumber <= lastLocationNumber; locationNumber++) {
					locationNumberAdditionalInfoMap.put(locationNumber, additionalColumns);
				}
			}
		}
	}

	private static List<Double> readValues(String moduleName, String tableName, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;

		//for RCHRES HYDR-INIT only read second column (VOL), because for the other columns it is not clear which parameters should be used for the exchange items.
		//for RCHRES BED-INIT only read second column (BEDDEP), because otherwise parameters SAND, SILT and CLAY would not be unique (these could be either
		//initial bed sediment composition fractions from BED-INIT or initial suspended sediment concentrations from SSED-INIT).
		int lastValueColumnIndex;
		if (tableName.equals("HYDR-INIT") || tableName.equals("BED-INIT")) {
			lastValueColumnIndex = 1;
		} else {
			lastValueColumnIndex = columns.length - 1;
		}

		List<Double> values = new ArrayList<>();
		for (int k = firstValueColumnIndex; k <= lastValueColumnIndex; k++) {
			String valueString = columns[k].trim();
			try {
				values.add(Double.parseDouble(valueString));
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse double '" + valueString + "' in " + moduleName + " init table '" + tableName + "' in uci state file.", e);
			}
		}
		return values;
	}

	public static int readFirstLocationNumber(String moduleName, String tableName, String firstColumn) {
		String part1 = firstColumn.substring(0, 5).trim();
		try {
			return Integer.parseInt(part1);
		} catch (NumberFormatException e) {
			throw new IllegalArgumentException("Cannot parse integer '" + part1 + "' in " + moduleName + " init table '" + tableName + "' in uci state file.", e);
		}
	}

	public static int readLastLocationNumber(String moduleName, String tableName, String firstColumn, int firstLocationNumber) {
		String part2 = firstColumn.substring(5, 10).trim();
		if (part2.isEmpty()) {
			return firstLocationNumber;
		} else {
			try {
				return Integer.parseInt(part2);
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse integer '" + part2 + "' in " + moduleName + " init table '" + tableName + "' in uci state file.", e);
			}
		}
	}

	/**
	 * @param firstLocationNumber is inclusive.
	 * @param lastLocationNumber is inclusive.
	 */
	private static void createExchangeItems(String moduleName, String tableName, String locationIdPrefix, int firstLocationNumber, int lastLocationNumber,
			List<String> parameterIds, List<Double> values, double stateTime, Set<Integer> uniqueLocationNumbers, Map<String, IExchangeItem> exchangeItems) {

		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (values.size() != parameterIds.size()) {
			throw new IllegalArgumentException("Number of values (" + values.size() + ") not equal to number of parameters (" + parameterIds.size() + ") in " + moduleName + " init table '" + tableName + "' in uci state file.");
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

	/**
	 * @param locationNumberAdditionalInfoMap can be null.
	 */
	public static void writeTable(Map<String, IExchangeItem> exchangeItems, List<String> outputLines, String tableName, String firstHeaderRow, String secondHeaderRow,
			String locationIdPrefix, List<Integer> locationNumbers, List<String> parameterIds, Map<Integer, String> locationNumberAdditionalInfoMap, Map<String, IExchangeItem> exchangeItemsDefaultInit) {
		outputLines.add("  " + tableName);

		//write header rows.
		outputLines.add(firstHeaderRow);
		outputLines.add(secondHeaderRow);

		//for each location write a values row.
		for (int locationNumber : locationNumbers) {
			String valuesRow = writeValuesRow(tableName, locationIdPrefix, locationNumber, parameterIds, exchangeItems, exchangeItemsDefaultInit);
			if (locationNumberAdditionalInfoMap != null) {
				//for RCHRES HYDR-INIT only the second column (VOL) is used, but the additional columns also need to be written.
				//for RCHRES BED-INIT only the second column (BEDDEP) is used, but the additional columns also need to be written.
				if (tableName.equals("HYDR-INIT") || tableName.equals("BED-INIT")) {
					String additionalColumns = locationNumberAdditionalInfoMap.get(locationNumber);
					if (additionalColumns == null) throw new IllegalStateException("additionalColumns for location number " + locationNumber + " not initialized during reading of uci state file.");
					valuesRow += additionalColumns;
				}
			}
			outputLines.add(valuesRow);
		}

		outputLines.add("  END " + tableName);
	}

	private static String writeValuesRow(String tableName, String locationIdPrefix, int locationNumber, List<String> parameterIds, Map<String, IExchangeItem> exchangeItems, Map<String, IExchangeItem> exchangeItemsDefaultInit) {
		String result = "";

		//use Locale.US so that always uses points as decimal symbols.
		Formatter valuesRow = new Formatter(Locale.US);
		DecimalFormat decimalFormatter8 = (DecimalFormat) DecimalFormat.getInstance(Locale.US);
		decimalFormatter8.applyPattern("0.00E0");
		DecimalFormat decimalFormatter10 = (DecimalFormat) DecimalFormat.getInstance(Locale.US);
		decimalFormatter10.applyPattern("0.0000E0");

		//write first column.
		//format locationNumber:
		//1$ means first argument.
		//5 means minimum width of 5 characters (left-padded with spaces).
		//d means integer.
		valuesRow.format("%1$5d     ", locationNumber);

		result += valuesRow.toString();

		//write other columns.
		String locationId = locationIdPrefix + String.valueOf(locationNumber);
		for (String parameterId : parameterIds) {
			String id = locationId + "." + parameterId;
			IExchangeItem item = exchangeItems.get(id);
			if (item == null) throw new IllegalStateException("Exchange item with id '" + id + "' not initialized during reading of uci state file.");

			double value = (double) item.getValues();
			if (QualInputTable.isQualInputTable(tableName)) {
				// Make optimal use of 8 characters, with one reserved for a sign.

				// Check if value is unrealistically small and set it to default when necessary (see email Changmin, 27-02-2017, ODA-542)
				if (value < 1E-10){
					IExchangeItem itemDefaultInit = exchangeItemsDefaultInit.get(id);
					if (itemDefaultInit==null){
						throw new RuntimeException("A value of "+ id +" that is less than 1E-10 is found in the updated state and should be set to a default value. Please specify an UCI file containing default initial values.");
					}
					value = (double) itemDefaultInit.getValues();
				}

				// Store absolute value and the sign.
				double absValue = Math.abs(value);
				String sign = " ";
				if (value < 0.0) sign = "-";

				// Limit the domain of value.
				if (value >= 9999999.5) {
					value = 9999999.0;
				}
				if (value <= -9999999.5) {
					value = -9999999.0;
				}

				// Integer part
				long iPart = (long)absValue;
				// Length of the integer part.
				long iPartLength = String.valueOf(iPart).length();

				// Print values with an integer part longer then 5 characters as an int.
				if (iPartLength > 5) {
					result += sign + String.valueOf(iPart);
					continue;
				}
				// Print absolute values between 100000.0 and 0.005 as well as zero explicitly.
				if ((100000.0 > absValue && absValue >= 0.005) || absValue == 0.0) {
					double frac = (absValue - iPart) * Math.pow(10,6-iPartLength);
					int iFrac = (int)frac;
					result += sign + String.valueOf(iPart) + "." + String.format("%0" + String.valueOf(6 - iPartLength) + "d", iFrac);
					continue;
				}

				// Print absolute values smaller then 0.005 using scientific notation.
				//  note that the exponent will be negative, consuming 1 character
				result += sign + decimalFormatter8.format(absValue).toString();
			} else {
				// Make optimal use of 10 characters, with one reserved for a sign.

				// Store absolute value and the sign.
				double absValue = Math.abs(value);
				String sign = " ";
				if (value < 0.0) sign = "-";

				// Limit the domain of value.
				if (value >= 999999999.5) {
					value = 999999999.0;
				}
				if (value <= -999999999.5) {
					value = -999999999.0;
				}

				// Integer part
				long iPart = (long)absValue;
				// Length of the integer part.
				long iPartLength = String.valueOf(iPart).length();

				// When the value is of the order of E19 or larger, the iPartLength stays equal to 20. In this case, print value as it is
				if (iPartLength >= 19) {
//					NumberFormat numberFormatter = new DecimalFormat("0.00###E0");
//					String resultString = numberFormatter.format(absValue);
//					result += sign + resultString + "  ";

					IExchangeItem itemDefaultInit = exchangeItemsDefaultInit.get(id);
					if (itemDefaultInit==null){
						throw new RuntimeException("The value of NaN (-1.00E+30) is found in the updated state. Please specify an UCI file containing default initial values.");
					}
					double defaultValue = (double) itemDefaultInit.getValues();

					absValue = Math.abs(defaultValue);
					sign = " ";
					if (defaultValue < 0.0) sign = "-";

					// Limit the domain of value.
					if (value >= 999999999.5) {
						value = 999999999.0;
					}
					if (value <= -999999999.5) {
						value = -999999999.0;
					}

					// Integer part
					iPart = (long)absValue;
					// Length of the integer part.
					iPartLength = String.valueOf(iPart).length();
//					continue;
				}

				// Print values with an integer part longer then 7 characters as an int.
				if (iPartLength > 7) {
					result += sign + String.valueOf(iPart);
					continue;
				}
				// Print absolute values between 10000000.0 and 0.005 as well as zero explicitly.
				if ((10000000.0 > absValue && absValue >= 0.005) || absValue == 0.0) {
					double frac = (absValue - iPart) * Math.pow(10,8-iPartLength);
					int iFrac = (int)frac;
					result += sign + String.valueOf(iPart) + "." + String.format("%0"+String.valueOf(8-iPartLength)+"d", iFrac);
					continue;
				}

				// Print absolute values smaller then 0.005 using scientific notation.
				//  note that the exponent will be negative, consuming 1 character
				result += sign + decimalFormatter10.format(absValue).toString();
			}
		}

		return result;
	}
}
