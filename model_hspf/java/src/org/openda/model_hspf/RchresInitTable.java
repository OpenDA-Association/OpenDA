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

import java.util.*;

/**
 * Class to read/write a RCHRES table with initial conditions from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class RchresInitTable {
	private final String tableType;
	/**
	 * Store the first parameterIds row and units row. These are re-used during writing.
	 */
	private final String parameterIdsRow;
	private final String unitsRow;
	/**
	 * Parameters and reaches used in this table.
	 */
	private final List<String> parameterIds;
	private final List<Integer> reachNumbers;
	/**
	 * Map to store additional info for each reach number.
	 */
	private final Map<Integer, String> reachNumberAdditionalInfoMap = new HashMap<>();

	/**
	 * The following parameters with initial conditions in the UCI file are supported by this class:
	 *
	 * <table type>: <parameters>
	 * BED-INIT:   BEDDEP
	 * BENAL-INIT: BENAL1, BENAL2, BENAL3, BENAL4
	 * HEAT-INIT:  TW, AIRTMP
	 * HYDR-INIT:  VOL
	 * IWT-INIT:   SOTMP, SODOX, SOCO2
	 * OX-INIT:    DOX, BOD, SATDO
	 * PH-INIT:    TIC, CO2, PH
	 * PLNK-INIT:  PHYTO, ZOO, BENAL, ORN, ORP, ORC
	 * NUT-DINIT:  NO3, TAM, NO2, PO4, PHVAL
	 */
	public static boolean isRchresTableWithInitialConditions(String tableType) {
		//tableType HYDR-CINIT is currently not supported, because it is not clear which parameters should be used for the exchange items.
		//tableType SSED-INIT is currently not supported, because otherwise parameters SAND, SILT and CLAY would not be unique (could be initial bed sediment composition fractions or initial suspended sediment concentrations).
		return tableType.equals("BED-INIT")
				|| tableType.equals("BENAL-INIT")
				|| tableType.equals("HEAT-INIT")
				|| tableType.equals("HYDR-INIT")
				|| tableType.equals("IWT-INIT")
				|| tableType.equals("OX-INIT")
				|| tableType.equals("PH-INIT")
				|| tableType.equals("PLNK-INIT")
				|| tableType.equals("NUT-DINIT");
	}

	/**
	 * Read one RCHRES init table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public RchresInitTable(String tableType, Iterator<String> inputLines, Map<String, IExchangeItem> exchangeItems) {
		if (tableType == null) throw new IllegalArgumentException("tableType == null");
		this.tableType = tableType;

		//read uci file.
		String parameterIdsRow = null;
		String unitsRow = null;
		List<String> parameterIds = null;
		Set<Integer> uniqueReachNumbers = new HashSet<>();
		while (inputLines.hasNext()) {
			String inputLine = inputLines.next();
			if (inputLine.trim().toUpperCase().startsWith("END")) break;

			//this code assumes that all relevant columns are exactly 10 characters wide in the .uci file, as defined in the HSPF Manual.
			String[] columns = UciUtils.splitEvery10Characters(inputLine);
			if (columns == null || columns.length < 2) {//if empty row.
				//skip row.
				continue;
			}
			String firstColumn = columns[0];

			//in example .uci files "RCHRES" is sometimes incorrectly spelled as "RC HRES".
			if (firstColumn.toUpperCase().contains("RCHRES") || firstColumn.toUpperCase().contains("RC HRES")) {//if parameterIds row.
				if (parameterIdsRow == null) parameterIdsRow = inputLine;
				//update parameterIds for reading the next value row.
				parameterIds = readParameterIds(tableType, columns);
				continue;
			}

			//in example .uci files "# -  #" is incorrectly spelled as "x -  x" or "x  - x".
			if (firstColumn.contains("#") || firstColumn.toUpperCase().contains("X")) {//if units row.
				if (unitsRow == null) unitsRow = inputLine;
				continue;
			}

			//if contains at least one digit.
			if (firstColumn.matches(".*\\d.*")) {//if values row.
				int firstReachNumber = readFirstReachNumber(tableType, firstColumn);
				int lastReachNumber = readLastReachNumber(tableType, firstColumn, firstReachNumber);
				List<Double> values = readValues(tableType, columns);
				createExchangeItems(firstReachNumber, lastReachNumber, parameterIds, values, uniqueReachNumbers, exchangeItems);

				//for HYDR-INIT only the second column (VOL) is used, but the additional columns also need to be stored, because these are needed during writing.
				//for BED-INIT only the second column (BEDDEP) is used, but the additional columns also need to be stored, because these are needed during writing.
				if (tableType.equals("HYDR-INIT") || tableType.equals("BED-INIT")) {
					String additionalColumns = inputLine.substring(20);
					for (int reachNumber = firstReachNumber; reachNumber <= lastReachNumber; reachNumber++) {
						reachNumberAdditionalInfoMap.put(reachNumber, additionalColumns);
					}
				}
				continue;
			}

			//do nothing, skip row.
		}

		if (parameterIdsRow == null) throw new RuntimeException("No valid parameter ids row found in RCHRES init table '" + tableType + "' in uci state file.");
		if (unitsRow == null) throw new RuntimeException("No valid units row found in RCHRES init table '" + tableType + "' in uci state file.");
		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in RCHRES init table '" + tableType + "' in uci state file.");
		if (uniqueReachNumbers.isEmpty()) throw new RuntimeException("No valid reach numbers found in RCHRES init table '" + tableType + "' in uci state file.");

		this.parameterIdsRow = parameterIdsRow;
		this.unitsRow = unitsRow;
		this.parameterIds = parameterIds;
		this.reachNumbers = new ArrayList<>(uniqueReachNumbers);
		Collections.sort(this.reachNumbers);
	}

	public void write(Map<String, IExchangeItem> exchangeItems, List<String> outputLines) {
		outputLines.add("  " + tableType);

		//for each reach write one block consisting of a parameterIds row, a units row and a values row.
		for (int reachNumber : reachNumbers) {
			outputLines.add(parameterIdsRow);
			outputLines.add(unitsRow);
			outputLines.add(writeValuesRow(reachNumber, exchangeItems));
		}

		outputLines.add("  END " + tableType);
	}

	private String writeValuesRow(int reachNumber, Map<String, IExchangeItem> exchangeItems) {
		StringBuilder valuesRow = new StringBuilder();

		//write first column.
		//format reachNumber:
		//d means integer.
		//5 means minimum width of 5 characters (left-padded with spaces).
		valuesRow.append(String.format("%1$5d", reachNumber)).append("     ");

		//write other columns.
		String locationId = "RCH" + String.valueOf(reachNumber);
		for (String parameterId : parameterIds) {
			String id = locationId + "." + parameterId;
			IExchangeItem item = exchangeItems.get(id);
			if (item == null) throw new IllegalStateException("Exchange item with id '" + id + "' not initialized during reading of uci state file.");

			double value = (double) item.getValues();
			//format value.
			//G means floating point or scientific notation if it would not fit otherwise.
			//10 means minimum width of 10 characters (left-padded with spaces).
			//.4 means 4 significant digits. This cannot be larger, since otherwise the scientific notation would not fit within the column width of 10 characters.
			valuesRow.append(String.format("%1$10.4G", value));
		}

		//for HYDR-INIT only the second column (VOL) is used, but the additional columns also need to be written.
		//for BED-INIT only the second column (BEDDEP) is used, but the additional columns also need to be written.
		if (tableType.equals("HYDR-INIT") || tableType.equals("BED-INIT")) {
			String additionalColumns = reachNumberAdditionalInfoMap.get(reachNumber);
			if (additionalColumns == null) throw new IllegalStateException("additionalColumns for reach number " + reachNumber + " not initialized during reading of uci state file.");
			valuesRow.append(additionalColumns);
		}

		return valuesRow.toString();
	}

	private static List<String> readParameterIds(String tableType, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;

		//for HYDR-INIT only read second column (VOL), because for the other columns it is not clear which parameters should be used for the exchange items.
		//for BED-INIT only read second column (BEDDEP), because otherwise parameters SAND, SILT and CLAY would not be unique (could be initial bed sediment composition fractions or initial suspended sediment concentrations).
		int lastValueColumnIndex;
		if (tableType.equals("HYDR-INIT") || tableType.equals("BED-INIT")) {
			lastValueColumnIndex = 1;
		} else {
			lastValueColumnIndex = columns.length - 1;
		}

		List<String> parameterIds = new ArrayList<>();
		for (int k = firstValueColumnIndex; k <= lastValueColumnIndex; k++) {
			String parameterId = columns[k].trim();
			parameterIds.add(parameterId);
		}
		return parameterIds;
	}

	private static int readFirstReachNumber(String tableType, String firstColumn) {
		String part1 = firstColumn.substring(0, 5).trim();
		try {
			return Integer.parseInt(part1);
		} catch (NumberFormatException e) {
			throw new IllegalArgumentException("Cannot parse integer '" + part1 + "' in RCHRES init table '" + tableType + "' in uci state file.", e);
		}
	}

	private static int readLastReachNumber(String tableType, String firstColumn, int firstReachNumber) {
		String part2 = firstColumn.substring(5, 10).trim();
		if (part2.isEmpty()) {
			return firstReachNumber;
		} else {
			try {
				return Integer.parseInt(part2);
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse integer '" + part2 + "' in RCHRES init table '" + tableType + "' in uci state file.", e);
			}
		}
	}

	private static List<Double> readValues(String tableType, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;

		//for HYDR-INIT only read second column (VOL), because for the other columns it is not clear which parameters should be used for the exchange items.
		//for BED-INIT only read second column (BEDDEP), because otherwise parameters SAND, SILT and CLAY would not be unique (could be initial bed sediment composition fractions or initial suspended sediment concentrations).
		int lastValueColumnIndex;
		if (tableType.equals("HYDR-INIT") || tableType.equals("BED-INIT")) {
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
				throw new IllegalArgumentException("Cannot parse double '" + valueString + "' in RCHRES init table '" + tableType + "' in uci state file.", e);
			}
		}
		return values;
	}

	private void createExchangeItems(int firstReachNumber, int lastReachNumber, List<String> parameterIds, List<Double> values, Set<Integer> uniqueReachNumbers, Map<String, IExchangeItem> exchangeItems) {
		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in RCHRES init table '" + tableType + "' in uci state file.");
		if (values.size() != parameterIds.size()) {
			throw new IllegalArgumentException("Number of values (" + values.size() + ") not equal to number of parameters (" + parameterIds.size() + ") in RCHRES init table '" + tableType + "' in uci state file.");
		}

		for (int reachNumber = firstReachNumber; reachNumber <= lastReachNumber; reachNumber++) {
			uniqueReachNumbers.add(reachNumber);
			String locationId = "RCH" + String.valueOf(reachNumber);

			for (int n = 0; n < values.size(); n++) {
				String parameterId = parameterIds.get(n);
				String id = locationId + "." + parameterId;

				DoubleExchangeItem newItem = new DoubleExchangeItem(id, IPrevExchangeItem.Role.InOut, values.get(n));
				//this code assumes that the time of the state in the uci file is equal to the end time of the run.
				//TODO read endTime from uci file or create runtime argument for state time?
//				double stateTime = endTime;
//				newItem.setTime(stateTime);
				IExchangeItem previous = exchangeItems.put(id, newItem);
				if (previous != null) throw new IllegalArgumentException("Multiple exchange items with id '" + id + "' found in uci state file.");
			}
		}
	}
}
