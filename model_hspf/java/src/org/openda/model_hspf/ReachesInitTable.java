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

import org.openda.interfaces.IExchangeItem;

import java.util.*;

/**
 * Class to read/write a RCHRES table with initial conditions from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class ReachesInitTable {

	private final String tableName;
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
	 * For RCHRES the following tables and parameters with initial conditions in the UCI file are supported:
	 *
	 * <table type>:  <parameters>
	 * BED-INIT:      BEDDEP
	 * BENAL-INIT:    BENAL1, BENAL2, BENAL3, BENAL4
	 * HEAT-INIT:     TW, AIRTMP
	 * HYDR-INIT:     VOL
	 * IWT-INIT:      SOTMP, SODOX, SOCO2
	 * OX-INIT:       DOX, BOD, SATDO
	 * PH-INIT:       TIC, CO2, PH
	 * PLNK-INIT:     PHYTO, ZOO, BENAL, ORN, ORP, ORC
	 * NUT-DINIT:     NO3, TAM, NO2, PO4, PHVAL
	 *
	 * For RCHRES the following tables are not supported:
	 * HYDR-CINIT is currently not supported, because it is not clear from the HSPF manual which parameters should be used for the exchange items.
	 * NUT-ADSINIT is currently not supported, because it is not clear from the HSPF manual which parameters should be used for the exchange items.
	 * SSED-INIT is currently not supported, because otherwise parameters SAND, SILT and CLAY would not be unique (these could be
	 * either initial bed sediment composition fractions from BED-INIT or initial suspended sediment concentrations from SSED-INIT).
	 */
	public static boolean isReachesInitTable(String tableName) {
		return tableName.equals("BED-INIT")
				|| tableName.equals("BENAL-INIT")
				|| tableName.equals("HEAT-INIT")
				|| tableName.equals("HYDR-INIT")
				|| tableName.equals("IWT-INIT")
				|| tableName.equals("OX-INIT")
				|| tableName.equals("PH-INIT")
				|| tableName.equals("PLNK-INIT")
				|| tableName.equals("NUT-DINIT");
	}

	/**
	 * Read one RCHRES init table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public ReachesInitTable(String tableName, Iterator<String> inputLines, double stateTime, Map<String, IExchangeItem> exchangeItems) {
		if (tableName == null) throw new IllegalArgumentException("tableName == null");
		this.tableName = tableName;

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

			//in example .uci files "RCHRES" is sometimes incorrectly written as "RC HRES".
			if (firstColumn.toUpperCase().contains("RCHRES") || firstColumn.toUpperCase().contains("RC HRES")) {//if parameterIds row.
				if (parameterIdsRow == null) parameterIdsRow = inputLine;
				//update parameterIds for reading the next value row.
				parameterIds = readParameterIds(tableName, columns);
				UciUtils.validateParameterIds(parameterIds, UciUtils.RCHRES_MODULE_NAME, tableName);
				continue;
			}

			//in example .uci files "# -  #" is incorrectly written as "x -  x" or "x  - x".
			if (firstColumn.contains("#") || firstColumn.toUpperCase().contains("X")) {//if units row.
				if (unitsRow == null) unitsRow = inputLine;
				continue;
			}

			//if contains at least one digit.
			if (firstColumn.matches(".*\\d.*")) {//if values row.
				int firstReachNumber = UciUtils.readFirstLocationNumber(UciUtils.RCHRES_MODULE_NAME, tableName, firstColumn);
				int lastReachNumber = UciUtils.readLastLocationNumber(UciUtils.RCHRES_MODULE_NAME, tableName, firstColumn, firstReachNumber);
				List<Double> values = readValues(tableName, columns);
				UciUtils.createExchangeItems(UciUtils.RCHRES_MODULE_NAME, tableName, UciUtils.RCHRES_LOCATION_ID_PREFIX, firstReachNumber, lastReachNumber, parameterIds, values, stateTime, uniqueReachNumbers, exchangeItems);

				//for HYDR-INIT only the second column (VOL) is used, but the additional columns also need to be stored, because these are needed during writing.
				//for BED-INIT only the second column (BEDDEP) is used, but the additional columns also need to be stored, because these are needed during writing.
				if (tableName.equals("HYDR-INIT") || tableName.equals("BED-INIT")) {
					String additionalColumns = inputLine.substring(20);
					for (int reachNumber = firstReachNumber; reachNumber <= lastReachNumber; reachNumber++) {
						reachNumberAdditionalInfoMap.put(reachNumber, additionalColumns);
					}
				}
				continue;
			}

			//do nothing, skip row.
		}

		if (parameterIdsRow == null) throw new RuntimeException("No valid parameter ids row found in " + UciUtils.RCHRES_MODULE_NAME + " init table '" + tableName + "' in uci state file.");
		if (unitsRow == null) throw new RuntimeException("No valid units row found in " + UciUtils.RCHRES_MODULE_NAME + " init table '" + tableName + "' in uci state file.");
		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in " + UciUtils.RCHRES_MODULE_NAME + " init table '" + tableName + "' in uci state file.");
		if (uniqueReachNumbers.isEmpty()) throw new RuntimeException("No valid reach numbers found in " + UciUtils.RCHRES_MODULE_NAME + " init table '" + tableName + "' in uci state file.");

		this.parameterIdsRow = parameterIdsRow;
		this.unitsRow = unitsRow;
		this.parameterIds = parameterIds;
		this.reachNumbers = new ArrayList<>(uniqueReachNumbers);
		Collections.sort(this.reachNumbers);
	}

	public void write(Map<String, IExchangeItem> exchangeItems, List<String> outputLines) {
		outputLines.add("  " + tableName);

		//for each reach write one block consisting of a parameterIds row, a units row and a values row.
		for (int reachNumber : reachNumbers) {
			outputLines.add(parameterIdsRow);
			outputLines.add(unitsRow);

			String valuesRow = UciUtils.writeValuesRow(UciUtils.RCHRES_LOCATION_ID_PREFIX, reachNumber, parameterIds, exchangeItems);
			//for HYDR-INIT only the second column (VOL) is used, but the additional columns also need to be written.
			//for BED-INIT only the second column (BEDDEP) is used, but the additional columns also need to be written.
			if (tableName.equals("HYDR-INIT") || tableName.equals("BED-INIT")) {
				String additionalColumns = reachNumberAdditionalInfoMap.get(reachNumber);
				if (additionalColumns == null) throw new IllegalStateException("additionalColumns for reach number " + reachNumber + " not initialized during reading of uci state file.");
				valuesRow += additionalColumns;
			}
			outputLines.add(valuesRow);
		}

		outputLines.add("  END " + tableName);
	}

	private static List<String> readParameterIds(String tableName, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;

		//for HYDR-INIT only read second column (VOL), because for the other columns it is not clear which parameters should be used for the exchange items.
		//for BED-INIT only read second column (BEDDEP), because otherwise parameters SAND, SILT and CLAY would not be unique (could be initial bed sediment composition fractions or initial suspended sediment concentrations).
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
		return parameterIds;
	}

	private static List<Double> readValues(String tableName, String[] columns) {
		//skip first column.
		int firstValueColumnIndex = 1;

		//for HYDR-INIT only read second column (VOL), because for the other columns it is not clear which parameters should be used for the exchange items.
		//for BED-INIT only read second column (BEDDEP), because otherwise parameters SAND, SILT and CLAY would not be unique (could be initial bed sediment composition fractions or initial suspended sediment concentrations).
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
				throw new IllegalArgumentException("Cannot parse double '" + valueString + "' in " + UciUtils.RCHRES_MODULE_NAME + " init table '" + tableName + "' in uci state file.", e);
			}
		}
		return values;
	}
}
