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
	private final String moduleName = UciUtils.RCHRES_MODULE_NAME;
	private final String locationIdPrefix = UciUtils.RCHRES_LOCATION_ID_PREFIX;

	private final String tableName;
	/**
	 * Store the first two header rows. These are re-used during writing.
	 */
	private final String firstHeaderRow;
	private final String secondHeaderRow;
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
	 * HYDR-INIT:     VOL
	 * HEAT-INIT:     TW, AIRTMP
	 * SSED-INIT:     Sand, Silt, Clay
	 * BED-INIT:      BEDDEP
	 * OX-INIT:       DOX, BOD, SATDO
	 * NUT-DINIT:     NO3, TAM, NO2, PO4, PHVAL
	 * NUT-ADSINIT:   NH4-sand, NH4-silt, NH4-clay, PO4-sand, PO4-silt, PO4-clay
	 * PLNK-INIT:     PHYTO, ZOO, BENAL, ORN, ORP, ORC
	 * BENAL-INIT:    BENAL1, BENAL2, BENAL3, BENAL4
	 * PH-INIT:       TIC, CO2, PH
	 *
	 * For RCHRES the following tables are not supported:
	 * HYDR-CINIT is currently not supported, because it has a completely different table format.
	 */
	public static boolean isReachesInitTable(String tableName) {
		return tableName.equals("HYDR-INIT")
				|| tableName.equals("HEAT-INIT")
				|| tableName.equals("SSED-INIT")
				|| tableName.equals("BED-INIT")
				|| tableName.equals("OX-INIT")
				|| tableName.equals("NUT-DINIT")
				|| tableName.equals("NUT-ADSINIT")
				|| tableName.equals("PLNK-INIT")
				|| tableName.equals("BENAL-INIT")
				|| tableName.equals("PH-INIT");
	}

	private static boolean isFirstHeaderRow(String firstColumn) {
		//in example .uci files "RCHRES" is sometimes incorrectly written as "RC HRES".
		return firstColumn.toUpperCase().contains("RCHRES") || firstColumn.toUpperCase().contains("RC HRES");
	}

	/**
	 * Read one RCHRES init table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public ReachesInitTable(String tableName, Iterator<String> inputLines, double stateTime, Map<String, IExchangeItem> exchangeItems) {
		if (tableName == null) throw new IllegalArgumentException("tableName == null");
		this.tableName = tableName;

		//read uci file.
		String firstHeaderRow = null;
		String secondHeaderRow = null;
		List<String> parameterIds = null;
		Set<Integer> uniqueReachNumbers = new HashSet<>();
		while (inputLines.hasNext()) {
			String inputLine = inputLines.next();
			if (inputLine.trim().toUpperCase().startsWith("END")) break;

			//this code assumes that all of the used columns are exactly 10 characters wide in the table, as defined in the HSPF Manual.
			String[] columns = UciUtils.splitAfter10ThenEveryNCharacters(inputLine, 10);
			if (columns == null || columns.length < 2) {//if empty row.
				//skip row.
				continue;
			}
			String firstColumn = columns[0];

			if (isFirstHeaderRow(firstColumn)) {
				if (firstHeaderRow == null) firstHeaderRow = inputLine;
				if (UciUtils.firstHeaderRowContainsParameterIds(tableName)) {
					//update parameterIds for reading the next value row.
					parameterIds = UciUtils.readParameterIds(moduleName, tableName, columns);
				}
				continue;
			}

			if (UciUtils.isSecondHeaderRow(firstColumn)) {
				if (secondHeaderRow == null) secondHeaderRow = inputLine;
				if (UciUtils.secondHeaderRowContainsParameterIds(tableName)) {
					//update parameterIds for reading the next value row.
					parameterIds = UciUtils.readParameterIds(moduleName, tableName, columns);
				}
				continue;
			}

			if (UciUtils.isValuesRow(firstColumn)) {
				UciUtils.readValuesRow(moduleName, tableName, columns, inputLine, locationIdPrefix, parameterIds, stateTime, uniqueReachNumbers, exchangeItems, reachNumberAdditionalInfoMap);
				continue;
			}

			//do nothing, skip row.
		}

		if (firstHeaderRow == null) throw new RuntimeException("No valid first header row found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (secondHeaderRow == null) throw new RuntimeException("No valid second header row found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (uniqueReachNumbers.isEmpty()) throw new RuntimeException("No valid reach numbers found in " + moduleName + " init table '" + tableName + "' in uci state file.");

		this.firstHeaderRow = firstHeaderRow;
		this.secondHeaderRow = secondHeaderRow;
		this.parameterIds = parameterIds;
		this.reachNumbers = new ArrayList<>(uniqueReachNumbers);
		Collections.sort(this.reachNumbers);
	}

	public void write(Map<String, IExchangeItem> exchangeItems, List<String> outputLines, Map<String, IExchangeItem> exchangeItemsDefaultInit) {
		UciUtils.writeTable(exchangeItems, outputLines, tableName, firstHeaderRow, secondHeaderRow, locationIdPrefix, reachNumbers, parameterIds, reachNumberAdditionalInfoMap, exchangeItemsDefaultInit);
	}
}
