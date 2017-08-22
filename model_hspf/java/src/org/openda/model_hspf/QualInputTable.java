/* OpenDA v2.4.1 
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
 * Class to read/write a PERLND or IMPLND QUAL-INPUT table with initial conditions from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class QualInputTable {
	public static final String TABLE_NAME = "QUAL-INPUT";

	private final String moduleName;
	private final String locationIdPrefix;
	/**
	 * Store the first two header rows. These are re-used during writing.
	 */
	private final String firstHeaderRow;
	private final String secondHeaderRow;
	/**
	 * Parameters and segments used in this table.
	 */
	private final List<String> augmentedParameterIds;
	private final List<Integer> segmentNumbers;

	public static boolean isQualInputTable(String tableName) {
		return TABLE_NAME.equals(tableName);
	}

	private static boolean isFirstHeaderRow(String firstColumn) {
		return firstColumn.toUpperCase().contains("PLS") || firstColumn.toUpperCase().contains("ILS");
	}

	/**
	 * Read one QUAL-INPUT table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public QualInputTable(String moduleName, int qualInputTableNumber, Iterator<String> inputLines, double stateTime, Map<String, IExchangeItem> exchangeItems) {
		if (moduleName == null) throw new IllegalArgumentException("moduleName == null");
		this.moduleName = moduleName;
		this.locationIdPrefix = UciUtils.getLocationIdPrefix(moduleName);

		//read uci file.
		String firstHeaderRow = null;
		String secondHeaderRow = null;
		List<String> augmentedParameterIds = null;
		Set<Integer> uniqueSegmentNumbers = new HashSet<>();
		while (inputLines.hasNext()) {
			String inputLine = inputLines.next();
			if (inputLine.trim().toUpperCase().startsWith("END")) break;

			//this code assumes that all but the first column are exactly 8 characters wide in the table, as defined in the HSPF Manual.
			String[] columns = UciUtils.splitAfter10ThenEveryNCharacters(inputLine, 8);
			if (columns == null || columns.length < 2) {//if empty row.
				//skip row.
				continue;
			}
			String firstColumn = columns[0];

			if (isFirstHeaderRow(firstColumn)) {
				if (firstHeaderRow == null) firstHeaderRow = inputLine;
				if (UciUtils.firstHeaderRowContainsParameterIds(TABLE_NAME)) {
					//update parameterIds for reading the next value row.
					augmentedParameterIds = augmentParameterIds(UciUtils.readParameterIds(moduleName, TABLE_NAME, columns), qualInputTableNumber);
				}
				continue;
			}

			if (UciUtils.isSecondHeaderRow(firstColumn)) {
				if (secondHeaderRow == null) secondHeaderRow = inputLine;
				if (UciUtils.secondHeaderRowContainsParameterIds(TABLE_NAME)) {
					//update parameterIds for reading the next value row.
					augmentedParameterIds = augmentParameterIds(UciUtils.readParameterIds(moduleName, TABLE_NAME, columns), qualInputTableNumber);
				}
				continue;
			}

			if (UciUtils.isValuesRow(firstColumn)) {
				UciUtils.readValuesRow(moduleName, TABLE_NAME, columns, inputLine, locationIdPrefix, augmentedParameterIds, stateTime, uniqueSegmentNumbers, exchangeItems, null);
				continue;
			}

			//do nothing, skip row.
		}

		if (firstHeaderRow == null) throw new RuntimeException("No valid first header row found in " + moduleName + " init table '" + TABLE_NAME + "' in uci state file.");
		if (secondHeaderRow == null) throw new RuntimeException("No valid second header row found in " + moduleName + " init table '" + TABLE_NAME + "' in uci state file.");
		if (augmentedParameterIds == null) throw new RuntimeException("No valid parameter ids found in " + moduleName + " init table '" + TABLE_NAME + "' in uci state file.");
		if (uniqueSegmentNumbers.isEmpty()) throw new RuntimeException("No valid segment numbers found in " + moduleName + " init table '" + TABLE_NAME + "' in uci state file.");

		this.firstHeaderRow = firstHeaderRow;
		this.secondHeaderRow = secondHeaderRow;
		this.augmentedParameterIds = augmentedParameterIds;
		this.segmentNumbers = new ArrayList<>(uniqueSegmentNumbers);
		Collections.sort(this.segmentNumbers);
	}

	private static List<String> augmentParameterIds(List<String> parameterIds, int qualInputTableNumber) {
		List<String> augmentedParameterIds = new ArrayList<>();
		for (String parameterId : parameterIds) {
			String augmentedParameterId = parameterId + qualInputTableNumber;
			augmentedParameterIds.add(augmentedParameterId);
		}
		return augmentedParameterIds;
	}

	public void write(Map<String, IExchangeItem> exchangeItems, List<String> outputLines, Map<String, IExchangeItem> exchangeItemsDefaultInit) {
		UciUtils.writeTable(exchangeItems, outputLines, TABLE_NAME, firstHeaderRow, secondHeaderRow, locationIdPrefix, segmentNumbers, augmentedParameterIds, null, exchangeItemsDefaultInit);
	}
}
