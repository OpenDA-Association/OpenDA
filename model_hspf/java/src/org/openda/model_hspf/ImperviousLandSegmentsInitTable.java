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
 * Class to read/write an IMPLND table with initial conditions from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class ImperviousLandSegmentsInitTable {
	private final String moduleName = UciUtils.IMPLND_MODULE_NAME;
	private final String locationIdPrefix = UciUtils.IMPLND_LOCATION_ID_PREFIX;

	private final String tableName;
	/**
	 * Store the first two header rows. These are re-used during writing.
	 */
	private final String firstHeaderRow;
	private final String secondHeaderRow;
	/**
	 * Parameters and segments used in this table.
	 */
	private final List<String> parameterIds;
	private final List<Integer> segmentNumbers;

	/**
	 * For IMPLND the following tables and parameters with initial conditions in the UCI file are supported:
	 *
	 * <table type>:  <parameters>
	 * SNOW-INIT1:    Pack-snow, Pack-ice, Pack-watr, RDENPF, DULL, PAKTMP
	 * SNOW-INIT2:    COVINX, XLNMLT, SKYCLR
	 * IWAT-STATE1:   RETS, SURS
	 * SLD-STOR:      SLDS
	 * IWT-INIT:      SOTMP, SODOX, SOCO2
	 * QUAL-INPUT:    SQO, POTFW, ACQOP, SQOLIM, WSQOP
	 */
	//for QUAL-INPUT table see class QualInputTable.
	public static boolean isImperviousLandSegmentsInitTable(String tableName) {
		return tableName.equals("SNOW-INIT1")
				|| tableName.equals("SNOW-INIT2")
				|| tableName.equals("IWAT-STATE1")
				|| tableName.equals("SLD-STOR")
				|| tableName.equals("IWT-INIT");
	}

	private static boolean isFirstHeaderRow(String firstColumn) {
		return firstColumn.toUpperCase().contains("ILS");
	}

	/**
	 * Read one IMPLND init table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public ImperviousLandSegmentsInitTable(String tableName, Iterator<String> inputLines, double stateTime, Map<String, IExchangeItem> exchangeItems) {
		if (tableName == null) throw new IllegalArgumentException("tableName == null");
		this.tableName = tableName;

		//read uci file.
		String firstHeaderRow = null;
		String secondHeaderRow = null;
		List<String> parameterIds = null;
		Set<Integer> uniqueSegmentNumbers = new HashSet<>();
		while (inputLines.hasNext()) {
			String inputLine = inputLines.next();
			if (inputLine.trim().toUpperCase().startsWith("END")) break;

			//this code assumes that all columns are exactly 10 characters wide in the table, as defined in the HSPF Manual.
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
				UciUtils.readValuesRow(moduleName, tableName, columns, inputLine, locationIdPrefix, parameterIds, stateTime, uniqueSegmentNumbers, exchangeItems, null);
				continue;
			}

			//do nothing, skip row.
		}

		if (firstHeaderRow == null) throw new RuntimeException("No valid first header row found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (secondHeaderRow == null) throw new RuntimeException("No valid second header row found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in " + moduleName + " init table '" + tableName + "' in uci state file.");
		if (uniqueSegmentNumbers.isEmpty()) throw new RuntimeException("No valid segment numbers found in " + moduleName + " init table '" + tableName + "' in uci state file.");

		this.firstHeaderRow = firstHeaderRow;
		this.secondHeaderRow = secondHeaderRow;
		this.parameterIds = parameterIds;
		this.segmentNumbers = new ArrayList<>(uniqueSegmentNumbers);
		Collections.sort(this.segmentNumbers);
	}

	public void write(Map<String, IExchangeItem> exchangeItems, List<String> outputLines, Map<String, IExchangeItem> exchangeItemsDefaultInit) {
		UciUtils.writeTable(exchangeItems, outputLines, tableName, firstHeaderRow, secondHeaderRow, locationIdPrefix, segmentNumbers, parameterIds, null, exchangeItemsDefaultInit);
	}
}
