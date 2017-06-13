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

import org.openda.interfaces.IExchangeItem;

import java.util.*;

/**
 * Class to read/write a PERLND table with initial conditions from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class PerviousLandSegmentsInitTable {
	private final String moduleName = UciUtils.PERLND_MODULE_NAME;
	private final String locationIdPrefix = UciUtils.PERLND_LOCATION_ID_PREFIX;

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
	 * For PERLND the following tables and parameters with initial conditions in the UCI file are supported:
	 *
	 * <table type>:  <parameters>
	 * SNOW-INIT1:    Pack-snow, Pack-ice, Pack-watr, RDENPF, DULL, PAKTMP
	 * SNOW-INIT2:    COVINX, XLNMLT, SKYCLR
	 * PWAT-STATE1:   CEPS, SURS, UZS, IFWS, LZS, AGWS, GWVS
	 * SED-STOR:      DETS
	 * PSTEMP-TEMPS:  AIRTC, SLTMP, ULTMP, LGTMP
	 * PWT-TEMPS:     SOTMP, IOTMP, AOTMP
	 * PWT-GASES:     SODOX, SOCO2, IODOX, IOCO2, AODOX, AOCO2
	 * QUAL-INPUT:    SQO, POTFW, POTFS, ACQOP, SQOLIM, WSQOP, IOQC, AOQC
	 * MST-TOPSTOR:   SMSTM, UMSTM, IMSTM
	 * MST-TOPFLX:    FSO, FSP, FII, FUP, FIO
	 * MST-SUBSTOR:   LMSTM, AMSTM
	 * MST-SUBFLX:    FLP, FLDP, FAO
	 * PEST-STOR1:    PSCY, PSAD, PSSU (only for one layer)
	 * PEST-STOR2:    IPS
	 * NIT-STOR1:     LORGN, AMAD, AMSU, NO3, PLTN, RORGN (only for one layer)
	 * NIT-STOR2:     IAMSU, INO3, ISLON, ISRON, AGPLTN, LITTRN
	 * PHOS-STOR1:    ORGP, P4AD, P4SU, PLTP (only for one layer)
	 * PHOS-STOR2:    IP4SU
	 * TRAC-TOPSTOR:  STRSU, UTRSU, ITRSU
	 * TRAC-SUBSTOR:  LTRSU, ATRSU
	 *
	 * For PERLND the following tables are not supported:
	 * UZSN-LZSN is currently not supported, because otherwise parameter SURS would not be unique (this could be either
	 * initial surface (overland flow) storage from PWAT-STATE1 or initial surface detention storage from UZSN-LZSN).
	 */
	//for QUAL-INPUT table see class QualInputTable.
	public static boolean isPerviousLandSegmentsInitTable(String tableName) {
		return tableName.equals("SNOW-INIT1")
				|| tableName.equals("SNOW-INIT2")
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
				|| tableName.equals("TRAC-SUBSTOR");
	}

	private static boolean isFirstHeaderRow(String firstColumn) {
		return firstColumn.toUpperCase().contains("PLS");
	}

	/**
	 * Read one PERLND init table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public PerviousLandSegmentsInitTable(String tableName, Iterator<String> inputLines, double stateTime, Map<String, IExchangeItem> exchangeItems) {
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
