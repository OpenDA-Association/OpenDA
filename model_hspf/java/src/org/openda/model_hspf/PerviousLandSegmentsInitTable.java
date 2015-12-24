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
 * Class to read/write a PERLND table with initial conditions from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class PerviousLandSegmentsInitTable {
	//TODO move to UciUtils. AK
	public static final String PERLND_MODULE_NAME = "PERLND";
	private static final String PERLND_LOCATION_ID_PREFIX = "P";

	private final String tableType;
	/**
	 * Store the first parameterIds row and units row. These are re-used during writing.
	 */
	private final String parameterIdsRow;
	private final String unitsRow;
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
	 * MST-TOPSTOR:   SMSTM, UMSTM, IMSTM
	 * MST-TOPFLX:    FSO, FSP, FII, FUP, FIO
	 * MST-SUBSTOR:   LMSTM, AMSTM
	 * MST-SUBFLX:    FLP, FLDP, FAO
	 * NIT-STOR1:     LORGN, AMAD, AMSU, NO3, PLTN, RORGN
	 * NIT-STOR2:     IAMSU, INO3, ISLON, ISRON, AGPLTN, LITTRN
	 * PHOS-STOR1:    ORGP, P4AD, P4SU, PLTP
	 * PHOS-STOR2:    IP4SU
	 * TRAC-TOPSTOR:  STRSU, UTRSU, ITRSU
	 * TRAC-SUBSTOR:  LTRSU, ATRSU
	 *
	 * For PERLND the following tables are not supported:
	 * PEST-STOR1 is currently not supported, because it is not clear from the HSPF manual which parameters should be used for the exchange items.
	 * PEST-STOR2 is currently not supported, because it is not clear from the HSPF manual which parameters should be used for the exchange items.
	 * UZSN-LZSN is currently not supported, because it has a different format for column headers and a different column width.
	 */
	public static boolean isPerviousLandSegmentsInitTable(String tableType) {
		return tableType.equals("SNOW-INIT1")
				|| tableType.equals("SNOW-INIT2")
				|| tableType.equals("PWAT-STATE1")
				|| tableType.equals("SED-STOR")
				|| tableType.equals("PSTEMP-TEMPS")
				|| tableType.equals("PWT-TEMPS")
				|| tableType.equals("PWT-GASES")
				|| tableType.equals("MST-TOPSTOR")
				|| tableType.equals("MST-TOPFLX")
				|| tableType.equals("MST-SUBSTOR")
				|| tableType.equals("MST-SUBFLX")
				|| tableType.equals("NIT-STOR1")
				|| tableType.equals("NIT-STOR2")
				|| tableType.equals("PHOS-STOR1")
				|| tableType.equals("PHOS-STOR2")
				|| tableType.equals("TRAC-TOPSTOR")
				|| tableType.equals("TRAC-SUBSTOR");
	}

	/**
	 * Read one PERLND init table from uci state file and create state exchangeItems for all state variables in that table.
	 * Also read and store the values of these state variables into memory.
	 */
	public PerviousLandSegmentsInitTable(String tableType, Iterator<String> inputLines, double stateTime, Map<String, IExchangeItem> exchangeItems) {
		if (tableType == null) throw new IllegalArgumentException("tableType == null");
		this.tableType = tableType;

		//read uci file.
		String parameterIdsRow = null;
		String unitsRow = null;
		List<String> parameterIds = null;
		Set<Integer> uniqueSegmentNumbers = new HashSet<>();
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

			//in example .uci files "# -  #" is incorrectly written as "x -  x" or "x  - x".
			if (firstColumn.contains("#") || firstColumn.toUpperCase().contains("X")) {//if parameterIds row.
				if (parameterIdsRow == null) parameterIdsRow = inputLine;
				//update parameterIds for reading the next value row.
				parameterIds = UciUtils.readParameterIds(columns);
				UciUtils.validateParameterIds(parameterIds, PERLND_MODULE_NAME, tableType);
				continue;
			}

			if (firstColumn.toUpperCase().contains("PLS")) {//if units row.
				if (unitsRow == null) unitsRow = inputLine;
				continue;
			}

			//if contains at least one digit.
			if (firstColumn.matches(".*\\d.*")) {//if values row.
				int firstSegmentNumber = UciUtils.readFirstLocationNumber(PERLND_MODULE_NAME, tableType, firstColumn);
				int lastSegmentNumber = UciUtils.readLastLocationNumber(PERLND_MODULE_NAME, tableType, firstColumn, firstSegmentNumber);
				List<Double> values = UciUtils.readValues(PERLND_MODULE_NAME, tableType, columns);
				UciUtils.createExchangeItems(PERLND_MODULE_NAME, tableType, PERLND_LOCATION_ID_PREFIX, firstSegmentNumber, lastSegmentNumber, parameterIds, values, stateTime, uniqueSegmentNumbers, exchangeItems);
				continue;
			}

			//do nothing, skip row.
		}

		if (parameterIdsRow == null) throw new RuntimeException("No valid parameter ids row found in " + PERLND_MODULE_NAME + " init table '" + tableType + "' in uci state file.");
		if (unitsRow == null) throw new RuntimeException("No valid units row found in " + PERLND_MODULE_NAME + " init table '" + tableType + "' in uci state file.");
		if (parameterIds == null) throw new RuntimeException("No valid parameter ids found in " + PERLND_MODULE_NAME + " init table '" + tableType + "' in uci state file.");
		if (uniqueSegmentNumbers.isEmpty()) throw new RuntimeException("No valid segment numbers found in " + PERLND_MODULE_NAME + " init table '" + tableType + "' in uci state file.");

		this.parameterIdsRow = parameterIdsRow;
		this.unitsRow = unitsRow;
		this.parameterIds = parameterIds;
		this.segmentNumbers = new ArrayList<>(uniqueSegmentNumbers);
		Collections.sort(this.segmentNumbers);
	}

	public void write(Map<String, IExchangeItem> exchangeItems, List<String> outputLines) {
		outputLines.add("  " + tableType);

		//for each segment write one block consisting of a units row, a parameterIds row and a values row.
		for (int segmentNumber : segmentNumbers) {
			outputLines.add(unitsRow);
			outputLines.add(parameterIdsRow);
			outputLines.add(UciUtils.writeValuesRow(PERLND_LOCATION_ID_PREFIX, segmentNumber, parameterIds, exchangeItems));
		}

		outputLines.add("  END " + tableType);
	}
}
