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
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.io.AsciiFileUtils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

/**
 * DataObject to read/write state variables from/to a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class UciStateDataObject implements IDataObject {

	private File uciFile = null;
	private Map<String, IExchangeItem> exchangeItems = new HashMap<>();

	/**
	 * @param workingDir the working directory.
	 * @param arguments the first argument should be the name of the file containing the data for this DataObject (relative to the working directory).
	 */
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException("No fileName argument specified for " + getClass().getSimpleName()
					+ ". The first argument should be the name of the file containing the data for this DataObject (relative to the working directory).");
		}
		uciFile = new File(workingDir, arguments[0]);

		readUciFile();
	}

	public String[] getExchangeItemIDs() {
		Set<String> ids = exchangeItems.keySet();
		return ids.toArray(new String[ids.size()]);
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		//all exchange items have role InOut, so always return all items.
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	public void finish() {
		writeUciFile();
	}

	/**
	 * Read uci state file and create state exchangeItems for all state variables.
	 * Also read and store the values of all state variables into memory.
	 */
	private void readUciFile() {
		if (!uciFile.exists()) throw new RuntimeException(getClass().getSimpleName() + ": Cannot find uci state file " + uciFile.getAbsolutePath());

		//create exchangeItems.
		exchangeItems.clear();

		//read file.
		Results.putMessage(getClass().getSimpleName() + ": reading uci state file " + uciFile.getAbsolutePath());
		try {
			BufferedReader input = new BufferedReader(new FileReader(uciFile));

			try {
				String line = input.readLine();
				while (line != null) {
					String tableType = line.trim().toUpperCase();
					if (isRchresTableWithInitialConditions(tableType)) {
						readRchresInitTable(tableType, input);
					}

					line = input.readLine();
				}
			} finally {
				input.close();
			}
		} catch (IOException e){
			throw new RuntimeException("Problem while reading file " + uciFile.getAbsolutePath() + " Message was " + e.getMessage(), e);
		}
	}

	/**
	 * Write the values of all state exchangeItems to the uci file.
	 */
	private void writeUciFile() {
		if (!uciFile.exists()) throw new RuntimeException(getClass().getSimpleName() + ": Cannot find uci state file " + uciFile.getAbsolutePath());
		Results.putMessage(getClass().getSimpleName() + ": replacing values in uci state file " + uciFile.getAbsolutePath());

		//read file.
		List<String> lines = AsciiFileUtils.readLines(uciFile);

		//TODO implement

		//write file.
		AsciiFileUtils.writeLines(uciFile, lines);
	}

	/**
	 * The following parameters with initial conditions in the UCI file are supported in UciStateDataObject:
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
	private static boolean isRchresTableWithInitialConditions(String tableType) {
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

	private void readRchresInitTable(String tableType, BufferedReader input) throws IOException {
		List<String> parameterIds = new ArrayList<>();

		String line = input.readLine();
		while (line != null && !line.trim().toUpperCase().startsWith("END")) {
			//this code assumes that all relevant columns are exactly 10 characters wide in the .uci file, as defined in the HSPF Manual.
			String[] columns = UciUtils.splitEvery10Characters(line);
			if (columns == null || columns.length < 2) {//if empty row.
				//skip row.
				line = input.readLine();
				continue;
			}
			String firstColumn = columns[0];

			//in example .uci files "RCHRES" is sometimes incorrectly spelled as "RC HRES".
			if (firstColumn.toUpperCase().contains("RCHRES") || firstColumn.toUpperCase().contains("RC HRES")) {//if column header row.
				//update parameterIds for reading the next value row.
				parameterIds = readParameterIds(tableType, columns);
				line = input.readLine();
				continue;
			}

			//in example .uci files "# -  #" is incorrectly spelled as "x -  x" or "x  - x".
			if (firstColumn.contains("#") || firstColumn.toUpperCase().contains("X")) {//if units row.
				//skip row.
				line = input.readLine();
				continue;
			}

			//if contains at least one digit.
			if (firstColumn.matches(".*\\d.*")) {//if value row.
				int firstReachNumber = readFirstReachNumber(tableType, firstColumn);
				int lastReachNumber = readLastReachNumber(tableType, firstColumn, firstReachNumber);
				List<Double> values = readValues(tableType, columns);
				createExchangeItems(tableType, firstReachNumber, lastReachNumber, parameterIds, values);
				line = input.readLine();
				continue;
			}

			//do nothing, skip row.
			line = input.readLine();
		}
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
			throw new IllegalArgumentException("Cannot parse integer '" + part1 + "' in table " + tableType + " in uci state file.", e);
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
				throw new IllegalArgumentException("Cannot parse integer '" + part2 + "' in table " + tableType + " in uci state file.", e);
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
				throw new IllegalArgumentException("Cannot parse double '" + valueString + "' in table " + tableType + " in uci state file.", e);
			}
		}
		return values;
	}

	private void createExchangeItems(String tableType, int firstReachNumber, int lastReachNumber, List<String> parameterIds, List<Double> values) {
		if (values.size() != parameterIds.size()) {
			throw new IllegalArgumentException("Number of values (" + values.size() + ") not equal to number of parameters (" + parameterIds.size() + ") in table " + tableType + " in uci state file.");
		}

		for (int reachNumber = firstReachNumber; reachNumber <= lastReachNumber; reachNumber++) {
			String locationId = String.valueOf(reachNumber);

			for (int n = 0; n < values.size(); n++) {
				String parameterId = parameterIds.get(n);
				String id = locationId + "." + parameterId;

				IExchangeItem newItem = new DoubleExchangeItem(id, IPrevExchangeItem.Role.InOut, values.get(n));
				IExchangeItem previousItem = exchangeItems.put(id, newItem);
				if (previousItem != null) {
					throw new IllegalArgumentException("Multiple exchange items with id '" + id + "' found in uci state file.");
				}
			}
		}
	}
}
