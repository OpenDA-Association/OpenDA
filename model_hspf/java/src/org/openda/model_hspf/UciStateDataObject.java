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

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.AsciiFileUtils;

import java.io.*;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
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
	private Map<String, ReachesInitTable> reachesInitTables = new HashMap<>();
	private Map<String, PerviousLandSegmentsInitTable> perviousLandSegmentsInitTables = new HashMap<>();
	private Map<String, ImperviousLandSegmentsInitTable> imperviousLandSegmentsInitTables = new HashMap<>();
	private String lastKnownQualId = null;

	/**
	 * This is the state time shift in seconds (can be negative). The timestamp for the state in the .uci file will be equal to (absolute state time) + (state time shift).
	 * This can be used e.g. when the absolute state time is the end time of the model run, but the output state of the model is only available for the last output time step before the end time of the model run.
	 * For HSPF model this is needed, because the data for the last timeStep of the model is written at (endTime - 1 timeStep) in the model output wdm files.
	 *
	 * This is 0 by default.
	 */
	private double stateTimeShiftInSeconds = 0;

	/**
	 * @param workingDir the working directory.
	 * @param arguments File pathname:
	 *                  The pathname of the .uci state file containing the data for this DataObject (relative to the working directory).
	 *                  Other arguments:
	 *                  The second argument should be the absolute state time (can be negative).
	 *                  The (optional) third argument should be the state time shift in seconds. The timestamp for the state in the .uci file will be equal to (absolute state time) + (state time shift).
	 */
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException("No fileName argument specified for " + getClass().getSimpleName()
					+ ". The first argument should be the name of the .uci file containing the data for this DataObject (relative to the working directory).");
		}
		uciFile = new File(workingDir, arguments[0]);

		//get absolute state time.
		if (arguments.length < 2) {
			throw new IllegalArgumentException("No absolute state time argument specified for " + getClass().getSimpleName() + ". The second argument should be the absolute state time.");
		}
		double absoluteStateTime;
		try {
			absoluteStateTime = TimeUtils.date2Mjd(arguments[1]);
		} catch (ParseException e) {
			throw new IllegalArgumentException("Invalid absolute state time argument specified for " + getClass().getSimpleName() + ". Cannot parse second argument '" + arguments[1]
					+ "'. The second argument should be the absolute state time.", e);
		}

		//get optional state time shift.
		if (arguments.length > 2) {
			try {
				stateTimeShiftInSeconds = Double.parseDouble(arguments[2]);
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse third argument '" + arguments[2] + "' for " + getClass().getSimpleName()
						+ ". The (optional) third argument should be the state time shift in seconds.", e);
			}
		}

		double stateTime = absoluteStateTime + stateTimeShiftInSeconds / 86400.0;

		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
		Results.putMessage(getClass().getSimpleName() + ": using state time of " + dateFormat.format(Time.mjdToMillies(stateTime)));
		readUciFile(stateTime);
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
	private void readUciFile(double stateTime) {
		if (!uciFile.exists()) throw new RuntimeException(getClass().getSimpleName() + ": Cannot find uci state file " + uciFile.getAbsolutePath());

		//create exchangeItems.
		exchangeItems.clear();
		reachesInitTables.clear();
		perviousLandSegmentsInitTables.clear();
		imperviousLandSegmentsInitTables.clear();

		//read file.
		Results.putMessage(getClass().getSimpleName() + ": reading uci state file " + uciFile.getAbsolutePath());
		List<String> inputLines = AsciiFileUtils.readLines(uciFile);
		Iterator<String> inputIterator = inputLines.iterator();
		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();

			if (trimmedUppercaseLine.equals(ReachesInitTable.RCHRES_MODULE_NAME)) {
				readReachesSection(inputIterator, stateTime);
				continue;
			}

			if (trimmedUppercaseLine.equals(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME)) {
				readPerviousLandSection(inputIterator, stateTime);
				continue;
			}

			if (trimmedUppercaseLine.equals(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME)) {
				readImperviousLandSection(inputIterator, stateTime);
				continue;
			}
		}
	}

	private void readReachesSection(Iterator<String> inputIterator, double stateTime) {
		Results.putMessage(getClass().getSimpleName() + ": reading " + ReachesInitTable.RCHRES_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(ReachesInitTable.RCHRES_MODULE_NAME)) break;

			if (ReachesInitTable.isReachesInitTable(trimmedUppercaseLine)) {
				String tableName = trimmedUppercaseLine;
				ReachesInitTable initTable = new ReachesInitTable(tableName, inputIterator, stateTime, exchangeItems);
				ReachesInitTable previous = reachesInitTables.put(tableName, initTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + ReachesInitTable.RCHRES_MODULE_NAME + " init tables '" + tableName + "' found in uci state file.");
			}
		}
	}

	private void readPerviousLandSection(Iterator<String> inputIterator, double stateTime) {
		Results.putMessage(getClass().getSimpleName() + ": reading " + PerviousLandSegmentsInitTable.PERLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME)) break;

			String tableName = trimmedUppercaseLine;
			if (PerviousLandSegmentsInitTable.isPerviousLandSegmentsInitTable(tableName)) {
				PerviousLandSegmentsInitTable initTable = new PerviousLandSegmentsInitTable(tableName, inputIterator, stateTime, exchangeItems);
				PerviousLandSegmentsInitTable previous = perviousLandSegmentsInitTables.put(tableName, initTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + PerviousLandSegmentsInitTable.PERLND_MODULE_NAME + " init tables '" + tableName + "' found in uci state file.");
				continue;
			}

			if (QualPropsTable.isQualPropsTable(tableName)) {
				//replace last known qualId with the one read from current QUAL-PROPS table.
				lastKnownQualId = QualPropsTable.readQualId(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME, inputIterator, null);
				continue;
			}

			//TODO
//			if (QualInputTable.isQualInputTable(tableName)) {
//				readQualInputTable(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME, inputIterator, stateTime);
//				continue;
//			}
		}
	}

	private void readImperviousLandSection(Iterator<String> inputIterator, double stateTime) {
		Results.putMessage(getClass().getSimpleName() + ": reading " + ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME)) break;

			String tableName = trimmedUppercaseLine;
			if (ImperviousLandSegmentsInitTable.isImperviousLandSegmentsInitTable(tableName)) {
				ImperviousLandSegmentsInitTable initTable = new ImperviousLandSegmentsInitTable(tableName, inputIterator, stateTime, exchangeItems);
				ImperviousLandSegmentsInitTable previous = imperviousLandSegmentsInitTables.put(tableName, initTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME + " init tables '" + tableName + "' found in uci state file.");
			}

			if (QualPropsTable.isQualPropsTable(tableName)) {
				//replace last known qualId with the one read from current QUAL-PROPS table.
				lastKnownQualId = QualPropsTable.readQualId(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME, inputIterator, null);
				continue;
			}

			//TODO
//			if (QualInputTable.isQualInputTable(tableName)) {
//				readQualInputTable(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME, inputIterator, stateTime);
//				continue;
//			}
		}
	}

	/**
	 * Write the values of all state exchangeItems to the uci file.
	 * Read file, then replace values of all exchange items in the file, then write file again.
	 * This way the parts of the file for which there are no exchange items will remain the same.
	 */
	private void writeUciFile() {
		if (!uciFile.exists()) throw new RuntimeException(getClass().getSimpleName() + ": Cannot find uci state file " + uciFile.getAbsolutePath());

		//read file.
		Results.putMessage(getClass().getSimpleName() + ": reading uci state file " + uciFile.getAbsolutePath());
		List<String> inputLines = AsciiFileUtils.readLines(uciFile);

		//replace values in file.
		Results.putMessage(getClass().getSimpleName() + ": replacing values in uci state file " + uciFile.getAbsolutePath());
		Iterator<String> inputIterator = inputLines.iterator();
		List<String> outputLines = new ArrayList<>();
		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();

			if (trimmedUppercaseLine.equals(ReachesInitTable.RCHRES_MODULE_NAME)) {
				outputLines.add(inputLine);
				writeReachesSection(inputIterator, outputLines);
				continue;
			}

			if (trimmedUppercaseLine.equals(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				writePerviousLandSection(inputIterator, outputLines);
				continue;
			}

			if (trimmedUppercaseLine.equals(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				writeImperviousLandSection(inputIterator, outputLines);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}

		//write file.
		AsciiFileUtils.writeLines(uciFile, outputLines);
	}

	private void writeReachesSection(Iterator<String> inputIterator, List<String> outputLines) {
		Results.putMessage(getClass().getSimpleName() + ": replacing values in " + ReachesInitTable.RCHRES_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(ReachesInitTable.RCHRES_MODULE_NAME)) {
				outputLines.add(inputLine);
				break;
			}

			if (ReachesInitTable.isReachesInitTable(trimmedUppercaseLine)) {
				String tableName = trimmedUppercaseLine;

				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				ReachesInitTable initTable = reachesInitTables.get(tableName);
				if (initTable == null) throw new IllegalStateException(ReachesInitTable.RCHRES_MODULE_NAME + " init table '" + tableName + "' not initialized during reading of uci state file.");
				initTable.write(exchangeItems, outputLines);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}
	}

	private void writePerviousLandSection(Iterator<String> inputIterator, List<String> outputLines) {
		Results.putMessage(getClass().getSimpleName() + ": replacing values in " + PerviousLandSegmentsInitTable.PERLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				break;
			}

			String tableName = trimmedUppercaseLine;
			if (PerviousLandSegmentsInitTable.isPerviousLandSegmentsInitTable(tableName)) {
				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				PerviousLandSegmentsInitTable initTable = perviousLandSegmentsInitTables.get(tableName);
				if (initTable == null) throw new IllegalStateException(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME + " init table '" + tableName + "' not initialized during reading of uci state file.");
				initTable.write(exchangeItems, outputLines);
				continue;
			}

			if (QualPropsTable.isQualPropsTable(tableName)) {
				//copy inputLine to output unchanged.
				outputLines.add(inputLine);
				//replace last known qualId with the one read from current QUAL-PROPS table and copy table.
				lastKnownQualId = QualPropsTable.readQualId(PerviousLandSegmentsInitTable.PERLND_MODULE_NAME, inputIterator, outputLines);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}
	}

	private void writeImperviousLandSection(Iterator<String> inputIterator, List<String> outputLines) {
		Results.putMessage(getClass().getSimpleName() + ": replacing values in " + ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				break;
			}

			String tableName = trimmedUppercaseLine;
			if (ImperviousLandSegmentsInitTable.isImperviousLandSegmentsInitTable(tableName)) {
				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				ImperviousLandSegmentsInitTable initTable = imperviousLandSegmentsInitTables.get(tableName);
				if (initTable == null) throw new IllegalStateException(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME + " init table '" + tableName + "' not initialized during reading of uci state file.");
				initTable.write(exchangeItems, outputLines);
				continue;
			}

			if (QualPropsTable.isQualPropsTable(tableName)) {
				//copy inputLine to output unchanged.
				outputLines.add(inputLine);
				//replace last known qualId with the one read from current QUAL-PROPS table and copy table.
				lastKnownQualId = QualPropsTable.readQualId(ImperviousLandSegmentsInitTable.IMPLND_MODULE_NAME, inputIterator, outputLines);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}
	}
}
