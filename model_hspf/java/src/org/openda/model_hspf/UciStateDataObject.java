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
	private Map<String, QualInputTable> perviousLandSegmentsQualInputTables = new HashMap<>();
	private Map<String, ImperviousLandSegmentsInitTable> imperviousLandSegmentsInitTables = new HashMap<>();
	private Map<String, QualInputTable> imperviousLandSegmentsQualInputTables = new HashMap<>();
	private Map<String, IExchangeItem> exchangeItemsDefaultInit = new HashMap<>();
//	private String lastKnownQualId = null;

	/**
	 * This is the state time shift in seconds (can be negative). The timestamp for the state in the .uci file will be equal to (absolute state time) + (state time shift).
	 * This can be used e.g. when the absolute state time is the end time of the model run, but the output state of the model is only available for the last output time step before the end time of the model run.
	 * For HSPF model this is needed, because the data for the last timeStep of the model is written at (endTime - 1 timeStep) in the model output wdm files. In this case it is not possible
	 * to just change the absolute state time argument to one hour earlier, because it uses an alias that is replaced dynamically with the end time of the model run by OpenDA in an operational system.
	 *
	 * This is 0 by default.
	 */
	private double stateTimeShiftInSeconds = 0;

	/**
	 * @param workingDir the working directory.
	 * @param arguments File pathname:
	 *                  The pathname of the .uci state file containing the data for this DataObject (relative to the working directory).
	 *                  Other arguments:
	 *                  The first argument should be the absolute state time.
	 *                  The (optional) second argument should be the state time shift in seconds (can be negative). The timestamp for the state in the .uci file
	 *                  will be equal to (absolute state time) + (state time shift).
	 */
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": File pathname of the uci state file (relative to the working directory) not specified.");
		}
		uciFile = new File(workingDir, arguments[0]);

		//get absolute state time.
		if (arguments.length < 2) {
			throw new IllegalArgumentException("No absolute state time argument specified for " + getClass().getSimpleName() + ". The first argument should be the absolute state time.");
		}
		double absoluteStateTime;
		try {
			absoluteStateTime = TimeUtils.date2Mjd(arguments[1]);
		} catch (ParseException e) {
			throw new IllegalArgumentException("Invalid absolute state time argument specified for " + getClass().getSimpleName() + ". Cannot parse first argument '" + arguments[1]
					+ "'. The first argument should be the absolute state time.", e);
		}

		//get optional state time shift.
		if (arguments.length > 2) {
			try {
				stateTimeShiftInSeconds = Double.parseDouble(arguments[2]);
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse second argument '" + arguments[2] + "' for " + getClass().getSimpleName()
						+ ". The (optional) second argument should be the state time shift in seconds (can be negative).", e);
			}
		}

		//get optional default-initial uci file.
		if (arguments.length > 3) {
			try {
				File uciDefaultInitFile = new File(workingDir, arguments[3]); // for trapping error, i.e. when file is not found
				String[] newarguments = new String[]{arguments[3],arguments[1],arguments[2]};
				UciStateDataObject defaultUciDataObject = new UciStateDataObject();
				defaultUciDataObject.initialize(workingDir, newarguments);
				exchangeItemsDefaultInit = defaultUciDataObject.exchangeItems;
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Cannot parse second argument '" + arguments[3] + "' for " + getClass().getSimpleName()
						+ ". The (optional) third argument should be the default initial uci file).", e);
			}
		}

		double stateTime = absoluteStateTime + stateTimeShiftInSeconds / 86400.0;

		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
		Results.putMessage(getClass().getSimpleName() + ": using state time of " + dateFormat.format(Time.mjdToMillies(stateTime)));
		readUciFile(stateTime);

//		UciStateDataObject defaultUciDataObject = new UciStateDataObject();
//		defaultUciDataObject.readUciFile(stateTime);
//		defaultUciDataObject.getDataObjectExchangeItem("hrhrh");
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

			if (trimmedUppercaseLine.equals(UciUtils.RCHRES_MODULE_NAME)) {
				readReachesSection(inputIterator, stateTime);
				continue;
			}

			if (trimmedUppercaseLine.equals(UciUtils.PERLND_MODULE_NAME)) {
				readPerviousLandSection(inputIterator, stateTime);
				continue;
			}

			if (trimmedUppercaseLine.equals(UciUtils.IMPLND_MODULE_NAME)) {
				readImperviousLandSection(inputIterator, stateTime);
				continue;
			}
		}
	}

	private void readReachesSection(Iterator<String> inputIterator, double stateTime) {
		Results.putMessage(getClass().getSimpleName() + ": reading " + UciUtils.RCHRES_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(UciUtils.RCHRES_MODULE_NAME)) break;

			if (ReachesInitTable.isReachesInitTable(trimmedUppercaseLine)) {
				String tableName = trimmedUppercaseLine;
				ReachesInitTable initTable = new ReachesInitTable(tableName, inputIterator, stateTime, exchangeItems);
				ReachesInitTable previous = reachesInitTables.put(tableName, initTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + UciUtils.RCHRES_MODULE_NAME + " init tables '" + tableName + "' found in uci state file.");
			}
		}
	}

	private void readPerviousLandSection(Iterator<String> inputIterator, double stateTime) {
		Results.putMessage(getClass().getSimpleName() + ": reading " + UciUtils.PERLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());
		int qualInputTableCounter = 0;

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(UciUtils.PERLND_MODULE_NAME)) break;

			String tableName = trimmedUppercaseLine;
			if (PerviousLandSegmentsInitTable.isPerviousLandSegmentsInitTable(tableName)) {
				PerviousLandSegmentsInitTable initTable = new PerviousLandSegmentsInitTable(tableName, inputIterator, stateTime, exchangeItems);
				PerviousLandSegmentsInitTable previous = perviousLandSegmentsInitTables.put(tableName, initTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + UciUtils.PERLND_MODULE_NAME + " init tables '" + tableName + "' found in uci state file.");
				continue;
			}

//			if (QualPropsTable.isQualPropsTable(tableName)) {
//				//replace last known qualId with the one read from current QUAL-PROPS table.
//				lastKnownQualId = QualPropsTable.readQualId(UciUtils.PERLND_MODULE_NAME, inputIterator, null);
//				continue;
//			}

			if (QualInputTable.isQualInputTable(tableName)) {
				qualInputTableCounter++;
				QualInputTable qualInputTable = new QualInputTable(UciUtils.PERLND_MODULE_NAME, qualInputTableCounter, inputIterator, stateTime, exchangeItems);
				QualInputTable previous = perviousLandSegmentsQualInputTables.put(QualInputTable.TABLE_NAME + '_' + qualInputTableCounter, qualInputTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + UciUtils.PERLND_MODULE_NAME + " " + QualInputTable.TABLE_NAME + " tables with number " + qualInputTableCounter + " found in uci state file.");
				continue;
			}
		}
	}

	private void readImperviousLandSection(Iterator<String> inputIterator, double stateTime) {
		Results.putMessage(getClass().getSimpleName() + ": reading " + UciUtils.IMPLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());
		int qualInputTableCounter = 0;

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(UciUtils.IMPLND_MODULE_NAME)) break;

			String tableName = trimmedUppercaseLine;
			if (ImperviousLandSegmentsInitTable.isImperviousLandSegmentsInitTable(tableName)) {
				ImperviousLandSegmentsInitTable initTable = new ImperviousLandSegmentsInitTable(tableName, inputIterator, stateTime, exchangeItems);
				ImperviousLandSegmentsInitTable previous = imperviousLandSegmentsInitTables.put(tableName, initTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + UciUtils.IMPLND_MODULE_NAME + " init tables '" + tableName + "' found in uci state file.");
			}

//			if (QualPropsTable.isQualPropsTable(tableName)) {
//				//replace last known qualId with the one read from current QUAL-PROPS table.
//				lastKnownQualId = QualPropsTable.readQualId(UciUtils.IMPLND_MODULE_NAME, inputIterator, null);
//				continue;
//			}

			if (QualInputTable.isQualInputTable(tableName)) {
				qualInputTableCounter++;
				QualInputTable qualInputTable = new QualInputTable(UciUtils.IMPLND_MODULE_NAME, qualInputTableCounter, inputIterator, stateTime, exchangeItems);
				QualInputTable previous = imperviousLandSegmentsQualInputTables.put(QualInputTable.TABLE_NAME + '_' + qualInputTableCounter, qualInputTable);
				if (previous != null) throw new IllegalArgumentException("Multiple " + UciUtils.IMPLND_MODULE_NAME + " " + QualInputTable.TABLE_NAME + " tables with number " + qualInputTableCounter + " found in uci state file.");
				continue;
			}
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

			if (trimmedUppercaseLine.equals(UciUtils.RCHRES_MODULE_NAME)) {
				outputLines.add(inputLine);
				writeReachesSection(inputIterator, outputLines);
				continue;
			}

			if (trimmedUppercaseLine.equals(UciUtils.PERLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				writePerviousLandSection(inputIterator, outputLines);
				continue;
			}

			if (trimmedUppercaseLine.equals(UciUtils.IMPLND_MODULE_NAME)) {
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
		Results.putMessage(getClass().getSimpleName() + ": replacing values in " + UciUtils.RCHRES_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(UciUtils.RCHRES_MODULE_NAME)) {
				outputLines.add(inputLine);
				break;
			}

			if (ReachesInitTable.isReachesInitTable(trimmedUppercaseLine)) {
				String tableName = trimmedUppercaseLine;

				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				ReachesInitTable initTable = reachesInitTables.get(tableName);
				if (initTable == null) throw new IllegalStateException(UciUtils.RCHRES_MODULE_NAME + " init table '" + tableName + "' not initialized during reading of uci state file.");
				initTable.write(exchangeItems, outputLines, exchangeItemsDefaultInit);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}
	}

	private void writePerviousLandSection(Iterator<String> inputIterator, List<String> outputLines) {
		Results.putMessage(getClass().getSimpleName() + ": replacing values in " + UciUtils.PERLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());
		int qualInputTableCounter = 0;

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(UciUtils.PERLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				break;
			}

			String tableName = trimmedUppercaseLine;
			if (PerviousLandSegmentsInitTable.isPerviousLandSegmentsInitTable(tableName)) {
				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				PerviousLandSegmentsInitTable initTable = perviousLandSegmentsInitTables.get(tableName);
				if (initTable == null) throw new IllegalStateException(UciUtils.PERLND_MODULE_NAME + " init table '" + tableName + "' not initialized during reading of uci state file.");
				initTable.write(exchangeItems, outputLines, exchangeItemsDefaultInit);
				continue;
			}

//			if (QualPropsTable.isQualPropsTable(tableName)) {
//				//copy inputLine to output unchanged.
//				outputLines.add(inputLine);
//				//replace last known qualId with the one read from current QUAL-PROPS table and copy table.
//				lastKnownQualId = QualPropsTable.readQualId(UciUtils.PERLND_MODULE_NAME, inputIterator, outputLines);
//				continue;
//			}

			if (QualInputTable.isQualInputTable(tableName)) {
				qualInputTableCounter++;

				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				QualInputTable qualInputTable = perviousLandSegmentsQualInputTables.get(QualInputTable.TABLE_NAME + '_' + qualInputTableCounter);
				if (qualInputTable == null) throw new IllegalStateException(UciUtils.PERLND_MODULE_NAME + " " + QualInputTable.TABLE_NAME + " table with number " + qualInputTableCounter + " not initialized during reading of uci state file.");
				qualInputTable.write(exchangeItems, outputLines, exchangeItemsDefaultInit);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}
	}

	private void writeImperviousLandSection(Iterator<String> inputIterator, List<String> outputLines) {
		Results.putMessage(getClass().getSimpleName() + ": replacing values in " + UciUtils.IMPLND_MODULE_NAME + " section in uci state file " + uciFile.getAbsolutePath());
		int qualInputTableCounter = 0;

		while (inputIterator.hasNext()) {
			String inputLine = inputIterator.next();
			String trimmedUppercaseLine = inputLine.trim().toUpperCase();
			if (trimmedUppercaseLine.startsWith("END") && trimmedUppercaseLine.endsWith(UciUtils.IMPLND_MODULE_NAME)) {
				outputLines.add(inputLine);
				break;
			}

			String tableName = trimmedUppercaseLine;
			if (ImperviousLandSegmentsInitTable.isImperviousLandSegmentsInitTable(tableName)) {
				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				ImperviousLandSegmentsInitTable initTable = imperviousLandSegmentsInitTables.get(tableName);
				if (initTable == null) throw new IllegalStateException(UciUtils.IMPLND_MODULE_NAME + " init table '" + tableName + "' not initialized during reading of uci state file.");
				initTable.write(exchangeItems, outputLines, exchangeItemsDefaultInit);
				continue;
			}

//			if (QualPropsTable.isQualPropsTable(tableName)) {
//				//copy inputLine to output unchanged.
//				outputLines.add(inputLine);
//				//replace last known qualId with the one read from current QUAL-PROPS table and copy table.
//				lastKnownQualId = QualPropsTable.readQualId(UciUtils.IMPLND_MODULE_NAME, inputIterator, outputLines);
//				continue;
//			}

			if (QualInputTable.isQualInputTable(tableName)) {
				qualInputTableCounter++;

				//skip reading to end of table.
				UciUtils.skipToEnd(inputIterator);

				//write new table.
				QualInputTable qualInputTable = imperviousLandSegmentsQualInputTables.get(QualInputTable.TABLE_NAME + '_' + qualInputTableCounter);
				if (qualInputTable == null) throw new IllegalStateException(UciUtils.IMPLND_MODULE_NAME + " " + QualInputTable.TABLE_NAME + " table with number " + qualInputTableCounter + " not initialized during reading of uci state file.");
				qualInputTable.write(exchangeItems, outputLines, exchangeItemsDefaultInit);
				continue;
			}

			//copy inputLine to output unchanged.
			outputLines.add(inputLine);
		}
	}
}
