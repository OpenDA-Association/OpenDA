/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.model_metaswap;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class SwapStateFile implements IDataObject{
	static final String PRESSURE_HEAD_ROOT_ZONE = "pressureHeadRootZone";
	private LinkedHashMap<String, SwapStateExchangeItem> exchangeItems = new LinkedHashMap<>();
	private int headerBytesLength = 0;
	private int lineBytesLength = 0;
	//private static final int START_LENGTH = 221;
	private static final int START_LENGTH = 521;
	private static final int LINE_BREAK_LENGTH = 2;
	private final DecimalFormat formatDouble;
	private File sourceFile = null;

	public SwapStateFile() {
		DecimalFormatSymbols symbols = new DecimalFormatSymbols();
		symbols.setDecimalSeparator('.');
		formatDouble = new DecimalFormat(" 0.0000000E00;-0.0000000E00", symbols);
	}

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		SwapStateExchangeItem swapStateExchangeItem = exchangeItems.get(PRESSURE_HEAD_ROOT_ZONE);
		if (swapStateExchangeItem.getValuesAsDoubles().length == 0) return;
		byte[][] bytesArray = convertDoublesToStringBytes(swapStateExchangeItem);
		editFile(bytesArray);
	}

	private void editFile(byte[][] bytesArray) {
		try (RandomAccessFile raf = new RandomAccessFile(sourceFile, "rw")) {
			int pos = headerBytesLength + START_LENGTH + LINE_BREAK_LENGTH;
			raf.seek(pos);
			for (int i = 0; i < bytesArray.length; i++) {
				raf.write(bytesArray[i]);
				pos += lineBytesLength + LINE_BREAK_LENGTH;
				raf.seek(pos);
			}
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
	}

	private byte[][] convertDoublesToStringBytes(SwapStateExchangeItem swapStateExchangeItem) {
		double[] doubles = swapStateExchangeItem.getValuesAsDoubles();
		logStateValues("AFTER ", doubles);
		byte[][] bytesArray = new byte[doubles.length][];
		for (int i = 0; i < doubles.length; i++) {
			String replace = formatDouble.format(doubles[i]);
			if (replace.length() < 14) replace += ' ';
			bytesArray[i] = replace.getBytes();
		}
		return bytesArray;
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String argument = arguments[0];
		sourceFile = new File(workingDir, argument);
		if (!sourceFile.exists()) throw new RuntimeException("Swap state file " + sourceFile + " not found.");

		List<String> strings = getStringsFrom16thColumn();
		if (strings == null) {
			SwapStateExchangeItem pressureHeadRootZone = new SwapStateExchangeItem(PRESSURE_HEAD_ROOT_ZONE, new double[0]);
			exchangeItems.put(PRESSURE_HEAD_ROOT_ZONE, pressureHeadRootZone);
			return;
		}
		int size = strings.size();
		double[] doubles = convertStringsToDoubles(strings, size);
		logStateValues("BEFORE", doubles);
		SwapStateExchangeItem pressureHeadRootZone = new SwapStateExchangeItem(PRESSURE_HEAD_ROOT_ZONE, doubles);
		exchangeItems.put(PRESSURE_HEAD_ROOT_ZONE, pressureHeadRootZone);
	}

	private static double[] convertStringsToDoubles(List<String> strings, int size) {
		double[] doubles = new double[size];
		for (int i = 0; i < size; i++) {
			String s = strings.get(i);
			doubles[i] = Double.valueOf(s);
		}
		return doubles;
	}

	private ArrayList<String> getStringsFrom16thColumn() {
		ArrayList<String> strings = new ArrayList<>();
		try (BufferedReader bufferedReader = new BufferedReader(new FileReader(sourceFile))) {
			headerBytesLength = bufferedReader.readLine().getBytes().length;
			String line = bufferedReader.readLine();
			if (line == null) return null;
			lineBytesLength = line.getBytes().length;
			while (line != null) {
				//strings.add(line.substring(221, 235));
				strings.add(line.substring(521, 535));
				line = bufferedReader.readLine();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return strings;
	}
	private void logStateValues(String beforeOrAfter, double[] stateValues) {

		File svatIdsfile = new File (sourceFile.getParentFile(), "svatids.txt");
		if (!svatIdsfile.exists())
			return;

		ArrayList<Integer> svatIds = new ArrayList<>();
		try {
			FileReader fileReader = new FileReader(svatIdsfile);
			BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);
			String line = inputFileBufferedReader.readLine();
			while (line != null) {
				String trimmedLine = line.trim();
				if (trimmedLine.length() > 0) {
					if (!trimmedLine.startsWith("#")) {
						svatIds.add(Integer.parseInt(trimmedLine));
					}
				}
				line = inputFileBufferedReader.readLine();
			}
			inputFileBufferedReader.close();
			fileReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not svids read from " + svatIdsfile.getAbsolutePath());
		}

		File file = new File(sourceFile.getParentFile(), "statevalues.log");
		Writer writer;
		try {
			writer = new FileWriter(file, true);
			writer.write(beforeOrAfter);
			for (int i = 0; i < svatIds.size(); i++) {
				writer.write(",");
				int id = svatIds.get(i);
				String valueAsString = formatDouble.format(stateValues[id - 1]);
				if (valueAsString.length() < 14) valueAsString += ' ';
				writer.write(valueAsString);
			}
			writer.write("\n");
			writer.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not close log file " + file.getAbsolutePath());
		}
	}
}
