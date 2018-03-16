package org.openda.model_metaswap;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.LinkedHashMap;

public class SwapStateFile implements IDataObject {
	public static final String PRESSURE_HEAD_ROOT_ZONE = "pressureHeadRootZone";
	private LinkedHashMap<String, SwapStateExchangeItem> exchangeItems = new LinkedHashMap<>();
	private int headerBytesLength;
	private int lineBytesLength;
	private static final int START_LENGTH = 221;
	private static final int LINE_BREAK_LENGTH = 2;
	private final DecimalFormat formatDouble;
	private File sourceFile;

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
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		SwapStateExchangeItem swapStateExchangeItem = exchangeItems.get(PRESSURE_HEAD_ROOT_ZONE);
		double[] doubles = swapStateExchangeItem.getValuesAsDoubles();
		try (RandomAccessFile raf = new RandomAccessFile(sourceFile, "rw")) {
			int pos = headerBytesLength + START_LENGTH + LINE_BREAK_LENGTH;
			raf.seek(pos);
			for (int i = 0; i < doubles.length; i++) {
				String replace = formatDouble.format(doubles[i]);
				if (replace.length() < 14) replace += ' ';
				raf.write(replace.getBytes());
				pos += lineBytesLength + LINE_BREAK_LENGTH;
				raf.seek(pos);
			}
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String argument = arguments[0];
		sourceFile = new File(workingDir, argument);
		if (!sourceFile.exists()) throw new RuntimeException("Swap state file " + sourceFile + " not found.");
		long start = System.currentTimeMillis();
		ArrayList<String> strings = new ArrayList<>();
		try (BufferedReader bufferedReader = new BufferedReader(new FileReader(sourceFile))) {
			headerBytesLength = bufferedReader.readLine().getBytes().length;
			String line = bufferedReader.readLine();
			lineBytesLength = line.getBytes().length;
			while (line != null) {
				String substring = line.substring(221, 235);
				strings.add(substring);
				line = bufferedReader.readLine();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		int size = strings.size();
		double[] doubles = new double[size];
		for (int i = 0; i < size; i++) {
			String s = strings.get(i);
			doubles[i] = Double.valueOf(s);
		}
		long readEnd = System.currentTimeMillis();
		System.out.println("Millis passed during read: " + (readEnd - start));

		SwapStateExchangeItem pressureHeadRootZone = new SwapStateExchangeItem(PRESSURE_HEAD_ROOT_ZONE, doubles);
		exchangeItems.put(PRESSURE_HEAD_ROOT_ZONE, pressureHeadRootZone);
	}

	private void streamReadReplaceWrite(File sourceFile, FileWriter fileWriter, double[] doubles, DecimalFormat formatDouble) {
		try (BufferedReader bufferedReader = new BufferedReader(new FileReader(sourceFile));
			 BufferedWriter bufferedWriter = new BufferedWriter(fileWriter)) {
			String headerLine = bufferedReader.readLine();
			bufferedWriter.write(headerLine);
			String line = bufferedReader.readLine();
			for (int i = 0; i < doubles.length; i++) {
				//String[] split = line.split("\\s+");
				//bufferedWriter.write(String.join(" ", split));
				String startString = line.substring(0, 221);
				StringBuilder builder = new StringBuilder(startString);
				String replaceString = formatDouble.format(doubles[i]);
				String padded = String.format("%1$14s", replaceString);
				builder.append(padded);
				/*builder.append(replaceString);*/
				String endString = line.substring(235);
				builder.append(endString);
				String edit = builder.toString();
				/*if (edit.length() != line.length()) {
					System.out.println("Here");
				}*/
				bufferedWriter.write(edit);
				bufferedWriter.newLine();
				line = bufferedReader.readLine();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
