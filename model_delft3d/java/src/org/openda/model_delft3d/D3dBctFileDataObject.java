package org.openda.model_delft3d;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

public class D3dBctFileDataObject implements IDataObject {

	private LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	private File targetFile;
	private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
	private DecimalFormat numberFormat = null;
	private String[] allLines;
	private double referenceTimeMJD;

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[0]);
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

		try {
			BufferedWriter output = new BufferedWriter(new FileWriter(targetFile));
			for (int i = 0; i < 11; i++) {
				try {
					output.write(allLines[i]);
					output.newLine();
				} catch (IOException e) {
					throw new RuntimeException("Cannot write line in roughness file");
				}
			}
			String[] ids = getExchangeItemIDs();
			IExchangeItem firstItem = exchangeItems.get(ids[0]);
			double[] times = firstItem.getTimes();
			for (int i = 0; i < allLines.length - 11; i++) {
				StringBuilder builder = new StringBuilder(30);
				double minutes = TimeUtils.mjdInMinutes(times[i] - referenceTimeMJD);
				builder.append(numberFormat.format(minutes).replace("E", "e+"));
				for (int j = 0; j < ids.length; j++) {
					builder.append(" ");
					double value = exchangeItems.get(ids[j]).getValuesAsDoubles()[i];
					String formattedNumber = numberFormat.format(value).replace('E', 'e');
					if (formattedNumber.charAt(11) != '-') formattedNumber = formattedNumber.substring(0, 11) + '+' + formattedNumber.substring(11);
					builder.append(formattedNumber);
				}
				output.write(builder.toString());
				output.newLine();
			}
			output.close();
		} catch (IOException ex) {
			throw new RuntimeException("Cannot write roughness file");
		}

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 1) throw new RuntimeException("Supply at least 1 argument as source file and optionally a second as target file");
		if (arguments.length > 2) throw new RuntimeException("Supply not more than 2 arguments, the first as source file and optionally a second as target file");

		DecimalFormatSymbols decimalFormatSymbols = new DecimalFormatSymbols();
		decimalFormatSymbols.setDecimalSeparator('.');
		numberFormat = new DecimalFormat(" 0.0000000E000;-0.0000000E000", decimalFormatSymbols);

		File sourceFile = new File(workingDir, arguments[0]);
		targetFile = arguments.length == 1 ? sourceFile : new File(workingDir, arguments[1]);

		allLines = readWholeFile(targetFile);

		String[] referenceTimeLineSplit = allLines[4].trim().split(" ");
		Date date;
		try {
			String referenceTimeText = referenceTimeLineSplit[referenceTimeLineSplit.length - 1];
			date = dateFormat.parse(referenceTimeText);
		} catch (ParseException e) {
			throw new RuntimeException("Reading reference time from " + targetFile + " failed due to " + e.getMessage());
		}

		referenceTimeMJD = TimeUtils.date2Mjd(date);

		int index = 8;

		List<String> parameterIds = new ArrayList<>();
		while (allLines[index].trim().startsWith("parameter")) {
			String[] split = allLines[index].split("'");
			String parameterId = split[1].trim();
			parameterIds.add(parameterId);
			index++;
		}
		String[] recordsLineSplit = allLines[index].trim().split("\\s+");
		String recordsText = recordsLineSplit[recordsLineSplit.length - 1];
		int recordsLength = Integer.parseInt(recordsText);
		double[] times = new double[recordsLength];
		double[][] allValues = new double[parameterIds.size()][recordsLength];

		//read values
		for (int i = ++index; i < allLines.length; i++) {
			String[] split = allLines[i].trim().split("\\s+");
			assert split.length == parameterIds.size() + 1;
			times[i - index] =  TimeUtils.addMinutesToMjd(referenceTimeMJD, Double.parseDouble(split[0]));
			for (int j = 1; j < split.length; j++) {
				allValues[j - 1][i - index] = Double.parseDouble(split[j]);
			}
		}

		for (int i = 0; i < parameterIds.size(); i++) {
			String id = parameterIds.get(i);
			D3dBctExchangeItem exchangeItem = new D3dBctExchangeItem(id, allValues[i], times);
			exchangeItems.put(id, exchangeItem);
		}
	}

	// Read content of the whole roughness file and store it in a string array
	public static String[] readWholeFile(File textFile) {

		ArrayList<String> content = new ArrayList();

		try {
			BufferedReader input = new BufferedReader(new FileReader(textFile));
			try {
				String line = null;
				while ((line = input.readLine()) != null) {
					content.add(line);
				}
			} finally {
				input.close();
			}
		} catch (IOException ex) {
			throw new RuntimeException("Cannot read bct file " + textFile.getAbsolutePath() + " due to " + ex.getMessage(), ex);
		}

		String[] sContent = new String[content.size()];
		return content.toArray(sContent);
	}

}
