package org.openda.model_hec_hms;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;

public class ControlFile implements IDataObject {
	private static final String[] EMPTY_STRING_ARRAY = new String[0];
	private static final String START_TIME = "start_time";
	private static final String END_TIME = "end_time";
	public static final String DATE_FORMAT = "dd MMMMM yyyy";
	public static final String TIME_FORMAT = "KK:mm";
	private File workingDirectory;
	private String filename = null;

	private final Map<String, DoubleExchangeItem> exchangeItems = new HashMap<>();

	private final Map<Integer, String> lines = new LinkedHashMap<>();
	private final Map<Integer, String> valuesToUpdate = new HashMap<>();

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return exchangeItems.keySet().toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		File outputFile = new File(workingDirectory, filename);

		try (FileOutputStream fileOutputStream = new FileOutputStream(outputFile);
			 OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream);
			 BufferedWriter bufferedWriter = new BufferedWriter(outputStreamWriter)) {
			int lineNumber = 0;
			for (Map.Entry<Integer, String> line : lines.entrySet()) {
				if (valuesToUpdate.containsKey(lineNumber)) {
					String label = line.getValue().split(":")[0];
					bufferedWriter.write(label);

					String value = null;

					switch (label.trim()) {
						case "Start Date":
							value = getDateTimeString(START_TIME, DATE_FORMAT);
							break;
						case "Start Time":
							value = getDateTimeString(START_TIME, TIME_FORMAT);
							break;
						case "End Date":
							value = getDateTimeString(END_TIME, DATE_FORMAT);
							break;
						case "End Time":
							value = getDateTimeString(END_TIME, TIME_FORMAT);
							break;
						default:
							// Do nothing with other labels
					}

					bufferedWriter.write(": " + value);
				} else {
					bufferedWriter.write(line.getValue());
				}
				bufferedWriter.newLine();

				lineNumber++;
			}
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}
	}

	private String getDateTimeString(String timeEIId, String dateFormat) {
		DoubleExchangeItem dateTimeExchangeItem = exchangeItems.get(timeEIId);
		double mjdDateTime = dateTimeExchangeItem.getValue();
		return TimeUtils.mjdToString(mjdDateTime, dateFormat, TimeZone.getTimeZone("UTC"), Locale.US);
	}

	@Override
	public void initialize(File workingDirectory, String[] arguments) {
		this.workingDirectory = workingDirectory;

		File inputFile = getInputFile(arguments);
		processInputFile(inputFile);
	}

	private File getInputFile(String[] arguments) {
		if (null == arguments || 1 != arguments.length) {
			throw new RuntimeException("Expected filename as only argument.");
		}

		filename = arguments[0];

		File inputFile = new File(workingDirectory, filename);

		try {
			if (!inputFile.exists()) {
				throw new RuntimeException("File: " + inputFile.getCanonicalFile() + " does not exist.");
			}

			if (!inputFile.isFile()) {
				throw new RuntimeException("File: " + inputFile.getCanonicalFile() + " is not a file.");
			}
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}
		return inputFile;
	}

	private void processInputFile(File inputFile) {
		try (FileInputStream fileInputStream = new FileInputStream(inputFile);
			 InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream);
			 BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
			String line = bufferedReader.readLine();
			int lineNumber = 0;

			String startDate = null;
			String startTime = null;
			String endDate = null;
			String endTime = null;

			while (null != line) {
				lines.put(lineNumber, line);

				if (line.contains(":")) {
					String[] labelAndValue = line.split(":");
					String label = labelAndValue[0].trim();

					switch (label) {
						case "Start Date":
							valuesToUpdate.put(lineNumber, label);
							startDate = labelAndValue[1].trim();
							break;
						case "Start Time":
							valuesToUpdate.put(lineNumber, label);
							startTime = labelAndValue[1].trim() + ":" + labelAndValue[2].trim();
							break;
						case "End Date":
							valuesToUpdate.put(lineNumber, label);
							endDate = labelAndValue[1].trim();
							break;
						case "End Time":
							valuesToUpdate.put(lineNumber, label);
							endTime = labelAndValue[1].trim() + ":" + labelAndValue[2].trim();
							break;
						default:
							// Do nothing with other labels
					}
				}

				line = bufferedReader.readLine();
				lineNumber++;
			}
			String pattern = DATE_FORMAT + TIME_FORMAT;
			SimpleDateFormat format = new SimpleDateFormat(pattern, Locale.US);
			format.setTimeZone(TimeZone.getTimeZone("UTC"));

			long startDateTimeMillis = format.parse(startDate + startTime).getTime();
			double startMjdDateTime = TimeUtils.date2Mjd(new Date(startDateTimeMillis));
			DoubleExchangeItem startMjdDateTimeExchangeItem = new DoubleExchangeItem(START_TIME, IExchangeItem.Role.InOut, startMjdDateTime);
			exchangeItems.putIfAbsent(START_TIME, startMjdDateTimeExchangeItem);

			long endDateTimeMillis = format.parse(endDate + endTime).getTime();
			double endMjdDateTime = TimeUtils.date2Mjd(new Date(endDateTimeMillis));
			DoubleExchangeItem endMjdDateTimeExchangeItem = new DoubleExchangeItem(END_TIME, IExchangeItem.Role.InOut, endMjdDateTime);
			exchangeItems.putIfAbsent(END_TIME, endMjdDateTimeExchangeItem);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
