package org.openda.model_hec_hms;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

public class ControlFile implements IDataObject {
	private static final String[] EMPTY_STRING_ARRAY = new String[0];
	private static final String START_TIME = "start_time";
	private static final String END_TIME = "end_time";
	private File workingDirectory;
	private String filename = null;

	private final Map<String, DoubleExchangeItem> exchangeItems = new HashMap<>();
	private String timeZoneId = null;

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

		ZoneId zoneId = ZoneId.of(timeZoneId);
		DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("dd MMMM yyyy").withZone(zoneId);
		DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm").withZone(zoneId);

		ZoneId utcZoneId = ZoneId.of("UTC");

		LocalDateTime startDateTime = getDateTime(utcZoneId, START_TIME);
		LocalDateTime endDateTime = getDateTime(utcZoneId, END_TIME);

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
							value = dateFormatter.format(startDateTime);
							break;
						case "Start Time":
							value = timeFormatter.format(startDateTime);
							break;
						case "End Date":
							value = dateFormatter.format(endDateTime);
							break;
						case "End Time":
							value = timeFormatter.format(endDateTime);
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

	private LocalDateTime getDateTime(ZoneId utcZoneId, String timeId) {
		DoubleExchangeItem dateTimeExchangeItem = exchangeItems.get(timeId);
		double mjdDateTime = dateTimeExchangeItem.getValue();
		Date dateTime = TimeUtils.mjdToDate(mjdDateTime);
		dateTime.setHours((int) Math.round((mjdDateTime % 1) * 24));
		return dateTime.toInstant().atZone(utcZoneId).toLocalDateTime();
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
						case "Time Zone ID":
							timeZoneId = labelAndValue[1].trim();
							break;
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

			DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd MMMM yyyyHH:mmVV");

			ZonedDateTime startDateTime = ZonedDateTime.parse(startDate + startTime + timeZoneId, dateTimeFormatter);
			long startDateTimeMillis = startDateTime.toInstant().toEpochMilli();
			double startMjdDateTime = TimeUtils.date2Mjd(new Date(startDateTimeMillis));
			DoubleExchangeItem startMjdDateTimeExchangeItem = new DoubleExchangeItem(START_TIME, IExchangeItem.Role.InOut, startMjdDateTime);
			exchangeItems.putIfAbsent(START_TIME, startMjdDateTimeExchangeItem);

			ZonedDateTime endDateTime = ZonedDateTime.parse(endDate + endTime + timeZoneId, dateTimeFormatter);
			long endDateTimeMillis = endDateTime.toInstant().toEpochMilli();
			double endMjdDateTime = TimeUtils.date2Mjd(new Date(endDateTimeMillis));
			DoubleExchangeItem endMjdDateTimeExchangeItem = new DoubleExchangeItem(END_TIME, IExchangeItem.Role.InOut, endMjdDateTime);
			exchangeItems.putIfAbsent(END_TIME, endMjdDateTimeExchangeItem);
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}
	}
}
