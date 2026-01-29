package org.openda.model_hec_hms;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class ControlFile implements IDataObject {
	private static final String[] EMPTY_STRING_ARRAY = new String[0];
	private File workingDirectory;
	private String filename = null;

	private String controlFileId = null;
	private final Map<String, IExchangeItem> exchangeItems = new HashMap<>();
	private String timeZoneId = null;

	private final Map<Integer, String> lines = new LinkedHashMap<>();
	private final Map<Integer, String> valuesToUpdate = new HashMap<>();

	@Override
	public String[] getExchangeItemIDs() {
		return new String[]{"ControlFileExchangeItem"};
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

		IExchangeItem exchangeItem = exchangeItems.get(controlFileId);
		double[] times = exchangeItem.getTimes();

		ZoneId utcZoneId = ZoneId.of("UTC");

		Date startDate = TimeUtils.mjdToDate(times[0]);
		startDate.setHours((int) Math.round((times[0] % 1) * 24));
		LocalDateTime startDateTime = startDate.toInstant().atZone(utcZoneId).toLocalDateTime();

		Date endDate = TimeUtils.mjdToDate(times[1]);
		endDate.setHours((int) Math.round((times[1] % 1) * 24));
		LocalDateTime endDateTime = endDate.toInstant().atZone(utcZoneId).toLocalDateTime();

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
						case "Control":
							controlFileId = labelAndValue[1].trim();
							break;
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
			ZonedDateTime endDateTime = ZonedDateTime.parse(endDate + endTime + timeZoneId, dateTimeFormatter);

			double[] times = new double[2];

			long startDateTimeMillis = startDateTime.toInstant().toEpochMilli();
			times[0] = TimeUtils.date2Mjd(new Date(startDateTimeMillis));
			long endDateTimeMillis = endDateTime.toInstant().toEpochMilli();
			times[1] = TimeUtils.date2Mjd(new Date(endDateTimeMillis));

			exchangeItems.putIfAbsent(controlFileId, new ControlFileExchangeItem(controlFileId, times));
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}
	}
}
