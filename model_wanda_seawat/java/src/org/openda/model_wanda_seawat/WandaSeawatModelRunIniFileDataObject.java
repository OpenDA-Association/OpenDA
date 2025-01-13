package org.openda.model_wanda_seawat;

import org.ini4j.Ini;
import org.ini4j.Profile;
import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.io.AsciiFileUtils;

import java.io.*;
import java.text.ParseException;
import java.util.List;

public class WandaSeawatModelRunIniFileDataObject extends AbstractDataObject {
	private static final String SECTION_RUN = "run";
	private static final String OPTION_START_DATE_TIME = "startDateTime";
	private static final String OPTION_END_DATE_TIME = "endDateTime";
	private static final String SECTION_PARAMETERS = "parameters";
	public static final String datePattern = "yyyyMMddHHmmss";
	private File file;
	private Ini ini;

	@Override
	public void finish() {
		if (exchangeItems.isEmpty()) {
			throw new RuntimeException("Exchange items not yet initialized.");
		}
		DoubleExchangeItem startTimeExchangeItem = (DoubleExchangeItem) exchangeItems.get(OPTION_START_DATE_TIME);
		ini.put(SECTION_RUN, OPTION_START_DATE_TIME, TimeUtils.mjdToString(startTimeExchangeItem.getValue(), datePattern));
		DoubleExchangeItem endTimeExchangeItem = (DoubleExchangeItem) exchangeItems.get(OPTION_END_DATE_TIME);
		double endTime = endTimeExchangeItem.getValue();
		ini.put(SECTION_RUN, OPTION_END_DATE_TIME, TimeUtils.mjdToString(endTime, datePattern));
		Profile.Section optionsFromParameters = ini.get(SECTION_PARAMETERS);
		for (String option : optionsFromParameters.keySet()) {
			IExchangeItem exchangeItem = exchangeItems.get(option);
			if (exchangeItem instanceof TimeSeries) {
				String csvFileName = optionsFromParameters.get(option);
				double[] times = exchangeItem.getTimes();
				double[] values = exchangeItem.getValuesAsDoubles();
				File csvFile = new File(file.getParentFile(), csvFileName);
				try (BufferedWriter writer = new BufferedWriter(new FileWriter(csvFile))) {
					for (int i = 0; i < times.length; i++) {
						writer.write(String.format("%s;%s", TimeUtils.mjdToString(times[i]), values[i]));
						writer.newLine();
					}
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
				continue;
			}
			if (exchangeItem instanceof WandaSeawatConstantAsTimeSeriesExchangeItem) {
				double[] values = exchangeItem.getValuesAsDoubles();
				ini.put(SECTION_PARAMETERS, option, values[values.length - 1]);
				continue;
			}
			DoubleExchangeItem parameterExchangeItem = (DoubleExchangeItem) exchangeItem;
			ini.put(SECTION_PARAMETERS, option, parameterExchangeItem.getValue());
		}
		try {
			ini.store(file);
		} catch (IOException exception) {
			throw new RuntimeException(exception);
		}
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		file = new File(workingDir, arguments[0]);
		ini = new Ini();
		ini.getConfig().setEscape(false);
		try {
			ini.load(file);
			double startDateTime = dateTimeExchangeItem(ini, OPTION_START_DATE_TIME, OPTION_START_DATE_TIME);
			double endDateTime = dateTimeExchangeItem(ini, OPTION_END_DATE_TIME, OPTION_END_DATE_TIME);
			Profile.Section optionsFromParameters = ini.get(SECTION_PARAMETERS);
			for (String option : optionsFromParameters.keySet()) {
				String valueString = ini.get(SECTION_PARAMETERS, option);
				if (valueString.endsWith(".csv")) {
					createTimeSeriesExchangeItem(workingDir, option, valueString);
					continue;
				}
				String timeStep = ini.get(SECTION_RUN, "timeStep");

				double timeStepInSeconds = Double.parseDouble(timeStep);
				double value = Double.parseDouble(valueString);
				WandaSeawatConstantAsTimeSeriesExchangeItem constantAsTimeSeriesExchangeItem = new WandaSeawatConstantAsTimeSeriesExchangeItem(option, value, timeStepInSeconds, startDateTime, endDateTime);
				exchangeItems.putIfAbsent(option, constantAsTimeSeriesExchangeItem);
			}
		} catch (IOException | ParseException exception) {
			throw new RuntimeException(exception);
		}
	}

	private void createTimeSeriesExchangeItem(File workingDir, String option, String valueString) throws ParseException {
		File csvFile = new File(workingDir, valueString);
		List<String> lines = AsciiFileUtils.readLines(csvFile);
		double[] times = new double[lines.size()];
		double[] values = new double[lines.size()];
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			String[] split = line.split(";");
			times[i] = TimeUtils.date2Mjd(split[0]);
			values[i] = Double.parseDouble(split[1]);
		}
		TimeSeries timeSeriesExchangeItem = new TimeSeries(times, values);
		timeSeriesExchangeItem.setId(option);
		exchangeItems.put(option, timeSeriesExchangeItem);
	}

	private double dateTimeExchangeItem(Ini ini, String optionName, String id) throws ParseException {
		String valueString = ini.get(SECTION_RUN, optionName);
		double dateTime = TimeUtils.date2Mjd(valueString);
		DoubleExchangeItem doubleExchangeItem = new DoubleExchangeItem(id, IExchangeItem.Role.InOut, dateTime);
		exchangeItems.putIfAbsent(id, doubleExchangeItem);
		return dateTime;
	}

}
