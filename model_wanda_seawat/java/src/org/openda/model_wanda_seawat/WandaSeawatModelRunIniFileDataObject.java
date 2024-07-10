package org.openda.model_wanda_seawat;

import org.ini4j.Ini;
import org.ini4j.Profile;
import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;

public class WandaSeawatModelRunIniFileDataObject extends AbstractDataObject {
	private static final String SECTION_RUN = "run";
	private static final String OPTION_START_DATE_TIME = "startDateTime";
	private static final String OPTION_END_DATE_TIME = "endDateTime";
	private static final String SECTION_PARAMETERS = "parameters";
	private File file;
	private Ini ini;

	@Override
	public void finish() {
		if (exchangeItems.isEmpty()) {
			throw new RuntimeException("Exchange items not yet initialized.");
		}
		DoubleExchangeItem startTimeExchangeItem = (DoubleExchangeItem) exchangeItems.get(OPTION_START_DATE_TIME);
		ini.put(SECTION_RUN, OPTION_START_DATE_TIME, startTimeExchangeItem.getValue());
		DoubleExchangeItem endTimeExchangeItem = (DoubleExchangeItem) exchangeItems.get(OPTION_END_DATE_TIME);
		ini.put(SECTION_RUN, OPTION_END_DATE_TIME, endTimeExchangeItem.getValue());
		Profile.Section optionsFromParameters = ini.get(SECTION_PARAMETERS);
		for (String option : optionsFromParameters.keySet()) {
			DoubleExchangeItem parameterExchangeItem = (DoubleExchangeItem) exchangeItems.get(option); 
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
		try {
			ini.load(file);
			dateTimeExchangeItem(ini, OPTION_START_DATE_TIME, OPTION_START_DATE_TIME);
			dateTimeExchangeItem(ini, OPTION_END_DATE_TIME, OPTION_END_DATE_TIME);
		} catch (IOException | ParseException exception) {
			throw new RuntimeException(exception);
		}
		Profile.Section optionsFromParameters = ini.get(SECTION_PARAMETERS);
		for (String option : optionsFromParameters.keySet()) {
			doubleExchangeItem(ini, option, option);
		}
	}

	private void dateTimeExchangeItem(Ini ini, String optionName, String id) throws ParseException {
		String valueString = ini.get(SECTION_RUN, optionName);
		double dateTime = TimeUtils.date2Mjd(valueString);
		DoubleExchangeItem doubleExchangeItem = new DoubleExchangeItem(id, IExchangeItem.Role.InOut, dateTime);
		exchangeItems.putIfAbsent(id, doubleExchangeItem);
	}
	
	private void doubleExchangeItem(Ini ini, String optionName, String id) {
		String valueString = ini.get(SECTION_PARAMETERS, optionName);
		DoubleExchangeItem doubleExchangeItem = new DoubleExchangeItem(id, IExchangeItem.Role.InOut, Double.parseDouble(valueString));
		exchangeItems.putIfAbsent(id, doubleExchangeItem);
	}
}
