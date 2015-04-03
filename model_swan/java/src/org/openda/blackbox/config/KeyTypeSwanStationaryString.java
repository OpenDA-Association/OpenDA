package org.openda.blackbox.config;

import org.openda.blackbox.interfaces.IKeyDateType;
import org.openda.utils.Time;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Class implementation of SwanStationaryString KeyType
 */
public class KeyTypeSwanStationaryString extends KeyTypeString implements IKeyDateType {
	static String DATE_FORMAT = "yyyyMMdd";
	static String TIME_FORMAT = "HHmm";
	static String USED_TIMEZONE = "UTC";
	Locale DEFAULT_LOCALE = Locale.UK;

	protected double startDate = Double.NaN;
	protected double endDate = Double.NaN;
	protected double timeStep;


	public void setStartDate(double date) {
		this.startDate = date;

	}

	
	public void setEndDate(double date) {
		this.endDate = date;

	}

	public void setTimeStep(double timeStep) {
		this.timeStep = timeStep;
	}

	protected String parseDateValue(double value) {
		Locale.setDefault(DEFAULT_LOCALE); //make sure date is written in UK format, independent of local settings
		DateFormat dateFormatter = new SimpleDateFormat(DATE_FORMAT);
		DateFormat timeFormatter = new SimpleDateFormat(TIME_FORMAT);
		dateFormatter.setTimeZone(TimeZone.getTimeZone(USED_TIMEZONE));
		timeFormatter.setTimeZone(TimeZone.getTimeZone(USED_TIMEZONE));
		Date date = new Time(value).getDate();
		return "COMPUTE STATIONARY " + dateFormatter.format(date) + "." + timeFormatter.format(date);
	}

	
	public String calculateValue() {
		if (Double.isNaN(startDate)) {
			throw new RuntimeException(this.getClass().getName() + ".calculateValue(): startDate needs to be set before calling this method.");
		}
		return parseDateValue(startDate);
	}

}
