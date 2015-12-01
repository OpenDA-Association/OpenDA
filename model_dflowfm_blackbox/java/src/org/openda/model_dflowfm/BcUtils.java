package org.openda.model_dflowfm;

import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Created by prevel on 24-Nov-15.
 */
public class BcUtils
{
	/*
		A Modified Julian Day (MJD) is created by subtracting 2400000.5 from a Julian day number,
		and thus represents the number of days elapsed since midnight (00:00) Universal Time on November 17, 1858.
	*/

	private static final Calendar MJD_ZERO = new GregorianCalendar(1858, Calendar.NOVEMBER, 17, 0, 0, 0);
	private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");

	private static final String TIME_UNIT_SECONDS = "seconds since";
	private static final String TIME_UNIT_MINUTES = "minutes since";
	private static final String TIME_UNIT_HOURS = "hours since";

	private static final long ONE_DAY_IN_MILLISECONDS = 86400000;
	private static final long ONE_HOUR_IN_MILLISECONDS = 3600000;
	private static final long ONE_MINUTE_IN_MILLISECONDS = 60000;
	private static final long ONE_SECOND_IN_MILLISECONDS = 1000;

	public static List<Double> ConvertDateTimesToModifiedJulianDayValues(String unitString, List<Double> values)
	{
		// Step 1: Parse Reference Date and convert values to milliseconds
		List<Double> valuesInMilliSeconds = new ArrayList<>();
		String referenceDateString = "";
		if(unitString.contains(TIME_UNIT_SECONDS))
		{
			referenceDateString = unitString.replace(TIME_UNIT_SECONDS, "").trim();
			for(double value : values) valuesInMilliSeconds.add(value * ONE_SECOND_IN_MILLISECONDS);
		}
		else if(unitString.contains(TIME_UNIT_MINUTES))
		{
			referenceDateString = unitString.replace(TIME_UNIT_MINUTES, "").trim();
			for(double value : values) valuesInMilliSeconds.add(value * ONE_MINUTE_IN_MILLISECONDS);
		}
		else if(unitString.contains(TIME_UNIT_HOURS))
		{
			referenceDateString = unitString.replace(TIME_UNIT_HOURS, "").trim();
			for(double value : values) valuesInMilliSeconds.add(value * ONE_HOUR_IN_MILLISECONDS);
		}
		else
		{
			throw new RuntimeException(String.format("Error parsing Date unit: %s", referenceDateString));
		}

		// Step 2: Create a Calendar object for the ref date
		Calendar reference_zero = new GregorianCalendar();
		try
		{
			reference_zero.setTime(DATE_FORMAT.parse(referenceDateString));
		}
		catch(Exception ex)
		{
			throw new RuntimeException(String.format("%s, Error parsing Date value: %s", ex.getMessage(), referenceDateString));
		}

		// Step 3: Convert values to ModifiedJulianDay values
		List<Double> modifiedJulianDayValues = new ArrayList<>();

		for(Double value : valuesInMilliSeconds)
		{
			double mjdValue = (value + (reference_zero.getTimeInMillis() - MJD_ZERO.getTimeInMillis())) / (double)ONE_DAY_IN_MILLISECONDS;
			modifiedJulianDayValues.add(mjdValue);
		}

		return modifiedJulianDayValues;
	}

	public static List<Double> ConvertDateTimesFromModifiedJulianDayValues(String unitString, List<Double> modifiedJulianDayValues)
	{
		List<Double> values = new ArrayList<>();

		// Step 1: Parse Reference Date and retrieve unit
		long unitFactor;
		String referenceDateString = "";
		if(unitString.contains(TIME_UNIT_SECONDS))
		{
			referenceDateString = unitString.replace(TIME_UNIT_SECONDS, "").trim();
			unitFactor = ONE_SECOND_IN_MILLISECONDS;
		}
		else if(unitString.contains(TIME_UNIT_MINUTES))
		{
			referenceDateString = unitString.replace(TIME_UNIT_MINUTES, "").trim();
			unitFactor = ONE_MINUTE_IN_MILLISECONDS;
		}
		else if(unitString.contains(TIME_UNIT_HOURS))
		{
			referenceDateString = unitString.replace(TIME_UNIT_HOURS, "").trim();
			unitFactor = ONE_HOUR_IN_MILLISECONDS;
		}
		else
		{
			throw new RuntimeException(String.format("Error parsing Date unit: %s", referenceDateString));
		}

		// Step 2: Create a Calendar object for the ref date
		Calendar reference_zero = new GregorianCalendar();
		try
		{
			reference_zero.setTime(DATE_FORMAT.parse(referenceDateString));
		}
		catch(Exception ex)
		{
			throw new RuntimeException(String.format("%s, Error parsing Date value: %s", ex.getMessage(), referenceDateString));
		}

		// Step 3: Convert ModifiedJulianDay values to unit factor values
		for(Double mjdValue : modifiedJulianDayValues)
		{
			Double value = ((mjdValue * ONE_DAY_IN_MILLISECONDS) - (reference_zero.getTimeInMillis() - MJD_ZERO.getTimeInMillis())) / unitFactor;
			values.add(value);
		}

		return values;
	}
}
