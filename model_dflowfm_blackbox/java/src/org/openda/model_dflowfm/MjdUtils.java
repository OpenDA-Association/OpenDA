package org.openda.model_dflowfm;

import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Created by prevel on 24-Nov-15.
 */
public class MjdUtils
{
	/*
		A Modified Julian Day (MJD) is created by subtracting 2400000.5 from a Julian day number,
		and thus represents the number of days elapsed since midnight (00:00) Universal Time on November 17, 1858.
	*/

	private static final Calendar MJD_ZERO = new GregorianCalendar(1858, Calendar.NOVEMBER, 17, 0, 0, 0);
	private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	private static final String TIME_UNIT_SECONDS = "seconds since";
	private static final String TIME_UNIT_MINUTES = "minutes since";
	private static final String TIME_UNIT_HOURS = "hours since";

	private static final long ONE_DAY_IN_MILLISECONDS = 86400000;
	private static final long ONE_HOUR_IN_MILLISECONDS = 3600000;
	private static final long ONE_MINUTE_IN_MILLISECONDS = 60000;
	private static final long ONE_SECOND_IN_MILLISECONDS = 1000;

	public static double ConvertDateTimeToModifiedJulianDay(Calendar dateTime)
	{
		return (dateTime.getTimeInMillis() - MJD_ZERO.getTimeInMillis()) / (double)ONE_DAY_IN_MILLISECONDS;
	}

	public static Calendar ConvertModifiedJulianDayToDateTime(double mdjValue)
	{
		Calendar dateTime = new GregorianCalendar();
		dateTime.setTimeInMillis((long)((mdjValue * ONE_DAY_IN_MILLISECONDS) + MJD_ZERO.getTimeInMillis())); // possible loss of precision?
		return dateTime;
	}

	public static List<Double> ConvertDateTimesWithUnitToModifiedJulianDays(String unitString, List<Double> values)
	{
		List<Double> valuesInMilliSeconds = GetValuesInMilliSeconds(unitString, values);
		Calendar reference_zero = ParseReferenceDate(unitString);
		double mjdReferenceZero = ConvertDateTimeToModifiedJulianDay(reference_zero);

		List<Double> modifiedJulianDayValues = new ArrayList<>();
		for(Double value : valuesInMilliSeconds)
		{
			double mjdValue = (value / (double)ONE_DAY_IN_MILLISECONDS) + mjdReferenceZero;
			modifiedJulianDayValues.add(mjdValue);
		}
		return modifiedJulianDayValues;
	}

	public static List<Double> ConvertModifiedJulianDaysToDateTimesWithUnit(String unitString, List<Double> modifiedJulianDayValues)
	{
		double multiplicationFactor = GetUnitMultiplicationFactor(unitString);
		Calendar reference_zero = ParseReferenceDate(unitString);
		double referenceZeroInMilliseconds = reference_zero.getTimeInMillis();

		List<Double> values = new ArrayList<>();
		for(Double mjdValue : modifiedJulianDayValues)
		{
			Calendar dateTime = ConvertModifiedJulianDayToDateTime(mjdValue);
			double dateTimeInMilliSeconds = dateTime.getTimeInMillis();
			Double value = (dateTimeInMilliSeconds - referenceZeroInMilliseconds) / multiplicationFactor;
			values.add(value);
		}
		return values;
	}

	private static double GetUnitMultiplicationFactor(String unitString)
	{
		double multiplicationFactor;
		if(unitString.contains(TIME_UNIT_SECONDS)) multiplicationFactor = ONE_SECOND_IN_MILLISECONDS;
		else if(unitString.contains(TIME_UNIT_MINUTES)) multiplicationFactor = ONE_MINUTE_IN_MILLISECONDS;
		else if(unitString.contains(TIME_UNIT_HOURS)) multiplicationFactor = ONE_HOUR_IN_MILLISECONDS;
		else throw new RuntimeException(String.format("Error parsing Date unit: %s", unitString));
		return multiplicationFactor;
	}

	private static List<Double> GetValuesInMilliSeconds(String unitString, List<Double> values)
	{
		List<Double> valuesInMilliSeconds = new ArrayList<>();
		for(double value : values) valuesInMilliSeconds.add(value * GetUnitMultiplicationFactor(unitString));
		return valuesInMilliSeconds;
	}

	private static Calendar ParseReferenceDate(String unitString)
	{
		Calendar reference_zero = new GregorianCalendar();
		String referenceDateString;

		if(unitString.contains(TIME_UNIT_SECONDS)) referenceDateString = unitString.replace(TIME_UNIT_SECONDS, "").trim();
		else if(unitString.contains(TIME_UNIT_MINUTES)) referenceDateString = unitString.replace(TIME_UNIT_MINUTES, "").trim();
		else if(unitString.contains(TIME_UNIT_HOURS)) referenceDateString = unitString.replace(TIME_UNIT_HOURS, "").trim();
		else throw new RuntimeException(String.format("Error parsing Date unit: %s", unitString));

		try	{ reference_zero.setTime(DATE_FORMAT.parse(referenceDateString)); }
		catch(Exception ex)	{ throw new RuntimeException(String.format("%s, Error parsing Date value: %s", ex.getMessage(), referenceDateString)); }

		return reference_zero;
	}
}
