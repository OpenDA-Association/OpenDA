/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.exchange.timeseries;

import org.openda.utils.SortUtils;
import org.openda.utils.Time;
import ucar.nc2.units.DateUnit;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import static java.lang.Double.*;

/**
 * This class provides some static functions to convert between time formats. For now this is between Modified Julian Date and a
 * formatted string like "200812312355"
 *
 * Known limitation: uses millis since 1970 stored in a long. This will fail after approximately 2038 and before approximately
 * 1902
 *
 * @author verlaanm
 *
 */

public class TimeUtils {
   private static final double mjdAtJanFirst1970 = 40587.0;
   private static final double millisToDays      = 1.0 / (1000 * 60 * 60 * 24);
   private static final double daysToMillis      = (1000 * 60 * 60 * 24);
   private static final double minutesToDays     = 1.0 / (60 * 24);
   private static final double daysToMinutes     = 60 * 24;
   private static final long hoursToMillis       = 1000 * 60 * 60;
   private static final long minutesToMillis     = 1000 * 60;

   // correct for local timezome; no obvious ways to avoid this.

   /**
    * Convert Modified Julian Date (days since 00:00 November 17, 1858 UTC) to lexical string
    *
    * @param mjd
    *           Modified Julian Date
    * @return date as string yyyyMMddHHmm e.g. 200912292359 for Dec 29 2009 23:59 UTC
    */
   public static String mjdToString(double mjd) {
	   return mjdToString(mjd, "yyyyMMddHHmm", TimeZone.getTimeZone("UTC"));
   }

   /**
    * Convert Modified Julian Date (days since 00:00 November 17, 1858 UTC) to lexical string
    *
    * @param mjd
    *           Modified Julian Date
    * @param format
    *           The format string to use
    * @return date as string
    */
   public static String mjdToString(double mjd, String format, TimeZone tz) {
      final long timeInMillis = Math.round((mjd - mjdAtJanFirst1970) * daysToMillis);
      final Date t = new java.util.Date(timeInMillis);
      final SimpleDateFormat formatter = new SimpleDateFormat(format);
      formatter.setTimeZone(tz);
	   return formatter.format(t);
   }

   /**
    * Convert Modified Julian Date (days since 00:00 November 17, 1858 UTC) to lexical string
    *
    * @param mjd
    *           Modified Julian Date
    * @param format
    *           The format string to use
    * @return date as string
    */
   public static String mjdToString(double mjd, String format) {
      final TimeZone tz = TimeZone.getTimeZone("UTC");
      final long timeInMillis = Math.round((mjd - mjdAtJanFirst1970) * daysToMillis);
      final Date t = new java.util.Date(timeInMillis);
      final SimpleDateFormat formatter = new SimpleDateFormat(format);
      formatter.setTimeZone(tz);
	   return formatter.format(t);
   }

   /**
    * Add days, hours, minutes to Modified Julian Date (days since 00:00 November 17, 1858 UTC) and return the result
    *
    * @param mjd
    *           Modified Julian Date
    * @param days
    *           number of days to add
    * @param hours
    *           number of hours to add
    * @param minutes
    *           number of minutes to add
    * @return result of addition in Modified Julian Date
    */
   public static double addDHMToMjd(double mjd, int days, int hours, int minutes) {
      double timeInMillis = (mjd - mjdAtJanFirst1970) * daysToMillis;
      double daysInMillis = days * daysToMillis;
      double hoursInMillis = hours * (1000 * 60 * 60);
      double minutesInMillis = minutes * (1000 * 60);

      double newTimeInMillis = timeInMillis + daysInMillis + hoursInMillis + minutesInMillis;
      return newTimeInMillis * millisToDays + mjdAtJanFirst1970;

   }

   /**
    * Add minutes to Modified Julian Date and return the result
    *
    * @param mjd
    *           Modified Julian Date
    * @param minutes
    *           number of minutes to add
    * @return result of addition ni Modified Julian Date
    */
   public static double addMinutesToMjd(double mjd, double minutes) {
      double timeInMinutes = (mjd - mjdAtJanFirst1970) * daysToMinutes;
      double newTimeInMinutes = timeInMinutes + minutes;
      return newTimeInMinutes * minutesToDays + mjdAtJanFirst1970;
   }

   /**
    * Recalculate the number of days (Modified Julian date) to minutes
    *
    * @param mjd
    *           Modified Julian Date
    * @return minutes since 00:00 November 17, 1858 UTC
    */
   public static double mjdInMinutes(double mjd) {
      return Math.round(mjd * daysToMinutes);
   }

   /**
    * Convert Modified Julian Date to a string of the format: <DAYS> <HOURS> <MINUTES>
    *
    * @param mjd
    *           Modified Julian Date
    * @return String of modified julian date in the form 'd h m'
    */
   @SuppressWarnings("boxing")
   public static String mjdToDHMAsString(double mjd) {
      double minutes = mjdInMinutes(mjd);
      int days = (int) Math.floor(minutes / daysToMinutes);
      minutes -= days * daysToMinutes;
      int hours = (int) Math.floor(minutes / 60);
      minutes -= hours * 60;
      int resMinutes = (int) minutes;
      return String.format("%3d %3d %2d", days, hours, resMinutes);
   }

   /**
    * Convert date-time as string to Modified Julian Date (days since 00:00 November 17, 1858 UTC)
    *
    * @param date
    *           as string yyyyMMddHHmm e.g. 200912292359 for Dec 29 2009 23:59 UTC
    * @return mjd Modified Julian Date
    * @throws ParseException
    *            Exception thrown when the parsing failed.
    */
   public static double date2Mjd(String date) throws ParseException {
	   return date2Mjd(date, TimeZone.getTimeZone("UTC"));
   }

   /**
    * Convert date-time as string to Modified Julian Date (days since 00:00 November 17, 1858 UTC)
    *
    * @param date
    *           as string yyyyMMddHHmm e.g. 200912292359 for Dec 29 2009 23:59 UTC
    * @return mjd Modified Julian Date
    * @throws ParseException
    *            Exception thrown when the parsing failed.
    */
   public static double date2Mjd(String date, TimeZone tz) throws ParseException {
      Date t;
      if (date.length() == 12) {
         SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmm", Locale.UK);
         formatter.setTimeZone(tz);
         t = formatter.parse(date);
      }
      else if (date.length() == 14) {
         SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmmss", Locale.UK);
         formatter.setTimeZone(tz);
         t = formatter.parse(date);
      }
	  else if (date.length() == 19) {
		  SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.UK);
		  formatter.setTimeZone(tz);
		  t = formatter.parse(date);
	  }
      else {
         throw new ParseException("DateTime string did not match length of formats yyyyMMddHHmm or yyyyMMddHHmmss;"
                  + " arg was " + date, 0);
      }
	   return (t.getTime()) * millisToDays + mjdAtJanFirst1970; // convert from millis to days and add offset for mjd
   }

   /**
    * Convert formatted string to mjd Modified Julian Date (days since 00:00 November 17, 1858 UTC)
    *
    * @param date
    *           formatted string (matching format)
    * @param format
    *           coded as java date format e.g. yyyy for eg. 2009
    * @return mjd Modified Julian Date
    * @throws ParseException
    *            Exception thrown when the parsing failed.
    */
   public static double date2Mjd(String date, String format) throws ParseException {
      TimeZone tz = TimeZone.getTimeZone("GMT");
      SimpleDateFormat formatter = new SimpleDateFormat(format, Locale.UK);
      formatter.setTimeZone(tz);
      Date t;
      t = formatter.parse(date);
      return (t.getTime()) * millisToDays + mjdAtJanFirst1970; // convert from millis to days and add offset for mjd
   }

   /**
    * Convert Modified Julian Date (days since 00:00 November 17, 1858 UTC) to lexical string
    *
    * @param mjd
    *           array of Modified Julian Date
    * @return date as array of string yyyyMMddHHmm e.g. 200912292359 for Dec 29 2009 23:59 UTC
    */
   public static String[] mjdToString(double[] mjd) {
      TimeZone tz = TimeZone.getTimeZone("GMT");
      SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmm");
      formatter.setTimeZone(tz);
      String result[] = new String[mjd.length];
      for (int i = 0; i < mjd.length; i++) {
         long timeInMillis = Math.round((mjd[i] - mjdAtJanFirst1970) * daysToMillis);
         Date t = new java.util.Date(timeInMillis);
         result[i] = formatter.format(t);
      }
      return result;
   }

   /**
    * Convert date-time as string to Modified Julian Date (days since 00:00 November 17, 1858 UTC)
    *
    * @param dates
    *           as array of string yyyyMMddHHmm e.g. 200912292359 for Dec 29 2009 23:59 UTC
    * @param dummy
    *           value to use when date can not be parsed
    * @return mjd[] as array of double Modified Julian Date
    */
   public static double[] date2Mjd(String[] dates, double dummy) {
      TimeZone tz = TimeZone.getTimeZone("GMT");
      SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmm");
      formatter.setTimeZone(tz);
      Date t;
      double result[] = new double[dates.length];
      for (int i = 0; i < dates.length; i++) {
         try {
            t = formatter.parse(dates[i]);
            result[i] = (t.getTime()) * millisToDays + mjdAtJanFirst1970; // convert from millis to days and add offset for mjd
         }
         catch (Exception e) {
            result[i] = dummy;
         }
      }
      return result;
   }

   /**
    * Join some strings
    *
    * @param strings
    *           Array of strings to join.
    * @param delimiter
    *           The delimiter to use between the strings to be joined.
    * @return A string consisting of the array strings, delimited by the delimiter.
    */
   public static String joinStrings(String[] strings, String delimiter) {
      String result = "";
      int last = strings.length - 1;
      for (int i = 0; i <= last; i++) {
         result += strings[i];
         if (i < last) result += delimiter;
      }
      return result;
   }

   /**
    * Convert a sequence of dateTimes to an array of Modified Julian Dates. There are two possible formats:
    * "201008240000,201008240600,201008241200" for a possibly irregular fixed list and
    * "201008240000,201008240100,...,201008241200" for a regular sequence, where the step-size is determined from the first two
    * values. The sequence is extended with equal steps until the final time.
    *
    * @param input
    *           string with sequence of dateTimes,
    * @return Mjd's as doubles
    */
   public static double[] dateTimeSequenceString2Mjd(String input) {
      double result[];
      String[] dateTimes = input.split(",");
      int n = dateTimes.length;
      if ((n == 4) && (dateTimes[2].equals("..."))) {
         double tfirst;
         double tsecond;
         double tlast;

         // regular sequence
         try {
            tfirst = TimeUtils.date2Mjd(dateTimes[0]);
         }
         catch (ParseException e) {
            throw new RuntimeException("Problem parsing dateTimeSequence at first element ="
                     + " value=" + dateTimes[0]);
         }
         try {
            tsecond = TimeUtils.date2Mjd(dateTimes[1]);
         }
         catch (ParseException e) {
            throw new RuntimeException("Problem parsing dateTimeSequence at second element ="
                     + " value=" + dateTimes[1]);
         }
         try {
            tlast = TimeUtils.date2Mjd(dateTimes[3]);
         }
         catch (ParseException e) {
            throw new RuntimeException("Problem parsing dateTimeSequence at last element ="
                     + " value=" + dateTimes[3]);
         }
         double tstep = tsecond - tfirst;
         if (tstep <= 0) throw new RuntimeException("timestep for regular sequence should be positive"
                  + dateTimes[0] + ">=" + dateTimes[1]);
         int nSequence = (int) Math.floor((tlast - tfirst) / tstep) + 1;
         result = new double[nSequence];
         for (int i = 0; i < nSequence; i++) {
            result[i] = tfirst + i * tstep;
         }
      }
      else {
         // irregular sequence
         result = new double[n];
         double previous = NEGATIVE_INFINITY;
         for (int i = 0; i < n; i++) {
            try {
               result[i] = TimeUtils.date2Mjd(dateTimes[i]);
            }
            catch (ParseException e) {
               throw new RuntimeException("Problem parsing dateTimeSequence at element with index =" + i
                        + " value=" + dateTimes[i]);
            }
            if (result[i] <= previous) { throw new RuntimeException(
                     "dateTimeSequenceShould be increasing (larger than i-1) at element with index=" + i); }
         }
      }
      return result;
   }

   /**
    * Parses a sequence of Modified Julian day values to an array of Modified Julian Dates. There are two possible formats:
    * "55432.0,55432.25,55432.50" for a possibly irregular fixed list and "55432.0,55432.014666666666,...,55432.50" for a
    * regular sequence, where the step-size is determined from the first two values. The sequence is extended with equal steps
    * until the final time.
    *
    * @param input
    *           string with sequence of dateTimes,
    * @return Mjd's as doubles
    */
   public static double[] MjdSequenceString2Mjd(String input) {
      double result[];
      String[] dateTimes = input.split(",");
      int n = dateTimes.length;
      if ((n == 4) && (dateTimes[2].equals("..."))) {
         double tfirst;
         double tsecond;
         double tlast;

         // regular sequence
         try {
            tfirst = parseDouble(dateTimes[0]);
         }
         catch (NumberFormatException e) {
            throw new RuntimeException("Problem parsing MjdSequence at first element ="
                     + " value=" + dateTimes[0]);
         }
         try {
            tsecond = parseDouble(dateTimes[1]);
         }
         catch (NumberFormatException e) {
            throw new RuntimeException("Problem parsing MjdSequence at second element ="
                     + " value=" + dateTimes[1]);
         }
         try {
            tlast = parseDouble(dateTimes[3]);
         }
         catch (NumberFormatException e) {
            throw new RuntimeException("Problem parsing MjdSequence at last element ="
                     + " value=" + dateTimes[3]);
         }
         double tstep = tsecond - tfirst;
         if (tstep <= 0) throw new RuntimeException("timestep for regular sequence should be positive"
                  + dateTimes[0] + ">=" + dateTimes[1]);
         int nSequence = (int) Math.floor((tlast - tfirst) / tstep) + 1;
         result = new double[nSequence];
         for (int i = 0; i < nSequence; i++) {
            result[i] = tfirst + i * tstep;
         }
      }
      else {
         // irregular sequence
         result = new double[n];
         double previous = NEGATIVE_INFINITY;
         for (int i = 0; i < n; i++) {
            try {
               result[i] = parseDouble(dateTimes[i]);
            }
            catch (NumberFormatException e) {
               throw new RuntimeException("Problem parsing MjdSequence at element with index =" + i
                        + " value=" + dateTimes[i]);
            }
            if (result[i] <= previous) { throw new RuntimeException(
                     "MjdSequence should be increasing (larger than i-1) at element with index=" + i); }
         }
      }
      return result;
   }

    public static TimeZone createTimeZoneFromDouble(double timeZoneOffsetInHours) {
        long offsetInMinutes = Math.round(timeZoneOffsetInHours * 60d);
        if (offsetInMinutes == 0) {
            return TimeZone.getTimeZone("GMT");
        }
        return TimeZone.getTimeZone(getTimeZoneString(offsetInMinutes * minutesToMillis));
    }

    /**
     * @param timeZoneOffsetMillis has to be between -12 and +12 hours and has to be rounded on minutes.
     * @return String timeZone.
     */
    public static String getTimeZoneString(long timeZoneOffsetMillis) {
        if (timeZoneOffsetMillis % minutesToMillis != 0) {
            throw new IllegalArgumentException("timeZoneOffsetMillis % minutesToMillis != 0");
        }

        if (Math.abs(timeZoneOffsetMillis) > 12 * hoursToMillis) {
            throw new IllegalArgumentException("Math.abs(timeZoneOffsetMillis) > 12 * hoursToMillis");
        }

        char sign = timeZoneOffsetMillis >= 0 ? '+' : '-';
        long unsignedTimeZoneOffsetMillis = Math.abs(timeZoneOffsetMillis);
        long minuteMillis = unsignedTimeZoneOffsetMillis % hoursToMillis;
        long hourMillis = unsignedTimeZoneOffsetMillis - minuteMillis;

        int minutes = (int) (minuteMillis / minutesToMillis);
        int hours = (int) (hourMillis / hoursToMillis);

        if (minutes == 0) {
            return "GMT" + sign + hours;
        }

        if (minutes <= 9) {
            return "GMT" + sign + hours + ":0" + minutes;
        } else {
            return "GMT" + sign + hours + ':' + minutes;
        }
    }

	public static long getDateTimeFromCastor(org.exolab.castor.types.Date date, org.exolab.castor.types.Time time, TimeZone timeZone) {
		if (date == null) {
			throw new IllegalArgumentException("date is null.");
		}
		if (time == null) {
			throw new IllegalArgumentException("time is null.");
		}
		if (timeZone == null) {
			throw new IllegalArgumentException("timeZone is null.");
		}

		GregorianCalendar calendar = new GregorianCalendar(timeZone);

		calendar.set(Calendar.YEAR, date.getCentury() * 100 + date.getYear());
		calendar.set(Calendar.MONTH, date.getMonth() - 1);
		calendar.set(Calendar.DAY_OF_MONTH, date.getDay());
		calendar.set(Calendar.HOUR_OF_DAY, time.getHour());
		calendar.set(Calendar.MINUTE, time.getMinute());
		calendar.set(Calendar.SECOND, time.getSeconds());
		calendar.set(Calendar.MILLISECOND, time.getMilli());

		return calendar.getTimeInMillis();
	}

	/**
	 * Returns an array with all times that are in the given timeHorizon, except
	 * the beginTime of the timeHorizon.
	 * The timeStep between times is the step of the given timeHorizon.
	 * The last time is the endTime of the horizon or the earliest time
	 * after the endTime of the timeHorizon if the timeHorizon period is not
	 * an integer multiple of the timeStep length.
	 *
	 * @param timeHorizon the time horizon
	 * @return times times in the time horizon
	 */
    @Deprecated
	public static double[] getOutputTimes(Time timeHorizon) {
		//to avoid rounding errors, here first round period and timeStep to whole seconds.
		long startTime = Time.mjdToMillies(timeHorizon.getBeginTime().getMJD());
		long endTime = Time.mjdToMillies(timeHorizon.getEndTime().getMJD());
		long periodInSeconds = (endTime - startTime)/1000;
		long timeStepInSeconds = Math.round(timeHorizon.getStepMJD() * 24 * 3600);
		int timeCount = (int) Math.ceil((double) periodInSeconds / ((double) timeStepInSeconds));

		double[] times = new double[timeCount];
		double timeStepInDays = (double) timeStepInSeconds/(3600.0 * 24.0);
		double time = Time.milliesToMjd(startTime);
		for (int n = 0; n < times.length; n++) {
			time += timeStepInDays;
			times[n] = time;
		}

		return times;
	}

	/**
	 * Find the matching time index in a sorted double[] array of times.
	 * @param values list of values to search through. The array is sorted in increasing order.
	 * @param toFind (inf and -inf are NOT allowed)
	 * @param tolerance in what range to accept as the same
	 * @return Index of matching value or -1 if not found
	 */
    public static int findMatchingTimeIndex(double values[], double toFind, double tolerance){
    	return SortUtils.findMatchingIndex(values, toFind, tolerance);
    }

	public static double udUnitsTimeToMjd(double udUnitsTime, String udUnitsTimeUnitsString) {
		DateUnit dateUnit = createDateUnit(udUnitsTimeUnitsString);
		try {
			Date date = dateUnit.makeDate(udUnitsTime);
			return Time.milliesToMjd(date.getTime());
		} catch (Exception e) {
			throw new RuntimeException("Cannot parse time " + udUnitsTime + " using UDUNITS time units '" + udUnitsTimeUnitsString + "'. Message was: " + e.getMessage(), e);
		}
	}

	public static double mjdToUdUnitsTime(double mjd, String udUnitsTimeUnitsString) {
		DateUnit dateUnit = createDateUnit(udUnitsTimeUnitsString);
		return dateUnit.makeValue(new Date(Time.mjdToMillies(mjd)));
	}

	private static DateUnit createDateUnit(String udUnitsTimeUnitsString) {
		try {
			return new DateUnit(udUnitsTimeUnitsString);
		} catch (Exception e) {
			throw new RuntimeException("Cannot create DateUnit from UDUNITS time units '" + udUnitsTimeUnitsString + "'. Message was: " + e.getMessage(), e);
		}
	}

	private static final String TIME_UNIT_SECONDS = "seconds since";
	private static final String TIME_UNIT_MINUTES = "minutes since";
	private static final String TIME_UNIT_HOURS = "hours since";

	private static final long ONE_DAY_IN_MILLISECONDS = 86400000;
	private static final long ONE_HOUR_IN_MILLISECONDS = 3600000;
	private static final long ONE_MINUTE_IN_MILLISECONDS = 60000;
	private static final long ONE_SECOND_IN_MILLISECONDS = 1000;


	public static double[] ConvertBcTimesToModifiedJulianDays(String timeUnitString, List<Double> timeOffsets)
	{
		double mjdReference = referenceDateTimeToMJD(timeUnitString);
		double multFactToMillies = getUnitMultiplicationFactor(timeUnitString);

		double[] modifiedJulianDayValues = new double[timeOffsets.size()];
		for (int i = 0; i < timeOffsets.size(); i++) {
			double milliesSinceReference = timeOffsets.get(i) * multFactToMillies;
			double mjdValue = (milliesSinceReference / (double) ONE_DAY_IN_MILLISECONDS) + mjdReference;
			modifiedJulianDayValues[i]= mjdValue;
		}
		return modifiedJulianDayValues;
	}

	public static List<Double> ConvertModifiedJulianDaysToBcTimes(String timeUnitString, double[] modifiedJulianDayValues)
	{
		double mjdReference = referenceDateTimeToMJD(timeUnitString);
		double multFactFromMillies = getUnitMultiplicationFactor(timeUnitString);

		List<Double> timeOffsets = new ArrayList<>();
		for(double mjdValue : modifiedJulianDayValues)
		{
			double bcTimeAsMjd = mjdValue - mjdReference;
			Double bcTime = Math.round(bcTimeAsMjd * (double) ONE_DAY_IN_MILLISECONDS / multFactFromMillies * 100d) / 100d;
			timeOffsets.add(bcTime);
		}
		return timeOffsets;
	}

	private static double getUnitMultiplicationFactor(String timeUnitString)
	{
		double multiplicationFactor;
		if(timeUnitString.contains(TIME_UNIT_SECONDS)) multiplicationFactor = ONE_SECOND_IN_MILLISECONDS;
		else if(timeUnitString.contains(TIME_UNIT_MINUTES)) multiplicationFactor = ONE_MINUTE_IN_MILLISECONDS;
		else if(timeUnitString.contains(TIME_UNIT_HOURS)) multiplicationFactor = ONE_HOUR_IN_MILLISECONDS;
		else throw new RuntimeException(String.format("Error reference time string unit: %s", timeUnitString));
		return multiplicationFactor;
	}

	private static double referenceDateTimeToMJD(String timeUnitString)
	{
		String referenceDateString;
		if(timeUnitString.contains(TIME_UNIT_SECONDS)) referenceDateString = timeUnitString.replace(TIME_UNIT_SECONDS, "").trim();
		else if(timeUnitString.contains(TIME_UNIT_MINUTES)) referenceDateString = timeUnitString.replace(TIME_UNIT_MINUTES, "").trim();
		else if(timeUnitString.contains(TIME_UNIT_HOURS)) referenceDateString = timeUnitString.replace(TIME_UNIT_HOURS, "").trim();
		else throw new RuntimeException(String.format("Error parsing Date unit: %s", timeUnitString));

		double referenceDateAsMJD;
		try {
			referenceDateAsMJD = TimeUtils.date2Mjd(referenceDateString, "yyyy-MM-dd HH:mm:ss");
		} catch (ParseException e) {
			throw new RuntimeException(String.format("Error parsing time unit string: %s", referenceDateString));
		}
		return referenceDateAsMJD;
	}
}
