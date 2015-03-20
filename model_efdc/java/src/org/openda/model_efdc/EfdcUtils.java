/* MOD_V2.0
 * Copyright (c) 2012 OpenDA Association
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

package org.openda.model_efdc;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * @author Arno Kockx
 */
public class EfdcUtils {

    /**
     * Read content of the given file and store it in a list with Strings.
     *
     * @param inputFile
     * @return ArrayList<String>
     */
    public static ArrayList<String> readFile(File inputFile) {
        ArrayList<String> content = new ArrayList<String>();

        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFile));
            try {
                String line = reader.readLine();
                while (line != null) {
                    content.add(line);
                    line = reader.readLine();
                }
            } finally {
               reader.close();
            }
        } catch (IOException e){
            throw new RuntimeException("Problem while reading file '" + inputFile.getAbsolutePath() + "'.", e);
        }

        return content;
    }

    /**
     * Write the given list with Strings to the given file.
     *
     * @param outputFile
     * @param content
     */
    public static void writeFile(File outputFile, ArrayList<String> content) {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(outputFile));
            try {
                for (int n = 0; n < content.size(); n++) {
                    writer.write(content.get(n));
                    writer.newLine();
                }
            } finally {
                writer.close();
            }
        } catch (IOException e){
            throw new RuntimeException("Problem while writing file '" + outputFile.getAbsolutePath() + "'.", e);
        }
    }

    public static int getLocationIdNumber(EfdcTimeSeriesExchangeItem timeSeries) {
        try {
            return Integer.parseInt(timeSeries.getLocation());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid location id for time series with id '"
                    + timeSeries.getId() + "'. The time series location id has to be an integer number.", e);
        }
    }

    /**
     * Returns the number of indices for which both the given arrays
     * have a non-missing value.
     *
     * @param times
     * @param values
     * @return the number of indices for which both the given arrays have a non-missing value.
     */
    public static int countNonMissingValues(double times[], double values[]) {
        if (times == null || values == null || times.length == 0 || values.length == 0) {
            return 0;
        }
        if (times.length != values.length) {
            throw new IllegalArgumentException("times must be the same length as values.");
        }

        int count = 0;
        for (int n = 0; n < times.length; n++) {
            if (!Double.isNaN(times[n]) && !Double.isNaN(values[n])) {
                count++;
            }
        }

        return count;
    }

    /**
     * Check if the given array contains only missing values.
     *
     * @param values
     * @return true if given array contains only missing values.
     */
    public static boolean containsOnlyMissing(double values[]) {
        if (values == null || values.length == 0) {
            return true;
        }

        for (int n = 0; n < values.length; n++) {
            if (!Double.isNaN(values[n])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Returns the reference time to use for times in the .INP input files.
     * The times in the .INP input files are relative to a referenceTime.
     * The EFDC model does not use absolute time, it uses only time relative
     * to an arbitrary reference time.
     * This referenceTime can be chosen here, as long as it is
     * applied consistently.
     *
     * @param startTime
     * @param timeZone
     * @return referenceTime.
     */
    public static long getReferenceTime(long startTime, TimeZone timeZone) {
        //currently for the EFDC model the reference time is defined
        //so that 1 January 00:00:00 of the year that contains the startTime
        //of the run, corresponds to a time of 1.0 days (see EVENT_TOX2.INP file).
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(timeZone);
        calendar.setTimeInMillis(startTime);
        calendar.set(calendar.get(Calendar.YEAR) - 1, 11, 31, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTimeInMillis();
    }

    /**
     * Returns the given time relative to the given reference time
     * in the units corresponding to the given secondsPerTimeUnit.
     *
     * @param absoluteTime
     * @param referenceTime
     * @param secondsPerTimeUnit
     * @return relativeTimeInTimeUnits.
     */
    public static double getRelativeTimeInTimeUnits(long absoluteTime, long referenceTime, double secondsPerTimeUnit) {
        //get time since referenceTime.
        long relativeTime = absoluteTime - referenceTime;
        //convert to seconds.
        double relativeTimeInSeconds = (double) relativeTime/1000.0;
        //convert to time units.
        return relativeTimeInSeconds/secondsPerTimeUnit;
    }

    /**
     * Returns the absolute time that corresponds to the given relativeTimeInTimeUnits,
     * referenceTime and secondsPerTimeUnit.
     *
     * @param relativeTimeInTimeUnits
     * @param referenceTime
     * @param secondsPerTimeUnit
     * @return absoluteTime.
     */
    public static long getAbsoluteTime(double relativeTimeInTimeUnits, long referenceTime, double secondsPerTimeUnit) {
        //convert to milliseconds.
        long relativeTime = Math.round(relativeTimeInTimeUnits * secondsPerTimeUnit * 1000);
        //return absolute time.
        return relativeTime + referenceTime;
    }
}
