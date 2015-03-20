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

package org.openda.model_efdc.timeseriesformat;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.model_efdc.EfdcTimeSeriesExchangeItem;
import org.openda.model_efdc.EfdcUtils;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * Abstract class for formatters for specific types of EFDC .INP files.
 *
 * @author Arno Kockx
 */
public abstract class EfdcTimeSeriesFormatter {

    public static final String CWQSR_FILE_TYPE = "CWQSR";
    public static final String QSER_FILE_TYPE = "QSER";
    public static final String TSER_FILE_TYPE = "TSER";
    public static final String PSER_FILE_TYPE = "PSER";
    public static final String ASER_FILE_TYPE = "ASER";

    protected final String fileType;
    /**
     * The times in the .INP input file are relative to this referenceTime.
     * Default is 1970-01-01 00:00:00.
     */
    protected long referenceTime = 0;
    /**
     * The times in the .INP input file are in this time unit.
     * Default is day.
     */
    protected double secondsPerTimeUnit = 24*60*60;
    /**
     * Stores file header lines.
     */
    private ArrayList<String> fileHeader = new ArrayList<String>();

    public EfdcTimeSeriesFormatter(String fileType) {
        this.fileType = fileType;
    }

    protected abstract String getFileType();

    /**
     * Reads the required parts of the given inputFile.
     *
     * @param inputFile
     */
    public abstract void readFile(File inputFile);

    /**
     * Removes the file header lines from the given list
     * and stores these lines in this.header.
     * All lines before the first non-empty line that is
     * not a comment line, are considered to be header comment lines.
     *
     * @param lines
     */
    protected void removeAndStoreFileHeader(List<String> lines) {
        this.fileHeader.clear();

        Iterator<String> iterator = lines.iterator();
        while (iterator.hasNext()) {
            String line = iterator.next();

            String trimmedLine = line.trim();
            if (trimmedLine.isEmpty() || trimmedLine.startsWith("C") || trimmedLine.startsWith("c")
                || trimmedLine.startsWith("E") || trimmedLine.startsWith("e")) {
                //if header comment line.
                //store original line.
                this.fileHeader.add(line);
                iterator.remove();
            } else {//if not a header comment line.
                return;
            }
        }
    }

    /**
     * Writes all given timeSeries to the given file.
     *
     * @param outputFile
     * @param timeSeriesMap
     */
    public void writeTimeSeriesToFile(File outputFile, Map<String, IPrevExchangeItem> timeSeriesMap) {
        if (timeSeriesMap == null || timeSeriesMap.isEmpty()) {
            throw new RuntimeException("No time series to write to file '" + outputFile.getAbsolutePath() + "'.");
        }

        //write.
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new FileWriter(outputFile));

            //write file header.
            writeFileHeader(writer);

            //write time series.
            //the model only uses the index number of the time series, i.e. the place in which it
            //is present in the .INP file. Therefore the time series must always be in the correct order.
            //Therefore here write the time series in the order of their location id number.
            int locationIdCount = timeSeriesMap.size();
            Results.putMessage(this.getClass().getSimpleName() + ": writing input time series for "
                    + locationIdCount + " locations to file " + outputFile.getAbsolutePath());
            for (int locationId = 1; locationId <= locationIdCount; locationId++) {
                writeTimeSeriesForLocation(writer, locationId, timeSeriesMap);
            }

            writer.flush();

        } catch (Exception e) {
            throw new RuntimeException("Exception while writing to file '" + outputFile.getAbsolutePath()
                    + "', message was: " + e.getMessage(), e);
        } finally {
            if (writer != null) {
                writer.close();
            }
        }
    }

    /**
     * Writes the file header that is stored in
     * this.header to the given writer.
     *
     * @param writer
     */
    protected void writeFileHeader(PrintWriter writer) {
        for (String line : this.fileHeader) {
            writer.println(line);
        }
    }

    /**
     * Writes all values in the TimeSeries for the given locationId
     * in the given timeSeriesMap to the given writer in EFDC input format.
     *
     * @param writer
     * @param locationId
     * @param timeSeriesMap
     */
    protected void writeTimeSeriesForLocation(PrintWriter writer, int locationId, Map<String, IPrevExchangeItem> timeSeriesMap) {
        EfdcTimeSeriesExchangeItem timeSeries =
                (EfdcTimeSeriesExchangeItem) timeSeriesMap.get(locationId + "." + getFileType());
        if (timeSeries == null) {
            throw new RuntimeException("No time series available to write to file "
                    + this.fileType + ".INP for locationId " + locationId + ".");
        }

        double[] times = timeSeries.getTimesRef();
        double[] values = timeSeries.getValuesRef();

        //Note: the model always expects the same total number of time series to be present
        //in the .INP file (see EFDC.INP file in sections C22 and C23).
        //Furthermore the model uses the index number of the time series,
        //i.e. the place in which it is present in the .INP file to refer to it in the
        //EFDC.INP file in section C24. Therefore all time series must always be present
        //and in the correct order. Therefore if only missing values, then
        //throw an exception.
        int numberOfNonMissingValues = EfdcUtils.countNonMissingValues(times, values);
        if (numberOfNonMissingValues <= 0) {//if no data.
            throw new RuntimeException("No data to write for time series with id '" + timeSeries.getId()
                    + "', time series is empty or contains only missing values.");
        }

        //write time series header.
        writeTimeSeriesHeaderForLocation(writer, locationId, numberOfNonMissingValues);

        //write data.
        for (int n = 0; n < times.length; n++) {
            if (Double.isNaN(times[n]) || Double.isNaN(values[n])) {//if missing value.
                //skip missing values, as the model apparently knows how to
                //interpolate between existing values.
                continue;
            }

            writeTimeValuePair(writer, times[n], values[n]);
        }
    }

    /**
     * Writes time series header for the given locationId to the given writer.
     * The format of the time series header depends on the type of .INP file.
     *
     * @param writer
     * @param locationId
     * @param numberOfNonMissingValues
     */
    protected abstract void writeTimeSeriesHeaderForLocation(PrintWriter writer,
            int locationId, int numberOfNonMissingValues);

    /**
     * Writes the given time and value to the given writer.
     *
     * @param writer
     * @param time
     * @param value
     */
    private void writeTimeValuePair(PrintWriter writer, double time, double value) {
        double timeInTimeUnits = EfdcUtils.getRelativeTimeInTimeUnits(Time.mjdToMillies(time),
                this.referenceTime, this.secondsPerTimeUnit);

        writer.print("  ");
        writer.print(timeInTimeUnits);
        writer.print(" ");
        writer.print(value);
        writer.println();
    }

    /**
     * Set the reference time. The times in the .INP input file
     * are relative to this reference time.
     *
     * @param referenceTime
     */
    public void setReferenceTime(long referenceTime) {
        this.referenceTime = referenceTime;
    }

    /**
     * Set the timeUnit for the times in the .INP input file.
     *
     * @param secondsPerTimeUnit
     */
    public void setTimeUnit(double secondsPerTimeUnit) {
        this.secondsPerTimeUnit = secondsPerTimeUnit;
    }
}
