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
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * IoObject for a .DAT (EFDC grid output) file that contains one or more grid time series.
 * The .DAT file with grid data is generated from the binary model output
 * files from the EFDC (Environmental Fluid Dynamics Code) model,
 * using a tool called POST_PCfile_ASCII.exe. This tool was developed by
 * EnssoHitech for NIER (National Institute of Environmental Research) in Korea.
 *
 * @author Arno Kockx
 */
public class EfdcGridTimeSeriesIoObject implements IoObjectInterface {

    /**
     * The timeZone that is used by the model.
     * This is required to convert the times of the data values
     * to/from the timeZone that is used by the model.
     * Default is GMT.
     */
    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    private File timeSeriesFile;
    private Map<Integer, IPrevExchangeItem> timeSeriesExchangeItems = new LinkedHashMap<Integer, IPrevExchangeItem>();

    /**
     * @param workingDir the working directory.
     * @param fileName the name of the file containing the data for this IoObject (relative to the working directory).
     * @param arguments the first argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  the second argument should be the startTime of the model run.
     */
    @Override
    public void initialize(File workingDir, String fileName, String[] arguments) {
        this.timeSeriesFile = new File(workingDir, fileName);

        //get timeZone.
        if (arguments == null || arguments.length < 1) {
            throw new IllegalArgumentException("No timeZone argument specified for " + this.getClass().getSimpleName()
                    + ". The first argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).");
        }
        try {
            double timeZoneOffsetInHours = Double.parseDouble(arguments[0]);
            this.timeZone = TimeUtils.createTimeZoneFromDouble(timeZoneOffsetInHours);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse first argument '" + arguments[0]
                    + "' for " + this.getClass().getSimpleName()
                    + ". The first argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).", e);
        }

        //get start time.
        if (arguments.length < 2) {
            throw new IllegalArgumentException("No startTime argument specified for " + this.getClass().getSimpleName()
                    + ". The second argument should be the start time of the model run.");
        }
        double startTime;
        try {
            startTime = TimeUtils.date2Mjd(arguments[1]);
        } catch (ParseException e) {
            throw new IllegalArgumentException("Invalid startTime argument specified for " + this.getClass().getSimpleName()
                    + ". Cannot parse second argument '" + arguments[1]
                    + "'. The second argument should be the startTime of the model run.", e);
        }
        //determine referenceTime.
        long referenceTime = EfdcUtils.getReferenceTime(Time.mjdToMillies(startTime), this.timeZone);

        //read data from file and create exchangeItems.
        createTimeSeriesExchangeItems(this.timeSeriesFile, referenceTime);
    }

    /**
     * Reads the data from the given inputFile and creates an
     * exchangeItem for each data column in the file.
     *
     * @param inputFile
     */
    private void createTimeSeriesExchangeItems(File inputFile, long referenceTime) {
        if (!inputFile.exists()) {
            throw new RuntimeException("Input file '" + inputFile.getAbsolutePath()
                    + "' does not exist.");
        }
        Results.putMessage(this.getClass().getSimpleName() + ": reading output grid time series data from file "
                + inputFile.getAbsolutePath());

        int columnCount = readColumnCount(inputFile);
        Results.putMessage(this.getClass().getSimpleName() + ": " + columnCount + " columns found in file "
                + inputFile.getAbsolutePath());
        if (columnCount < 5) {
            throw new RuntimeException("No data to read in file '" + inputFile.getAbsolutePath()
                    + "'. This file contains less then 5 columns. 5 or more columns expected.");
        }

        int cellCount = readCellCount(inputFile);
        Results.putMessage(this.getClass().getSimpleName() + ": " + cellCount + " grid cells found in file "
                + inputFile.getAbsolutePath());
        if (cellCount < 1) {
            throw new RuntimeException("No data to read in file " + inputFile.getAbsolutePath());
        }

        double[] times = readTimes(inputFile, cellCount, referenceTime);
        Results.putMessage(this.getClass().getSimpleName() + ": " + times.length + " times found in file "
                + inputFile.getAbsolutePath());
        if (times.length < 1) {
            throw new RuntimeException("No data to read in file " + inputFile.getAbsolutePath());
        }

        double[][][] data = readData(inputFile, columnCount, times.length, cellCount);

        createTimeSeriesExchangeItems(times, data);
    }

    /**
     * Returns the number of columns for the data in the given inputFile.
     *
     * @param inputFile
     */
    private int readColumnCount(File inputFile) {
        if (!inputFile.exists()) {
            throw new RuntimeException("Input file '" + inputFile.getAbsolutePath()
                    + "' does not exist.");
        }

        int columnCount = 0;

        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFile));

            try {
                //read header line.
                String line = reader.readLine();
                if (line == null) {
                    throw new RuntimeException("No data to read in file " + inputFile.getAbsolutePath());
                }

                //read first line to count number of parameters.
                line = reader.readLine();
                if (line != null) {
                    String[] columns = line.trim().split("\\s+");
                    if (columns != null) {
                        columnCount = columns.length;
                    }
                }

            } finally {
               reader.close();
            }

        } catch (IOException e){
            throw new RuntimeException("Problem while reading file '" + inputFile.getAbsolutePath() + "'.", e);
        }

        return columnCount;
    }

    /**
     * Returns the number of grid cells for the first time for the data in the given inputFile.
     *
     * @param inputFile
     */
    private int readCellCount(File inputFile) {
        if (!inputFile.exists()) {
            throw new RuntimeException("Input file '" + inputFile.getAbsolutePath()
                    + "' does not exist.");
        }

        int cellCount = 0;

        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFile));

            try {
                //read header line.
                String line = reader.readLine();
                if (line == null) {
                    throw new RuntimeException("No data to read in file " + inputFile.getAbsolutePath());
                }

                //read first column until the time changes, to count the number of grid cells.
                String firstTime = null;
                line = reader.readLine();
                while (line != null) {
                    String[] columns = line.trim().split("\\s+");
                    if (columns == null || columns.length < 1) {
                        throw new IllegalArgumentException("Invalid empty line in file " + inputFile.getAbsolutePath());
                    }

                    String time = columns[0];
                    if (firstTime == null) {
                        firstTime = time;
                    }

                    if (time.equalsIgnoreCase(firstTime)) {
                        cellCount++;
                    } else {
                        break;
                    }

                    line = reader.readLine();
                }

            } finally {
               reader.close();
            }

        } catch (IOException e){
            throw new RuntimeException("Problem while reading file '" + inputFile.getAbsolutePath() + "'.", e);
        }

        return cellCount;
    }

    /**
     * Returns an array with all unique times for the data in the given inputFile.
     *
     * @param inputFile
     * @param cellCount
     * @param referenceTime
     * @return array with absolute times.
     */
    private double[] readTimes(File inputFile, int cellCount, long referenceTime) {
        if (!inputFile.exists()) {
            throw new RuntimeException("Input file '" + inputFile.getAbsolutePath()
                    + "' does not exist.");
        }

        List<Double> timesList = new ArrayList<Double>();

        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFile));

            try {
                //read header line.
                String line = reader.readLine();
                if (line == null) {
                    throw new RuntimeException("No data to read in file " + inputFile.getAbsolutePath());
                }

                //read first column and store all unique times.
                line = reader.readLine();
                while (line != null) {
                    //read time.
                    String[] columns = line.trim().split("\\s+");
                    if (columns == null || columns.length < 1) {
                        throw new IllegalArgumentException("Invalid empty line in file " + inputFile.getAbsolutePath());
                    }
                    double time = parseDouble(columns[0], inputFile);
                    timesList.add(time);

                    //read until next time.
                    int cellIndex = 1;
                    line = reader.readLine();
                    while (line != null && cellIndex < cellCount) {
                        cellIndex++;
                        line = reader.readLine();
                    }
                }

            } finally {
               reader.close();
            }

        } catch (IOException e){
            throw new RuntimeException("Problem while reading file '" + inputFile.getAbsolutePath() + "'.", e);
        }

        //convert Double list with relative times to double array with absolute times.
        double[] times = new double[timesList.size()];
        for (int n = 0; n < timesList.size(); n++) {
            double relativeTime = timesList.get(n);
            times[n] = Time.milliesToMjd(EfdcUtils.getAbsoluteTime(relativeTime,
                    referenceTime, EfdcTimeSeriesIoObject.SECONDS_PER_TIME_UNIT));
        }

        return times;
    }

    /**
     * Returns an array with all data in the given inputFile.
     *
     * @param columnCount
     * @param timeCount
     * @param cellCount
     * @return double[][][] array.
     */
    private double[][][] readData(File inputFile, int columnCount, int timeCount, int cellCount) {
        if (!inputFile.exists()) {
            throw new RuntimeException("Input file '" + inputFile.getAbsolutePath()
                    + "' does not exist.");
        }

        int parameterCount = columnCount - 4;
        double[][][] data = new double[parameterCount][timeCount][cellCount];

        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFile));

            try {
                //read header line.
                String line = reader.readLine();
                if (line == null) {
                    throw new RuntimeException("No data to read in file " + inputFile.getAbsolutePath());
                }

                //loop over times.
                int timeIndex = 0;
                line = reader.readLine();
                while (line != null && timeIndex < timeCount) {

                    //read all grid cells for this time.
                    //this assumes the grid cells are in the correct order.
                    int cellIndex = 0;
                    while (line != null && cellIndex < cellCount) {
                        String[] columns = line.trim().split("\\s+");
                        if (columns == null || columns.length < columnCount) {
                            throw new IllegalArgumentException("Invalid line '" + line + "' in file '"
                                    + inputFile.getAbsolutePath() + "'. " + columnCount + " columns expected.");
                        }

                        for (int columnIndex = 4; columnIndex < columnCount; columnIndex++) {
                            double value = parseDouble(columns[columnIndex], inputFile);
                            int parameterIndex = columnIndex - 4;
                            data[parameterIndex][timeIndex][cellIndex] = value;
                        }

                        cellIndex++;
                        line = reader.readLine();
                    }

                    timeIndex++;
                }

            } finally {
               reader.close();
            }

        } catch (IOException e){
            throw new RuntimeException("Problem while reading file '" + inputFile.getAbsolutePath() + "'.", e);
        }

        return data;
    }

    private static double parseDouble(String string, File inputFile) {
        try {
            return Double.parseDouble(string);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Cannot parse string '" + string
                    + "' in inputFile '" + inputFile.getAbsolutePath() + "'.", e);
        }
    }

    private void createTimeSeriesExchangeItems(double[] times, double[][][] data) {
        //reset this.timeSeriesExchangeItems list.
        this.timeSeriesExchangeItems.clear();

        //create an exchangeItem for each data column. If only some columns are used,
        //then the whole file has to be read anyway, so always filling all column exchangeItems
        //if some are not used will not take much extra time.
        for (int parameterIndex = 0; parameterIndex < data.length; parameterIndex++) {
            //use the columnNumber (not columnIndex) as the id for the exchangeItem.
            int columnNumber = parameterIndex + 5;
            IPrevExchangeItem exchangeItem = new EfdcGridTimeSeriesExchangeItem(String.valueOf(columnNumber));

            //set times and values.
            exchangeItem.setTimes(times);
            exchangeItem.setValues(data[parameterIndex]);

            //add exchangeItem.
            this.timeSeriesExchangeItems.put(columnNumber, exchangeItem);
        }

        if (this.timeSeriesExchangeItems.isEmpty()) {
            throw new IllegalArgumentException("No time series found for time series file '"
                    + this.timeSeriesFile.getAbsolutePath() + "'.");
        }
    }

    @Override
    public IPrevExchangeItem[] getExchangeItems() {
        //return all available exchange items.
        List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>(this.timeSeriesExchangeItems.values());
        return exchangeItems.toArray(new IPrevExchangeItem[exchangeItems.size()]);
    }

    @Override
    public void finish() {
        //do nothing.
    }
}
