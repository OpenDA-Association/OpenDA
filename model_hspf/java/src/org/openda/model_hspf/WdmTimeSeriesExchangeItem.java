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

package org.openda.model_hspf;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.utils.Mask;

/**
 * Exchange item for a time series stored in a wdm file.
 *
 * See http://water.usgs.gov/cgi-bin/man_wrdapp?wdm(1) :
 * A WDM file is a binary, direct-access file used to store
 * hydrologic, hydraulic, meteorologic, water-quality, and
 * physiographic data.  The WDM file is organized into data
 * sets (DSN = Data Set Number).  Each data set contains a specific type of data, such
 * as streamflow at a specific site or air temperature at a
 * weather station.  Each data set contains attributes that
 * describe the data, such as station identification number,
 * time step of data, latitude, and longitude.  A WDM file may
 * contain a single data set or as many as 200,000 data sets.
 * A data set may be described by a few attributes or by
 * hundreds of attributes.  Data can be added, deleted, and
 * modified without restructuring the data in the file.  Space
 * from deleted data sets is reused.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class WdmTimeSeriesExchangeItem extends TimeSeries {

    private WdmTimeSeriesIoObject wdmTimeSeriesIoObject;

    public WdmTimeSeriesExchangeItem(String id, Role role,
            WdmTimeSeriesIoObject wdmTimeSeriesIoObject) {
        super();

        this.role = role;

        String location = BBUtils.getLocationFromId(id);
        String parameter = BBUtils.getParameterFromId(id);
        setLocation(location);
        setQuantity(parameter);

        this.wdmTimeSeriesIoObject = wdmTimeSeriesIoObject;
    }

    /**
     * Gets the times of this series Makes a copy of any stored values, so
     * changing the result is safe and does not change the TimeSeries.
     *
     * @return Copy of the time array.
     */
    @Override
    public double[] getTimes() {
        readValuesAndTimesFromFile();
        return super.getTimes();
    }

    /**
     * Gets the values of this series Makes a copy of any stored values, so
     * changing the result is safe and does not change the TimeSeries.
     *
     * @return The value array as double array.
     */
    @Override
    public double[] getValuesAsDoubles() {
        readValuesAndTimesFromFile();
        return super.getValuesAsDoubles();
    }

    /**
     * Create a series by applying a mask on this series.
     *
     * @param mask
     *           Mask indicating which values should stay.
     *
     * @return A new time series with only the elements indicated by the mask.
     */
    @Override
    public TimeSeries selectMaskSubset(Mask mask) {
        readValuesAndTimesFromFile();
        return super.selectMaskSubset(mask);
    }

    /**
     * Create a series by retaining the specified time values only.
     *
     * @param onlyTimes
     *           Time array with times to keep.
     *
     * @return A new time series with only the elements matching the indicated time values.
     */
    @Override
    public TimeSeries selectTimeSubset(double[] onlyTimes) {
        readValuesAndTimesFromFile();
        return super.selectTimeSubset(onlyTimes);
    }

    /**
     * Create a selection in time of the TimeSeries,
     * i.e. select items with startTime<t[i]<=endTime.
     * Does NOT create a selection of the extraValues.
     *
     * @param startTime
     *           Start time.
     * @param endTime
     *           End time.
     * @return Time series containing the selection requested.
     */
    @Override
    public TimeSeries selectTimeSubset(double startTime, double endTime) {
        readValuesAndTimesFromFile();
        return super.selectTimeSubset(startTime, endTime);
    }

    /**
     * Create a selection in values of the TimeSeries, i.e. select items with
     * minValue<=value[i]<=maxValue Use Double,NaN to turn of one of the two
     * bounds.
     * Does NOT create a selection of the extraValues.
     *
     * @param minValue
     *           Minimum value.
     * @param maxValue
     *           Maximum value.
     * @return Time series containing the selection requested.
     */
    @Override
    public TimeSeries selectValueSubset(double minValue, double maxValue) {
        readValuesAndTimesFromFile();
        return super.selectValueSubset(minValue, maxValue);
    }

    /**
     * Provide raw access to the TimeSeries by returning as itself. This is more
     * or less equivalent to casting.
     *
     * @return this as TimeSeries object
     */
    @Override
    public Object getValues() {
        readValuesAndTimesFromFile();
        return super.getValues();
    }

    /**
     * Get the index associated with the given moment in time
     * (or -1 in case the value isn't found).
     * ASSUMES ORDERED TIMES
     *
     * @param time
     *           The time to search for.
     * @return The resulting index.
     */
    @Override
    public int findOnTime(double time) {
        readValuesAndTimesFromFile();
        return super.findOnTime(time);
    }

    /**
     * Get the value for the specified time.
     * If the exact time isn't found, Double.NaN is returned
     * (depending on the value set with setInterpolate()).
     * If the value is outside the array, Double.NaN is returned.
     * ASSUMES ORDERED TIMES
     *
     * @param time
     *           The time to search for.
     * @return The resulting value.
     */
    @Override
    public double getValue(double time) {
        readValuesAndTimesFromFile();
        return super.getValue(time);
    }

    /**
     * Get the value for the specified time.
     * If the exact time isn't found an interpolated value is returned
     * If the value is outside the array, Double.NaN is returned.
     * ASSUMES ORDERED TIMES
     *
     * @param time
     *           The time to search for.
     * @return The resulting value.
     */
    @Override
    public double getInterpolatedValue(double time) {
        readValuesAndTimesFromFile();
        return super.getInterpolatedValue(time);
    }

    /**
     * Gets the values of this series at the specified times.
     * Does not change the TimeSeries.
     *
     * @param selectTimes
     *           at which to select values
     * @param tolerance
     *           for time selection. What difference should be considered equal
     * @param dummy
     *           values to return for non-matching times
     * @return values at selected times
     */
    @Override
    public double[] getValuesAsDoubles(double[] selectTimes, double tolerance, double dummy) {
        readValuesAndTimesFromFile();
        return super.getValuesAsDoubles(selectTimes, tolerance, dummy);
    }

    /**
     * @return The start time of the series.
     */
    @Override
    public double getStartTime() {
        readValuesAndTimesFromFile();
        return super.getStartTime();
    }

    /**
     * @return The end time of the series.
     */
    @Override
    public double getStopTime() {
        readValuesAndTimesFromFile();
        return super.getStopTime();
    }

    /**
     * Gets timestep size of the series.
     * If the step size is irregular or the series is not ordered (sorted ascending), returns Double.NaN.
     *
     * @return The time step.
     */
    @Override
    public double getTimeStep() {
        readValuesAndTimesFromFile();
        return super.getTimeStep();
    }

    /**
     * Indicates whether the time array is sorted ascending (as it should be).
     *
     * @return Whether this time series is sorted ascending.
     */
    @Override
    public boolean isTimeOrdered() {
        readValuesAndTimesFromFile();
        return super.isTimeOrdered();
    }

    /**
     * Check whether this time series intersects with the given time interval
     *
     * @param t_start
     *           The start time of the interval.
     * @param t_stop
     *           The end time of the interval.
     * @return Whether this time series intersects with the given time interval
     */
    @Override
    public boolean intersectsWithTimeInterval(double t_start, double t_stop) {
        readValuesAndTimesFromFile();
        return super.intersectsWithTimeInterval(t_start, t_stop);
    }

    /**
     * Compares the times, values and extraValues of two time series.
     * Doesn't compare metadata.
     *
     * @param other
     *           The time series to compare to.
     * @return Whether this time series' data equals that of the other time series.
     */
    @Override
    public boolean equals(TimeSeries other) {
        readValuesAndTimesFromFile();
        return super.equals(other);
    }

    /**
     * Compares the times, values and extraValues of two time series.
     * Doesn't compare metadata.
     *
     * @param other
     *           The time series to compare to.
     * @param verbose
     *           Flag to indicate whether an indication should be given about the difference.
     * @return Whether this time series' data equals that of the other time series.
     */
    @Override
    public boolean equals(TimeSeries other, boolean verbose) {
        readValuesAndTimesFromFile();
        return super.equals(other, verbose);
    }

    /**
     * @return The length of (number of elements in) this time series
     */
    @Override
    public int getSize() {
        readValuesAndTimesFromFile();
        return super.getSize();
    }

    /**
     * @return The average value of the values in this time series
     */
    @Override
    public double average() {
        readValuesAndTimesFromFile();
        return super.average();
    }

    /**
     * @return The average value of the absolute values in this time series
     */
    @Override
    public double averageAbs() {
        readValuesAndTimesFromFile();
        return super.averageAbs();
    }

    /**
     * @return The RMS value of the values in this time series
     */
    @Override
    public double rms() {
        readValuesAndTimesFromFile();
        return super.rms();
    }

    /**
     * @return The maximum value of the values in this time series
     */
    @Override
    public double maximum() {
        readValuesAndTimesFromFile();
        return super.maximum();
    }

    /**
     * @return The minimum value of the values in this time series
     */
    @Override
    public double minimum() {
        readValuesAndTimesFromFile();
        return super.minimum();
    }

    /**
     * Calculates the standard deviation of the values in this time series
     *
     * @param average
     *           The average value calculated before
     * @return The standard deviation
     */
    @Override
    public double stdDev(double average) {
        readValuesAndTimesFromFile();
        return super.stdDev(average);
    }

    /**
     * Create an error series by subtracting the values from a reference series
     * from the values of this series, resulting in a new time series.
     * Only times with values in both series will be included.
     *
     * @param reference
     *           The reference time series.
     * @return A new time series with the errors relative to the reference time series.
     */
    @Override
    public TimeSeries errorSeries(TimeSeries reference) {
        readValuesAndTimesFromFile();
        return super.errorSeries(reference);
    }

    /**
     * Updates the in memory stored values and times by reading from the wdm file.
     */
    private void readValuesAndTimesFromFile() {
        this.wdmTimeSeriesIoObject.readValuesAndTimesFromFile(this);
    }
}
