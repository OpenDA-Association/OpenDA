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
package org.openda.exchange.timeseries;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

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
/**
 * Analysis tools on a specified time series set.
 */
public final class TimeSeriesSetAnalyzer {
   private TimeSeriesSet                set;
   private boolean                      interpolate   = false;
   private boolean                      absoluteDiffs = false;

   private Map<Double, Set<TimeSeries>> onTime        = new TreeMap<Double, Set<TimeSeries>>();

   /**
    * Constructor
    * Sets the time series set that needs to be analyzed and does some indexing.
    *
    * @param set
    *           The set to analyze.
    */
   public TimeSeriesSetAnalyzer(TimeSeriesSet set) {
      setTimeSeriesSet(set);
   }

   /**
    * Change the set to analyze.
    *
    * @param set
    *           The set to analyze.
    */
   public void setTimeSeriesSet(TimeSeriesSet set) {
      this.set = set;
      buildTimeMap();
   }

   /**
    * Retrieve the set to analyze.
    *
    * @return The set to analyze.
    */
   public TimeSeriesSet getTimeSeriesSet() {
      return this.set;
   }

   /**
    * Build the time map
    */
   private void buildTimeMap() {
      this.onTime.clear();
      for (TimeSeries timeseries : this.set) {
         for (double time : timeseries.getTimesRef()) {
            TimeSeriesSet.addToMultiMap(this.onTime, new Double(time), timeseries);
         }
      }
   }

   /**
    * @return the earliest available time
    */
   public double getStartTime() {
      return this.set.getStartTime();
   }

   /**
    * @return the latest available time
    */
   public double getStopTime() {
      return this.set.getStopTime();
   }

   /**
    * Calculate a time series by taking the average values from the time series included in the set.
    * The values in this series are the average values of the series in the set.
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @param t_start
    *           First time to include
    * @param t_stop
    *           Last time to include
    * @return TimeSeries containing the calculated average values
    */
   public TimeSeries calcAverageTimeSeries(double t_start, double t_stop) {
      double tstart = t_start;
      double tstop = t_stop;

      // Fix tstart and tstop
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      // Calculate times and values arrays
      // As we do not know yet the number of elements between tstart and tstop,
      // we use a growing list.
      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> values = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate averages
         double sum = 0.0;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || this.onTime.get(timeObject).contains(timeseries)) {
               ++count;
               sum += (this.interpolate ? timeseries.getInterpolatedValue(time) : timeseries.getValue(time));
            }
         }
         final Double average = new Double((count > 0 ? (sum / count) : Double.NaN));

         // Store results for the current time
         times.add(timeObject);
         values.add(average);
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(values));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by taking the average values from the time series included in the set.
    * The values in this series are the average values of the series in the set.
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @return TimeSeries containing the calculated average values
    */
   public TimeSeries calcAverageTimeSeries() {
      return calcAverageTimeSeries(this.set.getStartTime(), this.set.getStopTime());
   }

   /**
    * Calculate a time series by taking the RMS values from the time series included in the set.
    * The values in this series are the RMS values of the series in the set.
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @param t_start
    *           First time to include
    * @param t_stop
    *           Last time to include
    * @return TimeSeries containing the calculated RMS values
    */
   public TimeSeries calcRMSTimeSeries(double t_start, double t_stop) {
      double tstart = t_start;
      double tstop = t_stop;

      // Fix tstart and tstop
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      // Calculate times and values arrays
      // As we do not know yet the number of elements between tstart and tstop,
      // we use a growing list.
      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> values = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate averages
         double sumsq = 0.0;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || this.onTime.get(timeObject).contains(timeseries)) {
               ++count;
               final double v = (this.interpolate ? timeseries.getInterpolatedValue(time) : timeseries.getValue(time));
               sumsq += (v * v);
            }
         }
         final Double rms = new Double((count > 0 ? Math.sqrt(sumsq / count) : Double.NaN));

         // Store results for the current time
         times.add(timeObject);
         values.add(rms);
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(values));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by taking the RMS values from the time series included in the set.
    * The values in this series are the RMS values of the series in the set.
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @return TimeSeries containing the calculated RMS values
    */
   public TimeSeries calcRMSTimeSeries() {
      return calcRMSTimeSeries(this.set.getStartTime(), this.set.getStopTime());
   }

   /**
    * Calculate a time series by calculating standard deviations from the time series included in the set,
    * using a time series calculated by calcAverageTimeSeries()
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @param averages
    *           time series calculated by calcAverageTimeSeries() containing the averages
    * @return TimeSeries containing the calculated standard deviations
    */
   public TimeSeries calcStdDevTimeSeries(TimeSeries averages) {
      // Get tstart and tstop
      double tstart = averages.getStartTime();
      double tstop = averages.getStopTime();
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> stddev = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate standard deviation
         double sumsqdev = 0.0;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || (this.onTime.get(timeObject).contains(timeseries) && -1 != averages.findOnTime(time))) {
               ++count;
               final double dev =
                        (this.interpolate ? (timeseries.getInterpolatedValue(time) - averages.getInterpolatedValue(time)) :
                                                             (timeseries.getValue(time) - averages.getValue(time)));
               sumsqdev += (dev * dev);
            }
         }
         final Double sd = new Double((count > 1 ? Math.sqrt(sumsqdev / (count - 1)) : (count == 1 ? 0 : Double.NaN)));

         // Store results for the current time
         times.add(timeObject);
         stddev.add(sd);
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(stddev));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by taking the maximum values from the time series included in the set.
    *
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @param t_start
    *           First time to include
    * @param t_stop
    *           Last time to include
    * @return TimeSeries containing the calculated maximum values
    */
   public TimeSeries calcMaximumTimeSeries(double t_start, double t_stop) {
      double tstart = t_start;
      double tstop = t_stop;

      // Fix tstart and tstop
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      // Calculate times and values arrays
      // As we do not know yet the number of elements between tstart and tstop,
      // we use a growing list.
      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> maxima = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate maximum
         double maxvalue = Double.NEGATIVE_INFINITY;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || this.onTime.get(timeObject).contains(timeseries)) {
               ++count;
               final double value = (this.interpolate ? timeseries.getInterpolatedValue(time) : timeseries.getValue(time));
               if (value > maxvalue) maxvalue = value;
            }
         }

         // Store results for the current time
         times.add(timeObject);
         maxima.add(new Double(maxvalue));
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(maxima));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by taking the maximum values from the time series included in the set.
    *
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @return TimeSeries containing the calculated maximum values
    */
   public TimeSeries calcMaximumTimeSeries() {
      return calcMaximumTimeSeries(this.set.getStartTime(), this.set.getStopTime());
   }

   /**
    * Calculate a time series by taking the minimum values from the time series included in the set.
    *
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @param t_start
    *           First time to include
    * @param t_stop
    *           Last time to include
    * @return TimeSeries containing the calculated minimum values
    */
   public TimeSeries calcMinimumTimeSeries(double t_start, double t_stop) {
      double tstart = t_start;
      double tstop = t_stop;

      // Fix tstart and tstop
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      // Calculate times and values arrays
      // As we do not know yet the number of elements between tstart and tstop,
      // we use a growing list.
      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> values = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate maximum
         double minvalue = Double.POSITIVE_INFINITY;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || this.onTime.get(timeObject).contains(timeseries)) {
               ++count;
               final double value = (this.interpolate ? timeseries.getInterpolatedValue(time) : timeseries.getValue(time));
               if (value < minvalue) minvalue = value;
            }
         }

         // Store results for the current time
         times.add(timeObject);
         values.add(new Double(minvalue));
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(values));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by taking the minimum values from the time series included in the set.
    *
    * In addition, as extra values are stored for each element in the resulting time series:
    * nseries, containing the number of time series used for this calculation
    *
    * @return TimeSeries containing the calculated minimum values
    */
   public TimeSeries calcMinimumTimeSeries() {
      return calcMinimumTimeSeries(this.set.getStartTime(), this.set.getStopTime());
   }

   /**
    * Calculate a time series by calculating the average difference between the values from the time series included in the set,
    * and the specified reference time series.
    *
    * This function is affected by the value of the absoluteDiffs flag. If it is set, the absolute value of the
    * differences between the included time series and the reference time series are used.
    *
    * @param reference
    *           reference time series to use to calculate the average differences
    * @return TimeSeries containing the calculated average values
    */
   public TimeSeries calcAverageDiffTimeSeries(TimeSeries reference) {
      // Get tstart and tstop
      double tstart = reference.getStartTime();
      double tstop = reference.getStopTime();
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> avgdiffs = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate the maximum difference
         double sum = 0.0;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || (this.onTime.get(timeObject).contains(timeseries) && -1 != reference.findOnTime(time))) {
               ++count;
               double diff =
                        (this.interpolate ? (timeseries.getInterpolatedValue(time) - reference.getInterpolatedValue(time)) :
                                                        (timeseries.getValue(time) - reference.getValue(time)));
               if (this.absoluteDiffs) diff = Math.abs(diff);
               sum += diff;
            }
         }
         final double average = (count > 0 ? (sum / count) : Double.NaN);

         // Store results for the current time
         times.add(timeObject);
         avgdiffs.add(new Double(average));
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(avgdiffs));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by calculating the RMS of the difference between the values from the time series included in the
    * set,
    * and the specified reference time series.
    *
    * @param reference
    *           reference time series to use to calculate the difference RMSs
    * @return TimeSeries containing the calculated RMS values
    */
   public TimeSeries calcRMSDiffTimeSeries(TimeSeries reference) {
      // Get tstart and tstop
      double tstart = reference.getStartTime();
      double tstop = reference.getStopTime();
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> rmsdiffs = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate the maximum difference
         double sumsq = 0.0;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || (this.onTime.get(timeObject).contains(timeseries) && -1 != reference.findOnTime(time))) {
               ++count;
               final double diff =
                        (this.interpolate ? (timeseries.getInterpolatedValue(time) - reference.getInterpolatedValue(time)) :
                                                              (timeseries.getValue(time) - reference.getValue(time)));
               // Absolute diffs do not make sense here
               sumsq += (diff * diff);
            }
         }
         final Double rms = new Double(count > 0 ? Math.sqrt(sumsq / count) : Double.NaN);

         // Store results for the current time
         times.add(timeObject);
         rmsdiffs.add(rms);
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(rmsdiffs));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by calculating standard deviations of the differences between
    * the time series included in the set and the reference time series,
    * using a time series calculated by calcAverageDiffTimeSeries()
    *
    * This function is affected by the value of the absoluteDiffs flag. If it is set, the absolute value of the
    * differences between the included time series and the reference time series are used.
    *
    * @param reference
    *           reference time series to use to calculate the differences
    * @param averageDiffs
    *           time series calculated by calcAverageDiffTimeSeries() containing the average differences
    * @return TimeSeries containing the calculated standard deviations
    */
   public TimeSeries calcStdDevDiffTimeSeries(TimeSeries reference, TimeSeries averageDiffs) {
      // Get tstart and tstop
      double tstart = averageDiffs.getStartTime();
      double tstop = averageDiffs.getStopTime();
      if (tstart < reference.getStartTime()) tstart = reference.getStartTime();
      if (tstop > reference.getStopTime()) tstop = reference.getStopTime();
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> stddev = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate standard deviation
         double sumsqdev = 0.0;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || (this.onTime.get(timeObject).contains(timeseries) && -1 != averageDiffs.findOnTime(time))) {
               ++count;
               double diff =
                        (this.interpolate ? (timeseries.getInterpolatedValue(time) - reference.getInterpolatedValue(time)) :
                                                        (timeseries.getValue(time) - reference.getValue(time)));
               if (this.absoluteDiffs) diff = Math.abs(diff);
               final double dev =
                        (diff - (this.interpolate ? averageDiffs.getInterpolatedValue(time) : averageDiffs.getValue(time)));
               sumsqdev += (dev * dev);
            }
         }
         final Double sd = new Double(count > 1 ? Math.sqrt(sumsqdev / (count - 1)) : (count == 1 ? 0 : Double.NaN));

         // Store results for the current time
         times.add(timeObject);
         stddev.add(sd);
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(stddev));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by calculating the maximum difference between the values from the time series included in the set,
    * and the specified reference time series.
    *
    * This function is affected by the value of the absoluteDiffs flag. If it is set, the absolute value of the
    * differences between the included time series and the reference time series are used.
    *
    * @param reference
    *           reference time series to use to calculate the maximum differences
    * @return TimeSeries containing the calculated maximum values
    */
   public TimeSeries calcMaximumDiffTimeSeries(TimeSeries reference) {
      // Get tstart and tstop
      double tstart = reference.getStartTime();
      double tstop = reference.getStopTime();
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> maxdiffs = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate the maximum difference
         double maxdiff = Double.NEGATIVE_INFINITY;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || (this.onTime.get(timeObject).contains(timeseries) && -1 != reference.findOnTime(time))) {
               ++count;
               double diff =
                        (this.interpolate ? (timeseries.getInterpolatedValue(time) - reference.getInterpolatedValue(time)) :
                                                        (timeseries.getValue(time) - reference.getValue(time)));
               if (this.absoluteDiffs) diff = Math.abs(diff);
               if (diff > maxdiff) maxdiff = diff;

            }
         }

         // Store results for the current time
         times.add(timeObject);
         maxdiffs.add(new Double(maxdiff));
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(maxdiffs));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Calculate a time series by calculating the minimum difference between the values from the time series included in the set,
    * and the specified reference time series.
    *
    * This function is affected by the value of the absoluteDiffs flag. If it is set, the absolute value of the
    * differences between the included time series and the reference time series are used.
    *
    * @param reference
    *           reference time series to use to calculate the minimum differences
    * @return TimeSeries containing the calculated minimum values
    */
   public TimeSeries calcMinimumDiffTimeSeries(TimeSeries reference) {
      // Get tstart and tstop
      double tstart = reference.getStartTime();
      double tstop = reference.getStopTime();
      if (tstart < this.set.getStartTime()) tstart = this.set.getStartTime();
      if (tstop > this.set.getStopTime()) tstop = this.set.getStopTime();

      ArrayList<Double> times = new ArrayList<Double>();
      ArrayList<Double> mindiffs = new ArrayList<Double>();
      ArrayList<Double> nseries = new ArrayList<Double>(); // Integers, in fact...

      for (Double timeObject : this.onTime.keySet()) {
         final double time = timeObject.doubleValue();
         if (time < tstart || time > tstop) continue;

         // Calculate the maximum difference
         double mindiff = Double.POSITIVE_INFINITY;
         int count = 0;
         for (TimeSeries timeseries : this.set) {
            if (time < timeseries.getStartTime() || time > timeseries.getStopTime()) continue;

            if (this.interpolate || (this.onTime.get(timeObject).contains(timeseries) && -1 != reference.findOnTime(time))) {
               ++count;
               double diff =
                        (this.interpolate ? (timeseries.getInterpolatedValue(time) - reference.getInterpolatedValue(time)) :
                                                        (timeseries.getValue(time) - reference.getValue(time)));
               if (this.absoluteDiffs) diff = Math.abs(diff);
               if (diff < mindiff) mindiff = diff;

            }
         }

         // Store results for the current time
         times.add(timeObject);
         mindiffs.add(new Double(mindiff));
         nseries.add(new Double(count));
      }

      // Construct a time series
      TimeSeries result = new TimeSeries(toArray(times), toArray(mindiffs));
      result.setExtraValues("nseries", toArray(nseries));
      return result;
   }

   /**
    * Get a double[] array from a ArrayList<Double>
    *
    * @param in
    *           Input array.
    * @return Output array.
    */
   private static double[] toArray(ArrayList<Double> in) {
      double[] out = new double[in.size()];
      for (int i = 0; i < in.size(); ++i) {
         out[i] = in.get(i).doubleValue();
      }
      return out;
   }

   /**
    * @return Whether missing values are interpolated calculating time series (true) or not (false; default)
    */
   public boolean getInterpolate() {
      return this.interpolate;
   }

   /**
    * @param interpolate
    *           Whether missing values are interpolated calculating time series (true) or not (false; default)
    */
   public void setInterpolate(boolean interpolate) {
      this.interpolate = interpolate;
   }

   /**
    * @return Whether absolute values are used in diff computations (true) or not (false; default)
    */
   public boolean getAbsoluteDiffs() {
      return this.absoluteDiffs;
   }

   /**
    * Indicate whether absolute values are used in diff computations (true) or not (false; default)
    *
    * @param absoluteDiffs
    *           Use absolute differences?
    */
   public void setAbsoluteDiffs(boolean absoluteDiffs) {
      this.absoluteDiffs = absoluteDiffs;
   }
}
