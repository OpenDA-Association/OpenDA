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

import java.text.ParseException;
import java.util.HashSet;
import java.util.Set;

/**
 * A time series selection. Allows the specification of a selection on specific properties.
 * If no selected items are specified, the selection is not applied.
 */
public class TimeSeriesSelection {

   protected Set<String> selectedLocations  = new HashSet<String>();
   protected Set<String> selectedSources    = new HashSet<String>();
   protected Set<String> selectedQuantities = new HashSet<String>();
   protected double      tstart             = Double.MIN_VALUE;
   protected double      tstop              = Double.MAX_VALUE;

   /**
    * Select a location.
    *
    * @param location
    *           The value to select.
    */
   public void addLocation(String location) {
      this.selectedLocations.add(location);
   }

   /**
    * Select a source.
    *
    * @param source
    *           The value to select.
    */
   public void addSource(String source) {
      this.selectedSources.add(source);
   }

   /**
    * Select a quantity.
    *
    * @param quantity
    *           The value to select.
    */
   public void addQuantity(String quantity) {
      this.selectedQuantities.add(quantity);
   }

   /**
    * Set the start time of the selection.
    *
    * @param tstart
    *           The start time of the selection.
    */
   public void setStartTime(double tstart) {
      this.tstart = tstart;
   }

   /**
    * Set the start time of the selection.
    *
    * @param tstart
    *           The start time of the selection.
    */
   public void setStartTime(String tstart) {
      try {
         this.tstart = TimeUtils.date2Mjd(tstart);
      }
      catch (ParseException e) {
         throw new RuntimeException(e);
      }
   }

   /**
    * Set the stop time of the selection.
    *
    * @param tstop
    *           The stop time of the selection.
    */
   public void setStopTime(double tstop) {
      this.tstop = tstop;
   }

   /**
    * Set the stop time of the selection.
    *
    * @param tstop
    *           The stop time of the selection.
    */
   public void setStopTime(String tstop) {
      try {
         this.tstop = TimeUtils.date2Mjd(tstop);
      }
      catch (ParseException e) {
         throw new RuntimeException(e);
      }
   }

   /**
    * Get all selected locations.
    *
    * @return A set containing all selected location names.
    */
   public Set<String> getLocations() {
      return this.selectedLocations;
   }

   /**
    * Get all selected sources.
    *
    * @return A set containing all selected source names.
    */
   public Set<String> getSources() {
      return this.selectedSources;
   }

   /**
    * Get all selected quantities.
    *
    * @return A set containing all selected quantity names.
    */
   public Set<String> getQuantities() {
      return this.selectedQuantities;
   }

   /**
    * Get the selected start time.
    *
    * @return The selected start time.
    */
   public double getStartTime() {
      return this.tstart;
   }

   /**
    * Get the selected stop time.
    *
    * @return The selected stop time.
    */
   public double getStopTime() {
      return this.tstop;
   }

   /**
    * Check whether a location is selected.
    *
    * @param location
    *           The value to check.
    * @return True if the specified value is selected.
    */
   public boolean isSelectedLocation(String location) {
      if (this.selectedLocations.isEmpty()) return true;
      if (location == null) return false;
      return this.selectedLocations.contains(location);
   }

   /**
    * Check whether a source is selected.
    *
    * @param source
    *           The value to check.
    * @return True if the specified value is selected.
    */
   public boolean isSelectedSource(String source) {
      if (this.selectedSources.isEmpty()) return true;
      if (source == null) return false;
      return this.selectedSources.contains(source);
   }

   /**
    * Check whether a quantity is selected.
    *
    * @param quantity
    *           The value to check.
    * @return True if the specified value is selected.
    */
   public boolean isSelectedQuantity(String quantity) {
      if (this.selectedQuantities.isEmpty()) return true;
      if (quantity == null) return false;
      return this.selectedQuantities.contains(quantity);
   }

   /**
    * Test whether the given time interval is within the selected time interval.
    *
    * @param t_start
    *           Lower bound of the time interval to test.
    * @param t_stop
    *           Upper bound of the time interval to test.
    * @return Whether the given time interval is within the selected time interval.
    */
   public boolean intersectsWithTimeInterval(double t_start, double t_stop) {
      if (this.tstart <= t_stop && this.tstart >= t_start) return true; // this.tstart inside [t_start,t_stop]
      if (this.tstop <= t_stop && this.tstop >= t_start) return true; // this.tstop inside [t_start,t_stop]
      if (t_start <= this.tstop && t_start >= this.tstart) return true; // t_start inside [this.tstart,this.tstop]
      if (t_stop <= this.tstop && t_stop >= this.tstart) return true; // t_stop inside [this.tstart,this.tstop]
      if (this.tstart <= t_start && this.tstop >= t_stop) return true; // [t_start,t_stop] inside [this.tstart,this.tstop]
      if (t_start <= this.tstart && t_stop >= this.tstop) return true; // [this.tstart,this.tstop] inside [t_start,t_stop]
      return false;
   }

   /**
    * Test whether the given time series is within the selection.
    *
    * @param ts
    *           The time series to test.
    * @return True if the given time series is within the selection.
    */
   public boolean isSelected(TimeSeries ts) {
      if (!isSelectedLocation(ts.getLocation())) return false;
      if (!isSelectedSource(ts.getSource())) return false;
      if (!isSelectedQuantity(ts.getQuantityId())) return false;
      if (!intersectsWithTimeInterval(ts.getStartTime(), ts.getStopTime())) return false;
      return true;
   }

}
