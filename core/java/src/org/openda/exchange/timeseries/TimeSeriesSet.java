/* OpenDA v2.4.1 
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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The TimeSeriesSet can be used to store and retrieve (references to) time
 * series. It can be queried for time series that share certain properties, like
 * a shared location.
 */
public final class TimeSeriesSet extends HashSet<TimeSeries> {

   // These are multimaps, mapping a single key to (possibly multiple) time series
   private Map<String, Set<TimeSeries>>              onSource         = new HashMap<String, Set<TimeSeries>>();
   private Map<String, Set<TimeSeries>>              onQuantity       = new HashMap<String, Set<TimeSeries>>();
   private Map<String, Set<TimeSeries>>              onLocation       = new HashMap<String, Set<TimeSeries>>();
   private Map<String, Map<String, Set<TimeSeries>>> onProperty       = new HashMap<String, Map<String, Set<TimeSeries>>>();

   private double                                    tstart           = Double.NaN;
   private double                                    tstop            = Double.NaN;

   /**
    * Default constructor
    */
   public TimeSeriesSet() {
      super();
   }

   /**
    * Constructor from a set of time series (or a TimeSeriesSet)
    *
    * @param set
    *           The set to construct from.
    */
   public TimeSeriesSet(Set<TimeSeries> set) {
      super();
      if (set != null) {
         for (TimeSeries timeseries : set) {
            add(timeseries);
         }
      }
   }

   /**
    * Add a time series
    */
   
   public boolean add(TimeSeries timeseries) {
      super.add(timeseries);

      addToMultiMap(this.onSource, timeseries.getSource(), timeseries);
      addToMultiMap(this.onQuantity, timeseries.getQuantityId(), timeseries);
      addToMultiMap(this.onLocation, timeseries.getLocation(), timeseries);

      for (String property : timeseries.getPropertiesKeySet()) {
         if (!this.onProperty.containsKey(property)) this.onProperty.put(property, new HashMap<String, Set<TimeSeries>>());
         addToMultiMap(this.onProperty.get(property), timeseries.getProperty(property), timeseries);
      }

      if (Double.isNaN(this.tstart) || timeseries.getStartTime() < this.tstart) this.tstart = timeseries.getStartTime();
      if (Double.isNaN(this.tstop) || timeseries.getStopTime() > this.tstop) this.tstop = timeseries.getStopTime();
      return true;
   }

   /**
    * Add a set of time series (or a TimeSeriesSet)
    *
    * @param set
    *           The set to add.
    * @return Boolean indicating success.
    */
   public boolean add(Set<TimeSeries> set) {
      for (TimeSeries timeseries : set) {
         add(timeseries);
      }
      return true;
   }

   /**
    * Add a key-value pair to the given multimap
    *
    * Note: the lack of scope specifier is on purpose.
    *
    * @param multimap
    *           The map to add to.
    * @param key
    *           The key to add.
    * @param timeseries
    *           The time series (value) to add.
    * @param <KeyType>
    *           The type of the key.
    */
   static <KeyType> void addToMultiMap(Map<KeyType, Set<TimeSeries>> multimap, KeyType key, TimeSeries timeseries) {
      if (multimap.containsKey(key)) {
         multimap.get(key).add(timeseries);
      }
      else {
         Set<TimeSeries> set = new HashSet<TimeSeries>();
         set.add(timeseries);
         multimap.put(key, set);
      }
   }

   /**
    * Remove a time series
    *
    * @param timeseries
    *           The time series to remove.
    * @return Boolean indicating success.
    */
   public boolean remove(TimeSeries timeseries) {
      super.remove(timeseries);

      removeFromMultiMap(this.onSource, timeseries.getSource(), timeseries);
      removeFromMultiMap(this.onQuantity, timeseries.getQuantityId(), timeseries);
      removeFromMultiMap(this.onLocation, timeseries.getLocation(), timeseries);

      for (String property : timeseries.getPropertiesKeySet()) {
         if (!this.onProperty.containsKey(property)) this.onProperty.put(property, new HashMap<String, Set<TimeSeries>>());
         removeFromMultiMap(this.onProperty.get(property), timeseries.getProperty(property), timeseries);
      }

      // If tstart or tstop match the time series to be removed, recalculate tstart and tstop
      if (this.tstart == timeseries.getStartTime() || this.tstop == timeseries.getStopTime()) {
         this.tstart = Double.POSITIVE_INFINITY;
         this.tstop = Double.NEGATIVE_INFINITY;
         for (TimeSeries ts : this) {
            if (ts.getStartTime() < this.tstart) this.tstart = ts.getStartTime();
            if (ts.getStopTime() > this.tstop) this.tstop = ts.getStopTime();
         }
      }
      return true;
   }

   /**
    * Remove a set of time series (or a TimeSeriesSet)
    *
    * @param set
    *           The set to remove.
    * @return Boolean indicating success.
    */
   public boolean remove(Set<TimeSeries> set) {
      for (TimeSeries timeseries : set) {
         remove(timeseries);
      }
      return true;
   }

   /**
    * Remove a key-value pair to the given multimap
    *
    * Note: the lack of scope specifier is on purpose.
    *
    * @param multimap
    *           The map to remove from.
    * @param key
    *           The key to remove.
    * @param timeseries
    *           The time series (value) to remove.
    * @param <KeyType>
    *           The type of the key.
    */
   static <KeyType> void removeFromMultiMap(Map<KeyType, Set<TimeSeries>> multimap, KeyType key, TimeSeries timeseries) {
      if (!multimap.containsKey(key)) return;

      multimap.get(key).remove(timeseries);
      if (multimap.get(key).isEmpty()) multimap.remove(key);
   }

   /**
    * Return all time series matching the specified selection.
    *
    * @param selection
    *           The selection to apply.
    * @return The selected time series.
    */
   public TimeSeriesSet getOnSelection(TimeSeriesSelection selection) {
      TimeSeriesSet result = new TimeSeriesSet();
      for (TimeSeries ts : this) {
         if (selection.isSelected(ts)) result.add(ts);
      }
      return result;
   }

   /**
    * Return all time series matching the specified source
    *
    * @param source
    *           Source to match.
    * @return Resulting set of time series.
    */
   public TimeSeriesSet getOnSource(String source) {
      Set<TimeSeries> result = this.onSource.get(source);
      return new TimeSeriesSet(result);
   }

   /**
    * Return all time series matching the specified quantity
    *
    * @param quantity
    *           Quantity to match.
    * @return Resulting set of time series.
    */
   public TimeSeriesSet getOnQuantity(String quantity) {
      Set<TimeSeries> result = this.onQuantity.get(quantity);
      return new TimeSeriesSet(result);
   }

   /**
    * Return all time series matching the specified location
    *
    * @param location
    *           Location to match.
    * @return Resulting set of time series.
    */
   public TimeSeriesSet getOnLocation(String location) {
      Set<TimeSeries> result = this.onLocation.get(location);
      return new TimeSeriesSet(result);
   }

   /**
    * Return all time series matching the specified property value
    *
    * @param property
    *           The property to match.
    * @param propertyValue
    *           The property's value to match.
    * @return Resulting set of time series.
    */
   public TimeSeriesSet getOnProperty(String property, String propertyValue) {
      if (!this.onProperty.containsKey(property)) return new TimeSeriesSet();

      Set<TimeSeries> result = this.onProperty.get(property).get(propertyValue);
      return new TimeSeriesSet(result);
   }

   /**
    * Return all time series that intersect the given time interval
    *
    * @param t_start
    *           Start time of the interval.
    * @param t_stop
    *           End time of the interval.
    * @return Resulting set of time series.
    */
   public TimeSeriesSet intersectsWithTimeInterval(double t_start, double t_stop) {
      Set<TimeSeries> result = new HashSet<TimeSeries>();
      for (TimeSeries ts : this) {
         if (ts.intersectsWithTimeInterval(t_start, t_stop)) {
            result.add(ts);
         }
      }
      return new TimeSeriesSet(result);
   }

   /**
    * Return all available sources
    *
    * @return Set with names of all available sources.
    */
   public Set<String> getSources() {
      return this.onSource.keySet();
   }

   /**
    * Return all available quantities
    *
    * @return Set with names of all available quantities.
    */
   public Set<String> getQuantities() {
      return this.onQuantity.keySet();
   }

   /**
    * Return all available locations
    *
    * @return Set with names of all available locations.
    */
   public Set<String> getLocations() {
      return this.onLocation.keySet();
   }

   /**
    * Return all available properties
    *
    * @return Set with names of all available properties.
    */
   public Set<String> getProperties() {
      return this.onProperty.keySet();
   }

   /**
    * Return all available property values
    *
    * @param property
    *           The property to return the values of.
    * @return Set with values for the given property.
    */
   public Set<String> getPropertyValues(String property) {
      if (!this.onProperty.containsKey(property)) return new HashSet<String>();
      return this.onProperty.get(property).keySet();
   }

   /**
    * @return the earliest available time
    */
   public double getStartTime() {
      return this.tstart;
   }

   /**
    * @return the last available time
    */
   public double getStopTime() {
      return this.tstop;
   }

   /**
    * Change a location name to another.
    *
    * This can be used to move results to a nearby location that doesn't measure specific values
    * (e.g. not all waterlevel stations have associated wind information).
    *
    * @param fromLocation
    *           Location name to change.
    * @param toLocation
    *           Location name to change to.
    */
   public void moveLocation(String fromLocation, String toLocation) {
      final TimeSeriesSet toModify = getOnLocation(fromLocation);
      for (TimeSeries timeseries : toModify) {
         removeFromMultiMap(this.onLocation, fromLocation, timeseries);
         timeseries.setLocation(toLocation);
         addToMultiMap(this.onLocation, toLocation, timeseries);
      }
   }
}
