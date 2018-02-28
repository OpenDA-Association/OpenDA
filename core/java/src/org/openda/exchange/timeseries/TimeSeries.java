/* OpenDA v2.4.3 
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

import org.openda.exchange.ArrayTimeInfo;
import org.openda.exchange.PointGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.Array;
import org.openda.utils.DoubleArraySearch;
import org.openda.utils.Mask;
import org.openda.utils.SortUtils;

import java.util.HashMap;
import java.util.Set;

/**
 * Implements a time-series class for storage of a time-series of single values.
 * Some meta data, such as position, location, quantity etc is included. There
 * are no readers and writers. These are put into separate TimeSeriesFormatters
 * <p/>
 * There are some predefined properties. These are: location : a name for the
 * location position : x,y-pair with the horizontal coordinated, preferably as
 * WGS84 longitude and latitude in degrees source : origin of the data, e.g.
 * 'observed' or 'tidal forecast from IHO constituents' quantity : what parameter
 * is included as values unit : unit of the values, preferably SI, unless where
 * very uncommon. Other properties can be included as a string and references by
 * their name (label)
 * <p/>
 * Time values are preferably given as Modified Julian Day
 *
 * @author verlaanm
 */

public class TimeSeries implements IPrevExchangeItem, IExchangeItem{
	// define and set defaults
	// meta data
	private String description = null;
	private String location = "";
	private final double position[] = {0.0, 0.0};				   // {longitude,latitude} WGS84
	private double height = 0.0;							// user defined for now
	private String source = "observed";					 // what produced the data
	private String quantity = "waterlevel";				   // what do values represent
	private String unit = "m";							// what is unit of the values
	private String id = "id-not-set";				   // unique identifier
	private boolean autoId = true;						   // automatically set to location.quantity
	// by default
	private final HashMap<String, String> properties = new HashMap<String, String>();  // additional properties
	private final java.util.Vector<String> defaultProps = new java.util.Vector<String>(); // data
	private double times[] = null;						   // times for values as MJD
	private double values[] = null;						   // this is the real data!
	private final HashMap<String, double[]> extraValues = new HashMap<String, double[]>(); // optional arrays for secondary values
	// (like Matroos analysis times)
	protected Role role;
	private double tstart = Double.NaN;
	private double tstop = Double.NaN;
	private double tstep = Double.NaN;
	private boolean timeOrdered = true;						   // Indicates whether the time array is
	// sorted increasing
	private DoubleArraySearch timesearcher = null;						   // Always update this after a new times
	// array is created
	private TimeSeries parent = null;						   // Source time series

	/**
	 * Default constructor
	 */
	public TimeSeries() {
		this.role = Role.InOut;
		this.defaultProps.add("location");
		this.defaultProps.add("xposition");
		this.defaultProps.add("yposition");
		this.defaultProps.add("height");
		this.defaultProps.add("quantity");
		this.defaultProps.add("source");
		this.defaultProps.add("unit");
		this.defaultProps.add("id");
	}

	/**
	 * Copy constructor
	 *
	 * @param other The time series to copy.
	 */
	public TimeSeries(TimeSeries other) {
		setMetaData(other);
		this.setData(other.times, other.values);
		for (String extraName : other.extraValues.keySet()) {
			this.setExtraValues(extraName, other.getExtraValuesAsDoubles(extraName));
		}
	}

	/**
	 * Create a TimeSeries from two arrays of doubles: an array for times and
	 * one for values
	 *
	 * @param times  The time array.
	 * @param values The value array.
	 */
	public TimeSeries(double times[], double values[]) {
		this();
		this.setData(times, values);
	}

	public TimeSeries(double times[], double values[], double x, double y, String source, String quantity, String unit,
					  String location, Role role) {
		this(times, values, x, y, source, quantity, unit, location);
		this.role = role;
	}

	/**
	 * Create a TimeSeries from two arrays of doubles and several properties with metadata
	 *
	 * @param times	The time stamps in the series
	 * @param values   A value for each time stamp
	 * @param x		x-coordinate
	 * @param y		y-coordinate
	 * @param source   source of the data, eg. 'observed'
	 * @param quantity String describing which quantity the values in the series represent
	 * @param unit	 String describing the value's unit
	 * @param location String describing the location of this series
	 */
	public TimeSeries(double times[], double values[], double x, double y, String source, String quantity, String unit,
					  String location) {
		this(times, values, source, quantity, unit, location);
		this.setPosition(x, y);
		this.setId(this.location + "." + this.getQuantityId());
	}

	public TimeSeries(double[] times, double[] values, String source, String quantity, String unit, String location) {
		this.setData(times, values);
		this.setSource(source);
		this.setQuantity(quantity);
		this.setUnit(unit);
		this.setLocation(location);
		this.setId(this.location + "." + this.getQuantityId());
	}

	/**
	 * Copy the other time series' metadata.
	 * Doesn't copy times, values or extraValues.
	 *
	 * @param other The time series to copy from.
	 */
	public void setMetaData(TimeSeries other) {
		this.setSource(other.source);
		this.setQuantity(other.quantity);
		this.setUnit(other.unit);
		this.setLocation(other.location);
		this.setId(other.id);
		this.setDescription(other.description);
		this.setPosition(other.position[0], other.position[1]);
		this.setHeight(other.height);
		this.autoId = other.autoId;
		this.parent = other.parent;

		for (String propertyName : other.properties.keySet()) {
			this.setProperty(propertyName, other.getProperty(propertyName));
		}
	}

	/**
	 * Sets the location property
	 *
	 * @param location Location name.
	 */
	public void setLocation(String location) {
		this.location = location;
		if (this.autoId) {
			this.id = this.location + "." + this.getQuantityId();
		}
	}

	/**
	 * Gets the location property
	 *
	 * @return Location name.
	 */
	public String getLocation() {
		return this.location;
	}

	/**
	 * Sets the source property
	 *
	 * @param source Source name.
	 */
	public void setSource(String source) {
		this.source = source;
	}

	/**
	 * Gets the source property
	 *
	 * @return Source name.
	 */
	public String getSource() {
		return this.source;
	}

	/**
	 * Sets the Quantity property
	 *
	 * @param quantity Quantity name.
	 */
	public void setQuantity(String quantity) {
		this.quantity = quantity;
		if (this.autoId) {
			this.id = this.location + "." + this.getQuantityId();
		}
	}

	/**
	 * Gets the Quantity property
	 *
	 * @return Quantity name.
	 */
	public String getQuantityId() {
		return this.quantity;
	}

	/**
	 * Sets the Unit property
	 *
	 * @param unit Unit name.
	 */
	public void setUnit(String unit) {
		this.unit = unit;
	}

	/**
	 * Gets the Unit property
	 *
	 * @return Unit name.
	 */
	public String getUnitId() {
		return this.unit;
	}

	/**
	 * Sets the position properties
	 *
	 * @param x X coordinate.
	 * @param y Y coordinate.
	 */
	public void setPosition(double x, double y) {
		this.position[0] = x;
		this.position[1] = y;
	}

	/**
	 * Gets the position properties
	 *
	 * @return Array containing the X and the Y coordinate.
	 */
	public double[] getPosition() {
		double result[] = new double[2];
		result[0] = this.position[0];
		result[1] = this.position[1];
		return result;
	}

	/**
	 * Sets the height property
	 *
	 * @param height The height value.
	 */
	public void setHeight(double height) {
		this.height = height;
	}

	/**
	 * Gets the height property
	 *
	 * @return The height value.
	 */
	public double getHeight() {
		return this.height;
	}

	/**
	 * Sets the times and values for this series
	 *
	 * @param times  The time array.
	 * @param values The value array.
	 * @param isize  The number of elements to be copied from times and values
	 */
	public void setData(double[] times, double[] values, int isize) {
		if (times != null && values != null) {
			if (times.length < isize || values.length < isize) {
				throw new RuntimeException(
						"TimeSeries: insufficient length for time and values.");
			}
			this.times = new double[isize];
			System.arraycopy(times, 0, this.times, 0, isize);
			this.values = new double[isize];
			System.arraycopy(values, 0, this.values, 0, isize);
			this.timesearcher = new DoubleArraySearch(this.times);
		} else {
			this.times = null;
			this.values = null;
			this.timesearcher = null;
		}
		calculateTime();
	}

	/**
	 * Sets the times and values for this series
	 *
	 * @param times  The time array.
	 * @param values The value array.
	 */
	public void setData(double[] times, double[] values) {
		if (times != null && values != null && times.length != values.length)
			throw new RuntimeException("TimeSeries: length for time and values do not match.");
		int size = (times != null ? times.length : 0);
		setData(times, values, size);
	}

	/**
	 * Gets the times of this series Makes a copy of any stored values, so
	 * changing the result is safe and does not change the TimeSeries.
	 *
	 * @return Copy of the time array.
	 */
	
	public double[] getTimes() {
		if (this.times != null) {
			double[] result = new double[this.times.length];
			System.arraycopy(this.times, 0, result, 0, this.times.length);
			return result;
		}
		return null;
	}

	/**
	 * @see org.openda.interfaces.IPrevExchangeItem#setTimes(double[])
	 */
	
	public void setTimes(double[] times) {
		if ((this.values != null) && (this.values.length != times.length)) {
			this.values = null;
		}
		this.times = new double[times.length];
		System.arraycopy(times, 0, this.times, 0, times.length);
		this.timesearcher = new DoubleArraySearch(times);
		calculateTime();
	}

	/**
	 * Gets the values of this series Makes a copy of any stored values, so
	 * changing the result is safe and does not change the TimeSeries.
	 *
	 * @return The value array as double array.
	 */
	
	public double[] getValuesAsDoubles() {
		if (this.values != null) {
			double[] result = new double[this.values.length];
			System.arraycopy(this.values, 0, result, 0, this.values.length);
			return result;
		}

		return new double[0];
	}

	/**
	 * @see org.openda.interfaces.IPrevExchangeItem#axpyOnValues(double, double[])
	 */
	
	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (this.values == null) {
			throw new RuntimeException(getId() + "this.values == null in axpyOnValues");
		}
		if (this.values.length != axpyValues.length) {
			throw new RuntimeException(getId() + "this.values.length (" + this.values.length +
					") /= axpyValues.length (" + axpyValues.length + ") in axpyOnValues");
		}
		for (int i = 0; i < this.values.length; i++) {
			this.values[i] += alpha * axpyValues[i];
		}
	}

	/**
	 * @see org.openda.interfaces.IPrevExchangeItem#multiplyValues(double[])
	 */
	
	public void multiplyValues(double[] multiplicationFactors) {
		if (this.values != null) {
			for (int i = 0; i < this.values.length; i++) {
				this.values[i] *= multiplicationFactors[i];
			}
		}
	}

	/**
	 * Gets the specified extra values of this series
	 * Makes a copy of any stored values, so changing the result is safe and does not change the
	 * TimeSeries.
	 *
	 * @param key - case sensitive key identifying the extra value series
	 * @return The extra values as double array.
	 */
	public double[] getExtraValuesAsDoubles(String key) {
		if (this.extraValues.containsKey(key)) {
			double[] result = new double[this.extraValues.get(key).length];
			System.arraycopy(this.extraValues.get(key), 0, result, 0, this.extraValues.get(key).length);
			return result;
		}
		return null;
	}

	/**
	 * Gets the specified extra values of this series
	 *
	 * @param key - case sensitive key identifying the extra value series
	 * @return The extra values as a new time series.
	 */
	public TimeSeries getExtraValuesAsTimeSeries(String key) {
		if (this.extraValues.containsKey(key)) {
			return new TimeSeries(this.times, this.extraValues.get(key));
		}
		return null;
	}

	/**
	 * Gets the times of this series. Possibly returns a reference, so changing
	 * values might corrupt the TimeSeries.
	 * <p/>
	 * Do not use this to modify the TimeSeries
	 *
	 * @return A reference to the time array.
	 */
	public double[] getTimesRef() {
		return this.times;
	}

	/**
	 * Gets the values of this series. Possibly returns a reference, so changing
	 * values might corrupt the TimeSeries.
	 * <p/>
	 * Do not use this to modify the TimeSeries
	 *
	 * @return A reference to the value array.
	 */
	public double[] getValuesRef() {
		return this.values;
	}

	/**
	 * Gets the specified extra values of this series. Possibly returns a
	 * reference, so changing values might corrupt the TimeSeries.
	 * <p/>
	 * Do not use this to modify the TimeSeries
	 *
	 * @param key - case sensitive key identifying the extra value series
	 * @return A reference to the extra values array.
	 */
	public double[] getExtraValuesRef(String key) {
		if (!(this.extraValues.containsKey(key))) return null;
		return this.extraValues.get(key);
	}

	/**
	 * Gets a set of the keys associated with extra values of this series.
	 *
	 * @return Set of keys of extra value arrays.
	 */
	public Set<String> getExtraValuesKeySet() {
		return this.extraValues.keySet();
	}

	/**
	 * Gets a set of the keys associated with properties of this series.
	 *
	 * @return Set of keys of properties.
	 */
	public Set<String> getPropertiesKeySet() {
		return this.properties.keySet();
	}

	/**
	 * Get property as a String. Return null if property does not exist
	 *
	 * @param propertyName label of the property
	 * @return property
	 */
	public String getProperty(String propertyName) {
		return this.properties.get(propertyName.toLowerCase());
	}

	/**
	 * Get a String property and use a default if this is not available
	 *
	 * @param propertyName The name of the property.
	 * @param defaultValue The default value.
	 * @return The value of the property.
	 */
	public String getStringProperty(String propertyName, String defaultValue) {
		String result = defaultValue;
		if (this.hasProperty(propertyName.toLowerCase())) {
			result = this.properties.get(propertyName.toLowerCase());
		}
		return result;
	}

	/**
	 * Gets a property with the given name and tries to interprete this as a
	 * double
	 *
	 * @param propertyName The name of the property.
	 * @param defaultValue A default value.
	 * @return The value of the property as a double.
	 */
	public double getDoubleProperty(String propertyName, double defaultValue) {
		double result = defaultValue;
		if (this.hasProperty(propertyName.toLowerCase())) {
			try {
				result = Double.parseDouble(this.properties.get(propertyName.toLowerCase()));
			}
			catch (Exception e) {
				result = defaultValue;
			}
		}
		return result;
	}

	/**
	 * Sets a named property to a value
	 *
	 * @param propertyName  The name of the property.
	 * @param propertyValue The value of the property.
	 */
	public void setProperty(String propertyName, String propertyValue) {
		this.properties.put(propertyName.toLowerCase(), propertyValue);
	}

	/**
	 * Returns all named properties. The names of default properties can be
	 * found with getDefaultPropertyNames()
	 *
	 * @return Array with the property names.
	 */
	public String[] getPropertyNames() {
		String result[] = new String[this.properties.size()];
		int i = 0;
		for (String label : this.properties.keySet()) {
			result[i] = label;
			i++;
		}
		return result;
	}

	/**
	 * Returns all default properties. The names of non-default named properties
	 * can be found with getPropertyNames()
	 *
	 * @return Array with the default property names.
	 */
	public String[] getDefaultPropertyNames() {
		String result[] = new String[this.defaultProps.size()];
		int i = 0;
		for (String label : this.defaultProps) {
			result[i] = label;
			i++;
		}
		return result;
	}

	/**
	 * Checks for the existence of a named property
	 *
	 * @param propertyName The name of the property.
	 * @return Whether the poroperty exists.
	 */
	public boolean hasProperty(String propertyName) {
		return this.properties.containsKey(propertyName.toLowerCase());
	}

	/**
	 * Checks for the existence of default properties. This should equal the
	 * names of the 'getter-methods' and 'setter-methods'
	 *
	 * @param propertyName The name of the property.
	 * @return Whether it has a default value.
	 */
	public boolean hasDefaultProperty(String propertyName) {
		return this.defaultProps.contains(propertyName.toLowerCase());
	}

	/**
	 * Checks for the existence of a named series of extra values.
	 *
	 * @param key - case sensitive key identifying the extra value series
	 * @return Whether the given array of extra values exists.
	 */
	public boolean hasExtraValues(String key) {
		return this.extraValues.containsKey(key);
	}

	/**
	 * Create a series by applying a mask on this series.
	 *
	 * @param mask Mask indicating which values should stay.
	 * @return A new time series with only the elements indicated by the mask.
	 */
	public TimeSeries selectMaskSubset(Mask mask) {
		if (this.times == null || this.times.length != mask.getSize()) {
			throw new RuntimeException(
					"TimeSeries: mask length doesn't match series length.");
		}

		double[] newTim = new double[this.times.length];
		double[] newVal = new double[this.values.length];
		int inew = 0;

		for (int i = 0; i < mask.getSize(); ++i) {
			if (!mask.getValue(i)) continue;
			newTim[inew] = this.times[i];
			newVal[inew] = this.values[i];
			++inew;
		}

		TimeSeries result = new TimeSeries();
		result.setMetaData(this);
		if (inew > 0) result.setData(newTim, newVal, inew);
		return result;
	}

	/**
	 * Create a series by retaining the specified time values only.
	 *
	 * @param onlyTimes Time array with times to keep.
	 * @return A new time series with only the elements matching the indicated time values.
	 */
	public TimeSeries selectTimeSubset(double[] onlyTimes) {
		if (this.times == null) {
			throw new RuntimeException("TimeSeries: time array may not be null.");
		}
		if (onlyTimes == null) {
			// Return an empty time series
			TimeSeries result = new TimeSeries();
			result.setMetaData(this);
			return result;
		}

		DoubleArraySearch das = new DoubleArraySearch(onlyTimes);

		double[] newTim = new double[this.times.length];
		double[] newVal = new double[this.values.length];
		int[] index = new int[this.values.length];

		int inew = 0;

		for (int i = 0; i < this.times.length; ++i) {
			if (-1 == das.search(this.times[i])) continue;
			newTim[inew] = this.times[i];
			newVal[inew] = this.values[i];
			index[inew] = i;
			++inew;
		}

		TimeSeries result = new TimeSeries();
		result.setMetaData(this);
		if (inew > 0) result.setData(newTim, newVal, inew);

		for (String extraName : this.extraValues.keySet()) {
			double[] oldExtraVal = this.extraValues.get(extraName);
			double[] newExtraVal = new double[inew];
			for (int i = 0; i < inew; ++i)
				newExtraVal[i] = oldExtraVal[index[i]];
			result.setExtraValues(extraName, newExtraVal);
		}

		return result;
	}

	/**
	 * Create a selection in time of the TimeSeries,
	 * i.e. select items with startTime<t[i]<=endTime.
	 * Does NOT create a selection of the extraValues.
	 *
	 * @param startTime Start time.
	 * @param endTime   End time.
	 * @return Time series containing the selection requested.
	 */
	public TimeSeries selectTimeSubset(double startTime, double endTime) {
		// make selection in time : ASSUMES ORDERED TIMES
		if (!this.timeOrdered) {
			throw new RuntimeException("TimeSeries: time array should be sorted ascending.");
		}

		// find first index that should be included
		// iStart will be set to length if forall times : startTime > times
		int n = 0;
		if (this.times != null) {
			n = this.times.length;
		}

		int iStart = 0;
		while ((iStart < n) && (this.times[iStart] < startTime)) {
			iStart++;
		}
		// find first index that should NOT be included anymore
		int iEnd = iStart;
		if ((n > 0) && (this.times[n - 1] < endTime)) {
			iEnd = this.times.length;
		} else {
			while ((iEnd < n) && (this.times[iEnd] <= endTime)) {
				iEnd++;
			}
		}
		double timesNew[] = null;
		double valuesNew[] = null;
		TimeSeries result;
		if (iEnd > iStart) {
			timesNew = new double[iEnd - iStart];
			valuesNew = new double[iEnd - iStart];
			System.arraycopy(this.times, iStart, timesNew, 0, iEnd - iStart);
			System.arraycopy(this.values, iStart, valuesNew, 0, iEnd - iStart);
			// create ne TimeSeries for result

			result = new TimeSeries(timesNew, valuesNew);
		} else {
			//Create empty timeSeries
			result = new TimeSeries();
		}
		// add metadata
		result.setHeight(this.getHeight());
		result.setLocation(this.getLocation());
		double pos[] = this.getPosition();
		result.setPosition(pos[0], pos[1]);
		result.setQuantity(this.getQuantityId());
		result.setSource(this.getSource());
		result.setUnit(this.getUnitId());
		String propNames[] = getPropertyNames();
		for (int i = 0; i < propNames.length; i++) {
			result.setProperty(propNames[i], this.getProperty(propNames[i]));
		}

		//Set the ID to the ID of the parent. This in case that the default ID Location.QuantityID has
		//been overridden in the parent.
		result.setId(this.getId());
		return result;
	}

	/**
	 * Create a selection in values of the TimeSeries, i.e. select items with
	 * minValue<=value[i]<=maxValue Use Double.NaN to turn of one of the two
	 * bounds.
	 * Does NOT create a selection of the extraValues.
	 *
	 * @param minValue Minimum value.
	 * @param maxValue Maximum value.
	 * @return Time series containing the selection requested.
	 */
	public TimeSeries selectValueSubset(double minValue, double maxValue) {
		// make selection of values
		java.util.Vector<Double> tempTimes = new java.util.Vector<Double>();
		java.util.Vector<Double> tempValues = new java.util.Vector<Double>();
		java.util.Vector<Integer> tempIndex = new java.util.Vector<Integer>();
		for (int i = 0; i < this.values.length; i++) {
			if ((Double.isNaN(minValue)) | (this.values[i] >= minValue)) {
				if ((Double.isNaN(maxValue)) | (this.values[i] <= maxValue)) {
					tempTimes.add(new Double(this.times[i]));
					tempValues.add(new Double(this.values[i]));
					tempIndex.add(new Integer(i));
				}
			}
		}
		double[] timesNew = new double[tempTimes.size()];
		double[] valuesNew = new double[tempTimes.size()];
		int[] index = new int[tempTimes.size()];
		for (int i = 0; i < tempTimes.size(); i++) {
			timesNew[i] = tempTimes.get(i).doubleValue();
			valuesNew[i] = tempValues.get(i).doubleValue();
			index[i] = tempIndex.get(i).intValue();
		}
		// create new TimeSeries for result
		TimeSeries result = new TimeSeries(timesNew, valuesNew);
		// add metadata
		result.setHeight(this.getHeight());
		result.setLocation(this.getLocation());
		double pos[] = this.getPosition();
		result.setPosition(pos[0], pos[1]);
		result.setQuantity(this.getQuantityId());
		result.setSource(this.getSource());
		result.setUnit(this.getUnitId());
		String propNames[] = getPropertyNames();
		for (int i = 0; i < propNames.length; i++) {
			result.setProperty(propNames[i], this.getProperty(propNames[i]));
		}
		// add extra values
		for (String extraName : this.extraValues.keySet()) {
			double[] oldExtraVal = this.extraValues.get(extraName);
			double[] newExtraVal = new double[index.length];
			for (int i = 0; i < index.length; ++i)
				newExtraVal[i] = oldExtraVal[index[i]];
			result.setExtraValues(extraName, newExtraVal);
		}
		return result;
	}

	/**
	 * Briefly describe this TimeSeries
	 */
	
	public String toString() {
		String result = "TimeSeries(\n";
		result += "   Location = " + this.getLocation() + "\n";
		double pos[] = this.getPosition();
		result += "   Position = (" + pos[0] + "," + pos[1] + ")\n";
		result += "   Height   = " + this.getHeight() + "\n";
		result += "   Quantity = " + this.getQuantityId() + "\n";
		result += "   Unit     = " + this.getUnitId() + "\n";
		result += "   Source   = " + this.getSource() + "\n";
		result += "   Id       = " + this.getId() + "\n";
		String propNames[] = getPropertyNames();
		for (int i = 0; i < propNames.length; i++) {
			result += "   " + propNames[i] + "  = " + this.getProperty(propNames[i]) + "\n";
		}
		result += "   Values   = \n";
		if (this.times != null) {
			final int previewLength = 5;
			int iEnd = Math.min(this.times.length, previewLength);
			for (int i = 0; i < iEnd; i++) {
				result += "   (" + this.times[i] + "=" + TimeUtils.mjdToString(this.times[i]) + "," + this.values[i] + ")\n";
			}
			if (this.times.length > (2 * previewLength)) {
				result += "   ...\n";
			}
			if (this.times.length > previewLength) {
				int iStart = Math.max(this.times.length - previewLength, previewLength);
				iEnd = this.times.length;
				for (int i = iStart; i < iEnd; i++) {
					result += "   (" + this.times[i] + "=" + TimeUtils.mjdToString(this.times[i]) + "," + this.values[i] + ")\n";
				}
			}
			result += "\n   Values.length()=" + this.times.length;
		} else {
			result += "()";
		}
		for (String extraValueName : this.extraValues.keySet()) {
			result += "   Extra value " + extraValueName + "; length =" + this.extraValues.get(extraValueName).length + "\n";
		}
		result += "\n";
		result += ")";
		return result;
	}

	
	public String getId() {
		return this.id;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	
	public String getDescription() {
		return this.description;
	}

	public void setId(String id) {
		this.id = id;
		this.autoId = false; // now set manually. Do not overwrite with
		// location.quantity
	}

	
	public void setValues(Object values) {
		if (values instanceof org.openda.exchange.timeseries.TimeSeries) {
			TimeSeries series = (TimeSeries) values;
			this.setData(series.getTimes(), series.getValuesAsDoubles());
		} else if (values instanceof double[]) {
			double[] valuesCast = (double[]) values;
			if ((this.times == null) || (this.times.length != valuesCast.length)) {
				throw new RuntimeException(
						"TimeSeries: length for times and values do not match.");
			}
			this.values = new double[valuesCast.length];
			System.arraycopy(valuesCast, 0, this.values, 0, valuesCast.length);
		}else if (values instanceof IArray){
			double valuesCast[]=((IArray)values).getValuesAsDoubles(); 
			if ((this.times == null) || (this.times.length != valuesCast.length)) {
				throw new RuntimeException(
						"TimeSeries: length for times and values do not match.");
			}
			this.values = new double[valuesCast.length];
			System.arraycopy(valuesCast, 0, this.values, 0, valuesCast.length);
		} else {
				throw new RuntimeException("TimeSeries: cannot digest this type to set values");
		}
	}

	
	public void setValuesAsDoubles(double[] values) {
		if ((this.times == null) || (this.times.length != values.length)) {
			throw new RuntimeException(
					"TimeSeries: length for times and values do not match.");
		}
		this.values = new double[values.length];
		System.arraycopy(values, 0, this.values, 0, values.length);
	}

	/**
	 * Set an extra value series (overwrites an existing series with the same
	 * key).
	 *
	 * @param key		 - case sensitive key identifying the extra value series
	 * @param extraValues - the values to be set as the extra value series
	 */
	public void setExtraValues(String key, double[] extraValues) {
		if ((this.times == null) || (this.times.length != extraValues.length)) {
			throw new RuntimeException(
					"TimeSeries: length for times and extra values do not match.");
		}
		this.extraValues.put(key, new double[extraValues.length]);
		System.arraycopy(extraValues, 0, this.extraValues.get(key), 0, extraValues.length);
	}

	/**
	 * Provide access to the TimeSeries values as an Array
	 *
	 * @return this as Array
	 */
	
	public Object getValues() {
		return new Array(this.getValuesAsDoubles());
	}

	/**
	 * Get the index associated with the given moment in time
	 * (or -1 in case the value isn't found).
	 * ASSUMES ORDERED TIMES
	 *
	 * @param time The time to search for.
	 * @return The resulting index.
	 */
	public int findOnTime(double time) {
		if (!this.timeOrdered) {
			throw new RuntimeException("TimeSeries: time array should be sorted ascending.");
		}

		if (time < this.tstart || time > this.tstop) return -1;

		if (this.timesearcher == null) this.timesearcher = new DoubleArraySearch(this.times);

		return this.timesearcher.search(time);
	}

	/**
	 * Get the value for the specified time.
	 * If the exact time isn't found, Double.NaN is returned
	 * (depending on the value set with setInterpolate()).
	 * If the value is outside the array, Double.NaN is returned.
	 * ASSUMES ORDERED TIMES
	 *
	 * @param time The time to search for.
	 * @return The resulting value.
	 */
	public double getValue(double time) {
		if (!this.timeOrdered) {
			throw new RuntimeException("TimeSeries: time array should be sorted ascending.");
		}

		if (time < this.tstart || time > this.tstop) return Double.NaN;

		int index = findOnTime(time);
		if (index != -1) return this.values[index];
		return Double.NaN;
	}

	/**
	 * Get the value for the specified time.
	 * If the exact time isn't found an interpolated value is returned
	 * If the value is outside the array, Double.NaN is returned.
	 * ASSUMES ORDERED TIMES
	 *
	 * @param time The time to search for.
	 * @return The resulting value.
	 */
	public double getInterpolatedValue(double time) {
		if (!this.timeOrdered) {
			throw new RuntimeException("TimeSeries: time array should be sorted ascending.");
		}

		if (time < this.tstart || time > this.tstop) return Double.NaN;

		int index = findOnTime(time);
		if (index != -1) return this.values[index];
		if (this.timesearcher.isOutOfBounds()) return Double.NaN;

		// Interpolate
		double value1 = this.values[this.timesearcher.getLowerBound()];
		double value2 = this.values[this.timesearcher.getUpperBound()];
		return value1 + (value2 - value1) * this.timesearcher.getPosition();
	}

	/**
	 * Get the specified extra value for the specified time.
	 * If the exact time isn't found, Double.NaN is returned.
	 * If the value is outside the array, Double.NaN is returned
	 *
	 * @param key  The key of the extra values array.
	 * @param time The time to search for.
	 * @return The resulting extra values' value.
	 */
	public double getExtraValueAsDouble(String key, double time) {
		if (time < this.tstart || time > this.tstop) return Double.NaN;

		if (this.timesearcher == null) this.timesearcher = new DoubleArraySearch(this.times);

		int index = this.timesearcher.search(time);
		if (index != -1) return this.extraValues.get(key)[index];
		return Double.NaN;
	}

	/**
	 * Get the specified extra value for the specified time.
	 * If the exact time isn't found, an interpolated value is returned.
	 * If the value is outside the array, Double.NaN is returned
	 *
	 * @param key  The key of the extra value array.
	 * @param time The time to search for.
	 * @return The resulting extra values' value.
	 */
	public double getInterpolatedExtraValueAsDouble(String key, double time) {
		if (time < this.tstart || time > this.tstop) return Double.NaN;

		if (this.timesearcher == null) this.timesearcher = new DoubleArraySearch(this.times);

		int index = this.timesearcher.search(time);
		if (index != -1) return this.extraValues.get(key)[index];
		if (this.timesearcher.isOutOfBounds()) return Double.NaN;

		// Interpolate
		double value1 = this.extraValues.get(key)[this.timesearcher.getLowerBound()];
		double value2 = this.extraValues.get(key)[this.timesearcher.getUpperBound()];
		return value1 + (value2 - value1) * this.timesearcher.getPosition();
	}

	/**
	 * Show Type of this object for proper casting
	 */
	
	@SuppressWarnings("rawtypes")
	public Class getValueType() {
		return org.openda.utils.Array.class;
	}

	
	public Role getRole() {
		return this.role;
	}

	/**
	 * Gets the values of this series at the specified times.
	 * Does not change the TimeSeries.
	 *
	 * @param selectTimes at which to select values
	 * @param tolerance   for time selection. What difference should be considered equal
	 * @param dummy	   values to return for non-matching times
	 * @return values at selected times
	 */
	public double[] getValuesAsDoubles(double[] selectTimes, double tolerance, double dummy) {
		double[] result = null;
		if (this.times != null) {
			int[][] index = SortUtils.mergeDoubleIndex(selectTimes, this.times, SortUtils.MergeType.left, tolerance);
			result = SortUtils.applyIndexToDoubles(this.values, index[1], dummy);
		}
		return result;
	}

	/**
	 * Calculate start and end time of the series, as well as the time steps.
	 * In addition, also checks whether the time series is sorted ascending.
	 */
	private void calculateTime() {
		// Calculate tstart, tstop, tstep and timeOrdered
		if (this.times == null || this.times.length == 0) {
			this.tstart = Double.NaN;
			this.tstop = Double.NaN;
			this.tstep = Double.NaN;
			this.timeOrdered = true;
		} else {
			this.tstart = this.times[0];
			this.tstop = this.times[this.times.length - 1];
			this.tstep = (this.times.length == 1 ? 0.0 : this.times[1] - this.times[0]);
			this.timeOrdered = true;

			double last = this.times[0];
			for (int i = 1; i < this.times.length; ++i) {
				double current = this.times[i];
				if (this.tstart > current) this.tstart = current;
				if (this.tstop < current) this.tstop = current;
				if (current < last) {
					this.timeOrdered = false;
					this.tstep = Double.NaN;
				} else if (Math.abs(current - last) > Double.MIN_VALUE) {
					this.tstep = Double.NaN;
				}
				last = current;
			}
		}
	}

	/**
	 * @return The start time of the series.
	 */
	public double getStartTime() {
		return this.tstart;
	}

	/**
	 * @return The end time of the series.
	 */
	public double getStopTime() {
		return this.tstop;
	}

	/**
	 * Gets timestep size of the series.
	 * If the step size is irregular or the series is not ordered (sorted ascending), returns Double.NaN.
	 *
	 * @return The time step.
	 */
	public double getTimeStep() {
		return this.tstep;
	}

	/**
	 * Indicates whether the time array is sorted ascending (as it should be).
	 *
	 * @return Whether this time series is sorted ascending.
	 */
	public boolean isTimeOrdered() {
		return this.timeOrdered;
	}

	/**
	 * Check whether this time series intersects with the given time interval
	 *
	 * @param t_start The start time of the interval.
	 * @param t_stop  The end time of the interval.
	 * @return Whether this time series intersects with the given time interval
	 */
	public boolean intersectsWithTimeInterval(double t_start, double t_stop) {
		if (this.tstart <= t_stop && this.tstart >= t_start) return true; // this.tstart inside [t_start,t_stop]
		if (this.tstop <= t_stop && this.tstop >= t_start) return true; // this.tstop inside [t_start,t_stop]
		if (t_start <= this.tstop && t_start >= this.tstart) return true; // t_start inside [this.tstart,this.tstop]
		if (t_stop <= this.tstop && t_stop >= this.tstart) return true; // t_stop inside [this.tstart,this.tstop]
		if (this.tstart <= t_start && this.tstop >= t_stop)
			return true; // [t_start,t_stop] inside [this.tstart,this.tstop]
		if (t_start <= this.tstart && t_stop >= this.tstop)
			return true; // [this.tstart,this.tstop] inside [t_start,t_stop]
		return false;
	}

	/**
	 * Compares the times, values and extraValues of two time series.
	 * Doesn't compare metadata.
	 *
	 * @param other The time series to compare to.
	 * @return Whether this time series' data equals that of the other time series.
	 */
	public boolean equals(TimeSeries other) {
		return equals(other, false);
	}

	/**
	 * Compares the times, values and extraValues of two time series.
	 * Doesn't compare metadata.
	 *
	 * @param other   The time series to compare to.
	 * @param verbose Flag to indicate whether an indication should be given about the difference.
	 * @return Whether this time series' data equals that of the other time series.
	 */
	public boolean equals(TimeSeries other, boolean verbose) {
		if (this.times == null && other.times == null) return true;
		if (this.values == null && other.values == null) return true;
		if (this.times.length != other.times.length) {
			if (verbose)
				System.err.println("Lengths of the time arrays do not match. this: " + String.valueOf(this.times.length)
						+ "; other: " + String.valueOf(other.times.length));
			return false;
		}
		if (this.values.length != other.values.length) {
			if (verbose)
				System.err.println("Lengths of the value arrays do not match. this: " + String.valueOf(this.values.length)
						+ "; other: " + String.valueOf(other.values.length));
			return false;
		}
		if (this.extraValues.keySet().size() != other.extraValues.keySet().size()) {
			if (verbose) System.err.println("Number of extraValue arrays does not match.");
			return false;
		}
		for (String key : this.extraValues.keySet()) {
			if (!other.extraValues.keySet().contains(key)) {
				if (verbose) System.err.println("this.extraValues key missing in other: " + key);
				return false;
			}
		}
		for (String key : other.extraValues.keySet()) {
			if (!this.extraValues.keySet().contains(key)) {
				if (verbose) System.err.println("other.extraValues key missing in this: " + key);
				return false;
			}
		}
		for (int i = 0; i < this.times.length; ++i) {
			if (this.times[i] != other.times[i] && !(Double.isNaN(this.times[i]) && Double.isNaN(other.times[i]))) {
				if (verbose)
					System.err.println("At index " + String.valueOf(i) + ": times do not match. this: "
							+ String.valueOf(this.times[i]) + "; other: " + String.valueOf(other.times[i]));
				return false;
			}
			if (this.values[i] != other.values[i] && !(Double.isNaN(this.values[i]) && Double.isNaN(other.values[i]))) {
				if (verbose)
					System.err.println("At index " + String.valueOf(i) + ": values do not match. this: "
							+ String.valueOf(this.values[i]) + "; other: " + String.valueOf(other.values[i]));
				return false;
			}
			for (String key : this.extraValues.keySet()) {
				if (this.extraValues.get(key)[i] != other.extraValues.get(key)[i] && !(Double.isNaN(this.extraValues.get(key)[i])
						&& Double.isNaN(other.extraValues.get(key)[i]))) {
					if (verbose)
						System.err.println("At index " + String.valueOf(i) + ": extraValues " + key + " do not match. this: "
								+ String.valueOf(this.values[i]) + "; other: " + String.valueOf(other.values[i]));
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * @return The length of (number of elements in) this time series
	 */
	public int getSize() {
		return (this.times == null ? 0 : this.times.length);
	}

	/**
	 * @return The average value of the values in this time series
	 */
	public double average() {
		if (this.values == null || this.values.length == 0) return Double.NaN;

		double sum = 0.0;
		int count = 0;
		for (double value : this.values) {
			if (!Double.isNaN(value)) {
				sum += value;
				++count;
			}
		}
		return sum / count;
	}

	/**
	 * @return The average value of the absolute values in this time series
	 */
	public double averageAbs() {
		if (this.values == null || this.values.length == 0) return Double.NaN;

		double sum = 0.0;
		int count = 0;
		for (double value : this.values) {
			if (!Double.isNaN(value)) {
				sum += Math.abs(value);
				++count;
			}
		}
		return sum / count;
	}

	/**
	 * @return The RMS value of the values in this time series
	 */
	public double rms() {
		if (this.values == null || this.values.length == 0) return Double.NaN;

		double sumsq = 0.0;
		int count = 0;
		for (double value : this.values) {
			if (!Double.isNaN(value)) {
				sumsq += (value * value);
				++count;
			}
		}
		return Math.sqrt(sumsq / count);
	}

	/**
	 * @return The maximum value of the values in this time series
	 */
	public double maximum() {
		if (this.values == null || this.values.length == 0) return Double.NaN;

		double max = Double.NEGATIVE_INFINITY;
		for (double value : this.values)
			if ((!Double.isNaN(value)) && (value > max)) max = value;
		return max;
	}

	/**
	 * @return The minimum value of the values in this time series
	 */
	public double minimum() {
		if (this.values == null || this.values.length == 0) return Double.NaN;

		double min = Double.POSITIVE_INFINITY;
		for (double value : this.values)
			if ((!Double.isNaN(value)) && (value < min)) min = value;
		return min;
	}

	/**
	 * Calculates the standard deviation of the values in this time series
	 *
	 * @param average The average value calculated before
	 * @return The standard deviation
	 */
	public double stdDev(double average) {
		if (this.values == null) return Double.NaN;

		double sumsqdev = 0.0;
		int count = 0;
		for (double value : this.values) {
			if (!Double.isNaN(value)) {
				final double dev = (value - average);
				sumsqdev += (dev * dev);
				++count;
			}
		}

		if (count == 0) return Double.NaN;
		if (count == 1) return 0.0;
		return Math.sqrt(sumsqdev / (count - 1));
	}

	/**
	 * Create an error series by subtracting the values from a reference series
	 * from the values of this series, resulting in a new time series.
	 * Only times with values in both series will be included.
	 *
	 * @param reference The reference time series.
	 * @return A new time series with the errors relative to the reference time series.
	 */
	public TimeSeries errorSeries(TimeSeries reference) {
		if (this.times == null || this.values == null) return new TimeSeries(this);

		double[] newTim = new double[this.times.length];
		double[] newVal = new double[this.values.length];
		int inew = 0;

		for (int i = 0; i < this.times.length; ++i) {
			final double time = this.times[i];
			if (time > reference.getStopTime() || time < reference.getStartTime()) continue;
			if (Double.isNaN(this.values[i])) continue;
			final double refvalue = reference.getValue(time);
			if (Double.isNaN(refvalue)) continue;
			newTim[inew] = time;
			newVal[inew] = this.values[i] - refvalue;
			++inew;
		}

		TimeSeries result = new TimeSeries();
		result.setMetaData(this);
		if (inew > 0) result.setData(newTim, newVal, inew);
		return result;
	}

	/**
	 * @return The parent time series (or null).
	 */
	public TimeSeries getParent() {
		return this.parent;
	}

	/**
	 * @param parent The parent time series (or null).
	 */
	public void setParent(TimeSeries parent) {
		this.parent = parent;
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		// TODO Auto-generated method stub
		
	}

	
	public ITimeInfo getTimeInfo() {
		ArrayTimeInfo tInfo=new ArrayTimeInfo(this.getTimes(), 0);
		return tInfo;
	}

	
	public IQuantityInfo getQuantityInfo() {
		QuantityInfo qInfo = new QuantityInfo(this.getQuantityId(), this.getUnitId());
		return qInfo;
	}

	
	public IGeometryInfo getGeometryInfo() {
		double pos[]=this.getPosition();
		if((pos!=null) && (pos.length==2)){
			return new PointGeometryInfo(pos[0], pos[1], this.getHeight());
		}else{
			return null;
		}
	}

	
	public ValueType getValuesType() {
		// TODO Auto-generated method stub
		return ValueType.IArrayType;
	}

}
