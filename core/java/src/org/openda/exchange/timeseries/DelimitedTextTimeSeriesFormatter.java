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

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.ConfigTree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.text.*;
import java.util.*;
import java.util.regex.Pattern;

public class DelimitedTextTimeSeriesFormatter extends TimeSeriesFormatter{

	private static final Logger logger = LoggerFactory.getLogger(DelimitedTextTimeSeriesFormatter.class);

	private DateFormat dateFormatter;
	private DecimalFormat decimalFormat;
	private TimeZone timeZone = TimeZone.getTimeZone("GMT");
	private String delimiter;
	private String commentMarker;
	private int skipLines;
	private boolean StoreAndwriteSkipped;
	private boolean StoreAndwriteComment;
	private int dateTimeSelector;
	private int valueSelector;

	private IPrevExchangeItem.Role role;

	public DelimitedTextTimeSeriesFormatter(ConfigTree configTree) {
		String datePattern = configTree.getAsString("dateTimePattern", null);
		if (datePattern != null ) this.dateFormatter = new SimpleDateFormat(datePattern);
		this.timeZone = TimeZone.getTimeZone(configTree.getAsString("timeZone","GMT"));
		this.delimiter = configTree.getAsString("delimiter",null);
		this.commentMarker = configTree.getAsString("commentMarker",null);
		this.skipLines = configTree.getAsInt("skipLines",0);
		this.StoreAndwriteSkipped = configTree.getAsBoolean("StoreAndwriteSkipped",true);
		this.StoreAndwriteComment = configTree.getAsBoolean("StoreAndwriteComment",true);
		this.dateTimeSelector = configTree.getAsInt("dateTimeSelector",  0);
		this.valueSelector = configTree.getAsInt("valueSelector",  1);
		DecimalFormatSymbols symbols = new DecimalFormatSymbols();
		symbols.setDecimalSeparator(configTree.getAsString("decimalSeparator",  ".").charAt(0));
		this.decimalFormat = new DecimalFormat();
		this.decimalFormat.setDecimalFormatSymbols(symbols);
		String role = configTree.getAsString("role", "input");

		if ("input".equalsIgnoreCase(role)){
			this.role = IPrevExchangeItem.Role.Input;
		} else if ("output".equalsIgnoreCase(role)) {
			this.role = IPrevExchangeItem.Role.Output;
		} else if ("inout".equalsIgnoreCase(role)) {
			this.role = IPrevExchangeItem.Role.InOut;
		}

		if (this.dateFormatter!=null) {
			this.dateFormatter.setTimeZone(this.timeZone);
		}
	}

	/**
	 * Method for writing a TimeSeries in Delimited Text format.
	 *
	 * @param out    OutputStream to write to
	 * @param series Series to write
	 */
	public void write(OutputStream out, TimeSeries series) throws IOException{
		OutputStreamWriter writer = new OutputStreamWriter(out);
		double[] times = series.getTimesRef();
		double[] values = series.getValuesRef();
		String headerContents = series.getProperty("header");
		writer.write(headerContents);
		if (Math.max(this.dateTimeSelector,this.valueSelector)> 1) {
			logger.error("Cannot write delimited file with more than two columns");
			throw new RuntimeException("Cannot write delimited file with more than two columns");
		}
		for (int i=0;i< times.length ; i++) {
			String[] parts = new String[2];
			String dateString;
			if (this.dateFormatter != null) {
				Date date = TimeUtils.mjdToDate(times[i]);
				dateString = this.dateFormatter.format(date);
				logger.debug(dateString);
			} else {
				dateString = String.valueOf(times[i]);
			}
			parts[this.dateTimeSelector] = dateString;
			parts[this.valueSelector] = this.decimalFormat.format(values[i]);
			for (int s=0; s<parts.length-1;s++) {
				writer.write(parts[s]);
				writer.write(this.delimiter);
			}
			writer.write(parts[parts.length-1]);
			writer.write("\n");
		}
		writer.close();
	}

	/**
	 * Method to read a TimeSeries from a Delimited Text format.
	 *
	 * @param in InputStream to read from
	 * @return series TimeSeries with the data
	 */
	public TimeSeries read(InputStream in) throws IOException, ParseException{
		LineNumberReader reader = new LineNumberReader(new InputStreamReader( in ));
		String field="";
		ArrayList<Double> times = new ArrayList<>();
		ArrayList<Double> values = new ArrayList<>();
		StringBuilder headerContents = new StringBuilder();
		String exponentSymbol = this.decimalFormat.getDecimalFormatSymbols().getExponentSeparator();
		int skip = skipLines;
		try {
			String line;
			while((line = reader.readLine()) != null) {
				Scanner s = new Scanner(line).useDelimiter(delimiter);
				field = line;
				s.useLocale(Locale.US);
				// skip header
				if ( skip > 0 ) { headerContents.append(line).append("\n"); skip--; continue;}
				if (this.commentMarker != null) {
					if (s.hasNext(Pattern.compile(this.commentMarker + ".*"))) continue;  //TODO use comment marker
				}
				//parse date time

				String[] parts = line.split(this.delimiter);
				if (parts.length < Math.max(this.dateTimeSelector,this.valueSelector)) {
					throw new InputMismatchException();
				}
				if (dateFormatter!=null) {
					Date time = dateFormatter.parse(parts[this.dateTimeSelector]);
					logger.trace("getTime: {} ", time.getTime());
					times.add(TimeUtils.date2Mjd(time));
					logger.trace("time={} at line {}",time, reader.getLineNumber());
				} else {
					ParsePosition pos = new ParsePosition(0);
					String timeString =  parts[this.dateTimeSelector].trim();
					double time = this.decimalFormat.parse(timeString,pos).doubleValue();
					if (pos.getIndex() != timeString.length() ) {
						throw new ParseException(String.format("Error parsing '%s' in '%s' ",  timeString.substring(pos.getIndex()) ,timeString), pos.getErrorIndex());
					}
					logger.trace("time={} at line {}",time, reader.getLineNumber());
					times.add(time);
				}
				// parse value
				ParsePosition pos = new ParsePosition(0);
				String valueString =  parts[this.valueSelector].trim();
				valueString = valueString.replaceFirst(exponentSymbol+"\\+", exponentSymbol);
				double value = this.decimalFormat.parse(valueString,pos).doubleValue();
				if (pos.getIndex() != valueString.length() ) {
					throw new ParseException(String.format("Error parsing '%s' in '%s' ",  valueString.substring(pos.getIndex()) ,valueString), pos.getErrorIndex());
				}
				logger.trace("value={} at line {}",value, reader.getLineNumber());
				values.add(value);
				s.close();
			}
		}
		catch (ParseException | InputMismatchException ex) {
			System.out.println("Error parsing '" +  field + "' at line: " +  reader.getLineNumber());
			logger.error("Error parsing '{}' at line: {}",field, reader.getLineNumber());
			throw ex;
		}
		double[] t = toPrimitive(times.toArray(new Double[0]) );
		double[] v = toPrimitive(values.toArray(new Double[0]) );
		logger.debug("times: {}",t);
		logger.debug("values: {}",v);

		TimeSeries ts = new TimeSeries(t,v);
		ts.setProperty("header",headerContents.toString());
		ts.role = this.role;
		return ts;
	}

	private static double[] toPrimitive(final Double[] array) {
		if (array == null) {
			return null;
		} else if (array.length == 0) {
			return new double[0];
		}
		final double[] result = new double[array.length];
		for (int i = 0; i < array.length; i++) {
			result[i] = array[i];
		}
		return result;
	}

}
