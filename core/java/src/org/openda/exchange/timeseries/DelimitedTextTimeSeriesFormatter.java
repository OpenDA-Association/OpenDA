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

import org.openda.utils.ConfigTree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Pattern;

public class DelimitedTextTimeSeriesFormatter extends TimeSeriesFormatter{

	private static final Logger logger = LoggerFactory.getLogger(DelimitedTextTimeSeriesFormatter.class);

	protected DateFormat dateFormatter;
	protected TimeZone timeZone = TimeZone.getTimeZone("GMT");
	protected String delimiter;
	protected int skipLines;

	public DelimitedTextTimeSeriesFormatter(ConfigTree configTree) {
		String datePattern = configTree.getAsString("dateTimePattern", null);
		if (datePattern != null ) this.dateFormatter = new SimpleDateFormat(datePattern);
		this.timeZone = TimeZone.getTimeZone(configTree.getAsString("timeZone","GMT"));
		this.delimiter = configTree.getAsString("delimiter",null);
		this.skipLines = configTree.getAsInt("skipLines",0);

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
		if (this.dateFormatter== null) {
			for (double time : times) {
				writer.write(String.valueOf(time));
			}
		} else {
			for (double time : times) {
				Date date = TimeUtils.mjdToDate(time);
				String dateString = this.dateFormatter.format(date);
				logger.debug(dateString);
				writer.write(dateString);
			}
		}
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
		int skip = skipLines;
		try {
			String line;
			while((line = reader.readLine()) != null) {
				Scanner s = new Scanner(line).useDelimiter(delimiter);
				field = line;
				// skip header
				if ( skip > 0 ) { skip--; continue;}
				if (s.hasNext(Pattern.compile("#.*"))) continue;
				//parse date time
				if (dateFormatter!=null) {
					field = s.next();
					Date time = dateFormatter.parse(field);
					logger.trace("getTime: {} ", time.getTime());
					times.add(TimeUtils.date2Mjd(time));
					logger.trace("time={} at line {}",time, reader.getLineNumber());
				} else {
					double time = s.nextDouble();
					logger.trace("time={} at line {}",time, reader.getLineNumber());
					times.add(time);
				}
				// parse value
					double value = s.nextDouble();
					logger.trace("value={} at line {}",value, reader.getLineNumber());
					values.add(value);

				s.close();
			}
		}
		catch (ParseException | InputMismatchException ex) {
			logger.error("Error parsing '{}' at line: {}",field, reader.getLineNumber());
			throw ex;
		}
		double[] t = toPrimitive(times.toArray(new Double[times.size()]) );
		double[] v = toPrimitive(values.toArray(new Double[values.size()]) );
		logger.debug("times: {}",t);
		logger.debug("values: {}",v);

		TimeSeries ts = new TimeSeries(t,v);

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
			result[i] = array[i].doubleValue();
		}
		return result;
	}

}
