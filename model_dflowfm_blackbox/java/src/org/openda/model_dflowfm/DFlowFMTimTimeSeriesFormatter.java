/* OpenDA v2.4 
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
package org.openda.model_dflowfm;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.Vector;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IPrevExchangeItem.Role;

public class DFlowFMTimTimeSeriesFormatter extends TimeSeriesFormatter {

	public static final String PROPERTY_ANALTIME = "analTime";
	public static final String PROPERTY_TIMEZONE = "timeZone";
	private static Double referenceDateInMjd = null;
	private static Double factorToMjd = null;
	
	public DFlowFMTimTimeSeriesFormatter() {
		super();
	}
	
	public DFlowFMTimTimeSeriesFormatter(Double refDate, Double timeFactor) {
		this();
		referenceDateInMjd = refDate;
		factorToMjd = timeFactor;
		// TODO: overrule timeFactor for TIM files it is always minutes, change this when the TIM file format is changed
		factorToMjd = 1.0 / (60.0*24.0); 
	}
	
	
	public void write(OutputStream out, TimeSeries series) {
		write(new PrintWriter(out), series);
	}
	
	public void write(PrintWriter printer, TimeSeries series) {

		Locale locale  = new Locale("en", "US");
		DecimalFormat formatter = (DecimalFormat)NumberFormat.getNumberInstance(locale);
		formatter.applyPattern("0.0######E000");

		// now the data
		final double times[] = series.getTimesRef();
		final double values[] = series.getValuesRef();
		for (int i = 0; i < times.length; i++) {
			String line = formatter.format((times[i] - referenceDateInMjd)/factorToMjd) + " " + formatter.format(values[i]);
			printer.println(line);
		}
		printer.flush();
	}
	
	
	
	public TimeSeries read(InputStream in) {
		return read(new BufferedReader(new InputStreamReader(in)));
	}

	public TimeSeries read(BufferedReader buff) {
		TimeSeries result = null;
		String description = "";
		boolean collectDescription = false;
		String location = ""; // Location : den helder
		double x = 0.0; // Position : (4.745356,52.966001)
		double y = 0.0;
		double height = Double.NaN;
		String source = ""; // Source : observed
		String quantity = ""; // Unit : waterlevel_astro !!! Note different label
		String analTime = ""; // Analyse time: most recent
		String timeZone = ""; // Timezone : GMT
		String unit     = "";
		Role role     = Role.InOut;
		double times[] = null;
		double values[] = null;
		double analTimes[] = null;
		Vector<Double> timeVector = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valueVector = new Vector<Double>(); // since we don't
		Vector<Double> analVector = new Vector<Double>(); // know the size beforehand

		boolean eof = false;
		String line;
		while (!eof) {
			try {
				line = buff.readLine();
			}
			catch (Exception e) {
				throw new RuntimeException("");
			}
			if (line == null) eof = true;
			else { // now parse this line
				//System.out.println("line = '" + line + "'");
				if (line.startsWith("#")) { // comment or metadata
					
				}
				else { // datetime value pair
					if (line.length() > 5) {
						String columns[] = line.trim().split("(\\s)+");
						double time;
						//System.out.println("time = '" + columns[0] + "'");
						try {
							time =  Double.parseDouble(columns[0]) * factorToMjd + referenceDateInMjd;
						}
						catch (Exception e) {
							throw new RuntimeException("Trouble parsing time :" + columns[0]);
						}
						timeVector.add(time);

						double value;
						//System.out.println("value = '" + columns[1] + "'");
						try {
							value = Double.parseDouble(columns[1]);
						}
						catch (Exception e) {
							throw new RuntimeException("Trouble parsing value :" + columns[1]);
						}
						valueVector.add(value);
					}
				}
			}

		}
		// create and fill TimeSeries object
		times = new double[timeVector.size()];
		values = new double[valueVector.size()];
		if (times.length != values.length) { throw new RuntimeException("Value vector length doesn't match time vector length."); }

		for (int i = 0; i < times.length; i++) {
			times[i] = timeVector.get(i);
			values[i] = valueVector.get(i);
		}
		result = new TimeSeries(times, values, x, y, source, quantity, unit, location, role);
		result.setHeight(height);
		result.setProperty(PROPERTY_ANALTIME, analTime);
		result.setProperty(PROPERTY_TIMEZONE, timeZone);
		result.setDescription(description);
		return result;
	}
	
	
	
	
}
