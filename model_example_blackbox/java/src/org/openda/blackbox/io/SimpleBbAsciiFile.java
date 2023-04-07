/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.blackbox.io;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Results;

import java.io.*;
import java.text.ParseException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;

/**
 *
 * Read from ascii file in following format
 *
 * # lines with a hash-sign are ignored
 * #
 * # input for 1d pollution model
 * #
 * # grid
 * x = [0.0, 100.0, 4000.0]
 * # stationary flow
 * u = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
 * # cross sectional area
 * a = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
 * # initial concentrations
 * c = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
 * # simulation timespan
 * refdate = '01 dec 1999'
 * #unit is always seconds
 * unit = 'seconds'
 * time = [ 0,100,10000]
 * # sources mass/m^3/s
 * source_locations = [5, 15, 25]
 * source_labels = ['factory1','factory2','factory3']
 * source_values['factory1'] = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.0,4.0,4.0,4.0,4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.0,4.0,4.0,4.0,4.0,4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
 * source_values['factory2'] = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0]
 * source_values['factory3'] = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.0,0.0,0.0,0.0,5.0,5.0,5.0,5.0,5.0,5.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,9.0,9.0,9.0,9.0,9.0,9.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
 * #output (index based and 0 based)
 * output_file = 'pollution_model.output'
 * output_locations = [10, 30, 35]
 * output_labels = ['locA','locB','locC']
 * # boundaries
 * # only left and right at locations 0 and -1 allowed at the moment
 * bound_labels=['left', 'right']
 * bound_locations=[0, -1]
 * bound_values['left']=[0.00, 0.01, 0.02, 0.03]
 * bound_values['right']=[0.0]
 *
 */


public class SimpleBbAsciiFile extends AbstractDataObject {
	File workingDir;
	String fileName = null;
	HashMap<String,String> variables = new LinkedHashMap<>();

	//cache these values
	double refdate;
	double tstart;
	double dt=1.0;
	double tstop;
	double unit=1.0;
	String[] sourceLabels;
	String[] boundLabels;
	String[] outputLabels;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		this.fileName = arguments[0];
		System.out.println("dataobject : filename = "+arguments[0]);


		if (arguments.length > 1) {
			for(int i=1;i<arguments.length;i++){
				System.out.println("dataobject : arg = "+arguments[i]);
			}
			Results.putMessage("dataobject for file="+arguments[0]+" arguments ignored");
			//throw new RuntimeException("DataObject SimpleBbAsciiFile does not expect any additional arguments");
		}
		File inputFile=null;
		// check file
		try{
			inputFile = new File(workingDir,arguments[0]);
			if(!inputFile.isFile()){
				throw new IOException("Can not find file"+inputFile);
			}
			this.fileName = inputFile.getCanonicalPath();
		}catch (Exception e) {
			System.out.println("SimpleBbAsciiFile: trouble opening file "+ this.fileName);
		}
		//read file and parse to hash
		try {
			FileInputStream in = new FileInputStream(inputFile);
			BufferedReader buff = new BufferedReader(new InputStreamReader(in));

			String line;
			boolean eof=false;
			while (!eof) {
				line = buff.readLine();
				if (line == null)
					eof = true;
				else { // now parse this line
					//System.out.println("line = '" + line + "'");
					if (line.startsWith("#")) {
						// comment or metadata
					} else if(line.indexOf("=")>0)
					{ // variable=value
						String[] columns = line.split("=");
						columns[0]=columns[0].trim();
						//System.out.println("variable = "+columns[0]);
						//System.out.println("value ="+columns[1]);
						variables.put(columns[0], columns[1]);
					}
				}
			}
			in.close();
		} catch (Exception e) {
			throw new RuntimeException("Problem reading from file "+arguments[0]+" : "+e.getMessage());
		}

		//extract exchangeitems
		// first general bits
		if(!variables.containsKey("refdate")){
			throw new RuntimeException("Keyword refdate not found in input");
		}
		String refDateString = parseString(variables.get("refdate"));
		try {
			refdate = org.openda.exchange.timeseries.TimeUtils.date2Mjd(refDateString,"dd MMM yyyy");
		} catch (ParseException e) {
			throw new RuntimeException("Trouble parsing a date. Expecting eg 01 jan 2010, found "+refDateString);
		}
		if(!variables.containsKey("unit")){
			throw new RuntimeException("Keyword unit not found in input");
		}
		String unitString = parseString(variables.get("unit"));

		if (unitString.equalsIgnoreCase("seconds")){
			unit = 1.0/24.0/60.0/60.0;
		}else{
			throw new RuntimeException("Unit not recognized (expected seconds) was "+unitString);
		}
		String timeString = variables.get("time");
		double[] time = parseVector(timeString);
		if(time.length!=3){
			throw new RuntimeException("expecting vector of length 3 for time, but length was"+time.length);
		}
		tstart=time[0]*unit;
		dt=time[1]*unit;
		tstop=time[2]*unit;

		//add exchange items for time
		exchangeItems.put("startTime", new DoubleExchangeItem("startTime", this.refdate+this.tstart));
		exchangeItems.put("endTime", new DoubleExchangeItem("endTime", this.refdate+this.tstop));

		//series for sources
		/*
		 * # sources mass/m^3/s
		 * source_locations = [5, 15, 25]
		 * source_labels = ['factory1','factory2','factory3']
		 * source_values['factory1'] = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.0,4.0,4.0,4.0,4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.0,4.0,4.0,4.0,4.0,4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
		 * source_values['factory2'] = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,0.0,0.0,0.0,0.0,0.0,0.0,8.0,8.0,8.0,8.0]
		 * source_values['factory3'] = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,0.0,0.0,0.0,0.0,5.0,5.0,5.0,5.0,5.0,5.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,9.0,9.0,9.0,9.0,9.0,9.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
		 */
		String sourceLabelsString = variables.get("source_labels");
		if(sourceLabelsString!=null){
			sourceLabels = parseStrings(sourceLabelsString);
			for (String sourceLabel : sourceLabels) {
				//System.out.println("source label ="+sourceLabels[i]);
				String valueLabelString = "source_values['" + sourceLabel + "']";
				String valueString = variables.get(valueLabelString);
				if (valueString == null) {
					throw new RuntimeException("Missing input. Was looking for " + valueLabelString);
				}
				double[] values = parseVector(valueString);
				int n = values.length;
				double[] times = new double[n];
				for (int k = 0; k < n; k++) {
					times[k] = refdate + tstart + k * dt;
				}
				TimeSeries series = new TimeSeries(times, values);
				series.setLocation("source." + sourceLabel);
				series.setUnit("kg/s");
				series.setQuantity("discharge");
				String id = "source." + sourceLabel + ".discharge";
				series.setProperty("Timezone", "GMT");
				exchangeItems.put(id, series);
			}
		}

		//series for boundaries
		String boundLabelsString = variables.get("bound_labels");
		if(boundLabelsString!=null){
			boundLabels = parseStrings(boundLabelsString);
			for (String boundLabel : boundLabels) {
				//System.out.println("bound label ="+boundLabels[i]);
				String valueLabelString = "bound_values['" + boundLabel + "']";
				String valueString = variables.get(valueLabelString);
				if (valueString == null) {
					throw new RuntimeException("Missing input. Was looking for " + valueLabelString);
				}
				double[] values = parseVector(valueString);
				int n = values.length;
				double[] times = new double[n];
				for (int k = 0; k < n; k++) {
					times[k] = refdate + tstart + k * dt;
				}
				TimeSeries series = new TimeSeries(times, values);
				series.setLocation("bound." + boundLabel);
				series.setUnit("kg/m^3");
				series.setQuantity("concentration");
				String id = "bound." + boundLabel + ".concentration";
				series.setProperty("Timezone", "GMT");
				exchangeItems.put(id, series);
			}
		}

		//series for outputs
		String outputLabelsString = variables.get("output_labels");
		if(outputLabelsString!=null){
			outputLabels = parseStrings(outputLabelsString);
			for (String outputLabel : outputLabels) {
				//System.out.println("output label ="+outputLabels[i]);
				String valueLabelString = "output_values['" + outputLabel + "']";
				TimeSeries series;
				if (variables.containsKey(valueLabelString)) {
					String valueString = variables.get(valueLabelString);
					if (valueString == null) {
						throw new RuntimeException("Missing input. Was looking for " + valueLabelString);
					}
					double[] values = parseVector(valueString);
					int n = values.length;
					double[] times = new double[n];
					for (int k = 0; k < n; k++) {
						times[k] = refdate + tstart + k * dt;
					}
					series = new TimeSeries(times, values);
				} else { // fill with empty values
					double[] times = {tstart + refdate};
					double[] values = {0.0};
					series = new TimeSeries(times, values);
				}

				series.setLocation("output." + outputLabel);
				series.setUnit("kg/m^3");
				series.setQuantity("concentration");
				String id = "output." + outputLabel + ".concentration";
				series.setId(id);
				series.setProperty("Timezone", "GMT");
				exchangeItems.put(id, series);
			}
		}
		//concentrations on grid
		String concentrationString = variables.get("c");
		IExchangeItem c;
		if(concentrationString!=null){
			double[] values = parseVector(concentrationString);
			c = new DoublesExchangeItem("concentration.grid",IExchangeItem.Role.InOut, values);
			exchangeItems.put("concentration.grid", c);
		}else{
			throw new RuntimeException("Missing input. Was looking for c");
		}

		boolean debug=false;
		if(debug){
			for(String key: exchangeItems.keySet()){
				System.out.println("key="+key);
				System.out.println(exchangeItems.get(key).toString());
			}
		}
	}

	@Override
	public void finish() {
		// update variables

		//update times
		double startAsMjd = exchangeItems.get("startTime").getValuesAsDoubles()[0];
		this.tstart = startAsMjd - this.refdate;
		double endAsMjd = exchangeItems.get("endTime").getValuesAsDoubles()[0];
		this.tstop = endAsMjd - this.refdate;

		double roundVal=100.0; //2 decimal places
		variables.put("time", "["+Math.round(roundVal*this.tstart/this.unit)/roundVal+
				","+Math.round(roundVal*this.dt/this.unit)/roundVal+
				","+Math.round(roundVal*this.tstop/this.unit)/roundVal+"]");


		// update sources
		String sourceLabelsString = variables.get("source_labels");
		if(sourceLabelsString!=null){
			for (String sourceLabel : sourceLabels) {
				String id = "source." + sourceLabel + ".discharge";
				TimeSeries item = (TimeSeries) exchangeItems.get(id);
				double[] times = item.getTimes();
				String key = "source_values['" + sourceLabel + "']";
				double[] values = item.getValuesAsDoubles();
				// first find index of start
				int skipCount = (int) Math.round((this.refdate + this.tstart - times[0]) / this.dt); //MVL
				//time is progressing; adjust start of series
				//find matching times
				int newTimesLength = times.length - skipCount;
				double[] newValues;
				if (newTimesLength > 0) {
					newValues = new double[newTimesLength];
					System.arraycopy(values, skipCount, newValues, 0, newTimesLength);
				} else {
					newValues = new double[1];
					int last = values.length - 1;
					newValues[0] = values[last];
				}
				String valueString = writeVector(newValues);
				this.variables.put(key, valueString);
			}
		}

		// update boundaries
		String boundLabelsString = variables.get("bound_labels");
		if(boundLabelsString!=null){
			for (String boundLabel : boundLabels) {
				String id = "bound." + boundLabel + ".concentration";
				TimeSeries item = (TimeSeries) exchangeItems.get(id);
				double[] times = item.getTimes();
				String key = "bound_values['" + boundLabel + "']";
				double[] values = item.getValuesAsDoubles();
				// first find index of start
				int skipCount = (int) Math.round((this.refdate + this.tstart - times[0]) / this.dt); //MVL
				//time is progressing; adjust start of series
				//find matching times
				int newTimesLength = times.length - skipCount;
				double[] newValues;
				if (newTimesLength > 0) {
					newValues = new double[newTimesLength];
					System.arraycopy(values, skipCount, newValues, 0, newTimesLength);
				} else {
					newValues = new double[1];
					int last = values.length - 1;
					newValues[0] = values[last];
				}
				String valueString = writeVector(newValues);
				this.variables.put(key, valueString);
			}
		}

		// update outputs
		String outputLabelsString = variables.get("output_labels");
		if(outputLabelsString!=null){
			for (String outputLabel : outputLabels) {
				String id = "output." + outputLabel + ".concentration";
				TimeSeries item = (TimeSeries) exchangeItems.get(id);
				double[] times = item.getTimes();
				String key = "output_values['" + outputLabel + "']";
				double[] values = item.getValuesAsDoubles();
				// first find index of start
				int skipCount = (int) Math.round((this.refdate + this.tstart - times[0]) / this.dt); //MVL
				//time is progressing; adjust start of series
				//find matching times
				int newTimesLength = times.length - skipCount;
				double[] newValues;
				if (newTimesLength > 0) {
					newValues = new double[newTimesLength];
					System.arraycopy(values, skipCount, newValues, 0, newTimesLength);
				} else {
					newValues = new double[1];
					int last = values.length - 1;
					newValues[0] = values[last];
				}
				String valueString = writeVector(newValues);
				this.variables.put(key, valueString);
			}
		}

		// concentration
		if(this.variables.containsKey("c")){
			String id = "concentration.grid";
			if(!exchangeItems.containsKey(id)){
				throw new RuntimeException("ExchangeItem with id ="+id+" got lost.");
			}
			IExchangeItem item = exchangeItems.get(id);
			String key="c";
			double[] values = item.getValuesAsDoubles();
			String valueString = writeVector(values);
			this.variables.put(key, valueString);
		}

		//write to file
		File outputFile = new File(fileName);
		try{
			if(outputFile.isFile()){
				outputFile.delete();
			}
		}catch (Exception e) {
			System.out.println("SimpleBbAsciiFile: trouble removing file "+ fileName);
		}
		try {
			FileWriter writer = new FileWriter(outputFile);
			BufferedWriter out = new BufferedWriter(writer);

			Set<String> keys = this.variables.keySet();
			for(String key : keys){
				out.write(key+"="+this.variables.get(key)+"\n");
				System.out.println(key+"="+this.variables.get(key));
			}

			out.close();
		} catch (Exception e) {
			throw new RuntimeException("Problem writing to file "+fileName+" :\n "+e.getMessage());
		}
	}

	//
	// internal methods
	//

	/**
	 * Parse a string with format like "[1.0, 2.2, 4.6 , 5.95]"
	 * @param valuestring to parse
	 */
	private double[] parseVector(String valuestring){
		double[] result;
		int ifirst = valuestring.indexOf("[") + 1;
		int ilast = valuestring.indexOf("]");
		String buffer = valuestring.substring(ifirst, ilast);
		String[] values = buffer.split(",");
		int n = values.length;
		result = new double[n];
		for (int i = 0; i < n; i++) {
			try {
				result[i] = Double.parseDouble(values[i]);
			} catch (NumberFormatException e) {
				throw new RuntimeException("Error parsing vector at "+values[i]+" in "+valuestring);
			}
		}
		return result;
	}

	private String writeVector(double[] values){
		StringBuilder result = new StringBuilder("[");
		for(int i=0;i<values.length;i++){
			if(i>0) result.append(",");
			result.append(values[i]);
		}
		result.append("]");
		return result.toString();
	}

	private String[] parseStrings(String valuestring){
		String[] result;
		int ifirst = valuestring.indexOf("[") + 1;
		int ilast = valuestring.indexOf("]");
		String buffer = valuestring.substring(ifirst, ilast);
		String[] values = buffer.split(",");
		int n = values.length;
		result = new String[n];
		for (int i = 0; i < n; i++) {
			int ifirstQuote = values[i].indexOf("'") + 1;
			int ilastQoute = values[i].lastIndexOf("'");
			if ((ifirstQuote<0)|(ilastQoute<0)){
				throw new RuntimeException("Expecting quotes around strings. Trouble reading "+values[i]);
			}
			if (ifirstQuote==ilastQoute){
				throw new RuntimeException("Expecting no empty strings. Trouble reading "+valuestring);
			}
			result[i] = values[i].substring(ifirstQuote, ilastQoute);
		}
		return result;
	}

	private String parseString(String valuestring){
		String result;
		int ifirstQuote = valuestring.indexOf("'") + 1;
		int ilastQoute = valuestring.lastIndexOf("'");
		if (ilastQoute < 0){
			throw new RuntimeException("Expecting quotes around strings. Trouble reading "+valuestring);
		}
		if (ifirstQuote==ilastQoute){
			throw new RuntimeException("Expecting no empty strings. Trouble reading "+valuestring);
		}
		result = valuestring.substring(ifirstQuote, ilastQoute);
		return result;
	}

}
