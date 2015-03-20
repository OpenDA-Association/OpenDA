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
package org.openda.blackbox.io;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.ExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
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


public class SimpleBbAsciiFile implements IoObjectInterface{
	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String,String> variables = new LinkedHashMap<String,String>();
	HashMap<String,IPrevExchangeItem> items = new LinkedHashMap<String,IPrevExchangeItem>();

	//cache these values
	double refdate;
	double tstart;
	double dt=1.0;
	double tstop;
	double unit=1.0;
	String sourceLabels[];
	String boundLabels[];
	String outputLabels[];

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.workingDir = workingDir;
		this.fileName = fileName;
		System.out.println("ioObject : filename = "+fileName);


		if (arguments != null && arguments.length > 0) {
			for(int i=0;i<arguments.length;i++){
				System.out.println("ioObject : arg = "+fileName);
			}
			Results.putMessage("ioObject for file="+fileName+" arguments ignored");
			//throw new RuntimeException("IoObject SimpleBbAsciiFile does not expect any additional arguments");
		}
		File inputFile=null;
		// check file
		try{
			inputFile = new File(workingDir,fileName);
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

			String line="";
			Boolean eof=false;
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
						String columns[] = line.split("=");
						columns[0]=columns[0].trim();
						//System.out.println("variable = "+columns[0]);
						//System.out.println("value ="+columns[1]);
						variables.put(columns[0], columns[1]);
					}
				}
			}
			in.close();
		} catch (Exception e) {
			throw new RuntimeException("Problem reading from file "+fileName+" : "+e.getMessage());
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
		double time[] = parseVector(timeString);
		if(time.length!=3){
			throw new RuntimeException("expecting vector of length 3 for time, but length was"+time.length);
		}
		tstart=time[0]*unit;
		dt=time[1]*unit;
		tstop=time[2]*unit;

		//add exchange items for time
		IPrevExchangeItem startTime = new DoubleExchangeItem("startTime", this.refdate+this.tstart);
		this.items.put("startTime",startTime);
		IPrevExchangeItem endTime = new DoubleExchangeItem("endTime", this.refdate+this.tstop);
		this.items.put("endTime",endTime);

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
			for(int i=0;i<sourceLabels.length;i++){
				//System.out.println("source label ="+sourceLabels[i]);
				String valueLabelString = "source_values['"+sourceLabels[i]+"']";
				String valueString = variables.get(valueLabelString);
				if(valueString==null){
					throw new RuntimeException("Missing input. Was looking for "+valueLabelString);
				}
				double values[] = parseVector(valueString);
				int n=values.length;
				// times
				double times[] = new double[n];
				for(int k=0;k<n;k++){
					times[k] = refdate+tstart+k*dt;
				}
				TimeSeries series = new TimeSeries(times, values);
				series.setLocation("source."+sourceLabels[i]);
				series.setUnit("kg/s");
				series.setQuantity("discharge");
				String id = "source."+sourceLabels[i]+".discharge";
				series.setProperty("Timezone", "GMT");
				this.items.put(id, series);
			}
		}

		//series for boundaries
		String boundLabelsString = variables.get("bound_labels");
		if(boundLabelsString!=null){
			boundLabels = parseStrings(boundLabelsString);
			for(int i=0;i<boundLabels.length;i++){
				//System.out.println("bound label ="+boundLabels[i]);
				String valueLabelString = "bound_values['"+boundLabels[i]+"']";
				String valueString = variables.get(valueLabelString);
				if(valueString==null){
					throw new RuntimeException("Missing input. Was looking for "+valueLabelString);
				}
				double values[] = parseVector(valueString);
				int n=values.length;
				// times
				double times[] = new double[n];
				for(int k=0;k<n;k++){
					times[k] = refdate+tstart+k*dt;
				}
				TimeSeries series = new TimeSeries(times, values);
				series.setLocation("bound."+boundLabels[i]);
				series.setUnit("kg/m^3");
				series.setQuantity("concentration");
				String id = "bound."+boundLabels[i]+".concentration";
				series.setProperty("Timezone", "GMT");
				this.items.put(id, series);
			}
		}

		//series for outputs
		String outputLabelsString = variables.get("output_labels");
		if(outputLabelsString!=null){
			outputLabels = parseStrings(outputLabelsString);
			for(int i=0;i<outputLabels.length;i++){
				//System.out.println("output label ="+outputLabels[i]);
				String valueLabelString = "output_values['"+outputLabels[i]+"']";
				TimeSeries series=null;
				if(variables.containsKey(valueLabelString)){
					String valueString = variables.get(valueLabelString);
					if(valueString==null){
						throw new RuntimeException("Missing input. Was looking for "+valueLabelString);
					}
					double values[] = parseVector(valueString);
					int n=values.length;
					// times
					double times[] = new double[n];
					for(int k=0;k<n;k++){
						times[k] = refdate+tstart+k*dt;
					}
					series = new TimeSeries(times, values);
				}else{ // fill with empty values
					double times[]={tstart+refdate};
					double values[]={0.0};
					series = new TimeSeries(times, values);
				}

				series.setLocation("output."+outputLabels[i]);
				series.setUnit("kg/m^3");
				series.setQuantity("concentration");
				String id = "output."+outputLabels[i]+".concentration";
				series.setId(id);
				series.setProperty("Timezone", "GMT");
				this.items.put(id, series);
			}
		}
		//concentrations on grid
		String concentrationString = variables.get("c");
		IExchangeItem c = null;
		if(concentrationString!=null){
			double values[] = parseVector(concentrationString);
			c = new DoublesExchangeItem("concentration.grid",Role.InOut, values);
			this.items.put("concentration.grid", c);
		}else{
			throw new RuntimeException("Missing input. Was looking for c");
		}

		boolean debug=false;
		if(debug){
			for(String key: this.items.keySet()){
				System.out.println("key="+key);
				System.out.println(this.items.get(key).toString());
			}
		}
	}

	public IPrevExchangeItem[] getExchangeItems() {
		//TODO for now return some dummy timeSeries
		int n = this.items.size();
		Set<String> keys = this.items.keySet();
		IPrevExchangeItem[] result=new IPrevExchangeItem[n];
		int i=0;
		for(String key : keys){
			result[i]=this.items.get(key);
			i++;
		}
		return result;
	}

	public void finish() {
		// update variables

		//update times
		double startAsMjd = this.items.get("startTime").getValuesAsDoubles()[0];
		this.tstart = startAsMjd - this.refdate;
		double endAsMjd = this.items.get("endTime").getValuesAsDoubles()[0];
		this.tstop = endAsMjd - this.refdate;

		double roundVal=100.0; //2 decimal places
		variables.put("time", "["+Math.round(roundVal*this.tstart/this.unit)/roundVal+
				","+Math.round(roundVal*this.dt/this.unit)/roundVal+
				","+Math.round(roundVal*this.tstop/this.unit)/roundVal+"]");


		// update sources
		String sourceLabelsString = variables.get("source_labels");
		if(sourceLabelsString!=null){
			for(int i=0;i<sourceLabels.length;i++){
				String id = "source."+sourceLabels[i]+".discharge";
				TimeSeries item = (TimeSeries)this.items.get(id);
				double times[] = item.getTimes();
				String key="source_values[\'"+sourceLabels[i]+"\']";
				double values[]=item.getValuesAsDoubles();
				// first find index of start
				int skipCount = (int) Math.round((this.refdate+this.tstart-times[0])/this.dt); //MVL
				//time is progressing; adjust start of series
				//find matching times
				int newTimesLength = times.length-skipCount;
				double[] newValues = null;
				if(newTimesLength>0){
					newValues = new double[newTimesLength];
					System.arraycopy(values, skipCount, newValues, 0, newTimesLength);
				}else{
					newValues = new double[1];
					int last=values.length-1;
					newValues[0]=values[last];
				}
				String valueString = writeVector(newValues);
				this.variables.put(key, valueString);
			}
		}

		// update boundaries
		String boundLabelsString = variables.get("bound_labels");
		if(boundLabelsString!=null){
			for(int i=0;i<boundLabels.length;i++){
				String id = "bound."+boundLabels[i]+".concentration";
				TimeSeries item = (TimeSeries)this.items.get(id);
				double times[] = item.getTimes();
				String key="bound_values[\'"+boundLabels[i]+"\']";
				double values[]=item.getValuesAsDoubles();
				// first find index of start
				int skipCount = (int) Math.round((this.refdate+this.tstart-times[0])/this.dt); //MVL
				//time is progressing; adjust start of series
				//find matching times
				int newTimesLength = times.length-skipCount;
				double[] newValues = null;
				if(newTimesLength>0){
					newValues = new double[newTimesLength];
					System.arraycopy(values, skipCount, newValues, 0, newTimesLength);
				}else{
					newValues = new double[1];
					int last=values.length-1;
					newValues[0]=values[last];
				}
				String valueString = writeVector(newValues);
				this.variables.put(key, valueString);
			}
		}

		// update outputs
		String outputLabelsString = variables.get("output_labels");
		if(outputLabelsString!=null){
			for(int i=0;i<outputLabels.length;i++){
				String id = "output."+outputLabels[i]+".concentration";
				TimeSeries item = (TimeSeries)this.items.get(id);
				double times[] = item.getTimes();
				String key="output_values[\'"+outputLabels[i]+"\']";
				double values[]=item.getValuesAsDoubles();
				// first find index of start
				int skipCount = (int) Math.round((this.refdate+this.tstart-times[0])/this.dt); //MVL
				//time is progressing; adjust start of series
				//find matching times
				int newTimesLength = times.length-skipCount;
				double[] newValues = null;
				if(newTimesLength>0){
					newValues = new double[newTimesLength];
					System.arraycopy(values, skipCount, newValues, 0, newTimesLength);
				}else{
					newValues = new double[1];
					int last=values.length-1;
					newValues[0]=values[last];
				}
				String valueString = writeVector(newValues);
				this.variables.put(key, valueString);
			}
		}

		// concentration
		if(this.variables.containsKey("c")){
			String id = "concentration.grid";
			if(!this.items.containsKey(id)){
				throw new RuntimeException("ExchangeItem with id ="+id+" got lost.");
			}
			IPrevExchangeItem item = this.items.get(id);
			String key="c";
			double values[] = item.getValuesAsDoubles();
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
	 * @param String to parse
	 */
	private double[] parseVector(String valuestring){
		double result[] = null;
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
		String result = "[";
		for(int i=0;i<values.length;i++){
			if(i>0) result+=",";
			result += values[i];
		}
		result+="]";
		return result;
	}

	private String[] parseStrings(String valuestring){
		String result[] = null;
		int ifirst = valuestring.indexOf("[") + 1;
		int ilast = valuestring.indexOf("]");
		String buffer = valuestring.substring(ifirst, ilast);
		String[] values = buffer.split(",");
		int n = values.length;
		result = new String[n];
		for (int i = 0; i < n; i++) {
			int ifirstQuote = values[i].indexOf("\'") + 1;
			int ilastQoute = values[i].lastIndexOf("\'");
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
		String result = null;
		int ifirstQuote = valuestring.indexOf("\'") + 1;
		int ilastQoute = valuestring.lastIndexOf("\'");
		if ((ifirstQuote<0)|(ilastQoute<0)){
			throw new RuntimeException("Expecting quotes around strings. Trouble reading "+valuestring);
		}
		if (ifirstQuote==ilastQoute){
			throw new RuntimeException("Expecting no empty strings. Trouble reading "+valuestring);
		}
		result = valuestring.substring(ifirstQuote, ilastQoute);
		return result;
	}

}
