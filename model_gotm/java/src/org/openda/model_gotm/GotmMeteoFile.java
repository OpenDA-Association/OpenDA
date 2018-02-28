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
package org.openda.model_gotm;
import java.io.File;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeriesSet;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
//import org.openda.model_dflowfm.DFlowFMTimTimeSeriesFormatter;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.utils.Results;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

/**
*
* Read from dat file in following format (Date Time x-Wind y-Wind Air-pressure Dry-air-temp Relative-humidity Cloud-cover)
* 2005-01-01 00:00:00   -3.57316e+00   -3.36668e+00    1.03510e+03    1.27264e+01    9.93837e+00    9.95460e-01
* 2005-01-01 12:00:00    8.89665e-01   -3.29528e+00    1.03642e+03    1.17271e+01    8.84553e+00    9.90943e-01
* 2005-01-02 00:00:00   -9.11740e-01   -4.99451e+00    1.03832e+03    1.32241e+01    9.25586e+00    9.58196e-01 
* 
* **/


public class GotmMeteoFile  implements IDataObject {


	private TimeSeriesSet timeSeriesSet = null;
	File workingDir;
	String fileName = null;
	String df = "yyyy-MM-dd HH:mm:ss";
	java.util.Vector<String> lines = new java.util.Vector<String>();		
	private static final String idSeparator= ".";

	
	public void initialize(File workingDir, String[] arguments) {
		// TODO Auto-generated method stub
		Vector<Double> timeVector = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valVector1 = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valVector2 = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valVector3 = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valVector4 = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valVector5 = new Vector<Double>(); // for temporary storage of values,
		Vector<Double> valVector6 = new Vector<Double>(); // for temporary storage of values,
		
		
		
		this.workingDir = workingDir;
		if((arguments==null) || (arguments.length==0)){
			throw new RuntimeException("No filename was provided for opening an meteo-file.");
		}
		fileName=arguments[0];
		if (arguments.length > 1) {
			Results.putMessage("iDataObject for file="+fileName+" arguments ignored");
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
			System.out.println("GotmMeteoFile: trouble opening file "+ this.fileName);
		}
		

		this.timeSeriesSet = new TimeSeriesSet();

		
		try {
			FileInputStream in = new FileInputStream(inputFile);
			BufferedReader buff = new BufferedReader(new InputStreamReader(in));

			String line="";
			Boolean eof=false;
			while (!eof) {
				line = buff.readLine();
				if (line == null)
					eof = true;
				else { // now process this line
					this.lines.add(line); // save the contents in memory
					if(!line.startsWith("!")){

						if(line.indexOf("-")>0)
						{ // variable=value
							String columns[] = line.trim().split("(\\s)+");
							//Try if valueString contains a date							
							try{
								if (columns[0].length() == 10){
								
								double dateInMjd = TimeUtils.date2Mjd(columns[0]+" " +columns[1], this.df);
								System.out.println("It's a date in Mjd "+dateInMjd);
								timeVector.add(dateInMjd);
								//continue;
								}
								
							}catch(Exception e){
								//System.out.println("Not a date. Ignoring "+valueAsString);
							}

							for (int i = 2; i< 8; i++){
								try{//Check for valid floating point constants only for now
									System.out.println("It's a number " +columns[i]);
									String valueAsString=columns[i];
									double value=Double.parseDouble(valueAsString);
									if (i==2){
										valVector1.add(value);
									}else if (i==3){
										valVector2.add(value);
									}else if (i==4){
										valVector3.add(value);
									}else if (i==5){
										valVector4.add(value);
									}else if (i==6){
										valVector5.add(value);
									}else if (i==7){
										valVector6.add(value);
									}	
								}catch( NumberFormatException e){
									throw new RuntimeException("Cannot parse " + columns[i] + " as a double in " + this.fileName );
									//System.out.println("Not a float. Ignoring");
								}
								
							}
							
						} // if line indexOf
					} // if ! line starts with
				} // else line != null
			} //while eof
			
			
		
			in.close();
		} catch (Exception e) {
			throw new RuntimeException("Problem reading from file "+fileName+" : "+e.getMessage());
		}
		
		
		double[] times = new double[timeVector.size()];
		double[] values = new double[valVector1.size()];
		if (times.length != values.length) { throw new RuntimeException("Value vector length doesn't match time vector length."); }
		for (int i = 0; i < times.length; i++) {
			times[i] = timeVector.get(i);
			values[i] = valVector1.get(i);
		}
		double x = 0.0; // Position : (4.745356,52.966001)
		double y = 0.0;
		String source = ""; // Source : observed
		String quantity = ""; // Unit : waterlevel_astro !!! Note different label
		String unit     = "";
		String location = "1";
		Role role     = Role.InOut;
		TimeSeries series = null;
		
		// wind_x					
		quantity = "u10";
		unit = "m/s";	
		series = new TimeSeries(times, values, x, y,source , quantity, unit, location, role);
		this.timeSeriesSet.add(series);
		System.out.println("Bla bla ");
		
		// wind_y
		for (int i = 0; i < times.length; i++) {
			values[i] = valVector2.get(i);
		}		
		quantity = "v10";
		unit = "m/s";	
		series = new TimeSeries(times, values, x, y,source , quantity, unit, location, role);
		this.timeSeriesSet.add(series);
		System.out.println(quantity);
		
		// Air pressure
		for (int i = 0; i < times.length; i++) {
			values[i] = valVector3.get(i);
		}		
		quantity = "airp";
		unit = "hectopascal";	
		series = new TimeSeries(times, values, x, y,source  , quantity, unit, location, role);
		this.timeSeriesSet.add(series);
		System.out.println(quantity);
		
		// Dry air temperature
		for (int i = 0; i < times.length; i++) {
			values[i] = valVector4.get(i);
		}		
		quantity = "airt";
		unit = "Celsius";	
		series = new TimeSeries(times, values, x, y,source  , quantity, unit, location, role);
		this.timeSeriesSet.add(series);
		System.out.println(quantity);
		
		// relative humidity, wet bulb temperature or dew point temperature or 
		for (int i = 0; i < times.length; i++) {
			values[i] = valVector5.get(i);
		}		
		quantity = "rh";
		unit = "%";	
		series = new TimeSeries(times, values, x, y,source  , quantity, unit, location, role);
		this.timeSeriesSet.add(series);	
		System.out.println(quantity);
		
		// Cloud coverage
		for (int i = 0; i < times.length; i++) {
			values[i] = valVector6.get(i);
		}		
		quantity = "cloud_obs";
		unit = "1/10";	
		series = new TimeSeries(times, values, x, y,source  , quantity, unit, location, role);
		this.timeSeriesSet.add(series);	
		System.out.println(quantity);
		
	}

	
	
 	public String [] getExchangeItemIDs() {
		String [] result = new String[this.timeSeriesSet.size()];
		Set<String> quantities = this.timeSeriesSet.getQuantities();
		int idx=0;
		for (String quantity: quantities) {
//			System.out.println(quantity);
			Set<String> locations = this.timeSeriesSet.getOnQuantity(quantity).getLocations();
			for (String location: locations) {
				String id = location + idSeparator + quantity;
//				System.out.println("getExhangeItemIDs: " + id);
				result[idx]= id;
				idx++;	
			}
		}
		return result;
	}

	


/*
 	public String [] getExchangeItemIDs() {
		String [] result = new String[this.timeSeriesSet.size()];
		Set<String> quantities = this.timeSeriesSet.
				
				getQuantities();
		quantities.toArray(result);
		return result;
	}
*/
 	
	public String [] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

 /*
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		
//		System.out.println(location + ", " + quantity );
		
		// Get the single time series based on location and quantity
		TimeSeriesSet myTimeSeriesSet = this.timeSeriesSet.getOnQuantity(exchangeItemID);
		Iterator<TimeSeries> iterator = myTimeSeriesSet.iterator();
		if (!iterator.hasNext()) {
		    throw new RuntimeException("No time series found for " + exchangeItemID);
		}
		TimeSeries timeSeries = iterator.next();
		if (iterator.hasNext()) {
		    throw new RuntimeException("Time series is not uniquely defined for  " + exchangeItemID);
		}

		return timeSeries;
	}
*/	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		
		String[] parts = exchangeItemID.split( "\\" +  idSeparator);
		if (parts.length != 2) {
			throw new RuntimeException("Invalid exchangeItemID " + exchangeItemID );
		}
		String location = parts[0];	
		String quantity = parts[1];
//		System.out.println(location + ", " + quantity );
		
		// Get the single time series based on location and quantity
		TimeSeriesSet myTimeSeriesSet = this.timeSeriesSet.getOnQuantity(quantity)
				.getOnLocation(location);
		Iterator<TimeSeries> iterator = myTimeSeriesSet.iterator();
		if (!iterator.hasNext()) {
		    throw new RuntimeException("No time series found for " + exchangeItemID);
		}
		TimeSeries timeSeries = iterator.next();
		if (iterator.hasNext()) {
		    throw new RuntimeException("Time series is not uniquely defined for  " + exchangeItemID);
		}

		return timeSeries;
	}
	
	
	

	
	public void finish() {
		if (this.timeSeriesSet == null) return;
		File outputFile = new File(fileName);
		try{
			if(outputFile.isFile()){
				outputFile.delete();
			}
		}catch (Exception e) {
			System.out.println("GotmNmlFile: trouble removing existing file "+ fileName);
		}

		double[] times = null;
		double[] values1 = null;
		double[] values2 = null;
		double[] values3 = null;
		double[] values4 = null;
		double[] values5 = null;
		double[] values6 = null;
		try{
			FileWriter writer = new FileWriter(outputFile);
			
			BufferedWriter out = new BufferedWriter(writer);
			int lineNumber = 0;
			System.out.println("Now output should be created!");
			int i = 1;
			for (TimeSeries series : this.timeSeriesSet) {
				if (i == 1){
					times =  series.getTimesRef();
					values1 = series.getValuesRef();
					i=i+1;
				}else if(i == 2){
					values2 = series.getValuesRef();
					i=i+1;
				}else if(i == 3){
					values3 = series.getValuesRef();
					i=i+1;
				}else if(i == 4){
					values4 = series.getValuesRef();
					i=i+1;
				}else if(i == 5){
					values5 = series.getValuesRef();
					i=i+1;
				}else if(i == 6){
					values6 = series.getValuesRef();
					i=i+1;
				}
			} 
			
			String myLine = null; 
			for (int j = 0; j <times.length;j ++){
         	   
               try {
            	   String valueAsString = null;
            	   
        		   valueAsString = "'" + TimeUtils.mjdToString(times[j], this.df) + "'";   
        		   System.out.println("Step 1: Check if it is time.");
        		   System.out.println(valueAsString);
            	   
        		   myLine = valueAsString + "  " + values1[j] + "  " + values2[j] + "  " +  values3[j] + "  " +  values4[j] + "  " + values5[j] + "  " + values6[j];
               
                	   // TODO: check if quantity is time

            	   
				    out.write(myLine);
				    out.newLine();
                } catch (IOException e) {
                	throw new RuntimeException("Cannot write line in file: "+outputFile);
                }
            }
			
			out.close();
			
			
		} catch (IOException e) {
			throw new RuntimeException("Problem writing to file "+fileName+" :\n "+e.getMessage());
		}	
	}
	


}
