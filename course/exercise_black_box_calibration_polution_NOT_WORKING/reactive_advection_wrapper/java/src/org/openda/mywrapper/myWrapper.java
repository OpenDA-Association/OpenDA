/* MOD_V2.0
 * Copyright (c) 2010 OpenDA Association
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
package org.openda.mywrapper;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.interfaces.IExchangeItem;
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
 * # input for 1d reactive pollution model
 * #
 * # grid
 * x = [0.0, 60.0, 3600.0]
 * # stationary flow
 * u = [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
 * # reation time
 * reaction_time = [600.0]
 * # cross sectional area
 * a = [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
 * # initial concentrations
 * c1 = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
 * c2 = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
 * # simulation timespan
 * refdate = '01 dec 1999'
 * #unit is always seconds
 * unit = 'seconds'
 * # step is 1min total 5hours, while a particle travels through domain in 1hour
 * time = [ 0,60,18000]
 * # sources mass/m^3/s
 * source_locations = [5, 30]
 * source_substance = [1, 1]
 * source_labels = ['c1_factory1','c1_factory2']
 * # generated as
 * source_values['c1_factory1'] = [17.28, 19.52, 16.91, 11.33, 20.36, 29.44, 24.64, 37.10, 22.97, 54.21, 53.90, 72.93, 90.06, 66.32,102.94,131.21, 93.50,101.44, 60.24, 52.41, 27.37, 21.18, 19.49, 30.84, 28.72, 20.52, 20.61, 12.78,  9.91, 19.45, 20.69, 14.90, 14.45, 11.08,  8.40,  7.83,  6.64,  7.23, 14.31, 12.81, 11.23, 16.43, 23.39, 30.15, 30.67, 19.18, 25.70, 23.02, 24.78, 20.42, 24.83, 22.26, 17.64,  9.87,  6.47, 13.88, 33.87, 22.08, 21.20, 21.90, 18.34, 39.77, 50.03, 38.34, 22.72, 23.77, 24.54, 13.91, 18.68, 19.29, 33.75, 30.60, 37.94, 28.82, 29.03, 48.19, 86.17, 87.85,249.58,232.70,384.94,409.47,341.70,708.61,758.58,643.23,567.13,619.63,588.24,575.65,511.42,327.04,310.95,157.40,143.46,145.32,163.02,232.71,138.14, 81.29,185.44,556.00,776.59,581.15,708.11,728.86,292.19,289.81,265.47,309.65,309.79, 69.25, 42.64, 23.01, 16.62,  7.74,  7.09,  4.95,  6.52,  5.00,  6.58,  3.78,  1.77,  1.22,  1.67,  2.82,  2.28,  4.55,  4.46,  4.85,  4.43,  3.63,  4.20,  5.51,  5.20,  1.75,  4.50,  3.12,  7.21, 11.29,  3.76,  3.89,  4.47,  6.42,  8.44, 10.97, 12.60, 15.17, 30.86, 38.26, 35.42, 35.49, 46.09, 45.09, 32.20, 20.85, 23.77, 35.90, 61.74, 88.90, 64.41, 89.58, 57.94, 54.44, 30.69, 53.45, 63.93, 93.01, 86.65, 96.71, 60.53, 34.75, 22.56, 48.11, 55.66, 48.27, 38.85, 24.89, 42.97, 64.32, 76.76, 91.08,153.65,240.08,426.99,269.48,199.28,319.31,484.68,730.76,733.98,775.06,641.28,600.51,1014.02,1088.48,1136.14,1268.69,596.75,1331.90,1001.49,2302.82,2121.98,2584.46,1959.43,3587.58,3475.89,3241.05,3801.16,5585.35,3339.13,1759.37,2219.68,2337.63,1050.63,1478.07,1079.84,1247.10,1910.00,2638.00,1861.88,1715.04,2359.83,3676.87,4350.27,7611.73,10257.37,11617.63,7813.15,8066.27,10087.88,14245.27,12823.52,11134.29,16110.31,25042.53,22969.67,15425.62,14071.87,13871.97,16324.38,3741.69,6422.94,2810.36,1812.94,879.36,597.37,326.74,371.78,650.30,426.50,307.82,177.40,230.28,135.42,126.94, 86.26, 72.16, 80.94,105.85, 59.16, 37.24, 26.85, 19.60, 24.99, 19.51, 13.66,  5.11,  4.47,  5.54,  8.25, 11.34,  8.78,  6.42, 11.45,  3.90,  6.67,  7.23,  4.93,  6.35,  4.66,  7.86, 16.77, 34.38, 32.23, 22.39, 15.73, 10.26,  7.98,  3.25,  4.00,  5.31,  5.03,  3.63,  3.80,  3.06,  2.78,  3.68,  4.93,  2.96,  4.58]
 * source_values['c1_factory2'] = [11.94,  9.65, 12.31, 23.18, 19.18, 15.65, 26.37, 26.77, 36.50, 60.54, 67.68,104.24,139.93, 78.38, 62.14, 77.08, 33.42, 24.20,  8.73,  6.74,  6.56,  7.60,  6.07,  1.72,  3.43,  2.99,  5.61,  3.27,  4.93,  6.34,  5.33, 15.06, 35.22, 48.79, 48.84, 60.45, 79.16, 60.67, 53.57, 99.48,147.84,162.76,112.14, 89.05,125.07,141.42,132.77,176.35, 80.81,131.32,124.46,102.81,129.37,164.50,243.68,346.14,552.62,472.74,334.96,250.28,215.77,349.25,175.12,144.19,156.94,214.65,137.59,127.95,131.24,130.94,148.61,138.04,100.27, 83.32, 82.15,142.42,164.50,163.93,179.48,211.37,428.64,455.47,814.23,876.44,1154.14,1378.83,2706.13,1692.23,1076.00,1181.62,1618.50,1609.51,1379.63,2399.44,1715.45,2239.70,4642.39,5018.11,3207.02,8150.37,5212.40,5358.86,6897.26,10476.99,9225.11,10938.64,4722.56,4435.42,2962.85,1351.64,2002.81,3424.25,3480.22,2027.73,5156.48,3989.14,5365.01,4634.49,7346.04,7642.12,5694.39,5368.56,4697.46,7770.25,9484.21,10013.29,7046.82,4495.71,5416.70,9139.25,5907.47,3167.33,5989.14,5702.55,3727.93,2035.40,3167.82,5524.40,10589.10,12453.98,11624.83,25607.55,38633.58,16476.48,14697.58,36977.95,24772.76,24609.60,18784.69,29147.34,19735.57,18122.03,11947.53,10002.55,14225.99,13549.44,12119.75,7392.07,3629.84,3883.94,4356.63,4570.35,1915.09,2365.11,2415.68,3751.81,3635.88,3605.72,2578.33,1548.50,1277.66,2194.89,4317.66,2271.89,2475.84,2270.87,2205.15,2871.04,1741.26,1066.84,1127.14,1193.13,1008.67,865.30,1185.86,850.42,743.42,947.23,1010.47,1811.36,1655.15,1523.91,1212.99,902.15,1252.98,677.92,565.57,181.55,242.99,300.63,427.04,265.64,248.10,246.91,113.70, 52.72, 39.50, 68.80,164.72,171.00, 81.29, 89.38,103.06, 96.53,101.51, 46.05, 24.35, 34.24, 30.86, 15.56, 13.72,  9.94, 13.44, 13.33, 15.41, 19.16, 12.52, 13.19, 12.69, 17.09, 37.67, 42.81, 40.74, 40.90, 31.38, 15.18, 18.66, 15.55, 18.39, 18.88, 15.47, 17.25, 23.40, 24.95, 26.36, 17.29, 29.97, 49.21, 16.60,  7.00,  8.39,  5.96,  3.65, 11.32,  7.86,  4.81,  5.60,  4.23,  2.70,  1.55,  1.27,  1.65,  0.93,  0.94,  0.99,  0.90,  1.44,  1.97,  6.93,  4.20,  4.62,  6.59,  5.29,  6.76, 13.85, 19.51, 28.29, 25.69, 29.03, 23.28, 21.83, 20.98, 31.85, 37.99, 37.29, 31.59, 28.41, 17.43, 24.01, 39.00, 25.24, 51.26, 41.18, 43.85, 50.31, 44.84, 41.75, 25.40, 25.14, 32.50, 70.38]
 * #output (index based and 0 based)
 * output_file = 'reactive_pollution_model.output'
 * matlab_output_file = 'reactive_pollution_model_output.m'
 * output_map_times = [0, 60, 180]
 * output_locations = [10, 20, 40, 10, 20, 40]
 * output_substance = [1, 1, 1, 2, 2, 2]
 * output_labels = ['c1_locA','c1_locB','c1_locC','c2_locA','c2_locB','c2_locC']
 * # boundaries
 * # only left and right at locations 0 and -1 allowed at the moment
 * bound_labels=['c1_left', 'c1_right','c2_left','c2_right']
 * bound_locations=[0, -1, 0, -1]
 * bound_substance=[1, 1, 2, 2]
 * bound_values['c1_left']=[0.01]
 * bound_values['c1_right']=[0.0]
 * bound_values['c2_left']=[0.01]
 * bound_values['c2_right']=[0.0]
 *
 */


public class myWrapper implements IoObjectInterface{
	File workingDir;
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
			//throw new RuntimeException("IoObject myWrapper does not expect any additional arguments");
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
			System.out.println("myWrapper: trouble opening file "+ this.fileName);
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

		//add reaction_time
		if(variables.containsKey("reaction_time")){
			double reactionTimevalues[] = parseVector(variables.get("reaction_time"));
			IPrevExchangeItem reactionTime = new DoubleExchangeItem("reactionTime", reactionTimevalues[0]);
			this.items.put("reactionTime",reactionTime);
		}

		//series for sources
		/*
		 * # sources mass/m^3/s
		 * source_locations = [5, 30]
		 * source_substance = [1, 1]
		 * source_labels = ['c1_factory1','c1_factory2']
		 * # generated as
		 * source_values['c1_factory1'] = [17.28, 19.52, 16.91, 11.33, 20.36, 29.44, 24.64, 37.10, 22.97, 54.21, 53.90, 72.93, 90.06, 66.32,102.94,131.21, 93.50,101.44, 60.24, 52.41, 27.37, 21.18, 19.49, 30.84, 28.72, 20.52, 20.61, 12.78,  9.91, 19.45, 20.69, 14.90, 14.45, 11.08,  8.40,  7.83,  6.64,  7.23, 14.31, 12.81, 11.23, 16.43, 23.39, 30.15, 30.67, 19.18, 25.70, 23.02, 24.78, 20.42, 24.83, 22.26, 17.64,  9.87,  6.47, 13.88, 33.87, 22.08, 21.20, 21.90, 18.34, 39.77, 50.03, 38.34, 22.72, 23.77, 24.54, 13.91, 18.68, 19.29, 33.75, 30.60, 37.94, 28.82, 29.03, 48.19, 86.17, 87.85,249.58,232.70,384.94,409.47,341.70,708.61,758.58,643.23,567.13,619.63,588.24,575.65,511.42,327.04,310.95,157.40,143.46,145.32,163.02,232.71,138.14, 81.29,185.44,556.00,776.59,581.15,708.11,728.86,292.19,289.81,265.47,309.65,309.79, 69.25, 42.64, 23.01, 16.62,  7.74,  7.09,  4.95,  6.52,  5.00,  6.58,  3.78,  1.77,  1.22,  1.67,  2.82,  2.28,  4.55,  4.46,  4.85,  4.43,  3.63,  4.20,  5.51,  5.20,  1.75,  4.50,  3.12,  7.21, 11.29,  3.76,  3.89,  4.47,  6.42,  8.44, 10.97, 12.60, 15.17, 30.86, 38.26, 35.42, 35.49, 46.09, 45.09, 32.20, 20.85, 23.77, 35.90, 61.74, 88.90, 64.41, 89.58, 57.94, 54.44, 30.69, 53.45, 63.93, 93.01, 86.65, 96.71, 60.53, 34.75, 22.56, 48.11, 55.66, 48.27, 38.85, 24.89, 42.97, 64.32, 76.76, 91.08,153.65,240.08,426.99,269.48,199.28,319.31,484.68,730.76,733.98,775.06,641.28,600.51,1014.02,1088.48,1136.14,1268.69,596.75,1331.90,1001.49,2302.82,2121.98,2584.46,1959.43,3587.58,3475.89,3241.05,3801.16,5585.35,3339.13,1759.37,2219.68,2337.63,1050.63,1478.07,1079.84,1247.10,1910.00,2638.00,1861.88,1715.04,2359.83,3676.87,4350.27,7611.73,10257.37,11617.63,7813.15,8066.27,10087.88,14245.27,12823.52,11134.29,16110.31,25042.53,22969.67,15425.62,14071.87,13871.97,16324.38,3741.69,6422.94,2810.36,1812.94,879.36,597.37,326.74,371.78,650.30,426.50,307.82,177.40,230.28,135.42,126.94, 86.26, 72.16, 80.94,105.85, 59.16, 37.24, 26.85, 19.60, 24.99, 19.51, 13.66,  5.11,  4.47,  5.54,  8.25, 11.34,  8.78,  6.42, 11.45,  3.90,  6.67,  7.23,  4.93,  6.35,  4.66,  7.86, 16.77, 34.38, 32.23, 22.39, 15.73, 10.26,  7.98,  3.25,  4.00,  5.31,  5.03,  3.63,  3.80,  3.06,  2.78,  3.68,  4.93,  2.96,  4.58]
		 * source_values['c1_factory2'] = [11.94,  9.65, 12.31, 23.18, 19.18, 15.65, 26.37, 26.77, 36.50, 60.54, 67.68,104.24,139.93, 78.38, 62.14, 77.08, 33.42, 24.20,  8.73,  6.74,  6.56,  7.60,  6.07,  1.72,  3.43,  2.99,  5.61,  3.27,  4.93,  6.34,  5.33, 15.06, 35.22, 48.79, 48.84, 60.45, 79.16, 60.67, 53.57, 99.48,147.84,162.76,112.14, 89.05,125.07,141.42,132.77,176.35, 80.81,131.32,124.46,102.81,129.37,164.50,243.68,346.14,552.62,472.74,334.96,250.28,215.77,349.25,175.12,144.19,156.94,214.65,137.59,127.95,131.24,130.94,148.61,138.04,100.27, 83.32, 82.15,142.42,164.50,163.93,179.48,211.37,428.64,455.47,814.23,876.44,1154.14,1378.83,2706.13,1692.23,1076.00,1181.62,1618.50,1609.51,1379.63,2399.44,1715.45,2239.70,4642.39,5018.11,3207.02,8150.37,5212.40,5358.86,6897.26,10476.99,9225.11,10938.64,4722.56,4435.42,2962.85,1351.64,2002.81,3424.25,3480.22,2027.73,5156.48,3989.14,5365.01,4634.49,7346.04,7642.12,5694.39,5368.56,4697.46,7770.25,9484.21,10013.29,7046.82,4495.71,5416.70,9139.25,5907.47,3167.33,5989.14,5702.55,3727.93,2035.40,3167.82,5524.40,10589.10,12453.98,11624.83,25607.55,38633.58,16476.48,14697.58,36977.95,24772.76,24609.60,18784.69,29147.34,19735.57,18122.03,11947.53,10002.55,14225.99,13549.44,12119.75,7392.07,3629.84,3883.94,4356.63,4570.35,1915.09,2365.11,2415.68,3751.81,3635.88,3605.72,2578.33,1548.50,1277.66,2194.89,4317.66,2271.89,2475.84,2270.87,2205.15,2871.04,1741.26,1066.84,1127.14,1193.13,1008.67,865.30,1185.86,850.42,743.42,947.23,1010.47,1811.36,1655.15,1523.91,1212.99,902.15,1252.98,677.92,565.57,181.55,242.99,300.63,427.04,265.64,248.10,246.91,113.70, 52.72, 39.50, 68.80,164.72,171.00, 81.29, 89.38,103.06, 96.53,101.51, 46.05, 24.35, 34.24, 30.86, 15.56, 13.72,  9.94, 13.44, 13.33, 15.41, 19.16, 12.52, 13.19, 12.69, 17.09, 37.67, 42.81, 40.74, 40.90, 31.38, 15.18, 18.66, 15.55, 18.39, 18.88, 15.47, 17.25, 23.40, 24.95, 26.36, 17.29, 29.97, 49.21, 16.60,  7.00,  8.39,  5.96,  3.65, 11.32,  7.86,  4.81,  5.60,  4.23,  2.70,  1.55,  1.27,  1.65,  0.93,  0.94,  0.99,  0.90,  1.44,  1.97,  6.93,  4.20,  4.62,  6.59,  5.29,  6.76, 13.85, 19.51, 28.29, 25.69, 29.03, 23.28, 21.83, 20.98, 31.85, 37.99, 37.29, 31.59, 28.41, 17.43, 24.01, 39.00, 25.24, 51.26, 41.18, 43.85, 50.31, 44.84, 41.75, 25.40, 25.14, 32.50, 70.38]
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
		String concentrationString1 = variables.get("c1");
		DoublesExchangeItem c1 = null;
		if(concentrationString1!=null){
			double values[] = parseVector(concentrationString1);
			c1 = new DoublesExchangeItem("concentration1.grid", IExchangeItem.Role.InOut,values);
			this.items.put("concentration1.grid", c1);
		}else{
			throw new RuntimeException("Missing input. Was looking for c1");
		}
		String concentrationString2 = variables.get("c2");
		DoublesExchangeItem c2 = null;
		if(concentrationString2!=null){
			double values[] = parseVector(concentrationString2);
			c2 = new DoublesExchangeItem("concentration2.grid", IExchangeItem.Role.InOut, values);
			this.items.put("concentration2.grid", c2);
		}else{
			throw new RuntimeException("Missing input. Was looking for c2");
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

		//update reaction_time
		if(this.items.containsKey("reactionTime")){
			double reactionTime = this.items.get("reactionTime").getValuesAsDoubles()[0];
			variables.put("reaction_time", " ["+reactionTime+"] ");
		}

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

		// concentrations c1 and c2
		if(this.variables.containsKey("c1")){
			String id = "concentration1.grid";
			if(!this.items.containsKey(id)){
				throw new RuntimeException("ExchangeItem with id ="+id+" got lost.");
			}
			IPrevExchangeItem item = this.items.get(id);
			String key="c1";
			double values[] = item.getValuesAsDoubles();
			String valueString = writeVector(values);
			this.variables.put(key, valueString);
		}
		if(this.variables.containsKey("c2")){
			String id = "concentration2.grid";
			if(!this.items.containsKey(id)){
				throw new RuntimeException("ExchangeItem with id ="+id+" got lost.");
			}
			IPrevExchangeItem item = this.items.get(id);
			String key="c2";
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
			System.out.println("myWrapper: trouble removing file "+ fileName);
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
