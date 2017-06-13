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
package org.openda.model_nemo;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.text.ParseException;
import java.util.*;


/**
 *
 * Read from ascii file in following format
 *
 */


public class NemoNamelistFileWrapper implements IoObjectInterface{

    private class NemoMetaExchangeItem{
		public IExchangeItem exchangeItem;
		public boolean fromRestartFile;
		String shortName;
		public int[] nDims=null;

		NemoMetaExchangeItem(IExchangeItem exchangeItem, boolean fromRestartFile, String fullName){
			this.exchangeItem=exchangeItem;
			this.fromRestartFile=fromRestartFile;
			if (fromRestartFile){
				nDims=fullNameToDims(fullName);
				shortName=fullNameToShortName(fullName);
			}
		}
	}



	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String,NemoMetaExchangeItem> items = new LinkedHashMap<String,NemoMetaExchangeItem>();
	List<String> namelistContent= new ArrayList<String>();

	//cache these values
	double refdate=0;
	double tstart;
	double dt=1.0;
	double tstop;
	double unit=1.0;
	String sourceLabels[];
	String boundLabels[];
	String outputLabels[];
	String netcdfFile=null;
	boolean debug=false;

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.workingDir = workingDir;
		this.fileName = fileName;
		if (debug) System.out.println("ioObject : filename = "+fileName);

		//Read NamelistFile;
		try {
			readNamelistFile(fileName);
		} catch (IOException e) {
			e.printStackTrace();
		}

		if(debug){
			for(String key: this.items.keySet()){
				System.out.println("key="+key);
				System.out.println(this.items.get(key).toString());
			}
		}
	}

	public IExchangeItem[] getExchangeItems() {
		//TODO for now return some dummy timeSeries
		int n = this.items.size();
		Set<String> keys = this.items.keySet();
		IExchangeItem[] result=new IExchangeItem[n];
		int i=0;
		for(String key : keys){
			result[i]=this.items.get(key).exchangeItem;
			i++;
		}
		return result;
	}

	public void finish() {
		if (debug) System.out.println("Finish method is called for Namelist file");
		//update times
		double startAsMjd = this.items.get("startTime").exchangeItem.getValuesAsDoubles()[0];
		this.tstart = startAsMjd - this.refdate;
		double endAsMjd = this.items.get("endTime").exchangeItem.getValuesAsDoubles()[0];
		this.tstop = endAsMjd - this.refdate;

		//write to file
		if (debug) System.out.println("Writing to file: "+this.workingDir+"/"+this.fileName);
		File outputFile = new File(this.workingDir,this.fileName);
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

			Iterator<String> iterator = namelistContent.iterator();
			while (iterator.hasNext()) {
				String line=iterator.next();
				if (line.indexOf("   nn_date0")>=0){
					String dataString= org.openda.exchange.timeseries.TimeUtils.mjdToString(refdate,"yyyyMMdd");
					out.write("   nn_date0="+dataString+"\n");
				}
				else if (line.indexOf("   nn_it000")>=0){
					int tstart_int=(int) (tstart*((60*60*24)/dt)+0.5);
					if (debug) System.out.println("tstart="+tstart);
					out.write("   nn_it000="+tstart_int+"\n");
				}
				else if (line.indexOf("   nn_itend")>=0){
					if (debug) System.out.println("tstop="+tstop);
					int tstop_int=(int) (tstop*((60*60*24)/dt)+0.5);
					out.write("   nn_itend="+tstop_int+"\n");
				//	tstop
				}
				//else if (line.indexOf("   rn_rdt")>=0){
				//	int dt_int=(int) (dt*96.0+0.5);
				//	out.write("   rn_rdt="+dt_int+"\n");
				//	dt
				//}
				else if (line.indexOf("   nn_stock")>=0){
					int tstop_int=(int) (tstop*((60*60*24)/dt)+0.5);
					out.write("   nn_stock="+tstop_int+"\n");
				}
				else {
					out.write(line+"\n");
				}
			}
			out.close();
			writer.close();

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

	private void readNamelistFile(String fileName) throws IOException {
           // Open de namelist file to get the times
		   // The interesting lines are:
		   // nn_it000    =       6913 !  first time step
   		   // nn_itend    =      10368 !  last  time step
   		   // nn_date0    =  00010101 !  initial calendar date yymmdd (used if nn_rstctl=1)
		   // rn_rdt      =  900.     !  time step for the dynamics (and tracer if nn_acc=0)

			File namelist=new File(workingDir,fileName);
        	if (!namelist.exists()){
				throw new RuntimeException("NemoWrapper: settings file "+ namelist.getAbsolutePath()+" does not exist");
			}
			FileInputStream in = new FileInputStream(namelist);
			BufferedReader buff = new BufferedReader(new InputStreamReader(in));
			String line="";
			Boolean eof=false;
			while (!eof) {
				line = buff.readLine();
				if (line == null) {
					eof = true;
				}
				else {
				    namelistContent.add(line);
				    // now parse this line
            		// Remove comments at end of line
					if (line.indexOf("!")>1){
						String columns[] = line.split("!");
						line=columns[0];
					}
					if (line.startsWith("!")) {
						// comment or metadata
					}
					else if(line.indexOf("=")>0) {
						// remove comments
						String[] columns = line.split("=");
						columns[0]=columns[0].trim();
						columns[1]=columns[1].trim();
						//Refdata
						if (columns[0].equals("nn_date0")){
				                        //System.out.println("DEBUG:nn_date0 "+ columns[1]);
							try {
								refdate = org.openda.exchange.timeseries.TimeUtils.date2Mjd(columns[1],"yyyyMMdd");
							} catch (ParseException e) {
								throw new RuntimeException("Trouble parsing a date. Expecting eg 01 jan 2010, found "+columns[1]);
							}
						}
						if (columns[0].equals("nn_it000")){
							//System.out.println("DEBUG:nn_it000");
							tstart=Double.valueOf(columns[1]);
						}
						if (columns[0].equals("nn_itend")){
				                        //System.out.println("DEBUG:nn_itend");
							tstop=Double.valueOf(columns[1]);
						}
						if (columns[0].equals("rn_rdt")){
                                                           	//System.out.println("DEBUG:rn_rdt");
        							dt=Double.valueOf(columns[1]);
						}
				                //System.out.println("Skipping variable: "+columns[0] + " value :"+columns[1]);
						//variables.put(columns[0], columns[1]);
					}
					else if (line.startsWith("&")){

						String section= new String(line.trim());
						if (debug) System.out.println("New section: "+section);
					}
				}
			}
		    buff.close();
			in.close();

		    //System.out.printf("DEBUG: tstart="+tstart);
		    //System.out.printf("DEBUG: tstop ="+tstop);
                    //System.out.printf("DEBUG: dt="+dt);
		    //System.out.printf("DEBUG: refdate="+refdate);
                        
                        //start and end points for the run
                        tstart = tstart / ((60*60*24)/dt);
                        tstop = tstop / ((60*60*24)/dt);
			
                        //add exchange items for time
			IExchangeItem startTime = new DoubleExchangeItem("startTime", this.refdate+this.tstart);
			this.items.put("startTime", new NemoMetaExchangeItem(startTime,false,null));
			IExchangeItem endTime = new DoubleExchangeItem("endTime", this.refdate+this.tstop);
			this.items.put("endTime",new NemoMetaExchangeItem(endTime,false,null));
	}

	private int [] fullNameToDims(String fullName){
		// The full name looks similar to
		// hdivn(t=1, z=11, y=81, x=121)
		int [] nDims=null;
		int istart=fullName.indexOf("(");
		int istop =fullName.indexOf(")");
		if (istart>0 && istop>istart){
			String allDims=fullName.substring(istart+1,istop);
			String[] aDim = allDims.split(",");
			nDims = new int[aDim.length];
			for (int i=0; i<aDim.length; i++){
				String[] str=aDim[i].split("=");
				nDims[i]= (int) Integer.parseInt(str[1]);
			}
		}
		if (debug) {
			if (nDims != null) {
				System.out.println("fullName=" + fullName + " nDims" + nDims.toString());
			} else {
				System.out.println("fullName=" + fullName + " NO-DIMENSIONS");
			}
		}
		return nDims;
	}

	private String fullNameToShortName(String fullName){
		// The full name looks similar to
		// hdivn(t=1, z=11, y=81, x=121)
		String[] subStr = fullName.split("\\(");
		if (debug) System.out.println("Short name is "+subStr[0]);
		return subStr[0];
	}
}
