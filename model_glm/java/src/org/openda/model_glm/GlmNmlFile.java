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
package org.openda.model_glm;
import org.openda.exchange.DoubleExchangeItem;
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
import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 *
 * Read from ascii file in following format
 *
 *!-------------------------------------------------------------------------------
 *! gotm_npzd - GOTM nutrient-phytoplankton-zooplankton-detritus model
 *!-------------------------------------------------------------------------------
 *&gotm_npzd
 *  n_initial = 4.0,
 *  p_initial = 1e-15,
 *  z_initial = 1e-15,
 *  d_initial = 1.5,
 *  p0 = 0.0225,
 *  z0 = 0.0225,
 *  w_p = -0.05,
 *  w_d = -0.05,
 *  kc = 0.01,
 *  I_min = 2.0,
 *  rmax = 2.0,
 *  gmax = 0.50,
 *  Iv = 1.1,
 *  alpha = 1.25,
 *  rpn = 0.04,
 *  rzn = 0.03,
 *  rdn = 0.01,
 *  rpdu = 0.01,
 *  rpdl = 0.1,
 *  rzd = 0.02,
 *  dic_variable='aed_carbon_dic'
 * /
 *
 */


public class GlmNmlFile implements IDataObject{
	File workingDir;
	String fileName = null;
	HashMap<String,IExchangeItem> items = new LinkedHashMap<String,IExchangeItem>();
	HashMap<String,Integer> lineNumers = new LinkedHashMap<String,Integer>();
	java.util.Vector<String> lines = new java.util.Vector<String>();


	
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		if((arguments==null) || (arguments.length==0)){
			throw new RuntimeException("No filename was provided for opening an nml-file.");
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
			System.out.println("GlmNmlFile: trouble opening file "+ this.fileName);
		}
		//read file and parse to hash
		try {
			FileInputStream in = new FileInputStream(inputFile);
			BufferedReader buff = new BufferedReader(new InputStreamReader(in));

			String currentGroup="";
			String line="";
			Boolean eof=false;
			int lineNumber=0;
			while (!eof) {
				line = buff.readLine();
				lineNumber++;
				if (line == null)
					eof = true;
				else { // now process this line
					//System.out.println(lineNumber+": line = '" + line + "'");
					this.lines.add(line); // save the contents in memory
					if(!line.startsWith("!")){

						if (line.startsWith("&")) {
							// start a new group
							currentGroup=line.substring(1, (line.length()-1)).trim();
							//System.out.println(" group : "+currentGroup);
						} else if(line.indexOf("=")>0)
						{ // variable=value
							String columns[] = line.split("=");
							columns[0]=columns[0].trim();
							//System.out.println("Testing potential variable:");
							//System.out.println("variable = "+columns[0]);
							//System.out.println("value ="+columns[1]);

							//Check for valid floating point constants only for now
							String valueAsString=columns[1];
							if(columns[1].contains(",")){
								valueAsString=valueAsString.substring(0, (valueAsString.indexOf(',')-1));
							}
							valueAsString=valueAsString.trim();
							try{
								double value=Double.parseDouble(valueAsString);

								//This looks ok --> now add it to the list
								String variable=columns[0].trim();
								String label=currentGroup+"_"+variable;
								//System.out.println("Adding "+label+" with value "+value);
								this.lineNumers.put(label,lineNumber);
								this.items.put(label, new DoubleExchangeItem(label, value));
							}catch(Exception e){
								//System.out.println("Not a float. Ignoring");
							}
						}
					}
				}
			}
			in.close();
		} catch (Exception e) {
			throw new RuntimeException("Problem reading from file "+fileName+" : "+e.getMessage());
		}
	}

	public void finish() {
		//write to file
		File outputFile = new File(fileName);
		try{
			if(outputFile.isFile()){
				outputFile.delete();
			}
		}catch (Exception e) {
			System.out.println("GlmNmlFile: trouble removing existing file "+ fileName);
		}
		try {
			FileWriter writer = new FileWriter(outputFile);
			BufferedWriter out = new BufferedWriter(writer);

			//TODO write actual output here

			out.close();
		} catch (Exception e) {
			throw new RuntimeException("Problem writing to file "+fileName+" :\n "+e.getMessage());
		}	
	}

	
	public String[] getExchangeItemIDs() {
		int n=this.items.size();
		String ids[] = null;
		if(n>0){
			ids=this.items.keySet().toArray(new String[n]);
		}
		return ids;
	}

	
	public String[] getExchangeItemIDs(Role role) {
		return getExchangeItemIDs();
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return this.items.get(exchangeItemID);
	}

	public String toString(){
		String result="";
		int n=this.items.size();
		String ids[] = null;
		for(String key:this.items.keySet()){
			result+=key+"="+((double) this.items.get(key).getValues())+"\n";
		}
		return result;
	}
}
