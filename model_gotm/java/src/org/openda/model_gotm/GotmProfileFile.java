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
package org.openda.model_gotm;
import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * Read from ascii file in following format
 *
 * 1998-09-07 08:16:00 12 2
 * -2.25  12.652
 * -2.75  12.658
 * -3.25  12.661
 * -3.75  12.66
 * -4.25  12.66
 * -4.75  12.659
 * -5.25  12.657
 * -5.75  12.66
 * -6.25  12.662
 * -6.75  12.662
 * -7.25  12.662
 * -7.75  12.664
 * 1998-09-07 09:16:00 14 2
 * -2.25  12.652
 * -2.75  12.658
 * -3.25  12.661
 * -3.75  12.66
 * -4.25  12.66
 * -4.75  12.659
 * -5.25  12.657
 * -5.75  12.66
 * -6.25  12.662
 * -6.75  12.662
 * -7.25  12.662
 * -7.75  12.664
 * -8.25  12.66
 * -8.75  12.657
 *
 */


public class GotmProfileFile implements IDataObject{
	File workingDir;
	String fileName = null;
	ArrayList<IExchangeItem> items = new ArrayList<IExchangeItem>();
	int nVar = 0;
	int axisDirection = 1;

	String df = "yyyy-MM-dd HH:mm:ss";
	double[] zData;
	String id = "";
	private Pattern datePattern = Pattern.compile("(\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2})\\s+(\\d+)\\s+(\\d+)$"); // 1998-09-07 09:16:00 14 2
	private static String dateOutFormat = "%s %d %d"; // 1998-09-07 09:16:00 14 2
	
	
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		if((arguments==null) || (arguments.length==0)){
			throw new RuntimeException("No filename was provided for opening an profile-file.");
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
			String[] temp = this.fileName.split("/");
			String[] temp2 = temp[temp.length-1].split("\\.");
			id = temp2[0];
			System.out.println("Id is:");
			System.out.println(id);
		}catch (Exception e) {
			System.out.println("GotmProfileFile: trouble opening file "+ this.fileName);
		}
		//read file and parse to hash
		Vector timesVector = new Vector(0); 
		Vector valuesVector[] = null ; 
		String[] idVar = null;
		boolean firstItem = true;
		
		try {
			Scanner input = new Scanner(inputFile);

			String line;
			int lineNumber=0;
			int profileNumber=0;

			while (  input.hasNextLine()  ) {
                line = input.nextLine();
				if (line.isEmpty() ) continue;
				lineNumber++;
				
				//System.out.println(lineNumber+": line = '" + line + "'");
				Matcher m = datePattern.matcher(line);

					if (m.find( )) {
						profileNumber++;
						//System.out.println(m.group(1));
						double[] timeInMjd = new double[1];
						try {
							
							timeInMjd[0] = TimeUtils.date2Mjd( m.group(1), this.df);
						}
						catch (ParseException e) {
							throw new RuntimeException("Cannot parse date from " + m.group(1));
						}
						timesVector = Vector.concatenate( timesVector,  new Vector( timeInMjd ) );
						// Parse the date number of z-points and direction from line
						int zPoints = Integer.parseInt(m.group(2));
						zData = new double[zPoints];
					
						double[][] values = null;						
						// Read profiles
						for (int i = 0; i< zPoints; i++) {
							line = input.nextLine();
							if (line.isEmpty() ) continue;
							lineNumber++;
							// Split line with whitespace as separator
							String[] lineParts = line.trim().split("\\s+");
							
							if ( firstItem ) {
								this.nVar = lineParts.length - 1;
								this.axisDirection = Integer.parseInt(m.group(3));
								valuesVector = new Vector[2];
								for (int iVar = 0; iVar<this.nVar ; iVar++){
								   valuesVector[iVar] = new Vector(0);
								}
								idVar = new String[this.nVar];
								for (int iVar = 0; iVar<this.nVar ; iVar++){
									idVar[iVar] = id + "_" + String.valueOf(iVar);
									//System.out.println(idVar[iVar]);
								}
								firstItem = false;
							}
							
							if ( values == null ){
								values = new double[this.nVar][zPoints];
							}
							try {
								// Read in z and data
								zData[i] = Double.parseDouble(lineParts[0]);
								for ( int iVar = 0 ; iVar < this.nVar ; iVar++ ) {
									values[iVar][i] = Double.parseDouble(lineParts[iVar + 1]);
								}
							}
							catch (NumberFormatException e) {
								throw new RuntimeException("Cannot parse line: " + line);
							}
						}

					    // Put all values into one vector
						for ( int iVar = 0 ; iVar < nVar ; iVar++  ) {
							valuesVector[iVar] = Vector.concatenate( valuesVector[iVar], new Vector(values[iVar]));
						}
					
					
                } else {
                   throw new RuntimeException("Cannot parse date from " + line + " using regex in file " + inputFile );
                }
					
			}
			input.close();

		} catch (FileNotFoundException e) {
			throw new RuntimeException("Problem reading from file "+fileName+" : "+e.getMessage());
		}
		
		for ( int iVar = 0 ; iVar < nVar ; iVar++  ) {
			ArrayExchangeItem item = new ArrayExchangeItem(idVar[iVar], IPrevExchangeItem.Role.InOut);
            item.setValuesAsDoubles(valuesVector[iVar].getValues());
            item.setTimeInfo(new TimeInfo(timesVector.getValues()));
            items.add(item);
		}
	
	}

	
    public void finish() {

		Locale.setDefault(Locale.UK);
		System.out.println("New file with this fileName is written:");
        System.out.println(this.fileName);
    	
        try {
            BufferedWriter output =  new BufferedWriter(new FileWriter(this.fileName));
            System.out.println("New file with this fileName is written:");
            System.out.println(this.fileName);
            
            double[] times = null;
            double[][] values = new double[this.nVar][];

            for (int i = 0; i < items.size(); i++) {
                IExchangeItem item = items.get(i);
                times = item.getTimes();
                values[i] = item.getValuesAsDoubles();
            }
            	
            for (int timeIndex=0; timeIndex < times.length; timeIndex ++){
                double timeInMjd = times[timeIndex];
                String dateString = TimeUtils.mjdToString(timeInMjd, this.df); 
                output.write(String.format( dateOutFormat, dateString, zData.length , axisDirection ));
                output.newLine();
                for ( int z=0 ; z < zData.length ; z++  ) {
                    String formatString = " %14.11f";
                    String line = String.format("%6.1f", zData[z]);
                    for (int iVar = 0; iVar < this.nVar; iVar++) {
                        line += String.format(formatString , values[iVar][timeIndex  * zData.length  +  z ] ) ;
                    }
                    output.write( line );
                    output.newLine();
                }
            }
            output.close();
        }
        catch (IOException ex){
            throw new RuntimeException("Cannot write file " + this.fileName);
        }

    }
	public String[] getExchangeItemIDs() {
		int n=this.items.size();
		String ids[] = null;
		if(n>0){
			ids = new String[n];
            for (int i = 0; i < items.size(); i++) {
                IExchangeItem item = items.get(i);
				ids[i] = item.getId();
            }
		}
		return ids;
	}
	
	public String[] getExchangeItemIDs(Role role) {
		return getExchangeItemIDs();
	}
	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int n=this.items.size();
		IExchangeItem item = null;
		if(n>0){
            for (int i = 0; i < items.size(); i++) {
                item = items.get(i);
				if ( exchangeItemID== item.getId() ) break;
            }
		}
		return item;	
	}

	public String toString(){
		String result="";
		int n=this.items.size();
		String ids[] = null;
		return result;
	}
}
