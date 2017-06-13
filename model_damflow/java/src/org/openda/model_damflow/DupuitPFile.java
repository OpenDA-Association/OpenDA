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
package org.openda.model_damflow;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.util.Locale;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 2-7-12
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */
public class DupuitPFile implements IDataObject {
	// TODO: For the moment, the wrapper simply works with the sTime and eTime parameters. In the future,
	// there may be a need to work with other parameters, e.g. for consistency check of information
	// on other files.
	// TODO: file format of the output is not yet identical with the original file. Check if this is of
	// importance, with John van Esch.
	private File dupuitPFile;
	private double eTime;
	private double sTime;
	private double sPolder;
	private String[] exchangeItemIDs = new String[]{"sTime","eTime","sPolder"};
	private IExchangeItem[] exchangeItems;

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.dupuitPFile = new File(workingDir, fileName);
		readDupuitPFile();
		exchangeItems = new DupuitPFileExchangeItem[3];
		exchangeItems[0] = new DupuitPFileExchangeItem(exchangeItemIDs[0],this);
		exchangeItems[1] = new DupuitPFileExchangeItem(exchangeItemIDs[1],this);
		exchangeItems[2] = new DupuitPFileExchangeItem(exchangeItemIDs[2],this);
	}

	
	public String[] getExchangeItemIDs() {
		return new String[] {exchangeItems[0].getId(),exchangeItems[1].getId(),exchangeItems[2].getId()};
	}

	
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFile - Method Name : getExchangeItemIDs");
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (exchangeItemID.equalsIgnoreCase(this.exchangeItems[0].getId())){
			return this.exchangeItems[0];
		} else if (exchangeItemID.equalsIgnoreCase(this.exchangeItems[1].getId())) {
			return this.exchangeItems[1];
		} else if (exchangeItemID.equalsIgnoreCase(this.exchangeItems[2].getId())) {
			return this.exchangeItems[2];
		} else {
			throw new RuntimeException(this.getClass()+": no exchange item with ID "+exchangeItemID+". Available exchange item: "+
					this.exchangeItems[0].getId()+", "+this.exchangeItems[1].getId()+", "+this.exchangeItems[2].getId()+".");
		}
	}

	public void finish(){
		Locale locale = new Locale("EN");
		String dupuitPFormat = "%+6.4e";
		FileWriter fileWriter;

		String line;
		try {
			File tempFile = new File(this.dupuitPFile.getParent(), "dupuitP.temp");
			fileWriter = new FileWriter(tempFile);
			BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

			BufferedReader dupuitPFileBufferedReader = new BufferedReader(new FileReader(this.dupuitPFile));
			line = dupuitPFileBufferedReader.readLine();
			while (line != null) {
				if (line.contains("sTime")){
					outputFileBufferedWriter.write("sTime\t"+String.format(locale,dupuitPFormat,this.sTime));
				} else if (line.contains("eTime")){
					outputFileBufferedWriter.write("eTime\t"+String.format(locale,dupuitPFormat,this.eTime));
				} else if (line.contains("sPolder")){
					outputFileBufferedWriter.write("sPolder\t"+String.format(locale,dupuitPFormat,this.sPolder));
				} else {
					outputFileBufferedWriter.write(line);
				}
				outputFileBufferedWriter.newLine();
				line = dupuitPFileBufferedReader.readLine();
			}
			dupuitPFileBufferedReader.close();
			outputFileBufferedWriter.close();
			BBUtils.copyFile(tempFile, this.dupuitPFile);
			tempFile.deleteOnExit();
		} catch (IOException e){
			throw new RuntimeException("Could not read/write DAMFlow input file "+this.dupuitPFile.getAbsolutePath());
		}
	}

	private void readDupuitPFile() {
		String lineFields[];
		String line;
		int dupuitPParamFound = 0;
		try {
			BufferedReader dupuitPFileBufferedReader = new BufferedReader(new FileReader(this.dupuitPFile));
			line = dupuitPFileBufferedReader.readLine();
			while (line != null) {
				if (line.contains("sTime")){
					lineFields = line.trim().split("[ \t]+");
					this.sTime = Double.parseDouble(lineFields[1]);
					dupuitPParamFound++;
				} else if (line.contains("eTime")){
					lineFields = line.trim().split("[ \t]+");
					this.eTime = Double.parseDouble(lineFields[1]);
					dupuitPParamFound++;
				} else if (line.contains("sPolder")){
					lineFields = line.trim().split("[ \t]+");
					this.sPolder = Double.parseDouble(lineFields[1]);
					dupuitPParamFound++;
				}
				line = dupuitPFileBufferedReader.readLine();
			}
			dupuitPFileBufferedReader.close();
		} catch (IOException e){
			throw new RuntimeException("Could not read DAMFlow input file "+this.dupuitPFile.getAbsolutePath());
		}
		if (dupuitPParamFound!=exchangeItemIDs.length){
			throw new RuntimeException("Could not find sTime, eTime and/or sPolder in DAMFlow input file "+this.dupuitPFile.getAbsolutePath());
		}

	}

	protected double getSTime() {
		return this.sTime;
	}

	protected double getETime() {
		return this.eTime;
	}

	protected double getSPolder() {
		return this.sPolder;
	}

	protected void setSTime(double values) {
		this.sTime = values;
	}

	protected void setETime(double values) {
		this.eTime = values;
	}

	protected void setSPolder(double values) {
		this.sPolder = values;
	}

	
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}
}
