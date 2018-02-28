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
package org.openda.model_damflow;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Locale;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 4-7-12
 * Time: 13:44
 * To change this template use File | Settings | File Templates.
 */
public class DupuitGFile implements IDataObject {

	private ArrayList<String> lines = new ArrayList<String>();
	private File dupuitGFile;
	private String fileHeader = "material";
	private String quantity = "Kx";
	private String[] parameters = new String[]{"bottom","gW","gD","mv","phi","c","n","Kx","Ky"};
	private IExchangeItem[] exchangeItems;
	private int nLayer;

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.dupuitGFile = new File(workingDir, fileName);
		readDupuitGFile();
	}

	private void writeDupuitGFile() {
		Locale locale = new Locale("EN");
		String dupuitGFormat = "%+6.4e";
		FileWriter fileWriter;
		String line;
	    try {
			File tempFile = new File(this.dupuitGFile.getParent(), "dupuitG.temp");
			fileWriter = new FileWriter(tempFile);
			BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

			// write header:
			BufferedReader dupuitGFileBufferedReader = new BufferedReader(new FileReader(this.dupuitGFile));
			line = dupuitGFileBufferedReader.readLine();
			outputFileBufferedWriter.write(line);
			outputFileBufferedWriter.newLine();
			// write nLayer:
			line = dupuitGFileBufferedReader.readLine();
			outputFileBufferedWriter.write(line);
			outputFileBufferedWriter.newLine();
			// write columns title:
			line = dupuitGFileBufferedReader.readLine();
			outputFileBufferedWriter.write(line);
			outputFileBufferedWriter.newLine();

			// write table of material properties:
			String[] lineFields;
			String delim = " ";
			int iColumnBottom = 1; // first parameter in the table in dupuit.G
			int iColumnKy = 9; // last parameter in the table in dupuit.G
			int iExchangeItem = 0;
			for (int iLayer=0; iLayer<nLayer; iLayer++){
				line = dupuitGFileBufferedReader.readLine();
				lineFields = line.trim().split("[ \t]+");
				for (int iParam=iColumnBottom; iParam<=iColumnKy; iParam++){
					iExchangeItem = iLayer*parameters.length+iParam-iColumnBottom;
					double param = this.exchangeItems[iExchangeItem].getValuesAsDoubles()[0];
					lineFields[iParam] = String.format(locale,dupuitGFormat,param);
				}
				String updatedLine = Arrays.toString(lineFields).replace(", ", delim).replaceAll("[\\[\\]]", "");
				outputFileBufferedWriter.write(updatedLine);
				outputFileBufferedWriter.newLine();
			}
			// write the rest:
			line = dupuitGFileBufferedReader.readLine();
			while (line != null) {
				iExchangeItem++;
				double param = this.exchangeItems[iExchangeItem].getValuesAsDoubles()[0];
				lineFields = line.trim().split("[ \t]+");
				lineFields[1] = String.format(locale,dupuitGFormat,param);
				String updatedLine = Arrays.toString(lineFields).replace(", ", delim).replaceAll("[\\[\\]]", "");
				outputFileBufferedWriter.write(updatedLine);
				outputFileBufferedWriter.newLine();
				line = dupuitGFileBufferedReader.readLine();
			}
//			while (line != null) {
//				outputFileBufferedWriter.write(line);
//				outputFileBufferedWriter.newLine();
//				line = dupuitGFileBufferedReader.readLine();
//			}
			dupuitGFileBufferedReader.close();
			outputFileBufferedWriter.close();
			BBUtils.copyFile(tempFile, this.dupuitGFile);
			tempFile.deleteOnExit();
		} catch (IOException e) {
			throw new RuntimeException("Could not write to " + this.dupuitGFile.getAbsolutePath());
		}
	}

	private void readDupuitGFile() {
		String lineFields[];
		String line;
		try {
			BufferedReader dupuitHOFileBufferedReader = new BufferedReader(new FileReader(this.dupuitGFile));
			line = dupuitHOFileBufferedReader.readLine();
			if (!line.toLowerCase().contains(fileHeader)) {
				throw new RuntimeException("DAMFlow timeseries file does not contain keyword '"+fileHeader+"':"+this.dupuitGFile.getAbsolutePath());
			} else {
				line = dupuitHOFileBufferedReader.readLine();
			}
			while (line != null) {
				lines.add(line);
				line = dupuitHOFileBufferedReader.readLine();
			}
			dupuitHOFileBufferedReader.close();
		} catch (IOException e){
			throw new RuntimeException("Could not read DAMFlow material properties file "+this.dupuitGFile.getAbsolutePath());
		}

		line = lines.get(0);
		lineFields = line.trim().split("[ \t]+");
		if (lineFields[0].equalsIgnoreCase("nLayer")) {
			nLayer = Integer.parseInt(lineFields[1]);
		} else {
			throw new RuntimeException("Could not find nLayer in DAMFlow material properties file "+this.dupuitGFile.getAbsolutePath());
		}
		// nExchangeItems = #parameter in the table + #extraparameter below the table
		String[] extraParamNames = new String[]{"cDitch","cRiverBed","cPolder","cDike","beta"};
		int nExtraParam = extraParamNames.length; //Those are (in fixed order): cDitch,cRiverBed,cPolder,cDike,beta.
		int nExchangeItems = nLayer*parameters.length+nExtraParam;
		exchangeItems = new DupuitGFileExchangeItem[nExchangeItems];

		// This wrapper requires that the parameters available in the dupuit.G file in the following order:
		// layer   bottom(m)   gW(kN/m3)   gD(kN/m3)   mv(m2/kN)      phi(o)    c(kN/m2)        n(-)     Kx(m/d)     Ky(m/d)
		// i.e. int iColumnBottom = 1; 	int iGW = 2; int iGD = 3; int iMV = 4; int iPhi = 5; int iC = 6; int iN = 7; int iColumnKx = 8; int iColumnKy = 9;
		int iColumnBottom = 1;
		int iColumnKy = 9;
		int iLine=2;
		int iExchangeItem=0;
		// Loop through the table of parameters:
		for (int iLayer=0; iLayer<nLayer; iLayer++){
			line = lines.get(iLine);
			lineFields = line.trim().split("[ \t]+");
			for (int iParam=iColumnBottom; iParam<=iColumnKy; iParam++){
				double param = Double.parseDouble(lineFields[iParam]);
				String id = "layer"+iLayer+"."+parameters[iParam-iColumnBottom];
				iExchangeItem=iLayer*parameters.length+iParam-iColumnBottom;
				exchangeItems[iExchangeItem] = new DupuitGFileExchangeItem(id,param);
			}
			iLine++;
		}
		// Loop through the extra parameters:
		for (int iExtraParam=0; iExtraParam<nExtraParam; iExtraParam++){
			iExchangeItem++;
			line = lines.get(iLine);
			lineFields = line.trim().split("[ \t]+");
			if (lineFields[0].contains(extraParamNames[iExtraParam])){
				double param = Double.parseDouble(lineFields[1]);
				exchangeItems[iExchangeItem] = new DupuitGFileExchangeItem(extraParamNames[iExtraParam],param);
			} else {
				throw new RuntimeException("Could not find "+ extraParamNames[iExtraParam] +" in DAMFlow material properties file "+this.dupuitGFile.getAbsolutePath());
			}
			iLine++;
		}
	}

	
	public String[] getExchangeItemIDs() {
		int nExchangeItem = exchangeItems.length;
		String[] exchangeItemIDs = new String[nExchangeItem];
		for (int i=0; i<nExchangeItem; i++){
			exchangeItemIDs[i] = exchangeItems[i].getId();
		}
		return exchangeItemIDs;
	}

	
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFile - Method Name : getExchangeItemIDs");
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int i;
		for (i=0; i<this.exchangeItems.length;i++){
			if (exchangeItemID.contentEquals(this.exchangeItems[i].getId())){
				break;
			}
		}
		return this.exchangeItems[i];
	}

	
	public void finish() {
		writeDupuitGFile();
	}

	
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}
}
