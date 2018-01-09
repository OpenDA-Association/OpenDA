/* OpenDA v2.3.1
* Copyright (c) 2016 OpenDA Association 
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

package org.openda.model_lhm;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
//import org.openda.interfaces.IPrevExchangeItem;
import org.openda.model_lhm.LHMParameters;
import org.openda.utils.Array;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * Reading and writing LHM MetaSwap State file
*/
public class LHMSoilMoisture implements IDataObject {

    private IExchangeItem[] exchangeItems;
    private File smFile;
	private File configFile;
	private File satSmFile;
	private File tempFile;
    public int numberOfSvats;
    private double[] satSmList;
    private String lhmStateId="soilmoisture";
//    private int[] svatList;
	private String[] paramId;
    private int[] startCol;
	private int[] endCol;
	public String[] paramNames;
	private String lhmSoilMoistureId = "soilmoisture";
	private ArrayList<String> cLines = new ArrayList<String>();
	public double[][] smParams;

    public void ReadFiles(File lhmSMFile, File lhmConfigFile, File lhmSatSMFile) {
		System.out.println("Reading statefile...");
    	try {
			BufferedReader cFileBufferedReader = new BufferedReader(new FileReader(lhmConfigFile));
			String cLine = cFileBufferedReader.readLine();
			while (cLine != null) {
				cLines.add(cLine);
				cLine = cFileBufferedReader.readLine();
			}
			cFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not read LHM input file " + lhmConfigFile.getAbsolutePath());
		}
		this.configFile = lhmConfigFile;
        try{
			if(this.numberOfSvats==0){
				getNumberOfSvats(smFile);
			}
			BufferedReader sFileBufferedReader = new BufferedReader(new FileReader(lhmSatSMFile));
			String sLine = sFileBufferedReader.readLine();
			satSmList = new double[numberOfSvats];
			int c=0;
			while (sLine != null){
				sLine = sFileBufferedReader.readLine();
				if(sLine==null){
					break;
				}
				String[] fields = sLine.split(",");
				//svatList[c] = Integer.parseInt(fields[1]);
				satSmList[c] = Double.parseDouble(fields[2]);
				c++;
			}
			sFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not read porosity-file with  " + lhmSatSMFile.getAbsolutePath());
		}
		this.configFile = lhmConfigFile;
		try {
			if(this.paramId==null){
				readConfigFile();
			}
			if(this.numberOfSvats==0){
				getNumberOfSvats(smFile);
			}
            BufferedReader smFileBufferedReader = new BufferedReader(new FileReader(lhmSMFile));
			double[][] paramVec = new double[numberOfSvats][this.paramId.length];
            String line = smFileBufferedReader.readLine();
            int lineCount = 0;
            while (line != null) {
				line = smFileBufferedReader.readLine();
				if(line==null){
					break;
				}
            	double[] params = readSMParams(line);
				paramVec[lineCount] = params;
                lineCount++;
            }
            this.smParams = paramVec;
            this.numberOfSvats = lineCount;
            smFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read LHM input file " + lhmSMFile.getAbsolutePath());
        }
        this.smFile = lhmSMFile;
    }

    public double readStopTimeFromFile(){
		File tempFile = new File(this.smFile.getParent(),"../../stopTFile.dat");
    	double tStop = 0.0;
    	try {
			BufferedReader tempFileBufferedReader = new BufferedReader(new FileReader(tempFile));
			String timeStr = tempFileBufferedReader.readLine();
			tStop = Double.parseDouble(timeStr);
		} catch (IOException e) {
			throw new RuntimeException("Could not read temporary TStop-file " + tempFile.getAbsolutePath());
    	}
		System.out.println("Reading temporary file with end-time..."+tStop);
		return(tStop);
	}

    public void getNumberOfSvats(File smFile){
    	System.out.println(smFile);
		try {
			BufferedReader smFileBufferedReader = new BufferedReader(new FileReader(smFile));
			String line = smFileBufferedReader.readLine();
			int lineCount = 0;
			while (line != null) {
				lineCount++;
				line = smFileBufferedReader.readLine();
			}
			this.numberOfSvats = lineCount-1;
		} catch (IOException e) {
			throw new RuntimeException("Could not read LHM input file " + smFile.getAbsolutePath());
		}
	}

	public void readConfigFile(){
		String[] paramId = new String[cLines.size()-1];
		int[] startCol = new int[cLines.size()-1];
		int[] endCol = new int[cLines.size()-1];
		for(int i=1; i < cLines.size(); i++){
			String line  = cLines.get(i);
			String[] lineFields = line.trim().split(",");
			if(lineFields.length<5){
				throw new RuntimeException("File"+configFile+" is in the wrong format - should have 5 comma-separated fields");
			}
			paramId[i-1] = lineFields[0];
			startCol[i-1] = Integer.parseInt(lineFields[1]);
			endCol[i-1] = Integer.parseInt(lineFields[2]);
		}
		this.paramId = paramId;
		this.startCol = startCol;
		this.endCol = endCol;
	}
	public double[] readSMParams(String line){
		double[] params = new double[paramId.length];
    	String[] paramNames = new String[paramId.length];
		//System.out.println(paramId.length+" "+startCol.length+" "+endCol.length+" "+line);
		//System.out.println(line);
		for(int j=0; j < paramId.length; j++){
			//System.out.println(j+" "+paramId[j]+" "+startCol[j]);
    		params[j] = Double.parseDouble(line.substring(startCol[j],endCol[j]));
    		paramNames[j] = paramId[j];
		}
		this.paramNames= paramNames;
		return params;

	}

	public double getAverage(double[] vector){
		double sum=0.0;
		int count=0;
		for(int i = 0; i < vector.length; i++){
			Double test = vector[i];
			if(!test.isNaN()) {
				sum = sum + vector[i];
				count++;
			}
		}
		return sum/count;
	}

	/*private double[] getSatMoist(){
		// these three will come from svat_dtgw eventually - so smParams has to be read from there, but we can keep the same format
		int actInd = Arrays.asList(paramNames).indexOf("SM_1");
		int defInd = Arrays.asList(paramNames).indexOf("SD_1");
		int rzInd = Arrays.asList(paramNames).indexOf("RZ_D");
		double[] satMoistVec = new double[numberOfSvats];
		for(int i=0; i < numberOfSvats; i++){
			if(smParams[i][actInd] < smParams[i][defInd]){ smParams[i][actInd] = smParams[i][defInd]; }
			satMoistVec[i] = (smParams[i][actInd] + smParams[i][defInd])/smParams[i][rzInd];
		}
		return satMoistVec;
	}
*/
	public double[] getMoistDef(){
		// moisture deficit (in  mm) from the init_svat
		int defInd = Arrays.asList(paramNames).indexOf("SD_1");
		//int rzInd = Arrays.asList(paramNames).indexOf("RZ_D");
		double[] moistDefVec = new double[numberOfSvats];
		for(int i=0; i < numberOfSvats; i++){
			moistDefVec[i] = (smParams[i][defInd]); // /smParams[i][rzInd];
		}
		return moistDefVec;
	}


	private double[] getActMoist(){
		// calculated volumetric soil moisture: (saturated water disk - deficit water disk) / root zone depth
		double[] moistSatVec = this.satSmList;
		double[] moistDefVec = getMoistDef();
		int rzInd = Arrays.asList(paramNames).indexOf("RZ_D");
		double[] actMoistVec = new double[numberOfSvats];
		for(int i=0; i < numberOfSvats; i++){
			double rzDepth = 0.0;
			if(smParams[i][rzInd]< 0.1 ){
				rzDepth = 0.5;
			}else{
				rzDepth = smParams[i][rzInd];
			}
			actMoistVec[i] = (moistSatVec[i] - moistDefVec[i])/rzDepth;
		}
		return actMoistVec;
	}

	public double getMoisture(){
		System.out.println("Average volumetric soil moisture content:"+getAverage(getActMoist()));
		return getAverage(getActMoist());
	}

	public void setMoisture(double[] optVolSoilMoistList) {
		double optVolSoilMoist = optVolSoilMoistList[0];
		System.out.println("Optimal soil moiisture value: "+optVolSoilMoist);
		Locale locale = new Locale("EN");
		// based on the optimal avarage VSM, we rescale the vector and get a new vector of deficits. Write those and the rootzones to the restart file.
		int rzInd = Arrays.asList(paramNames).indexOf("RZ_D");
		int defInd = Arrays.asList(paramNames).indexOf("SD_1");
		double[] actMoistVec = getActMoist();
		double[] satMoistVec = this.satSmList;
		double[] moistDefVec = getMoistDef();
		double avMoist = getAverage(actMoistVec);
		try {
			BufferedReader smFileBufferedReader = new BufferedReader(new FileReader(smFile));
			int rzStartCol = startCol[rzInd];
			int dmStartCol = startCol[defInd];

			File tempFile = new File(this.smFile.getParent(), "../init_svatTemp.inp");
			FileWriter fileWriter = new FileWriter(tempFile);
			BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

			// write header line
			String oldLine = smFileBufferedReader.readLine();
			outputFileBufferedWriter.write(oldLine);
			outputFileBufferedWriter.newLine();

			for (int i = 0; i < numberOfSvats; i++) {
				double actMoistNew = actMoistVec[i] + (optVolSoilMoist - avMoist);
				double rzDepth = 0.0;
				if(smParams[i][rzInd]< 0.1 ){
					rzDepth = 0.5;
				}else{
					rzDepth = smParams[i][rzInd];
				}
				double moistDefNew = (satMoistVec[i] - actMoistNew * rzDepth);
				double rzNew = smParams[i][rzInd];

				oldLine = smFileBufferedReader.readLine();
				String newLine = oldLine.substring(0, rzStartCol - 2) + String.format(locale, "%15.4f", rzNew) +
						oldLine.substring(rzStartCol + 12, dmStartCol - 2) + String.format(locale, "%15.4f", moistDefNew) + oldLine.substring(dmStartCol + 14);

				outputFileBufferedWriter.write(newLine);
				outputFileBufferedWriter.newLine();
			}
			outputFileBufferedWriter.close();
			smFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not read/write from/to LHM input files " + smFile.getAbsolutePath());
		}
	}

    public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId()};
    }

    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItemID.equals(lhmStateId)) {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		}
		return exchangeItems[0];
	}


	public IExchangeItem getExchangeItem(String exchangeItemID) {
        if (!exchangeItemID.equals(lhmSoilMoistureId)) {
            throw new RuntimeException("unknown exchange item: " + exchangeItemID);
        }
        return exchangeItems[0];
    }

    public void finish(){};//System.out.println("Do not do anything.");};

	private void initialize(File workingDir, String smFileName, String configFileName, String satSmFileName, String[] arguments) {
		// before we can read it, we need to copy it
		tempFile = new File(workingDir,"metaswap/init_svat.out");
		this.smFile = new File(workingDir,smFileName);
		System.out.println(this.smFile);
		//try{
		//	BBUtils.copyFile(tempFile,smFile);
		//} catch (IOException e) {
		//	throw new RuntimeException("Could not copy to "+smFileName);
	    //	}
		tempFile = null;
		this.configFile = new File(workingDir,configFileName);
		this.satSmFile = new File(workingDir,satSmFileName);
		ReadFiles(this.smFile,this.configFile,this.satSmFile);
		exchangeItems = new LHMSoilMoistureExchangeItem[1];
		exchangeItems[0] = new LHMSoilMoistureExchangeItem(lhmSoilMoistureId,this);
	}

    public void initialize(File workingDir, String[] arguments) {
        //System.out.println("Initializing LHMSoilMoisture");
		String smFileName = arguments[0];
        String configFileName = arguments[1];
		String satSmFileName = arguments[2];
        String[] remainingArguments = new String[arguments.length-3];
        System.arraycopy(arguments, 3, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, smFileName, configFileName, satSmFileName, remainingArguments);
    }
}





