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

package org.openda.model_swan;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.model_swan.SwanParameters;

import java.io.*;
import java.text.ParseException;

/**
 * Reading and writing SWAN main input file.
 */
public class SwanInputFile implements IDataObject {

    private IPrevExchangeItem[] exchangeItems;

    private SwanParameters swanParameter;
    private File swnFile;
    private String tStart = null;
    private String tStop = null;
    private String dT = null;
    private String dTUnit = null;
    private String swanInputFileId = "inputfile";

    public String getTStartSimulation(){
        if (tStart==null) {
            tStart = swanParameter.getTStartSimulation();
        }
        return tStart;
    }

    public double getDblTStartSimulation() {
        double dblTStart;
        if (tStart==null) {
            tStart = swanParameter.getTStartSimulation();
        }
        try {
            String timeLine = tStart.replace(".","");
            dblTStart = TimeUtils.date2Mjd(timeLine);
        } catch (ParseException e) {
            throw new RuntimeException("TStart can not be parsed.");
        }
        return dblTStart;
    }

    public void setTStartSimulation(String tStart){
        if (isDateFormat(tStart)) {
            this.tStart = tStart;
        } else {
            throw new RuntimeException("TStart string format is not correct: "+tStart);
        }
    }

    public void setDblTStartSimulation(double dblTStart){
        String timeLine = TimeUtils.mjdToString(dblTStart);
        timeLine = new StringBuffer(timeLine).insert(8, ".").toString();
        tStart = timeLine;
     }

    public String getTStopSimulation(){
        if (tStop==null){
            tStop =  swanParameter.getTStopSimulation();
        }
        return tStop;
    }

    public double getDblTStopSimulation() {
        double dblTStop;
        if (tStop==null) {
            tStop = swanParameter.getTStopSimulation();
        }
        try {
            String timeLine = tStop.replace(".","");
            dblTStop = TimeUtils.date2Mjd(timeLine);
        } catch (ParseException e) {
            throw new RuntimeException("TStop can not be parsed.");
        }
        return dblTStop;
    }

    public void setDblTStopSimulation(double dblTStop){
        String timeLine = TimeUtils.mjdToString(dblTStop);
        timeLine = new StringBuffer(timeLine).insert(8, ".").toString();
        tStop = timeLine;
     }

    public void setTStopSimulation(String tStop){
        if (isDateFormat(tStop)) {
        this.tStop = tStop;
        } else {
            throw new RuntimeException("TStop string format is not correct: "+tStop);
        }
    }

    public String getDtSimulation(){
        if (dT==null){
            dT =  swanParameter.getDtSimulation();
        }
        return dT;
    }

    public String getDtUnitSimulation(){
        if (dTUnit==null){
            dTUnit =  swanParameter.getDtUnitSimulation();
        }
        return dTUnit;
    }

    public void writeInputFile(){
        FileWriter fileWriter;
        try {
            File tempFile = new File(this.swnFile.getParent(), "temp.SWN");
            fileWriter = new FileWriter(tempFile);
            BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

            FileReader fileReader = new FileReader(this.swnFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

            String line = inputFileBufferedReader.readLine();
            while (line != null) {
              if (!(line.toUpperCase().contains("COMPUTE"))) {
                  outputFileBufferedWriter.write(line);
                  outputFileBufferedWriter.newLine();
              } else {
                  if (tStart==null){
                      getTStartSimulation();
                  }
                  if (tStop==null){
                      getTStopSimulation();
                  }
                  if (dT==null){
                      getDtSimulation();
                  }
                  if (dTUnit==null){
                      getDtUnitSimulation();
                  }
                  String lineCompute = "COMPUTE NONSTAT "+tStart+" "+dT+" "+dTUnit+" "+tStop;
                  outputFileBufferedWriter.write(lineCompute);
                  outputFileBufferedWriter.newLine();
              }
              line = inputFileBufferedReader.readLine();
            }
            inputFileBufferedReader.close();
            outputFileBufferedWriter.close();

            // move temp.SWN to the actual input file:
            BBUtils.copyFile(tempFile,this.swnFile);
            tempFile.deleteOnExit();

        } catch (IOException e) {
            throw new RuntimeException("Could not write to " + this.swnFile.getAbsolutePath());
        }
    }


    private boolean isDateFormat(String strInput){
        String[] fields = strInput.trim().split("[.]");
        if (!strInput.contains(".")){
            return false;
        }
        if (fields[0].length()!=8){
            return false;
        }
        if (fields[1].length()!=4){
            return false;
        }
        try{
            Integer.parseInt(fields[0]);
        } catch(NumberFormatException nfe) {
            return false;
        }
        try{
            Integer.parseInt(fields[1]);
        } catch(NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    private void initialize(File workingDir, String fileName, String[] arguments) {
        this.swnFile = new File(workingDir,fileName);
        swanParameter = new SwanParameters(swnFile);
        exchangeItems = new SwanInputFileExchangeItem[1];
        exchangeItems[0] = new SwanInputFileExchangeItem(swanInputFileId,this);
    }

    public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId()};
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFile.getDataObjectExchangeItem(): Not implemented yet.");
	}

    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        if (!exchangeItemID.equals(swanInputFileId)) {
            throw new RuntimeException("unknown exchange item: " + exchangeItemID);
        }
        return exchangeItems[0];
    }

    public void finish() {
        writeInputFile();
    }

    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);
    }
}
