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
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * Reading and writing LHM MetaSwap State file
*/
public class LHMTimeSettings implements IDataObject {

    private IExchangeItem[] exchangeItems;
    //private LHMParameters lhmParameter;
    private String startDate;
    private String endDate;
    private ArrayList<String> niLines = new ArrayList<String>();
    private File dateFile;
    private File nhibatFile;
	private File tempFile;
	private double[] dates;
	private String[] dateStrings;
	private String[] lhmTimeSettingsId = new String[] { "start_time","end_time"};

    public double[] convertTimesToDouble(){
    	String[] dateStr = new String[2];
        if(dateStr[0]==null){
            dateStr = ReadDates(dateFile);
        }
        try {
		   	String startLine = dateStr[0] + "0000";
			String endLine = dateStr[1] + "0000";
		    double MjdStartTime = TimeUtils.date2Mjd(startLine);
			double 	MjdStopTime = TimeUtils.date2Mjd(endLine);
			try {
				FileWriter fileWriter;
				Locale locale = new Locale("EN");
				File tempFile = new File(this.dateFile.getParent(), "stopTFile.dat");
				fileWriter = new FileWriter(tempFile);
				BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
				outputFileBufferedWriter.write(String.format(locale,"%12.2f",MjdStopTime));
				outputFileBufferedWriter.newLine();
				outputFileBufferedWriter.close();
			} catch (IOException e) {
				throw new RuntimeException("Could not write to " + tempFile.getAbsolutePath());
			}
			double[] timesDouble = new double[2];
			timesDouble[0] = MjdStartTime;
			timesDouble[1] = MjdStopTime;
			return(timesDouble);
        } catch (ParseException e) {
            throw new RuntimeException("startdate and enddate can not be parsed into MJD-format.");
        }
    }
	public void convertTimeToString(double dates, String id){
		String dateString =  TimeUtils.mjdToString(dates);
		if(id=="start_time") {
			this.startDate = dateString;
		}else{
			this.endDate = dateString;
		}
		//return(dateString);
	}

    public String[] ReadDates(File dFile) {
		System.out.println("Reading datefile...");
		System.out.println(dFile);
        try {

            BufferedReader dFileBufferedReader = new BufferedReader(new FileReader(dFile));
            String line = dFileBufferedReader.readLine();
            dFileBufferedReader.close();
            String[] fields = line.split(" ");
            String[] dates = new String[2];
            dates[0] = fields[1].trim();
			dates[1] = fields[2].trim();
			return(dates);
        } catch (IOException e) {
            throw new RuntimeException("Could not read LHM start/stop dates " + dFile.getAbsolutePath());
        }
    }


    public void writeNBFile(){
        //if(dates==null){
	    //		this.dates = convertTimesToDouble();
		//

		System.out.println("Writing NHI.bat file....");
		System.out.println(this.dates[0]+" "+this.dates[1]);
        FileWriter fileWriter;
        Locale locale = new Locale("EN");
        try {
            File tempFile = new File(this.nhibatFile.getParent(), "nhi.bat_temp");
            fileWriter = new FileWriter(tempFile);
            BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
            String PyExePath = "d:\\tools\\Anaconda\\python.exe";
			String nhiPyPath = "e:\\LHM\\configuratie_scripts\\svn948b\\nhi.py";
			String cfgDir = "e:\\Extern\\HKV\\runs\\control\\ontrol_NHI330_final_nieuwe_structuur_openDA";
			String runDir = "e:\\Extern\\HKV\\runs\\run_dir\\";

			String start = this.startDate.substring(0,8);//convertTimeToString(dates[0]).substring(0,8);
			String end = this.endDate.substring(0,8); //convertTimeToString(dates[1]).substring(0,8);

			String command = PyExePath + " " + nhiPyPath + " " + "-pre -cfgdir "+cfgDir+" -rundir "+runDir+" -comp mod sim -tstart "+ start+" -tend "+end+" -transient\n";
			outputFileBufferedWriter.write(command);
			outputFileBufferedWriter.newLine();
            outputFileBufferedWriter.close();

            // move temp.SWN to the actual input file:
            BBUtils.copyFile(tempFile,this.nhibatFile);
            tempFile.deleteOnExit();
        } catch (IOException e) {
            throw new RuntimeException("Could not write to " + this.nhibatFile.getAbsolutePath());
        }
    }

	public IPrevExchangeItem[] getExchangeItems(){
		return exchangeItems;
	}

	public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId(),exchangeItems[1].getId()};
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		//throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettings.getDataObjectExchangeItem(): Not implemented yet.");
        int indexExchangeItem;
        if (exchangeItemID.equals(lhmTimeSettingsId[0])) {
            indexExchangeItem = 0;
        } else if (exchangeItemID.equals(lhmTimeSettingsId[1])) {
            indexExchangeItem = 1;
        } else {
            throw new RuntimeException("unknown exchange item: " + exchangeItemID);
        }
        return exchangeItems[indexExchangeItem];
    }


	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        if (!exchangeItemID.equals(lhmTimeSettingsId)) {
            throw new RuntimeException("unknown exchange item: " + exchangeItemID);
        }
        return exchangeItems[0];
    }


    public void finish() {
        //System.out.println("Finish called.");
    	writeNBFile();
        //System.out.println("Files written.");
    }

	private void initialize(File workingDir, String nbFileName, String dateFileName, String[] arguments) {
		this.nhibatFile = new File(workingDir,nbFileName);
		this.dateFile = new File(workingDir,dateFileName);
		this.dateStrings = ReadDates(this.dateFile);
        exchangeItems = new LHMTimeSettingsExchangeItem[2];
		exchangeItems[0] = new LHMTimeSettingsExchangeItem(lhmTimeSettingsId[0],this);
        exchangeItems[1] = new LHMTimeSettingsExchangeItem(lhmTimeSettingsId[1],this);
	}

    public void initialize(File workingDir, String[] arguments) {
	    //System.out.println("Initializing timesettings...");
        String nbFileName = arguments[0];
		String dateFileName = arguments[1];
        String[] remainingArguments = new String[arguments.length-2];
        System.arraycopy(arguments, 2, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, nbFileName, dateFileName, remainingArguments);
    }
}



