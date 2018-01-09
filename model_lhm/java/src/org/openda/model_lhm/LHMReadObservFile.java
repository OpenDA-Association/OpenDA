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

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.*;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Class containing reading an obs file        */
public class LHMReadObservFile implements IDataObject, Serializable {
    private File lhmObsFile;
    private double[] time;
    private double[] obsVal;
    ArrayList<String> ids = new ArrayList<String>();
    LinkedHashMap<String, TimeSeries> series = new LinkedHashMap<String, TimeSeries>();

	// Read observation file and construct vectors of dates and values
    private void LHMReadFile(File lhmObsPSFile) {
    	int fileLength = checkLength(lhmObsFile);
        double[] obsVal = new double[fileLength];
        double[] time = new double[fileLength];
        System.out.println("Reading observations...");
        try {
            BufferedReader obsFileBufferedReader = new BufferedReader(new FileReader(lhmObsFile));
            String line = obsFileBufferedReader.readLine();
            int lCount=0;
            while (line != null) {
                line = obsFileBufferedReader.readLine();
                if(line==null){
                    break;
                }
                String[] lineFields = line.split(",");
                String timeStr = lineFields[0];
                time[lCount] = convertTimeFormat(timeStr);
                obsVal[lCount] = Double.parseDouble(lineFields[1]);
                System.out.println("Obs. at "+timeStr+"("+time[lCount]+"):"+obsVal[lCount]);
				lCount++;
            }
            obsFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read observation file " + lhmObsFile.getAbsolutePath());
        }
        this.obsVal = obsVal;
        this.time = time;
        TimeSeries s = new TimeSeries(time, obsVal);
        s.setId("soilmoisture");
        this.ids.add("soilmoisture");
        this.series.put("soilmoisture", s);
    }

    private int checkLength(File lhmObsFile){
        int lCount=0;
        try {
            BufferedReader obsFileBufferedReader = new BufferedReader(new FileReader(lhmObsFile));
            String line = obsFileBufferedReader.readLine();
            lCount = -1;
            while (line != null) {
                line = obsFileBufferedReader.readLine();
                lCount++;
            }
        } catch (IOException e) {
            throw new RuntimeException("Could not read observation file " + lhmObsFile.getAbsolutePath());
        }
        return(lCount);
    }

    private double convertTimeFormat(String timeStr){
         double mjdTime=0.0;
         GregorianCalendar cal = new GregorianCalendar();
         int year = Integer.parseInt(timeStr.substring(0,4));
		 int month = Integer.parseInt(timeStr.substring(5,6));
		 int day = Integer.parseInt(timeStr.substring(7,8));
		 int hour = Integer.parseInt(timeStr.substring(10,11));
         cal.set(Calendar.YEAR,year);
         cal.set(GregorianCalendar.DAY_OF_MONTH,day);
         cal.set(GregorianCalendar.MONTH,month-1);
         cal.set(GregorianCalendar.HOUR_OF_DAY,hour);
         cal.set(Calendar.MINUTE,0);
         cal.set(Calendar.SECOND,0);
         DateFormat df = new SimpleDateFormat("yyyyMMddHHmm");
         String timeLine = df.format(cal.getTime());
         try {
             mjdTime = TimeUtils.date2Mjd(timeLine);
         } catch (ParseException e) {
             throw new RuntimeException("startDay and startYear can not be parsed into a startTime in MJD-format.");
         }
		 //System.out.println("Time in OBSERVER: "+mjdTime+" / "+timeStr+" "+year+"-"+month+"-"+day+"/"+timeLine);
         return(mjdTime);
    }

	public void finish(){};

	private void initialize(File workingDir, String obsFileName) {
		this.lhmObsFile = new File(workingDir,obsFileName);
		LHMReadFile(this.lhmObsFile);
	}
    public double[] getTime(){
	    return(this.time);
    }
    public double[] getObsVal(){
        return(this.obsVal);
    }

	public void initialize(File workingDir, String[] arguments) {
		String obsFileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, obsFileName);
	}

    public String[] getExchangeItemIDs() {
        return this.ids.toArray(new String[this.ids.size()]);
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        if(role== IPrevExchangeItem.Role.Output){
            return this.ids.toArray(new String[this.ids.size()]);
        }else{
            return null;
        }
    }
    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        return this.series.get(exchangeItemID);
    }

}

