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

package org.openda.model_swan;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITimeInfo;

import java.io.*;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Reading and writing SWAN open boundary file in TPAR format.
 * Changes to Hs also lead to corrections of the period to preserve steepness. Tp_new = Tp * sqrt(HS_new/Hs)
 */
public class SwanOpenBoundaryTPARFile implements IDataObject, ITimeInfo {
	private IExchangeItem[] exchangeItems;
	private File openBoundaryFile;
	private String[] swanOpenBoundaryTPARFileId = new String[]{"Hs","period","peakdir","dirspread"};

	// actual data
	private int nTimeLevel;
	private String[] time=null;
	private double[] Hs=null;
	private double[] period=null;
	private double[] peakDirection=null;
	private double[] directSpread=null;
	private final double Hs_min=0.01;
	private final double period_min=0.01;
	public boolean periodCorrection=true;

	private void initialize(File workingDir, String fileName, String[] arguments) {
		this.openBoundaryFile = new File(workingDir, fileName);
		readOpenBoundaryFile();
		exchangeItems = new SwanOpenBoundaryTPARFileExchangeItem[4];
		exchangeItems[0] = new SwanOpenBoundaryTPARFileExchangeItem(swanOpenBoundaryTPARFileId[0],this);
		exchangeItems[1] = new SwanOpenBoundaryTPARFileExchangeItem(swanOpenBoundaryTPARFileId[1],this);
		exchangeItems[2] = new SwanOpenBoundaryTPARFileExchangeItem(swanOpenBoundaryTPARFileId[2],this);
		exchangeItems[3] = new SwanOpenBoundaryTPARFileExchangeItem(swanOpenBoundaryTPARFileId[3],this);
	}

	protected String[] getTimeStr(){
		return this.time;
	}

	protected void setTime(String[] time){
		this.nTimeLevel=time.length;
		this.time = new String[time.length];
		for(int i=0;i<time.length;i++){
			this.time[i]=time[i];
		}
	}

	protected double[] getTimeDbl(){
		String[] timeStr = getTimeStr();
		double[] timeDbl = new double[timeStr.length];
		for (int i=0;i<timeStr.length;i++){
			String timeLine = timeStr[i];
			timeLine = timeLine.replace(".","");
			try {
				timeDbl[i] = TimeUtils.date2Mjd(timeLine);
			} catch (ParseException e) {
				throw new RuntimeException("org.openda.model_swan.SwanOpenBoundaryTPARFile.getTimes(): could not parse timeLine");
			}
		}
		return timeDbl;
	}

	protected double[] getHs(){
		double[] Hs = new double[this.Hs.length];
		System.arraycopy(this.Hs,0,Hs,0,Hs.length);
		return Hs;
	}

	protected void setHs(double[] Hs){
		if(this.Hs==null){
			this.Hs=new double[Hs.length];
			System.arraycopy(Hs,0,this.Hs,0,Hs.length);
		}else{
			for(int i=0;i<Hs.length;i++){
				if(Hs[i]>Hs_min){
					double period=this.period[i];
					double ratio=Hs[i]/this.Hs[i];
					// correction to preserve steepness
					if(this.periodCorrection){
						this.period[i]=Math.sqrt(ratio)*period;
					}
					this.Hs[i]=Hs[i];
				}else{
					this.Hs[i]=Hs_min;
				}
			}
		}
	}

	protected void multiplyHs(double[] alpha){
		double[] newHs = getHs();
		for (int i=0;i<newHs.length;i++){
			newHs[i] = alpha[0]*newHs[i];
		}
		setHs(newHs);
	}

	protected void axpyOnHs(double alpha, double[] axpyValues){
		double[] newHs = getHs();
		for (int i=0;i<newHs.length;i++){
			newHs[i] = alpha*axpyValues[i] + newHs[i];
		}
		setHs(newHs);
	}

	protected double[] getPeriod(){
		double[] period = new double[this.period.length];
		System.arraycopy(this.period,0,period,0,period.length);
		return period;
	}

	protected void setPeriod(double[] period){
		if(this.period==null){
			this.period=new double[period.length];
			System.arraycopy(period,0,this.period,0,period.length);
		}else{
			for(int i=0;i<period.length;i++){
				if(period[i]>period_min){
					this.period[i]=period[i];
				}else{
					this.period[i]=period_min;
				}
			}
		}
	}

	protected void multiplyPeriod(double[] alpha){
		double[] newPeriod = getPeriod();
		for (int i=0;i<newPeriod.length;i++){
			newPeriod[i] = alpha[0]*newPeriod[i];
		}
		setPeriod(newPeriod);
	}

	protected void axpyOnPeriod(double alpha, double[] axpyValues){
		double[] newPeriod = getPeriod();
		for (int i=0;i<newPeriod.length;i++){
			newPeriod[i] = alpha*axpyValues[i] + newPeriod[i];
		}
		setPeriod(newPeriod);
	}

	protected double[] getPeakDirection(){
		double[] peakDirection = new double[this.peakDirection.length];
		System.arraycopy(this.peakDirection,0,peakDirection,0,peakDirection.length);
		return peakDirection;
	}

	protected void setPeakDirection(double[] peakDirection){
		if(this.peakDirection==null){
			this.peakDirection=new double[peakDirection.length];
		}
		System.arraycopy(peakDirection,0,this.peakDirection,0,peakDirection.length);
	}

	protected void multiplyPeakDirection(double[] alpha){
		double[] newPeakDir = getPeakDirection();
		for (int i=0;i<newPeakDir.length;i++){
			newPeakDir[i] = alpha[0]*newPeakDir[i];
		}
		setPeakDirection(newPeakDir);
	}

	protected void axpyOnPeakDirection(double alpha, double[] axpyValues){
		double[] newPeakDir = getPeakDirection();
		for (int i=0;i<newPeakDir.length;i++){
			newPeakDir[i] = alpha*axpyValues[i] + newPeakDir[i];
		}
		setPeakDirection(newPeakDir);
	}

	protected double[] getDirectSpread(){
		double[] directSpread = new double[this.directSpread.length];
		System.arraycopy(this.directSpread,0,directSpread,0,directSpread.length);
		return directSpread;
	}

	protected void setDirectSpread(double[] directSpread){
		if(this.directSpread==null){
			this.directSpread=new double[directSpread.length];
		}
		System.arraycopy(directSpread,0,this.directSpread,0,directSpread.length);
	}

	protected void multiplyDirectSpread(double[] alpha){
		double[] newDirSpread = getDirectSpread();
		for (int i=0;i<newDirSpread.length;i++){
			newDirSpread[i] = alpha[0]*newDirSpread[i];
		}
		setDirectSpread(newDirSpread);
	}

	protected void axpyOnDirectSpread(double alpha, double[] axpyValues){
		double[] newDirSpread = getDirectSpread();
		for (int i=0;i<newDirSpread.length;i++){
			newDirSpread[i] = alpha*axpyValues[i] + newDirSpread[i];
		}
		setDirectSpread(newDirSpread);
	}

	private void writeOpenBoundaryFile(){
		Locale locale = new Locale("EN");
		String openBoundaryFormat = "%8.2f";
		FileWriter fileWriter;
		try {
			File tempFile = new File(this.openBoundaryFile.getParent(), "temp.RVW");
			fileWriter = new FileWriter(tempFile);
			BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

			outputFileBufferedWriter.write("TPAR");
			outputFileBufferedWriter.newLine();
			for (int i=0;i<this.nTimeLevel;i++) {
				//TO DO: it appears that for windows version SWAN swan_4081_del_w32_i11_omp.exe,
				//it requires 'space' before the time column at each row. We should check how it is
				//under linux.
				outputFileBufferedWriter.write(" "+this.time[i]);
				outputFileBufferedWriter.write(String.format(locale, openBoundaryFormat,this.Hs[i]));
				outputFileBufferedWriter.write(String.format(locale, openBoundaryFormat,this.period[i]));
				outputFileBufferedWriter.write(String.format(locale, openBoundaryFormat,this.peakDirection[i]));
				outputFileBufferedWriter.write(String.format(locale, openBoundaryFormat,this.directSpread[i]));
				outputFileBufferedWriter.newLine();
			}
			outputFileBufferedWriter.close();
			// move temp.RVW to the actual restart file:
			BBUtils.copyFile(tempFile,this.openBoundaryFile);
			tempFile.deleteOnExit();
		} catch (IOException e) {
			throw new RuntimeException("Could not write to " + this.openBoundaryFile.getAbsolutePath());
		}
	}

	private void readOpenBoundaryFile(){
		ArrayList<String> lines = new ArrayList<String>();
		String lineFields[];
		int nTimeLevel = 0;
		try {
			BufferedReader boundFileBufferedReader = new BufferedReader(new FileReader(this.openBoundaryFile));
			String line = boundFileBufferedReader.readLine();
			if (!line.toUpperCase().contains("TPAR")) {
				throw new RuntimeException("Open-boundary file is not an TPAR file. Other types of boundary file are not yet implemented.");
			} else {
				line = boundFileBufferedReader.readLine();
			}
			while (line != null) {
				nTimeLevel++;
				lines.add(line);
				line = boundFileBufferedReader.readLine();
			}
			boundFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not read swan openboundaryTPAR file " + this.openBoundaryFile.getAbsolutePath());
		}

		String[] time = new String[nTimeLevel];
		double[] Hs = new double[nTimeLevel];
		double[] period = new double[nTimeLevel];
		double[] peakDirection = new double[nTimeLevel];
		double[] directSpread = new double[nTimeLevel];
		String line;
		for (int i=0;i<nTimeLevel;i++){
			line = lines.get(i);
			lineFields = line.trim().split("[ \t]+");
			time[i] = lineFields[0];
			Hs[i] = Double.parseDouble(lineFields[1]);
			period[i] = Double.parseDouble(lineFields[2]);
			peakDirection[i] = Double.parseDouble(lineFields[3]);
			directSpread[i] = Double.parseDouble(lineFields[4]);
		}
		setTime(time);
		setHs(Hs);
		setPeriod(period);
		setDirectSpread(directSpread);
		setPeakDirection(peakDirection);
		lines.clear();
	}

	public String[] getExchangeItemIDs() {
		return new String[] {exchangeItems[0].getId(),exchangeItems[1].getId(),exchangeItems[2].getId(),exchangeItems[3].getId()};
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int indexExchangeItem;
		if (exchangeItemID.equals(swanOpenBoundaryTPARFileId[0])) {
			indexExchangeItem = 0;
		} else if (exchangeItemID.equals(swanOpenBoundaryTPARFileId[1])) {
			indexExchangeItem = 1;
		} else if (exchangeItemID.equals(swanOpenBoundaryTPARFileId[2])) {
			indexExchangeItem = 2;
		} else if (exchangeItemID.equals(swanOpenBoundaryTPARFileId[3])) {
			indexExchangeItem = 3;
		} else {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		}
		return exchangeItems[indexExchangeItem];
	}

	public void finish() {
		writeOpenBoundaryFile();
		this.exchangeItems=null;
		this.nTimeLevel=0;
		this.Hs=null;
		this.period=null;
		this.peakDirection=null;
		this.directSpread=null;
		this.time=null;
	}

	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}

	
	public double[] getTimes() {
		return getTimeDbl();
	}

	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFile.setTimes(): Not implemented yet.");
	}

}
