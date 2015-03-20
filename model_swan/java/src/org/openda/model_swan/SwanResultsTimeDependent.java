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

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

import java.io.*;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.LinkedHashMap;

/**
 * Class containing the quantities, locations and value a swan results or observations file
 */
public class SwanResultsTimeDependent implements IDataObject, Serializable {

	LinkedHashMap<String, TimeSeries> series = new LinkedHashMap<String, TimeSeries>();
	ArrayList<String> ids = new ArrayList<String>();
	ArrayList<String> quantityIds = new ArrayList<String>();
	ArrayList<String> locationIds = new ArrayList<String>();

	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}

	private void initialize(File workingDir, String fileName, String[] arguments) {
		// Open the observations file for reading
		File observationsFile = new File(workingDir, fileName);
		if (!observationsFile.exists()) {
			// Specified observation file does not exist. It could be a nested run
			//composeObservationsForNestedRun(observationsFile);
		}
		readSwanResultsFile(observationsFile);
	}

	private void readSwanResultsFile(File swanResultsFile) {

		// Open the swanResults file for reading

		FileReader resultsFileReader;
		try {
			resultsFileReader = new FileReader(swanResultsFile);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("File " + swanResultsFile.getAbsolutePath() + " does not exist");
		}
		BufferedReader resultsFileBufferedReader = new BufferedReader(resultsFileReader);

		ArrayList<String> allIds = new ArrayList<String>();
		ArrayList<Integer> quantityColumn = new ArrayList<Integer>();
		int xColumn=-1;
		int yColumn=-1;
		int tColumn=-1;

		// read and parse header
		String line;
		try {
			line = resultsFileBufferedReader.readLine().trim();
			while (line != null && quantityIds.size() == 0) {
				line = line.trim();
				if (line.startsWith("%") && line.length() > 1) {
					String quantsLine = line.substring(1).trim();
					String[] timeKeyWord = new String[]{"Tsec","Time"};
					if (quantsLine.contains(timeKeyWord[0]) || quantsLine.contains(timeKeyWord[1])) {
						//expecting a Header line like Time Xp Yp Hsig ...
						String[] quantStrings = quantsLine.split("[ \t]+");
						String xPositionKeyWord = "Xp";
						String yPositionKeyWord = "Yp";
						for (int i = 0; i < quantStrings.length; i++) {
							String id=quantStrings[i];
							allIds.add(id);
							if(id.equalsIgnoreCase(xPositionKeyWord)){
								xColumn=i;
							}else if(id.equalsIgnoreCase(yPositionKeyWord)){
								yColumn=i;
							}else if(id.equalsIgnoreCase(timeKeyWord[0])||id.equalsIgnoreCase(timeKeyWord[1])){
								tColumn=i;
							}else{
								quantityIds.add(id);
								quantityColumn.add(i);
							}
						}
						if(!allIds.contains(xPositionKeyWord)){
							throw new RuntimeException("Invalid header line, expecting at least Xp: " + line);
						}
					}
				}
				line = resultsFileBufferedReader.readLine();
			}
		} catch (IOException e) {
			throw new RuntimeException("Could not read quantities from swan file " +
					swanResultsFile.getAbsolutePath() + " (error:" + e.getMessage() + ")");
		}
		// additional checks
		if(yColumn<0){
			Results.putMessage("Reading SWAN results ascii table: no y-position found, assuming 1D computation.");
		}
		if(tColumn<0){
			throw new RuntimeException("Reading SWAN results ascii table. No time colunmn found.");
		}
		int quantityCount = quantityIds.size();
		if (quantityCount == 0) {
			throw new RuntimeException("No quantities available in swan file " +
					swanResultsFile.getAbsolutePath() + ")");
		}

		// read first time block, i.e. lines until EOF or next time
		int columnCount=allIds.size();
		ArrayList<Double>[] values=null;
		ArrayList<Double> times=new ArrayList<Double>();
		try {
			boolean done=false;
			ArrayList<String> lines=new ArrayList<String>();
			String firstTime=null;
			line = resultsFileBufferedReader.readLine().trim();
			if(line==null){
				throw new RuntimeException("Unexpected end of file for "+swanResultsFile.getName()+".");
			}
			while (done==false) {
				if (!line.startsWith("%") && line.length() > 1) {
					line = line.trim();
					String[] fields = line.split("[ \t]+");
					if(fields.length!=columnCount){
						throw new RuntimeException("Unexpected #values (" + (fields.length) +
								") in swan file (#columns.: " + columnCount + ")\n"+line);
					}
					String time=fields[tColumn];
					if(firstTime==null){
						firstTime=time;
						lines.add(line);
						line = resultsFileBufferedReader.readLine();
					}else if(time.equals(firstTime)){
						//2nd to last line of first block
						lines.add(line);
						line = resultsFileBufferedReader.readLine();
					}else{
						// end of block
						done=true;                		
					}
				}else{ //comment
					line = resultsFileBufferedReader.readLine();                	
				}
				if(line==null){
					//end of file i.e. only one time block
					done=true;
				}
			}
			// analyse first block
			int nLocs=lines.size();
			int nQuants=quantityIds.size();
			double xValues[] = new double[nLocs];
			double yValues[] = new double[nLocs];
			values=new ArrayList[nLocs*nQuants]; // order quants fastest as in file
			for(int iLoc=0;iLoc<nLocs;iLoc++){
				String l=lines.get(iLoc);
				l = l.trim();
				String[] fields = l.split("[ \t]+");
				// special fields
				// x
				double x=0.;
				String xString="0.";
				if(xColumn>=0){
					xString=fields[xColumn];
					x=Double.valueOf(xString);
					xValues[iLoc]=x;
				}
				// y
				double y=0.;
				String yString="0.";
				if(yColumn>=0){
					yString=fields[yColumn];
					y=Double.valueOf(yString);
					yValues[iLoc]=y;
				}
				// time
				double t=0.;
				String tString="";
				if(tColumn>=0){
					tString=fields[tColumn];
					try {
						tString = tString.replace(".","");
						t = TimeUtils.date2Mjd(tString);
						if(times.size()==0){
							times.add(t);
						}
					} catch (ParseException e) {
						throw new RuntimeException("Time of observed data can not be parsed: "+tString);
					}
				}
				String locationId=xString+","+yString;
				this.locationIds.add(locationId);
				//values
				for(int iQuant=0;iQuant<nQuants;iQuant++){
					values[iLoc*nQuants+iQuant]=new ArrayList<Double>();
					double value=Double.parseDouble(fields[quantityColumn.get(iQuant)]);
					values[iLoc*nQuants+iQuant].add(value);
					String id=quantityIds.get(iQuant)+" @ "+locationId;
					//System.out.println("id="+id);
					this.ids.add(id);
				}
			}
			//read remainder of data
			String previousTime=firstTime;
			done=false;
			if(line==null){
				done=true;
			}
			int iLoc=0;
			while (done==false) {
				if (!line.startsWith("%") && line.length() > 1) {
					line = line.trim();
					String[] fields = line.split("[ \t]+");
					if(fields.length!=columnCount){
						throw new RuntimeException("Unexpected #values (" + (fields.length) +
								") in swan file (#columns.: " + columnCount + ")\n"+line);
					}
					// time
					double t=0.;
					String tString="0.";
					if(tColumn>=0){
						tString=fields[tColumn];
						try {
							tString = tString.replace(".","");
							t = TimeUtils.date2Mjd(tString);
						} catch (ParseException e) {
							throw new RuntimeException("Time of observed data can not be parsed: "+tString);
						}
					}
					if(!tString.equals(previousTime)){
						//1st line of new time block
						times.add(t);
						iLoc=0;
						previousTime=tString;
					}else{
						iLoc++;
					}
					//values
					for(int iQuant=0;iQuant<nQuants;iQuant++){
						double value=Double.parseDouble(fields[quantityColumn.get(iQuant)]);
						values[iLoc*nQuants+iQuant].add(value);
					}
					line = resultsFileBufferedReader.readLine();
				}else{ //comment
					line = resultsFileBufferedReader.readLine();                	
				}
				if(line==null){
					//end of file i.e. only one time block
					done=true;
				}
			}
			//put data into items
			for(iLoc=0;iLoc<nLocs;iLoc++){
				double x=xValues[iLoc];
				double y=yValues[iLoc];
				String location=this.locationIds.get(iLoc);
				String source="SWAN table file";
				for(int iQuant=0;iQuant<nQuants;iQuant++){
					String quantity=this.quantityIds.get(iQuant);
					String unit="TODO";
					IPrevExchangeItem.Role role=IPrevExchangeItem.Role.Output;
					double seriesTimes[]=new double[times.size()];
					for(int i=0;i<times.size();i++){seriesTimes[i]=times.get(i);}
					ArrayList<Double> thisSeries=values[iLoc*nQuants+iQuant];
					int m=thisSeries.size();
					double seriesValues[]=new double[m];
					for(int i=0;i<m;i++){seriesValues[i]=thisSeries.get(i);}
					String id=quantity+" @ "+location;
					TimeSeries s = new TimeSeries(seriesTimes, seriesValues, x, y, source, quantity, unit, location, role);
					s.setId(id);
					this.series.put(id, s);
				}
			}
			resultsFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Trouble reading from swan output table file " +
					swanResultsFile.getAbsolutePath() + " (error:" + e.getMessage() + ")");
		}
	}


	//    public static void composeObservationsForNestedRun(File observationsFile) {
	//
	//        final String locTabString = "_loc.tab";
	//        String baseFileName = observationsFile.getName();
	//        int locTabPos = baseFileName.indexOf(locTabString);
	//        if (locTabPos < 0) return;
	//        baseFileName = baseFileName.substring(0, locTabPos);
	//
	//        File observationsDir = observationsFile.getParentFile();
	//        Locale locale = new Locale("EN");
	//        ArrayList<File> numberedFiles = new ArrayList<File>();
	//        for (int i = 1; i < 10; i++) {
	//            File numberedFile = new File(observationsDir, baseFileName + "_" +  String.format(locale, "%02d", i) + locTabString);
	//            if (numberedFile.exists()) {
	//                numberedFiles.add(numberedFile);
	//            }
	//        }
	//        if (numberedFiles.size() > 0) {
	//            File currentNumberedFile = null;
	//            try {
	//                BufferedWriter writer = new BufferedWriter(new FileWriter(observationsFile));
	//                boolean writeHeader = true;
	//                for (File numberedFile : numberedFiles) {
	//
	//                    currentNumberedFile = numberedFile;
	//                    BufferedReader reader = new BufferedReader(new FileReader(numberedFile));
	//
	//                    String lineFromNumberFile = reader.readLine();
	//                    while (lineFromNumberFile !=null) {
	//                        if (lineFromNumberFile.trim().startsWith("%")) {
	//                            if (writeHeader) {
	//                                writer.write(lineFromNumberFile);
	//                                writer.newLine();
	//                            }
	//                        } else {
	//                            writer.write(lineFromNumberFile);
	//                            writer.newLine();
	//                        }
	//                        lineFromNumberFile = reader.readLine();
	//                    }
	//                    writeHeader = false;
	//                }
	//                currentNumberedFile = null;
	//                writer.close();
	//            } catch (IOException e) {
	//                throw new RuntimeException("Error composing nested observation file " +
	//                        observationsFile.getAbsolutePath() +
	//                        (currentNumberedFile == null ? "" : " when reading from " + currentNumberedFile.getAbsolutePath()) +
	//                        ", error: " + e.getMessage());
	//            }
	//        }
	//    }

	@Override
	public String[] getExchangeItemIDs() {
		return this.ids.toArray(new String[this.ids.size()]);
	}

	@Override
	public String[] getExchangeItemIDs(Role role) {
		if(role==Role.Output){
			return this.ids.toArray(new String[this.ids.size()]);
		}else{
			return null;
		}
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return this.series.get(exchangeItemID);
	}

	@Override
	public void finish() {
		// TODO Nothing todo since we cannot write for now
	}
	
	public String toString(){
		String result="SwanResultsTimeDependent{\n";
		for(String id: this.ids){
			result+=this.series.get(id).toString()+"\n";
		}
		result+="}";
		return result;
	}

}
