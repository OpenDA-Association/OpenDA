/* OpenDA v2.4.1 
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
package org.openda.model_delft3d;
import org.apache.commons.lang3.ArrayUtils;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.util.*;

/**
 * Created by Theo on 12.04.2016.
 */
public class D3dMdFileDataObject implements IDataObject {

	protected HashMap<String, IExchangeItem> exchangeItems = new HashMap<>();

	public static final String Stanton = "Stantn";
	public static final String Dalton = "Dalton";
	public static final String D_H = "Dicouv";
	public static final String D_Z = "Dicoww";

	static final String PROPERTY_STARTTIME = "Tstart";
	static final String PROPERTY_STOPTIME = "Tstop";
	static final String PROPERTY_INITDATE = "Itdate";
	static final String PROPERTY_TUNIT = "Tunit";

	public static final String[] fileKeys = {Stanton, Dalton, D_H, D_Z};
	public static final String[] timeKeys = {PROPERTY_STARTTIME, PROPERTY_STOPTIME, PROPERTY_INITDATE, PROPERTY_TUNIT};
	String[] allKeys = (String[]) ArrayUtils.addAll(fileKeys, timeKeys);

	private File mdFile;
	private String outputFileName = null;
	private File workingDir = null;
	private double mjdRefDate;
	private double mjdFactor;

	private static Map<String, Double> timeMap;
	static {
		timeMap = new HashMap<String, Double>();
		timeMap.put("S", 1.0 / (60.0*60.0*24.0) );
		timeMap.put("M", 1.0 / (60.0*24.0) );
		timeMap.put("H", 1.0 / 24.0 );
	}

	@java.lang.Override
	public String[] getExchangeItemIDs() {

		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@java.lang.Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {

		List<String> matchingExchangeItemIds = new ArrayList<>();
		for(IExchangeItem exchangeItem : exchangeItems.values())
			if(exchangeItem.getRole() == role) matchingExchangeItemIds.add(exchangeItem.getId());

		return matchingExchangeItemIds.toArray(new String[matchingExchangeItemIds.size()]);

	}

	@java.lang.Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {

		return exchangeItems.get(exchangeItemID);

	}

	@java.lang.Override
	public void initialize(File workingDir, String[] arguments) {

		if(arguments.length !=1 && arguments.length !=2) throw new RuntimeException("D3dMdFile DataObject must be initialised with 1 or 2 arguments: InputFilePath [OutputFilePath]");
		String inputFileName = arguments[0];
		this.outputFileName = inputFileName;
		if (arguments.length == 2) {
			this.outputFileName = arguments[1];
		}

		this.workingDir = workingDir;

		File mdFile = new File(workingDir, inputFileName);
		this.mdFile = mdFile;
		double StartTime;
		double StopTime;

		try {
			BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(mdFile));
			String line = inputFileBufferedReader.readLine();
			while (line != null) {
				String[] fields = line.split("= *");
				if (fields.length == 2) {
					String key = fields[0].trim();
					String value = fields[1].trim();
//					for (int i = 0; i < allKeys.length; i++) {
						if (Arrays.asList(fileKeys).contains(key)) {

							double valueAsDouble = Double.parseDouble(value);
							exchangeItems.put(key, new DoubleExchangeItem(key, IPrevExchangeItem.Role.InOut, valueAsDouble));

						}else if (Arrays.asList(timeKeys).contains(key)) {

							if (key.equalsIgnoreCase(PROPERTY_INITDATE)) {
								this.mjdRefDate = getReferenceDateInMjd(value);
							}else if(key.equalsIgnoreCase(PROPERTY_TUNIT)){
								this.mjdFactor = getTimeToMjdFactor(value);
							}else if(key.equalsIgnoreCase(PROPERTY_STARTTIME)){
								StartTime = getMjdFromDiff(Double.parseDouble(value));
								exchangeItems.put(PROPERTY_STARTTIME, new DoubleExchangeItem(PROPERTY_STARTTIME, IPrevExchangeItem.Role.InOut, StartTime));
							}else if(key.equalsIgnoreCase(PROPERTY_STOPTIME)){
								StopTime = getMjdFromDiff(Double.parseDouble(value));
								exchangeItems.put(PROPERTY_STOPTIME, new DoubleExchangeItem(PROPERTY_STOPTIME, IPrevExchangeItem.Role.InOut, StopTime));
							}

						}
//					}
				}
				line = inputFileBufferedReader.readLine();
			}
			inputFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not read from " + mdFile.getAbsolutePath());
		}

	}

	@java.lang.Override
	public void finish() {

		String[] exchangeItemIDs = getExchangeItemIDs();

		try {
			BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(mdFile));
			String line = inputFileBufferedReader.readLine();
			List<String> lines = new ArrayList<String>();
			while (line != null) {
				String[] fields = line.split("= *");
					for (int i = 0; i < exchangeItemIDs.length; i++) {
						String key = fields[0].trim();
						if (key.equalsIgnoreCase(exchangeItemIDs[i])) {

							IExchangeItem exchangeItem = getDataObjectExchangeItem(exchangeItemIDs[i]);
							double valueAsdouble = exchangeItem.getValuesAsDoubles()[0];

							// If exchangeItem is TStart or TStop (in mjd), convert back to difference since reference
							if (exchangeItemIDs[i]==PROPERTY_STARTTIME | exchangeItemIDs[i]==PROPERTY_STOPTIME){
								valueAsdouble = getDiffFromMjd(valueAsdouble);
							}

							String valueAsString = String.format(Locale.US, "%.7e",valueAsdouble);
							line = exchangeItemIDs[i] + " = " + valueAsString;
						}
					}

				lines.add(line);
				line = inputFileBufferedReader.readLine();
			}

			inputFileBufferedReader.close();

			FileWriter outputFile = new FileWriter(new File(workingDir, outputFileName));
			for (String outputLine :lines) {
				outputFile.write(outputLine + "\n");
			}
			outputFile.close();

		}	catch (FileNotFoundException e1) {
			e1.printStackTrace();
		} catch (IOException e1) {
			e1.printStackTrace();
		}

	}

	public Double getReferenceDateInMjd(String dateString) {
		// get reference date for future use
		dateString = dateString.replace("#","");
		dateString = dateString.replace("-","");
		Double dateInMjd;
		try {
			dateInMjd = TimeUtils.date2Mjd(dateString + "0000" );
		}
		catch (Exception e)  {
			throw new RuntimeException("Error parsing reference date");
		}
		return dateInMjd;
	}

	public Double getTimeToMjdFactor(String timeUnit) {
		timeUnit = timeUnit.replace("#","");
		Double factor = timeMap.get(timeUnit);
		if (factor == null) {
			throw new RuntimeException("Incorrect Tunit in MDF-file: " + timeUnit);
		}
		return factor;
	}

	public double getMjdFromDiff(double TimeDiff) {
		double TimeMjd;
		TimeMjd = TimeDiff * this.mjdFactor;
		TimeMjd = TimeMjd + this.mjdRefDate;
		return TimeMjd;
	}

	public double getDiffFromMjd(double TimeMjd) {
		double TimeDiff;
		TimeDiff = TimeMjd - this.mjdRefDate;
		TimeDiff = TimeDiff / this.mjdFactor;
		return TimeDiff;
	}
//	public double getTStartSimulationInMjd() {
//		double TStartMjd;
//		TStartMjd = this.StartTime * this.mjdFactor;
//		TStartMjd = TStartMjd +  this.mjdRefDate;
//		return TStartMjd;
//	}
//
//	public double getTStopSimulationInMjd() {
//		double TStopMjd;
//		TStopMjd = this.StopTime * this.mjdFactor;
//		TStopMjd = TStopMjd +  this.mjdRefDate;
//		return TStopMjd;
//	}
//
//	public double getTStopSimulationInit(double StopMjd) {
//		double TStopInit;
//		TStopInit = StopMjd - this.mjdRefDate;
//		TStopInit = TStopInit /  this.mjdFactor;
//		return TStopInit;
//	}
//
}
