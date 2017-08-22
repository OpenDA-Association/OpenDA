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
package org.openda.model_dflowfm;
import org.ini4j.Ini;
import org.ini4j.InvalidFileFormatException;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Data object implementation for FLOW 1D's md1d-file (needed to get and adjust time frame)
 */
public class Md1dFile implements IDataObject
{
	static final String PROPERTY_STARTTIME = "StartTime";
	static final String PROPERTY_STOPTIME = "StopTime";
	static final String PROPERTY_TIMESTEP = "TimeStep";

	private static final String CATEGORY_TIME = "Time";
	private static final String PROPERTY_OUTPUT_TIMESTEP = "OutTimeStepGridPoints";

	private File workingDirectory;
	private String outputFileName = null;
	private Ini ini = new Ini();

	protected HashMap<String, IExchangeItem> exchangeItems;

	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length !=1 && arguments.length !=2) throw new RuntimeException("Md1dFile DataObject must be initialised with 1 or 2 arguments: InputFilePath [OutputFilePath]");
		String inputFileName = arguments[0];
		this.outputFileName = inputFileName;
		if (arguments.length == 2) {
			this.outputFileName = arguments[1];
		}

		this.workingDirectory = workingDirectory;

		File inputFile = new File(workingDirectory, inputFileName);

		try	{ ini.load(inputFile); }
		catch (InvalidFileFormatException e) { throw new RuntimeException("Invalid Formatting in '" + inputFile.getAbsolutePath() + "'.", e); }
		catch (IOException ex) { throw new RuntimeException(String.format("%s, File does not exist: %s", ex.getMessage(), inputFile.getPath())); }

		String startTimeString = ini.get(CATEGORY_TIME, PROPERTY_STARTTIME);
		if(startTimeString == null) throw new RuntimeException(String.format("Error parsing %s, property not found in file: %s", PROPERTY_STARTTIME, inputFile.getPath()));
		else startTimeString = removeTrailingComment(startTimeString);

		String stopTimeString = ini.get(CATEGORY_TIME, PROPERTY_STOPTIME);
		if(stopTimeString == null) throw new RuntimeException(String.format("Error parsing %s, property not found in file: %s", PROPERTY_STOPTIME, inputFile.getPath()));
		else stopTimeString = removeTrailingComment(stopTimeString);

		String timeStepString = ini.get(CATEGORY_TIME, PROPERTY_TIMESTEP);
		if(timeStepString == null) throw new RuntimeException(String.format("Error parsing %s, property not found in file: %s", PROPERTY_TIMESTEP, inputFile.getPath()));
		else timeStepString = removeTrailingComment(timeStepString);

		String outputTimeStepString = ini.get(CATEGORY_TIME, PROPERTY_OUTPUT_TIMESTEP);
		if(outputTimeStepString == null) throw new RuntimeException(String.format("Error parsing %s, property not found in file: %s", PROPERTY_OUTPUT_TIMESTEP, inputFile.getPath()));
		else outputTimeStepString = removeTrailingComment(outputTimeStepString);

		double mjdStartTime;
		try
		{
			mjdStartTime = TimeUtils.date2Mjd(startTimeString);
		}
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_STARTTIME, startTimeString)); }

		double mjdStopTime;
		try
		{
			mjdStopTime = TimeUtils.date2Mjd(stopTimeString);
		}
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_STOPTIME, stopTimeString)); }

		double timeStep;
		try	{ timeStep = convertSecondsToDays(Double.valueOf(timeStepString)); }
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_TIMESTEP, timeStepString)); }

		double outputTimeStep;
		try { outputTimeStep = convertSecondsToDays(Double.valueOf(outputTimeStepString)); }
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_OUTPUT_TIMESTEP, outputTimeStepString)); }

		exchangeItems = new HashMap<>();
		exchangeItems.put(PROPERTY_STARTTIME, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.StartTime, mjdStartTime));
		exchangeItems.put(PROPERTY_STOPTIME, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.StopTime, mjdStopTime));
		exchangeItems.put(PROPERTY_TIMESTEP, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.TimeStep, timeStep));
		exchangeItems.put(PROPERTY_OUTPUT_TIMESTEP, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.OutputTimeStep, outputTimeStep));
	}

	public String[] getExchangeItemIDs()
	{
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role)
	{
		List<String> matchingExchangeItemIds = new ArrayList<>();
		for(IExchangeItem exchangeItem : exchangeItems.values())
			if(exchangeItem.getRole() == role) matchingExchangeItemIds.add(exchangeItem.getId());

		return matchingExchangeItemIds.toArray(new String[matchingExchangeItemIds.size()]);
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) { return exchangeItems.get(exchangeItemID); }

	public void finish()
	{
		IExchangeItem startTimeExchangeItem = exchangeItems.get(PROPERTY_STARTTIME);
		if(startTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_STARTTIME));
		double mjdStartTime = startTimeExchangeItem.getValuesAsDoubles()[0];
		String comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_STARTTIME));
		ini.put(CATEGORY_TIME, PROPERTY_STARTTIME, String.format("%s %s", TimeUtils.mjdToString(mjdStartTime, "yyyy-MM-dd HH:mm:ss"), comment));

		IExchangeItem stopTimeExchangeItem = exchangeItems.get(PROPERTY_STOPTIME);
		if(stopTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_STOPTIME));
		double mjdStopTime = stopTimeExchangeItem.getValuesAsDoubles()[0];
		comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_STOPTIME));
		ini.put(CATEGORY_TIME, PROPERTY_STOPTIME, String.format("%s %s", TimeUtils.mjdToString(mjdStopTime, "yyyy-MM-dd HH:mm:ss"), comment));

		IExchangeItem timeStepExchangeItem = exchangeItems.get(PROPERTY_TIMESTEP);
		if(timeStepExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_TIMESTEP));
		double timeStep = convertDaysToSeconds(timeStepExchangeItem.getValuesAsDoubles()[0]);
		comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_TIMESTEP));
		ini.put(CATEGORY_TIME, PROPERTY_TIMESTEP, String.format("%s %s", timeStep, comment));

		IExchangeItem outTimeStepExchangeItem = exchangeItems.get(PROPERTY_OUTPUT_TIMESTEP);
		if(outTimeStepExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_OUTPUT_TIMESTEP));
		double outputTimeStep = convertDaysToSeconds(outTimeStepExchangeItem.getValuesAsDoubles()[0]);
		comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_OUTPUT_TIMESTEP));
		ini.put(CATEGORY_TIME, PROPERTY_OUTPUT_TIMESTEP, String.format("%s %s", outputTimeStep, comment));

		File outputFile = new File(workingDirectory, outputFileName);
		try { ini.store(outputFile); }
		catch (IOException ex) { throw new RuntimeException(String.format("%s, Error writing to file: %s", ex.getMessage(), outputFile.getPath())); }
	}

	HashMap<String, IExchangeItem> getExchangeItems() {
		return exchangeItems;
	}

	private String removeTrailingComment(String originalString)
	{
		if(originalString == null) return null;
		if(originalString.contains("#")) originalString = originalString.substring(0, originalString.indexOf("#"));
		return originalString.trim();
	}

	private String retrieveTrailingComment(String originalString)
	{
		if(originalString != null && originalString.contains("#"))
			return originalString.substring(originalString.indexOf("#"), originalString.length()).trim();
		return "";
	}

	private double convertSecondsToDays(double seconds)	{ return seconds / 60 / 60 / 24; }
	private double convertDaysToSeconds(double days) { return days * 24 * 60 * 60; }
}
