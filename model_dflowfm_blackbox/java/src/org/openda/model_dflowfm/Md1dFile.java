package org.openda.model_dflowfm;

import org.ini4j.Ini;
import org.ini4j.InvalidFileFormatException;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Created by prevel on 30-Nov-15.
 */
public class Md1dFile implements IDataObject
{
	private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	private static final String CATEGORY_TIME = "Time";
	private static final String PROPERTY_STARTTIME = "StartTime";
	private static final String PROPERTY_STOPTIME = "StopTime";
	private static final String PROPERTY_TIMESTEP = "TimeStep";
	private static final String PROPERTY_OUTPUT_TIMESTEP = "OutTimeStepGridPoints";

	private File workingDirectory;
	private String inputFileName = null;
	private String outputFileName = null;
	private Ini ini = new Ini();

	protected HashMap<String, IExchangeItem> exchangeItems;

	@Override
	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length < 2) throw new RuntimeException("Md1dFile DataObject must be initialised with 2 arguments: InputFilePath and OutputFilePath");
		this.inputFileName = arguments[0];
		this.outputFileName = arguments[1];
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
			Calendar startTime = new GregorianCalendar();
			startTime.setTime(DATE_FORMAT.parse(startTimeString));
			mjdStartTime = MjdUtils.ConvertDateTimeToModifiedJulianDay(startTime);
		}
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing DateTime value: %s", ex.getMessage(), startTimeString)); }

		double mjdStopTime;
		try
		{
			Calendar stopTime = new GregorianCalendar();
			stopTime.setTime(DATE_FORMAT.parse(stopTimeString));
			mjdStopTime = MjdUtils.ConvertDateTimeToModifiedJulianDay(stopTime);
		}
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_STARTTIME, stopTimeString)); }

		double timeStep;
		try { timeStep = Double.valueOf(timeStepString); }
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_STOPTIME, timeStepString)); }

		double outputTimeStep;
		try { outputTimeStep = Double.valueOf(outputTimeStepString); }
		catch(Exception ex) { throw new RuntimeException(String.format("%s, Error parsing %s value: %s", ex.getMessage(), PROPERTY_TIMESTEP, outputTimeStepString)); }

		exchangeItems = new HashMap<>();
		exchangeItems.put(PROPERTY_STARTTIME, new Md1dTimeInfoExchangeItem(PROPERTY_STARTTIME, mjdStartTime));
		exchangeItems.put(PROPERTY_STOPTIME, new Md1dTimeInfoExchangeItem(PROPERTY_STOPTIME, mjdStopTime));
		exchangeItems.put(PROPERTY_TIMESTEP, new Md1dTimeInfoExchangeItem(PROPERTY_TIMESTEP, timeStep));
		exchangeItems.put(PROPERTY_OUTPUT_TIMESTEP, new Md1dTimeInfoExchangeItem(PROPERTY_OUTPUT_TIMESTEP, outputTimeStep));
	}

	@Override
	public String[] getExchangeItemIDs()
	{
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role)
	{
		List<String> matchingExchangeItemIds = new ArrayList<>();
		for(IExchangeItem exchangeItem : exchangeItems.values())
			if(exchangeItem.getRole() == role) matchingExchangeItemIds.add(exchangeItem.getId());

		return matchingExchangeItemIds.toArray(new String[matchingExchangeItemIds.size()]);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) { return exchangeItems.get(exchangeItemID); }

	@Override
	public void finish()
	{
		IExchangeItem startTimeExchangeItem = exchangeItems.get(PROPERTY_STARTTIME);
		if(startTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_STARTTIME));
		double mjdStartTime = startTimeExchangeItem.getValuesAsDoubles()[0];
		Calendar startTime = MjdUtils.ConvertModifiedJulianDayToDateTime(mjdStartTime);
		String comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_STARTTIME));
		ini.put(CATEGORY_TIME, PROPERTY_STARTTIME, String.format("%s %s", DATE_FORMAT.format(startTime.getTime()), (comment == null ? "" : comment)));

		IExchangeItem stopTimeExchangeItem = exchangeItems.get(PROPERTY_STOPTIME);
		if(stopTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_STOPTIME));
		double mjdStopTime = stopTimeExchangeItem.getValuesAsDoubles()[0];
		Calendar stopTime = MjdUtils.ConvertModifiedJulianDayToDateTime(mjdStopTime);
		comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_STOPTIME));
		ini.put(CATEGORY_TIME, PROPERTY_STOPTIME, String.format("%s %s", DATE_FORMAT.format(stopTime.getTime()), (comment == null ? "" : comment)));

		IExchangeItem timeStepExchangeItem = exchangeItems.get(PROPERTY_TIMESTEP);
		if(timeStepExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_TIMESTEP));
		double timeStep = timeStepExchangeItem.getValuesAsDoubles()[0];
		comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_TIMESTEP));
		ini.put(CATEGORY_TIME, PROPERTY_TIMESTEP, String.format("%s %s", timeStep, (comment == null ? "" : comment)));

		IExchangeItem outTimeStepExchangeItem = exchangeItems.get(PROPERTY_OUTPUT_TIMESTEP);
		if(outTimeStepExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_OUTPUT_TIMESTEP));
		double outputTimeStep = outTimeStepExchangeItem.getValuesAsDoubles()[0];
		comment = retrieveTrailingComment(ini.get(CATEGORY_TIME, PROPERTY_OUTPUT_TIMESTEP));
		ini.put(CATEGORY_TIME, PROPERTY_OUTPUT_TIMESTEP, String.format("%s %s", outputTimeStep, (comment == null ? "" : comment)));

		File outputFile = new File(workingDirectory, outputFileName);
		try { ini.store(outputFile); }
		catch (IOException ex) { throw new RuntimeException(String.format("%s, Error writing to file: %s", ex.getMessage(), outputFile.getPath())); }
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
}
