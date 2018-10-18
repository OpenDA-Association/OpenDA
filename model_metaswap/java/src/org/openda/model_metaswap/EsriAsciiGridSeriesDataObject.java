package org.openda.model_metaswap;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.dataobjects.EsriAsciiGridDataObject;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Array;
import org.openda.utils.generalJavaUtils.StringUtilities;

import java.io.File;
import java.text.ParseException;

/**
 * Created by hummel on 2018-02-12.
 */
public class EsriAsciiGridSeriesDataObject implements IDataObject {

	private String outputFilePrefix = null;
	private String timeStampFormat = null;
	private String currentTime = null;
	private String targetTime = null;
	private int timeStepInDays = 1;
	String[] inputFileNames = null;
	String[] outputFileNames = null;
	private double[] allValues = null;
	private String exchangeItemId = "NOT-SET";
	private double[] timeStamps = null;
	private ArrayExchangeItem exchangeItem = null;
	private String currentTimeArgument = null;
	private String targetTimeArgument = null;
	int filesCount = -1;
	private File workingDir;

	public void initialize(File workingDir, String[] arguments) {
		// check argument

		String usageMessage = "EsriAsciiGridDataObject usage: inputFile [outputFile=...] [timeStamp=... timeStampFormat=...]," +
			" Files relative to working dir. If outputFile is ommitted, writing will be done to inputFile." +
			" If timeStampFormat is not specified, timeStamp contains the time stamp for which the " +
			" exchange item in the data object is valid." +
			" If timeStampFormat is specified, a file <inputFile>_<timeStamp>.asc is selected" +
			" (and <outputFile>_<timeStamp>.asc for output), with timestamp specified in" +
			" yyyyMMddHHmmss or yyyyMMdd format.";

		if (arguments == null || arguments.length < 3 || arguments.length > 5) {
			throw new RuntimeException(usageMessage);
		}

		this.workingDir = workingDir;

		for (int i = 1; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case "outputFilePrefix":
					outputFilePrefix = value;
					continue;
				case "timeStampFormat":
					timeStampFormat = value;
					continue;
				case "currentTime":
					currentTimeArgument = value;
					currentTime =	EsriAsciiGridDataObject.checkValidTimeString(value, null);
					continue;
				case "targetTime":
					targetTimeArgument = value;
					targetTime = EsriAsciiGridDataObject.checkValidTimeString(value, null);
					continue;
				case "timeStepInDays":
					timeStepInDays = Integer.parseInt(value);
					continue;
				default:
					throw new RuntimeException(usageMessage);
			}
		}


		String inputFileName = arguments[0];
		if (outputFilePrefix == null || timeStampFormat == null || currentTime == null || targetTime == null) {
			throw new RuntimeException(usageMessage);
		}

		double targetTimeAsMJD = 0;
		double currentTimeAsMJD = 0;
		try {
			targetTimeAsMJD = TimeUtils.date2Mjd(targetTime, "yyyyMMddHHmmss");
			currentTimeAsMJD = TimeUtils.date2Mjd(currentTime, "yyyyMMddHHmmss");
		} catch (ParseException e) {
			e.printStackTrace();
		}
		filesCount = (int) Math.floor((targetTimeAsMJD - currentTimeAsMJD) / timeStepInDays + 1);

		inputFileNames = new String[filesCount];
		outputFileNames = new String[filesCount];
		timeStamps = new double[filesCount];

		for (int i = 0; i < filesCount; i++) {
			String timeStampString = TimeUtils.mjdToString(currentTimeAsMJD+i*timeStepInDays, timeStampFormat);
			inputFileNames[i] = inputFileName + "_" + timeStampString + ".asc";
			if (outputFilePrefix == null) {
				outputFileNames[i] = inputFileNames[i];
			} else {
				outputFileNames[i] = outputFilePrefix + "_" + timeStampString + ".asc";
			}
		}

		IArrayGeometryInfo geometryInfo = null;
		for (int i = 0; i < filesCount; i++) {
			IDataObject timeStepDataObject = new EsriAsciiGridDataObject();
			timeStepDataObject.initialize(workingDir, new String[] {
				inputFileNames[i], "outputFile="+outputFileNames[i]});
			String[] exchangeItemIDs = timeStepDataObject.getExchangeItemIDs();
			IExchangeItem exchangeItem = timeStepDataObject.getDataObjectExchangeItem(exchangeItemIDs[0]);
			timeStamps[i] = currentTimeAsMJD + i * timeStepInDays;
			double[] exchangeItemValues = exchangeItem.getValuesAsDoubles();
			if (i == 0) {
				allValues = new double[exchangeItemValues.length*filesCount];
				exchangeItemId = exchangeItem.getId();
				geometryInfo = (IArrayGeometryInfo) exchangeItem.getGeometryInfo();
			}
			System.arraycopy(exchangeItemValues, 0 , allValues, i*exchangeItemValues.length, exchangeItemValues.length);
		}
		exchangeItem = new ArrayExchangeItem(exchangeItemId, IPrevExchangeItem.Role.InOut);
		exchangeItem.setGeometryInfo(geometryInfo);
		IArray array = new Array(allValues, new int[]{ filesCount,
			geometryInfo.getLongitudeArray().length(), geometryInfo.getLatitudeArray().length()},
			false);
		exchangeItem.setArray(array);
		exchangeItem.setTimeInfo(new TimeInfo(timeStamps));
	}

	public String[] getExchangeItemIDs() {
		return new String[] {exchangeItem.getId()};

	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItemID.equals(exchangeItem.getId())) {
			throw new RuntimeException("Unknown exchange item ID \"" + exchangeItemID + "\", expected " + exchangeItem.getId());
		}
		return exchangeItem;
	}

	public void finish() {
		double allValues[] = exchangeItem.getValuesAsDoubles();
		int timeStartIndex = 0;
		for (int i = 0; i < filesCount; i++) {
			IDataObject timeStepDataObject = new EsriAsciiGridDataObject();
			timeStepDataObject.initialize(workingDir, new String[] {
				inputFileNames[i], "outputFile="+outputFileNames[i]});
			String[] exchangeItemIDs = timeStepDataObject.getExchangeItemIDs();
			IExchangeItem exchangeItem = timeStepDataObject.getDataObjectExchangeItem(exchangeItemIDs[0]);
			double[] exchangeItemValues = exchangeItem.getValuesAsDoubles();
			int numEIvalues = exchangeItemValues.length;
			for (int j = 0; j < numEIvalues; j++) {
				if (exchangeItemValues[j] > 0) {
					exchangeItemValues[j] = allValues[timeStartIndex + j];
					if (exchangeItemValues[j] < 0) {
						exchangeItemValues[j] = 0d;
					}
				}
			}
			// System.arraycopy(exchangeItemValues, 0 , allValues, timeStartIndex, numEIvalues);
			exchangeItem.setValuesAsDoubles(exchangeItemValues);
			timeStartIndex += numEIvalues;
			if(i>0){ // skip first file
				timeStepDataObject.finish();
			}
		}
	}
}
