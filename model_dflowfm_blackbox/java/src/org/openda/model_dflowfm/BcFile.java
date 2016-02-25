package org.openda.model_dflowfm;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Time;

import java.io.*;
import java.text.ParseException;
import java.util.*;

/**
 * IDataObject for Flow-1D's Boundary Condition file
 */
public class BcFile implements IDataObject
{
	private static final String EXCHANGE_ITEM_NAME_SEPARATOR = ".";
	private File workingDirectory;

	private static final String TIME_UNIT_SECONDS = "seconds since";
	private static final String TIME_UNIT_MINUTES = "minutes since";
	private static final String TIME_UNIT_HOURS = "hours since";

	private static final long ONE_DAY_IN_MILLISECONDS = 86400000;
	private static final long ONE_HOUR_IN_MILLISECONDS = 3600000;
	private static final long ONE_MINUTE_IN_MILLISECONDS = 60000;
	private static final long ONE_SECOND_IN_MILLISECONDS = 1000;


	private String outputFileName = null;
	private List<BcCategory> categories;

	protected HashMap<String, IExchangeItem> exchangeItems; // protected for Mock class in tests

	@Override
	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length < 2) throw new RuntimeException("BcFile DataObject must be initialised with 2 arguments: InputBcFilePath and OutputBcFilePath");
		String inputFileName = arguments[0];
		this.outputFileName = arguments[1];
		this.workingDirectory = workingDirectory;

		File bcFile = new File(workingDirectory, inputFileName);
		if(!bcFile.exists()) throw new RuntimeException(String.format("BcFile does not exist: %s", inputFileName));

		//Step 1: Read BcCategories from (Input) BcFile
		categories = BcFileReaderWriter.readBcFile(bcFile);

		//Step 2: Create ExchangeItems from time series categories
		List<BcCategory> categoriesWithTimeSeriesData = getCategoriesWithTimeSeriesData();

		exchangeItems = new HashMap<>();
		for (BcCategory category : categoriesWithTimeSeriesData)
		{
			List<BcQuantity> table = category.getTable();
			BcQuantity timeSeriesQuantity = table.get(0);
			BcQuantity valueQuantity = table.get(1);
			String exchangeItemId = getBoundaryName(category) + EXCHANGE_ITEM_NAME_SEPARATOR + valueQuantity.getQuantity().getValue();

			List<Double> values = valueQuantity.getValues();
			double[] valuesAsDoubles = new double[values.size()];
			for(int i = 0; i < values.size(); i++) valuesAsDoubles[i] = values.get(i);

			double[] timesAsDoubles = ConvertBcTimesToModifiedJulianDays(timeSeriesQuantity.getUnit().getValue(), timeSeriesQuantity.getValues());

			exchangeItems.put(exchangeItemId, new BcExchangeItem(exchangeItemId, timesAsDoubles, valuesAsDoubles));
		}
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
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish()
	{
		// Step 1: Update categories with ExchangeItemData
		List<BcCategory> categoriesWithTimeSeriesData = getCategoriesWithTimeSeriesData();

		for (BcCategory category : categoriesWithTimeSeriesData)
		{
			List<BcQuantity> table = category.getTable();
			BcQuantity timeSeriesQuantity = table.get(0);
			BcQuantity valueQuantity = table.get(1);
			String exchangeItemId = getBoundaryName(category) + EXCHANGE_ITEM_NAME_SEPARATOR + valueQuantity.getQuantity().getValue();

			IExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);

			List<Double> valueData = new ArrayList<>();
			for (double value : exchangeItem.getValuesAsDoubles()) valueData.add(value);
			valueQuantity.setColumnData(valueData);

			List<Double> bcTimes = ConvertModifiedJulianDaysToBcTimes(timeSeriesQuantity.getUnit().getValue(), exchangeItem.getTimes());
			timeSeriesQuantity.setColumnData(bcTimes);
		}

		//Step 2: Write updated BcCategories to (Output) BcFile
		File bcFile = new File(workingDirectory, outputFileName);
		BcFileReaderWriter.writeBcFile(bcFile, categories);
	}

	private List<BcCategory> getCategoriesWithTimeSeriesData()
	{
		List<BcCategory> categoriesWithTimeSeriesData = new ArrayList<>();
		for (BcCategory category : categories)
		{
			Boolean hasTimeSeriesData = false;
			for (BcProperty property : category.getProperties())
				if(property.getName().equals("function")) hasTimeSeriesData |=  property.getValue().equals("timeseries");

			if(hasTimeSeriesData) categoriesWithTimeSeriesData.add(category);
		}
		return categoriesWithTimeSeriesData;
	}

	private String getBoundaryName(BcCategory category)
	{
		for (BcProperty property : category.getProperties())
			if (property.getName().equals("name")) return property.getValue();
		return "";
	}

	private static double[] ConvertBcTimesToModifiedJulianDays(String timeUnitString, List<Double> bcTimes)
	{
		double mjdReference = referenceDateTimeToMJD(timeUnitString);
		double multFactToMillies = getUnitMultiplicationFactor(timeUnitString);

		double[] modifiedJulianDayValues = new double[bcTimes.size()];
		for (int i = 0; i < bcTimes.size(); i++) {
			double milliesSinceReference = bcTimes.get(i) * multFactToMillies;
			double mjdValue = (milliesSinceReference / (double) ONE_DAY_IN_MILLISECONDS) + mjdReference;
			modifiedJulianDayValues[i]= mjdValue;
		}
		return modifiedJulianDayValues;
	}

	private static List<Double> ConvertModifiedJulianDaysToBcTimes(String timeUnitString, double[] modifiedJulianDayValues)
	{
		double mjdReference = referenceDateTimeToMJD(timeUnitString);
		double multFactFromMillies = getUnitMultiplicationFactor(timeUnitString);

		List<Double> values = new ArrayList<>();
		for(double mjdValue : modifiedJulianDayValues)
		{
			double bcTimeAsMjd = mjdValue - mjdReference;
			Double bcTime = Math.round(bcTimeAsMjd * (double) ONE_DAY_IN_MILLISECONDS / multFactFromMillies * 100d) / 100d;
			values.add(bcTime);
		}
		return values;
	}

	private static double getUnitMultiplicationFactor(String timeUnitString)
	{
		double multiplicationFactor;
		if(timeUnitString.contains(TIME_UNIT_SECONDS)) multiplicationFactor = ONE_SECOND_IN_MILLISECONDS;
		else if(timeUnitString.contains(TIME_UNIT_MINUTES)) multiplicationFactor = ONE_MINUTE_IN_MILLISECONDS;
		else if(timeUnitString.contains(TIME_UNIT_HOURS)) multiplicationFactor = ONE_HOUR_IN_MILLISECONDS;
		else throw new RuntimeException(String.format("Error reference time string unit: %s", timeUnitString));
		return multiplicationFactor;
	}

	private static double referenceDateTimeToMJD(String timeUnitString)
	{
		String referenceDateString;
		if(timeUnitString.contains(TIME_UNIT_SECONDS)) referenceDateString = timeUnitString.replace(TIME_UNIT_SECONDS, "").trim();
		else if(timeUnitString.contains(TIME_UNIT_MINUTES)) referenceDateString = timeUnitString.replace(TIME_UNIT_MINUTES, "").trim();
		else if(timeUnitString.contains(TIME_UNIT_HOURS)) referenceDateString = timeUnitString.replace(TIME_UNIT_HOURS, "").trim();
		else throw new RuntimeException(String.format("Error parsing Date unit: %s", timeUnitString));

		double referenceDateAsMJD;
		try {
			referenceDateAsMJD = TimeUtils.date2Mjd(referenceDateString, "yyyy-MM-dd HH:mm:ss");
		} catch (ParseException e) {
			throw new RuntimeException(String.format("Error parsing time unit string: %s", referenceDateString));
		}
		return referenceDateAsMJD;
	}
}
