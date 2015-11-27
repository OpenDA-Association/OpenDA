package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.util.*;

/**
 * Created by prevel on 20-Nov-15.
 */
public class BcFile implements IDataObject
{
	private static final String EXCHANGE_ITEM_NAME_SEPARATOR = ".";
	private File workingDirectory;
	private String inputFileName = null;
	private String outputFileName = null;
	private List<BcCategory> categories;

	protected HashMap<String, IExchangeItem> exchangeItems; // protected for Mock class in tests

	@Override
	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length < 2) throw new RuntimeException("BcFile DataObject must be initialised with 2 arguments: InputBcFilePath and OutputBcFilePath");
		this.inputFileName = arguments[0];
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

			List<Double> times = BcUtils.ConvertDateTimesToModifiedJulianDayValues(timeSeriesQuantity.getUnit().getValue(), timeSeriesQuantity.getValues());
			double[] timesAsDoubles = new double[times.size()];
			for(int i = 0; i < times.size(); i++) timesAsDoubles[i] = times.get(i);

			exchangeItems.put(exchangeItemId, new BcExchangeItem(exchangeItemId, timesAsDoubles, valuesAsDoubles));
		}
	}

	@Override
	public String[] getExchangeItemIDs()
	{
		Set<String> keys = exchangeItems.keySet();
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role)
	{
		List<String> matchingExchangeItemIds = new ArrayList<>();
		for(IExchangeItem exchangeItem : exchangeItems.values())
		{
			if(exchangeItem.getRole() == role) matchingExchangeItemIds.add(exchangeItem.getId());
		}

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

			List<Double> timeSeriesData = new ArrayList<>();
			for (double time : exchangeItem.getTimes()) timeSeriesData.add(time);
			timeSeriesQuantity.setColumnData(BcUtils.ConvertDateTimesFromModifiedJulianDayValues(timeSeriesQuantity.getUnit().getValue(), timeSeriesData));
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
}
