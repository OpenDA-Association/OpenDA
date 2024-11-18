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
package org.openda.model_dflowfm;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * IDataObject for Flow-1D's Boundary Condition file
 */
public class BcFile implements IDataObject
{
	private static final String EXCHANGE_ITEM_NAME_SEPARATOR = ".";
	private static final String[] EMPTY_STRING_ARRAY = new String[0];
	private File workingDirectory;

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

			double[] timesAsDoubles = TimeUtils.ConvertBcTimesToModifiedJulianDays(timeSeriesQuantity.getUnit().getValue(), timeSeriesQuantity.getValues());

			exchangeItems.put(exchangeItemId, new BcExchangeItem(exchangeItemId, timesAsDoubles, valuesAsDoubles));
		}

		//Step 3: Create ExchangeItems from scalar categories
		List<BcCategory> categoriesWithAstronomicData = getCategoriesWithAstronomicData();

		for (BcCategory category : categoriesWithAstronomicData)
		{
			List<BcQuantity> table = category.getTable();
			BcQuantity componentQuantity = table.get(0);
			BcQuantity amplitudeQuantity = table.get(1);
			BcQuantity phaseQuantity = table.get(2);

			List<String> components = componentQuantity.getStrings();
			List<Double> amplitudes = amplitudeQuantity.getValues();
			List<Double> phases = phaseQuantity.getValues();

			for (int componentIndex = 0; componentIndex < components.size(); componentIndex++)
			{
				String component = components.get(componentIndex);
				Double amplitude = amplitudes.get(componentIndex);
				Double phase = phases.get(componentIndex);

				String amplitudeExchangeItemId = getBoundaryName(category) + EXCHANGE_ITEM_NAME_SEPARATOR + amplitudeQuantity.getQuantity().getValue() + EXCHANGE_ITEM_NAME_SEPARATOR + component;

				exchangeItems.put(amplitudeExchangeItemId, new DoubleExchangeItem(component, amplitude));

				String phaseExchangeItemId = getBoundaryName(category) + EXCHANGE_ITEM_NAME_SEPARATOR + phaseQuantity.getQuantity().getValue() + EXCHANGE_ITEM_NAME_SEPARATOR + component;

				exchangeItems.put(phaseExchangeItemId, new DoubleExchangeItem(component, phase));
			}
		}
	}

	@Override
	public String[] getExchangeItemIDs()
	{
		return exchangeItems.keySet().toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role)
	{
		List<String> matchingExchangeItemIds = new ArrayList<>();
		for(IExchangeItem exchangeItem : exchangeItems.values())
			if(exchangeItem.getRole() == role) matchingExchangeItemIds.add(exchangeItem.getId());

		return matchingExchangeItemIds.toArray(EMPTY_STRING_ARRAY);
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

			List<Double> bcTimes = TimeUtils.ConvertModifiedJulianDaysToBcTimes(timeSeriesQuantity.getUnit().getValue(), exchangeItem.getTimes());
			timeSeriesQuantity.setColumnData(bcTimes);
		}

		List<BcCategory> categoriesWithAstronomicData = getCategoriesWithAstronomicData();

		for (BcCategory category : categoriesWithAstronomicData)
		{
			List<BcQuantity> table = category.getTable();
			BcQuantity componentQuantity = table.get(0);
			BcQuantity amplitudeQuantity = table.get(1);
			BcQuantity phaseQuantity = table.get(2);

			List<String> components = componentQuantity.getStrings();
			List<Double> amplitudes = amplitudeQuantity.getValues();
			List<Double> phases = phaseQuantity.getValues();

			for (int componentIndex = 0; componentIndex < components.size(); componentIndex++)
			{
				String component = components.get(componentIndex);

				String amplitudeExchangeItemId = getBoundaryName(category) + EXCHANGE_ITEM_NAME_SEPARATOR + amplitudeQuantity.getQuantity().getValue() + EXCHANGE_ITEM_NAME_SEPARATOR + component;

				IExchangeItem amplitudeExchangeItem = exchangeItems.get(amplitudeExchangeItemId);

				amplitudes.set(componentIndex, ((DoubleExchangeItem) amplitudeExchangeItem).getValue());

				String phaseExchangeItemId = getBoundaryName(category) + EXCHANGE_ITEM_NAME_SEPARATOR + phaseQuantity.getQuantity().getValue() + EXCHANGE_ITEM_NAME_SEPARATOR + component;

				IExchangeItem phaseExchangeItem = exchangeItems.get(phaseExchangeItemId);

				phases.set(componentIndex, ((DoubleExchangeItem) phaseExchangeItem).getValue());
			}

			amplitudeQuantity.setColumnData(amplitudes);
			phaseQuantity.setColumnData(phases);
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
			boolean hasTimeSeriesData = false;
			for (BcProperty property : category.getProperties())
				if(property.getName().equalsIgnoreCase("function")) hasTimeSeriesData |=  property.getValue().equalsIgnoreCase("timeseries");

			if(hasTimeSeriesData) categoriesWithTimeSeriesData.add(category);
		}
		return categoriesWithTimeSeriesData;
	}

	private List<BcCategory> getCategoriesWithAstronomicData()
	{
		List<BcCategory> categoriesWithTimeSeriesData = new ArrayList<>();
		for (BcCategory category : categories)
		{
			boolean hasTimeSeriesData = false;
			for (BcProperty property : category.getProperties())
				if(property.getName().equalsIgnoreCase("function")) hasTimeSeriesData |=  property.getValue().equalsIgnoreCase("astronomic");

			if(hasTimeSeriesData) categoriesWithTimeSeriesData.add(category);
		}
		return categoriesWithTimeSeriesData;
	}

	private String getBoundaryName(BcCategory category)
	{
		for (BcProperty property : category.getProperties())
			if (property.getName().equalsIgnoreCase("name")) return property.getValue() + getFunctionIndex(category);
		return "";
	}

	private String getFunctionIndex(BcCategory category)
	{
		for (BcProperty property : category.getProperties())
			if (property.getName().equalsIgnoreCase("FunctionIndex")) return EXCHANGE_ITEM_NAME_SEPARATOR + property.getValue();
		return "";
	}

	public List<BcCategory> getCategories() {
		return categories;
	}
}
