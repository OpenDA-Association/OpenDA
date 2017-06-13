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

			List<Double> bcTimes = TimeUtils.ConvertModifiedJulianDaysToBcTimes(timeSeriesQuantity.getUnit().getValue(), exchangeItem.getTimes());
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
}
