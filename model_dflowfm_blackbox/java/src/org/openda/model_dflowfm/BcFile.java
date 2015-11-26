package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by prevel on 20-Nov-15.
 */
public class BcFile implements IDataObject
{
	// TODO: this class should only contain code for reading/writing the BcFile, anything to do with the creation of IExchangeItems should be refactored out

	private static final String KEY_VALUE_COMMENT_PATTERN = "^\\s*(?<key>[^=\\s]+)\\s*=\\s*(?<value>[^#=]*)(#(?<comment>.*))?$";
	private static final String EXCHANGE_ITEM_NAME_SEPARATOR = ".";
	File workingDirectory;
	String inputFileName = null;
	String outputFileName = null;
	List<BcCategory> categories;

	HashMap<String, IExchangeItem> exchangeItems;

	@Override
	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length < 2) throw new RuntimeException("BcFile DataObject must be initialised with 2 arguments: InputBcFilePath and OutputBcFilePath");
		this.inputFileName = arguments[0];
		this.outputFileName = arguments[1];
		this.workingDirectory = workingDirectory;

		File bcFile = new File(workingDirectory, inputFileName);
		if(!bcFile.exists()) throw new RuntimeException(String.format("BcFile does not exist: %s", inputFileName));

		categories = new ArrayList<>();
		BcCategory lastCategory = null;
		int lineNumber = 1;

		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(bcFile));
			String nextLine = reader.readLine(); // skip 'generated on' comment at top of file

			//Step 1: Create categories
			while((nextLine = reader.readLine()) != null)
			{
				lineNumber++;
				nextLine = nextLine.trim();
				if(nextLine.isEmpty() || nextLine.startsWith("#")) continue;

				if(nextLine.startsWith("["))
				{
					String header = nextLine.substring(1, nextLine.lastIndexOf("]"));
					lastCategory = new BcCategory(lineNumber, header);
					categories.add(lastCategory);
				}
				else if(lastCategory != null)
				{
					if(nextLine.contains("="))
					{
						Pattern r = Pattern.compile(KEY_VALUE_COMMENT_PATTERN);
						Matcher m = r.matcher(nextLine);
						if(m.find())
						{
							String name = m.group("key");
							String value = m.group("value");
							String comment = m.group("comment") == null ? "" : m.group("comment");

							BcProperty property = new BcProperty(lineNumber, name, value, comment);

							if(name.equals("quantity"))
							{
								lastCategory.getTable().add(new BcQuantity(property));
							}
							else if(name.equals("unit"))
							{
								List<BcQuantity> table = lastCategory.getTable();
								table.get(table.size() -1).setUnit(property);
							}
							else
							{
								lastCategory.addProperty(property);
							}
						}
					}
					else
					{
						List<BcQuantity> table = lastCategory.getTable();
						String[] values = nextLine.split(" ");
						if(table.size() != values.length)
							throw new IllegalArgumentException("Number of values does not match number of quantities");

						for(int i = 0; i < table.size(); i++)
						{
							Double value = Double.valueOf(values[i]);
							table.get(i).addColumnData(value);
						}
					}
				}
			}
			reader.close();
		}
		catch(Exception ex)
		{
			String errorMessage = ex.getMessage();
			if(lastCategory != null) errorMessage = String.format("%s, Category: %s", errorMessage, lastCategory.getName());
			errorMessage = String.format("%s, BcFile: %s, LineNumber: %s", errorMessage, bcFile.getPath(), lineNumber);

			throw new RuntimeException("Error parsing BcFile: " + errorMessage);
		}

		//Step 2: Create ExchangeItems from time series categories
		List<BcCategory> categoriesWithTimeSeriesData = new ArrayList<>();

		for (BcCategory category : categories)
		{
			Boolean hasTimeSeriesData = false;
			for (BcProperty property : category.getProperties())
				if(property.name.equals("function")) hasTimeSeriesData |=  property.value.equals("timeseries");

			if(hasTimeSeriesData) categoriesWithTimeSeriesData.add(category);
		}

		exchangeItems = new HashMap<>();
		BcQuantity timeSeriesQuantity;
		BcQuantity valueQuantity;
		String boundaryName = "";

		for (BcCategory category : categoriesWithTimeSeriesData)
		{
			for(BcProperty property : category.getProperties())
			{
				if(property.name.equals("name"))
				{
					boundaryName = property.value;
					break;
				}
			}

			List<BcQuantity> table = category.getTable();

			if(table.get(0).quantity.value.equals("time"))
			{
				timeSeriesQuantity = table.get(0);
				valueQuantity = table.get(1);
			}
			else
			{
				timeSeriesQuantity = table.get(1);
				valueQuantity = table.get(0);
			}
			String exchangeItemId = boundaryName + EXCHANGE_ITEM_NAME_SEPARATOR + valueQuantity.quantity.value; // TODO: Add const for separator

			List<Double> values = valueQuantity.getValues();
			double[] valuesAsDoubles = new double[values.size()];
			for(int i = 0; i < values.size(); i++)
			{
				valuesAsDoubles[i] = values.get(i);
			}

			List<Double> times = BcUtils.ConvertDateTimesToModifiedJulianDayValues(timeSeriesQuantity.getUnit().value, timeSeriesQuantity.getValues());
			double[] timesAsDoubles = new double[times.size()];
			for(int i = 0; i < times.size(); i++)
			{
				timesAsDoubles[i] = times.get(i);
			}

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
		// TODO: most of this step 1 is repeated code from initialisation step 2- this needs to be refactored out
		// Step 1: Update categories with ExchangeItemData
		List<BcCategory> categoriesWithTimeSeriesData = new ArrayList<>();

		for (BcCategory category : categories)
		{
			Boolean hasTimeSeriesData = false;
			for (BcProperty property : category.getProperties())
				if(property.name.equals("function")) hasTimeSeriesData |=  property.value.equals("timeseries");

			if(hasTimeSeriesData) categoriesWithTimeSeriesData.add(category);
		}

		BcQuantity timeSeriesQuantity;
		BcQuantity valueQuantity;
		String boundaryName = "";

		for (BcCategory category : categoriesWithTimeSeriesData)
		{
			for (BcProperty property : category.getProperties())
			{
				if (property.name.equals("name"))
				{
					boundaryName = property.value;
					break;
				}
			}

			List<BcQuantity> table = category.getTable();

			if (table.get(0).quantity.value.equals("time"))
			{
				timeSeriesQuantity = table.get(0);
				valueQuantity = table.get(1);
			}
			else
			{
				timeSeriesQuantity = table.get(1);
				valueQuantity = table.get(0);
			}
			String exchangeItemId = boundaryName + EXCHANGE_ITEM_NAME_SEPARATOR + valueQuantity.quantity.value; // TODO: Add const for separator

			IExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);

			List<Double> valueData = new ArrayList<>();
			for (double value : exchangeItem.getValuesAsDoubles()) valueData.add(value);
			valueQuantity.setColumnData(valueData);

			List<Double> timeSeriesData = new ArrayList<>();
			for (double time : exchangeItem.getTimes()) timeSeriesData.add(time);
			timeSeriesQuantity.setColumnData(BcUtils.ConvertDateTimesFromModifiedJulianDayValues(timeSeriesQuantity.getUnit().value, timeSeriesData));
		}
		try
		{
			File bcFile = new File(workingDirectory, outputFileName);
			BufferedWriter writer = new BufferedWriter(new FileWriter(bcFile));

			// Step 2: Write generated on comment
			String currentDateTime = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss").format(Calendar.getInstance().getTime());
			writer.write(String.format("# Generated on %s", currentDateTime));
			writer.newLine();
			writer.newLine();

			// Step 3: Write categories
			for (BcCategory category : categories)
			{
				writer.write(String.format("[%s]", category.name));
				writer.newLine();

				for (BcProperty property : category.getProperties())
				{
					writer.write(GeneratePropertyString(property.name, property.value, property.comment));
					writer.newLine();
				}

				List<BcQuantity> table = category.getTable();
				if(table.size() > 0) // General category will have zero entries in table
				{
					String[] tableRows = new String[table.get(0).getValues().size()];
					Arrays.fill(tableRows, "");

					for(BcQuantity column : table)
					{
						BcProperty quantity = column.getQuantity();
						BcProperty unit = column.getUnit();

						writer.write(GeneratePropertyString(quantity.name, quantity.value, quantity.comment));
						writer.newLine();

						writer.write(GeneratePropertyString(unit.name, unit.value, quantity.comment));
						writer.newLine();

						List<Double> values = column.getValues();

						for(int j = 0; j < values.size(); j++)
						{
							tableRows[j] += values.get(j).toString() + " ";
						}
					}

					for(String row : tableRows)
					{
						writer.write(String.format("    %s", row));
						writer.newLine();
					}
				}
				writer.newLine();
			}
			writer.close();
		}
		catch(Exception ex)
		{
			throw new RuntimeException("Error writing BcFile: " + ex.getMessage());
		}
	}

	private String GeneratePropertyString(String name, String value, String comment)
	{
		// formatted name value comment string with trailing spaces removed
		return String.format("    %-22s= %-34s%s", name, value, comment.equals("") ? "" : String.format("# %s", comment)).replaceFirst("\\s+$", "");
	}

	// TODO: These private classes will have to be moved (up a level) from here when we refactor the IExchangeItem creation logic
	private class BcCategory
	{
		private int lineNumber;
		private String name;
		private List<BcProperty> properties;
		private List<BcQuantity> table;

		public BcCategory(int lineNumber, String name)
		{
			this.lineNumber = lineNumber;
			this.name = name;
			properties = new ArrayList<>();
			table = new ArrayList<>();
		}

		public void addProperty(BcProperty property) { properties.add(property); }
		public void addTableColumn(BcQuantity column) { table.add(column); }

		public int getLineNumber() { return lineNumber; }
		public String getName() { return name; }
		public List<BcProperty> getProperties() { return properties; }
		public List<BcQuantity> getTable() { return table; }
	}

	private class BcProperty
	{
		private int lineNumber;
		private String name;
		private String value;
		private String comment;

		public BcProperty(int lineNumber, String name, String value)
		{
			new BcProperty(lineNumber, name, value, "");
		}

		public BcProperty(int lineNumber, String name, String value, String comment)
		{
			this.lineNumber = lineNumber;
			this.name = name;
			this.value = value;
			this.comment = comment;
		}

		public int getLineNumber() { return lineNumber; }
		public String getName() { return name; }
		public String getValue() { return value; }
		public String getComment() { return comment; }
	}

	private class BcQuantity
	{
		private BcProperty quantity;
		private BcProperty unit;
		private List<Double> values;

		public BcQuantity(BcProperty quantity)
		{
			this.quantity = quantity;
			values = new ArrayList<>();
		}

		public BcQuantity(BcProperty quantity, BcProperty unit)
		{
			new BcQuantity(quantity);
			this.unit = unit;
		}

		public void setUnit(BcProperty unit) { this.unit = unit; }
		public void setColumnData(List<Double> values) { this.values = values; }
		public void addColumnData(Double value) { values.add(value); }

		public BcProperty getQuantity() { return quantity; }
		public BcProperty getUnit() { return unit; }
		public List<Double> getValues() { return values; }
	}
}
