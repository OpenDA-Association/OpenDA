package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.ConfigTree;

import java.io.File;
import java.util.*;

/**
 * IDataObject for configuration file for d_hydro.exe.
 * Data object is used to modify start/end time
 */
public class DHydroConfigFile implements IDataObject {

	private static final String CONFIG_TREE_CATEGORY_TIME = "control/parallel/startGroup/time";
	private static final String PROPERTY_STARTTIME = "StartTime";
	private static final String PROPERTY_STOPTIME = "StopTime";
	private static final String PROPERTY_TIMESTEP = "TimeStep";

	private File workingDirectory;
	private String inputFileName = null;
	private String outputFileName = null;
	ConfigTree configTree = null;

	protected HashMap<String, IExchangeItem> exchangeItems;

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

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItems.containsKey(exchangeItemID)) throw new RuntimeException("Invalid exchange item id: " + exchangeItemID);
		return exchangeItems.get(exchangeItemID);
	}

	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length < 2) throw new RuntimeException("DHydroConfigFile DataObject must be initialised with 2 arguments: InputFilePath and OutputFilePath");
		this.inputFileName = arguments[0];
		this.outputFileName = arguments[1];
		this.workingDirectory = workingDirectory;
		configTree = new ConfigTree(workingDirectory, inputFileName);

		String timeField = configTree.getAsString(CONFIG_TREE_CATEGORY_TIME, "");
		String[] lineParts = timeField.trim().split("\\s", 3);
		double startTimeValue = Double.parseDouble(lineParts[0]);
		double timeStepValue = Double.parseDouble(lineParts[1]);
		double stopTimeValue = Double.parseDouble(lineParts[2]);

		exchangeItems = new HashMap<>();
		exchangeItems.put(PROPERTY_STARTTIME, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.StartTime, startTimeValue));
		exchangeItems.put(PROPERTY_STOPTIME, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.StopTime, stopTimeValue));
		exchangeItems.put(PROPERTY_TIMESTEP, new Flow1DTimeInfoExchangeItem(Flow1DTimeInfoExchangeItem.PropertyId.TimeStep, timeStepValue));
	}

	public void finish()
	{
		IExchangeItem startTimeExchangeItem = exchangeItems.get(PROPERTY_STARTTIME);
		if(startTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_STARTTIME));
		double startTimeValue = startTimeExchangeItem.getValuesAsDoubles()[0];

		IExchangeItem stopTimeExchangeItem = exchangeItems.get(PROPERTY_STOPTIME);
		if(stopTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_STOPTIME));
		double stopTimeValue = stopTimeExchangeItem.getValuesAsDoubles()[0];

		IExchangeItem timeStepExchangeItem = exchangeItems.get(PROPERTY_TIMESTEP);
		if(timeStepExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", PROPERTY_TIMESTEP));
		double timeStepValue = timeStepExchangeItem.getValuesAsDoubles()[0];

		String content = String.format("%s %s %s",formatNumberString(startTimeValue), formatNumberString(timeStepValue), formatNumberString(stopTimeValue));
		configTree.setContentString(CONFIG_TREE_CATEGORY_TIME, content);
		configTree.toFile(workingDirectory, outputFileName);
	}

	private String formatNumberString(double value)
	{
		// remove trailing zeros from double
		return value == (long)value ? String.valueOf((long)value) : String.valueOf(value);
	}
}
