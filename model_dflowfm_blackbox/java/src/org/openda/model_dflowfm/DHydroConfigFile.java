package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.ConfigTree;

import java.io.File;

/**
 * IDataObject for configuration file for d_hydro.exe.
 * Data object is used to modify start/end time
 */
public class DHydroConfigFile implements IDataObject {

	private static final String CONFIG_TREE_ELEMENT_TIME = "control/parallel/startGroup/time";

	private static final String CONFIG_TREE_ELEMENT_COMPONENT  = "component";
	private static final String CONFIG_TREE_ELEMENT_WORKDIR    = "workingDir";
	private static final String CONFIG_TREE_ELEMENT_LIBRARY    = "library";
	private static final String CONFIG_TREE_ELEMENT_INPUTFILE  = "inputFile";

	private String flowDllArgPrefix = "flowDllName:";
	String usageString = "DHydroConfigFile DataObject must be initialised with 1, 2 or 3 arguments: " +
			"InputFilePath [OutputFilePath] [" + flowDllArgPrefix + "dll-name]";

	private static final String FLOW1D_DLL_NAME = "cf_dll";

	private String flowLibraryName = FLOW1D_DLL_NAME;
	private String rtcLibraryName = "RTCTools_BMI";

	private File workingDirectory;
	private String outputFileName = null;
	ConfigTree configTree = null;

	IDataObject flowMdFile;
	IDataObject rtcToolsRuntimeConfigFile;

	public String[] getExchangeItemIDs()
	{
		return flowMdFile.getExchangeItemIDs();
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role)
	{
		return flowMdFile.getExchangeItemIDs(role);
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return flowMdFile.getDataObjectExchangeItem(exchangeItemID);
	}

	public void initialize(File workingDirectory, String[] arguments)
	{
		if(arguments.length < 1 || arguments.length > 3 ) {
			throw new RuntimeException(usageString);
		}
		String inputFileName = arguments[0];
		this.outputFileName = inputFileName;

		if (arguments.length > 1) {
			String dllName = getDLLNameFromArgument(arguments[1]);
			if (dllName != null ) {
				flowLibraryName = dllName;
			} else {
				this.outputFileName = arguments[1];
				if (arguments.length > 2) {
					dllName = getDLLNameFromArgument(arguments[1]);
					if (dllName != null ) {
						flowLibraryName = dllName;
					} else {
						throw new RuntimeException(usageString);
					}
				}
			}
		}

		this.workingDirectory = workingDirectory;
		configTree = new ConfigTree(workingDirectory, inputFileName);

		String inputFilePath = new File(workingDirectory, inputFileName).getAbsolutePath();
		File md1dFile = getSubComponentConfigFile(inputFilePath, flowLibraryName);
		File runtimeConfigFile = getSubComponentConfigFile(inputFilePath, rtcLibraryName);

		flowMdFile = new Md1dFile();
		flowMdFile.initialize(md1dFile.getParentFile(), new String[] {md1dFile.getName()});

		rtcToolsRuntimeConfigFile = new RtcToolsRuntimeConfigFile();
		rtcToolsRuntimeConfigFile.initialize(runtimeConfigFile.getParentFile(), new String[] {runtimeConfigFile.getName()});
	}

	public void finish()
	{
		IExchangeItem startTimeExchangeItem = flowMdFile.getDataObjectExchangeItem(Md1dFile.PROPERTY_STARTTIME);
		if(startTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", Md1dFile.PROPERTY_STARTTIME));
		IExchangeItem rtcStartTimeExchangeItem = rtcToolsRuntimeConfigFile.getDataObjectExchangeItem(Md1dFile.PROPERTY_STARTTIME);
		rtcStartTimeExchangeItem.setValues(startTimeExchangeItem.getValues());
		double startTimeAsMJD = startTimeExchangeItem.getValuesAsDoubles()[0];

		IExchangeItem stopTimeExchangeItem = flowMdFile.getDataObjectExchangeItem(Md1dFile.PROPERTY_STOPTIME);
		if(stopTimeExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", Md1dFile.PROPERTY_STOPTIME));
		IExchangeItem rtcEndTimeExchangeItem = rtcToolsRuntimeConfigFile.getDataObjectExchangeItem(Md1dFile.PROPERTY_STOPTIME);
		rtcEndTimeExchangeItem.setValues(stopTimeExchangeItem.getValues());
		double stopTimeAsMJD = stopTimeExchangeItem.getValuesAsDoubles()[0];

		IExchangeItem timeStepExchangeItem = flowMdFile.getDataObjectExchangeItem(Md1dFile.PROPERTY_TIMESTEP);
		if(timeStepExchangeItem == null) throw new RuntimeException(String.format("Exchange item %s does not exist", Md1dFile.PROPERTY_TIMESTEP));

		double timeStepAsMJD = timeStepExchangeItem.getValuesAsDoubles()[0];
		long timeStepInSeconds = Math.round(timeStepAsMJD*86400d);

		double periodAsMJD = stopTimeAsMJD - startTimeAsMJD;
		long periodInSeconds = Math.round(periodAsMJD*86400d);

		String content = String.format("0 %s %s", timeStepInSeconds, periodInSeconds);
		configTree.setContentString(CONFIG_TREE_ELEMENT_TIME, content);
		configTree.toFile(workingDirectory, outputFileName);

		flowMdFile.finish();
		rtcToolsRuntimeConfigFile.finish();
	}

	private String getDLLNameFromArgument(String dllString) {
		if (dllString.toLowerCase().startsWith(flowDllArgPrefix)) {
			return dllString.substring(flowDllArgPrefix.length());
		}
		return null;
	}

	private File getSubComponentConfigFile(String inputFilePath, String subComponentLibraryName) {
		String inputFile= null;
		String workingDir= null;
		ConfigTree[] componentConfigs = configTree.getSubTrees(CONFIG_TREE_ELEMENT_COMPONENT);
		for (ConfigTree componentConfig : componentConfigs) {
			String libraryName = componentConfig.getAsString(CONFIG_TREE_ELEMENT_LIBRARY, "");
			if (libraryName.equalsIgnoreCase(subComponentLibraryName)) {
				inputFile = componentConfig.getAsString(CONFIG_TREE_ELEMENT_INPUTFILE, "");
				workingDir = componentConfig.getAsString(CONFIG_TREE_ELEMENT_WORKDIR, "");
			}
		}
		if (inputFile == null || inputFile.isEmpty()) {
			throw new RuntimeException("Could not find input file for " + subComponentLibraryName + " in config file " + inputFilePath);
		}
		if (workingDir == null || workingDir.isEmpty()) {
			throw new RuntimeException("Could not find working dir  for " + subComponentLibraryName + " in config file " + inputFilePath);
		}
		File md1dFile = new File(workingDirectory, inputFile);
		if (md1dFile.exists()) {
			throw new RuntimeException("Flow1D's md1d file " + md1dFile.getAbsolutePath());
		}
		return md1dFile;
	}
}
