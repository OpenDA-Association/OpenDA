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

package org.openda.model_wflow;

import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.NetcdfScalarTimeSeriesExchangeItem;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.iotools.DataCopier;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Array;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.AsciiFileUtils;
import org.openda.utils.io.FileBasedModelState;
import org.openda.utils.io.GridExchangeItemNetcdfWriter;

import java.io.File;
import java.util.*;

/**
 * Model instance of a WFLOW model. This communicates in-memory with the WFLOW model.
 *
 * For information about the WFLOW model see www.openstreams.org
 * and https://publicwiki.deltares.nl/display/OpenS/wflow+-+PCRaster-Python+based+distributed+hydrological+models
 * For information about the PCRaster framework see http://pcraster.geo.uu.nl/
 *
 * @author Arno Kockx
 */
public class WflowModelInstance extends Instance implements IModelInstance {
	private static final String WFLOW_MASK_VARIABLE_NAME = "TopoId";

	private final File modelRunDir;
	private final ITime timeHorizon;
	private final String pythonModuleNameOfModelToUse;
	private final File caseDirectory;
	private final String instanceRunId;
	private final String configFileName;
	private final String cloneMapFileName;
	/**
	 * Length of one timeStep in milliseconds.
	 */
	private final long timeStepLength;
	private final int numberOfTimeSteps;
	private final File stateInputDir;
	private final File stateOutputDir;
    private final String[] outputExchangeItemIds;
    private final Collection<BBStochModelVectorConfig> scalarOutputVectorCollection;

    private WflowPythonToJavaAdapter adapter = null;
	private Map<String, IExchangeItem> modelExchangeItems = new HashMap<String, IExchangeItem>();
	private int currentTimeStep = 0;
	/**
	 * State stored on disk. The wflow model can only store one state at a time.
	 */
	private FileBasedModelState stateStoredOnDisk = null;

	private final IDataObject[] inputDataObjects;
	private GridExchangeItemNetcdfWriter gridModelOutputWriter;
	private GridExchangeItemNetcdfWriter gridAnalysisOutputWriter;
    private IDataObject modelScalarOutputDataObject;
    private IDataObject analysisScalarOutputDataObject = null;
	private boolean firstTime = true;

    /**
     * @param timeHorizon ITime object that includes the startTime and endTime for the model run.
     * @param pythonModuleNameOfModelToUse
     * @param caseDirectory
     * @param instanceRunId
     * @param configFileName
     * @param cloneMapFileName
     * @param inputDataObjects relative to the instanceDir
     * @param modelOutputFilePath relative to the instanceDir
     * @param analysisOutputFilePath relative to the instanceDir
     * @param outputExchangeItemIds selected eI to be written to the model output and analysis nc files
     * @param scalarModelOutputFilePath
     * @param scalarAnalysisFilePath
     * @param scalarOutputVectorCollection
     */
	public WflowModelInstance(ITime timeHorizon, String pythonModuleNameOfModelToUse, File caseDirectory,
                              String instanceRunId, String configFileName, String cloneMapFileName,
                              IDataObject[] inputDataObjects, String modelOutputFilePath, String analysisOutputFilePath,
                              String[] outputExchangeItemIds, String scalarModelOutputFilePath,
                              String scalarAnalysisFilePath, Collection<BBStochModelVectorConfig> scalarOutputVectorCollection) {
		this.caseDirectory = caseDirectory;
		this.instanceRunId = instanceRunId;
		this.modelRunDir = new File(caseDirectory, instanceRunId);
		this.configFileName = configFileName;
		this.cloneMapFileName = cloneMapFileName;
		this.inputDataObjects = inputDataObjects;
		this.pythonModuleNameOfModelToUse = pythonModuleNameOfModelToUse;

		//set timeStep length from model config in timeHorizon.
		this.timeStepLength = getTimeStepLengthFromIniFile(new File(caseDirectory, configFileName));
		double timeStepLengthInDays = (double) this.timeStepLength / (24.0 * 3600.0 * 1000.0);
		this.timeHorizon = new Time(timeHorizon.getBeginTime().getMJD(), timeHorizon.getEndTime().getMJD(), timeStepLengthInDays);
		//initialize number of time steps.
		long startTimeMillies = Time.mjdToMillies(this.timeHorizon.getBeginTime().getMJD());
		long endTimeMillies = Time.mjdToMillies(this.timeHorizon.getEndTime().getMJD());
		this.numberOfTimeSteps = (int) Math.ceil((double) (endTimeMillies - startTimeMillies)/(double) this.timeStepLength);

		//all WflowModelInstances refer to the same input state folder, because the wflow model
		//expects the input state files to be in that folder. In practice this is not a problem,
		//since the methods WflowModelInstance.loadPersistentState and WflowModelInstance.restoreInternalState
		//are always called in succession for one WflowModelInstance before they are called for the next WflowModelInstance.
		//So any existing input state files are overwritten and immediately read by one WflowModelInstance,
		//then overwritten and immediately read by the next WflowModelInstance.
		this.stateInputDir = new File(modelRunDir, "../instate");
		this.stateOutputDir = new File(modelRunDir, "outstate");

		//initialize model.
		initModel();

		//initialize exchangeItems.
		createModelExchangeItems();

        //set output exchange item IDs, should be called before createOutputDataObjects is called.
        this.outputExchangeItemIds = outputExchangeItemIds;
        this.scalarOutputVectorCollection = scalarOutputVectorCollection;

        //initialize dataObjects.
        createOutputDataObjects(modelOutputFilePath, analysisOutputFilePath,
                scalarModelOutputFilePath, scalarAnalysisFilePath);

		//initialize data in boundary condition (forcing) exchange items already for the first timeStep.
		//This cannot be done at the start of this.compute method, because this data
		//is already needed by the noiseModel before this.compute method is called.
		//this code assumes that boundary condition (forcing) exchange items are not part of the state.
		if (this.inputDataObjects.length > 0) {
			putInputDataInBoundaryConditionExchangeItems();
		}
	}

    private void initModel() {
		this.adapter = new WflowPythonToJavaAdapter();
		this.adapter.performPythonImports(this.pythonModuleNameOfModelToUse);
		this.adapter.createWflowModel(this.pythonModuleNameOfModelToUse, this.caseDirectory,
				this.instanceRunId, this.configFileName, this.cloneMapFileName);
		this.adapter.createWfDynamicFramework(this.pythonModuleNameOfModelToUse, this.numberOfTimeSteps);
		this.adapter.createRunId(this.pythonModuleNameOfModelToUse);
		this.adapter.runInitial();
		this.adapter.runResume();
	}

	/**
	 * Creates geometryInfo for the grids in the wflow model.
	 *
	 * @return ArrayGeometryInfo
	 */
	private ArrayGeometryInfo createGeometryInfo() {
		double upperLeftCenterX = this.adapter.getFirstGridCellCenterX();
		double upperLeftCenterY = this.adapter.getFirstGridCellCenterY();
		double cellWidth = this.adapter.getGridCellWidth();
		double cellHeight = this.adapter.getGridCellHeight();
		int rowCount = this.adapter.getRowCount();
		int columnCount = this.adapter.getColumnCount();
		int[] activeGridCellMask = BBUtils.toIntArray(this.adapter.getMapAsList(WFLOW_MASK_VARIABLE_NAME));

		//this code assumes that grid values in wflow start at upperLeft corner, then contain the first row from left to right,
		//then the second row from left to right, etc. Also see org.openda.model_wflow.WflowModelFactory.createInputDataObjects.
		double[] xValues = new double[columnCount];
		for (int n = 0; n < xValues.length; n++) {
			xValues[n] = upperLeftCenterX + n*cellWidth;
		}
		double[] yValues = new double[rowCount];
		for (int n = 0; n < yValues.length; n++) {
			yValues[n] = upperLeftCenterY - n*cellHeight;
		}

		//here for the purpose of writing to Netcdf the geometryInfo needs all grid cell coordinates and activeGridCellMask.
		IArray latitudeArray = new Array(yValues);
		IArray longitudeArray = new Array(xValues);
		IQuantityInfo latitudeQuantityInfo = new QuantityInfo("y coordinate according to model coordinate system", "meter");
		IQuantityInfo longitudeQuantityInfo = new QuantityInfo("x coordinate according to model coordinate system", "meter");
		return new ArrayGeometryInfo(latitudeArray, null, latitudeQuantityInfo,
				longitudeArray, null, longitudeQuantityInfo, null, null, null, activeGridCellMask);
	}

	private void createModelExchangeItems() {
		String[] variableNames = this.adapter.getVariableNames();
		int[] variableUnits = this.adapter.getVariableUnits();
		int[] variableRoles = this.adapter.getVariableRoles();
		if (variableRoles.length != variableNames.length || variableUnits.length != variableNames.length) {
			throw new RuntimeException("Lists with names, roles and units returned from model are not the same size.");
		}

		ArrayGeometryInfo geometryInfo = createGeometryInfo();

		//create one exchangeItem for each variable.
		this.modelExchangeItems.clear();
		for (int n = 0; n < variableNames.length; n++) {
			String variableName = variableNames[n];
			String variableUnit = String.valueOf(variableUnits[n]);

			int variableRole = variableRoles[n];
			//role: 0 = input (to the model)
			//      1 = is output (from the model)
			//      2 = input/output (state information)
			//      3 = model parameter
			IPrevExchangeItem.Role role;
			switch (variableRole) {
				case 0:
					//boundary condition.
					role = Role.Input;
					break;
				case 1:
					//result.
					role = Role.Output;
					break;
				case 2:
					//state.
					role = Role.InOut;
					break;
				case 3:
					//parameter.
					role = Role.InOut;
					break;
				default:
					throw new RuntimeException("Model returned invalid role integer " + variableRole + " for variable " + variableName);
			}

			//create exchangeItem.
			IQuantityInfo quantityInfo = new QuantityInfo(variableName, variableUnit);
			IExchangeItem item = new Wflow2DMapStateExchangeItem(variableName, role, quantityInfo, geometryInfo, this.timeHorizon, this.adapter);
			this.modelExchangeItems.put(item.getId(), item);
		}
	}

	private void createOutputDataObjects(String modelOutputFilePath, String analysisOutputFilePath,
                                         String scalarModelOutputFilePath, String scalarAnalysisOutputFilePath) {
        double[] modelOutputTimes = TimeUtils.getOutputTimes((Time) this.timeHorizon);
        if (modelOutputFilePath != null){
            //remove existing output files.
            File modelOutputFile = new File(this.modelRunDir, modelOutputFilePath);
            if (modelOutputFile.exists()) {
                if (!modelOutputFile.delete()) {
                    throw new RuntimeException("Cannot delete existing output file " + modelOutputFile.getAbsolutePath());
                }
            }
            //create output dataObjects.
            this.gridModelOutputWriter = createGridOutputWriter(modelOutputFile);
        }

        if (analysisOutputFilePath != null){
            //remove existing output files.
            File analysisOutputFile = new File(this.modelRunDir, analysisOutputFilePath);
            if (analysisOutputFile.exists()) {
                if (!analysisOutputFile.delete()) {
                    throw new RuntimeException("Cannot delete existing output file " + analysisOutputFile.getAbsolutePath());
                }
            }
            //create output dataObjects.
            //analysisOutputDataObject can use the same times as modelOutputDataObject, only
            //for the analysisOutputDataObject not all times will be filled with data.
            this.gridAnalysisOutputWriter = createGridOutputWriter(analysisOutputFile);
        }

        if (scalarModelOutputFilePath != null){
            //remove existing output files.
            File scalarOutputFile = new File(this.modelRunDir, scalarModelOutputFilePath);
            if (scalarOutputFile.exists()) {
                if (!scalarOutputFile.delete()) {
                    throw new RuntimeException("Cannot delete existing output file " + scalarOutputFile.getAbsolutePath());
                }
            }
            //create output dataObjects.
            this.modelScalarOutputDataObject = createScalarOutputDataObject(scalarModelOutputFilePath, modelOutputTimes);
        }

        if (scalarAnalysisOutputFilePath != null){
            //remove existing output files.
            File scalarOutputFile = new File(this.modelRunDir, scalarAnalysisOutputFilePath);
            if (scalarOutputFile.exists()) {
                if (!scalarOutputFile.delete()) {
                    throw new RuntimeException("Cannot delete existing output file " + scalarOutputFile.getAbsolutePath());
                }
            }
            //create output dataObjects.
            this.analysisScalarOutputDataObject = createScalarOutputDataObject(scalarAnalysisOutputFilePath, modelOutputTimes);
        }
//		//create output dataObjects.
//		double[] modelOutputTimes = TimeUtils.getOutputTimes((Time) this.timeHorizon);
//		this.modelOutputDataObject = createOutputDataObject(modelOutputFilePath, modelOutputTimes);
//		//analysisOutputDataObject can use the same times as modelOutputDataObject, only
//		//for the analysisOutputDataObject not all times will be filled with data.
//		this.analysisOutputDataObject = createOutputDataObject(analysisOutputFilePath, modelOutputTimes);

//        if (scalarModelOutputFilePath != null){
//            this.modelScalarOutputDataObject = createScalarOutputDataObject(scalarModelOutputFilePath, modelOutputTimes);
//        }

//        if (scalarAnalysisOutputFilePath != null){
//            this.analysisScalarOutputDataObject = createScalarOutputDataObject(scalarAnalysisOutputFilePath, modelOutputTimes);
//        }
	}

	private GridExchangeItemNetcdfWriter createGridOutputWriter(File netcdfOutputFile) {
		List<IExchangeItem> items = new ArrayList<IExchangeItem>();

        if (this.outputExchangeItemIds==null){
            //if no outputExchangeItemIds configured, then use all output exchangeItems.
            for (IExchangeItem item : this.modelExchangeItems.values()) {
                if (item.getRole() != Role.Output && item.getRole() != Role.InOut) {
                    continue;
                }
                items.add(item);
            }
        } else {
            for (String itemId : this.outputExchangeItemIds){
                IExchangeItem item = this.modelExchangeItems.get(itemId);
                if (item==null){
                    throw new RuntimeException("Invalid value of output exchange item ID '" + itemId
                            + "' configured in the wflowModelFactoryConfig file. ");
                }
                items.add(item);
            }
        }

        return new GridExchangeItemNetcdfWriter(items.toArray(new IExchangeItem[items.size()]), netcdfOutputFile);
    }

    private IDataObject createScalarOutputDataObject(String netcdfOutputFilePath, double[] outputTimes) {
        NetcdfDataObject netcdfOutputDataObject = new NetcdfDataObject();
        netcdfOutputDataObject.initialize(this.modelRunDir, new String[]{netcdfOutputFilePath, "true", "false"});
        for (java.util.Iterator<BBStochModelVectorConfig> it = this.scalarOutputVectorCollection.iterator(); it.hasNext();) {
            BBStochModelVectorConfig vectorConfig = it.next();
            String id = vectorConfig.getId();
				//TODO here wrap source grid exchangeItem in a new SubVector/SelectedIndicesExchangeItem that selects the configured indices in its getValues method,
				//then use NetcdfScalarExchangeItemWriter to write data for the SubVector/SelectedIndicesExchangeItems (use vectorConfig.getId()
				//as id for SubVector/SelectedIndicesExchangeItem, this becomes the locationId in the netcdf file).
				//Then can remove hacks for writing scalars from NetcdfScalarTimeSeriesExchangeItem and NetcdfDataObject. AK
			//here assume that stationDimensionIndex is 1.
			IExchangeItem newItem = new NetcdfScalarTimeSeriesExchangeItem(1, -1, BBUtils.getLocationFromId(id), BBUtils.getParameterFromId(id), -1, -1, Role.Output, new TimeInfo(outputTimes), netcdfOutputDataObject);
			netcdfOutputDataObject.addExchangeItem(newItem);
		}

        return netcdfOutputDataObject;
    }

    /**
	 * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
	 * where its configuration file is), and provide its arguments.
	 *
	 * @param workingDir The directory indicating the where the configurable is started (not as 'current
	 *				   working directory', but as the root path for its configuration files etc).
	 * @param arguments The arguments needed to initialize. Typically the first argument can be a configuration
	 *				  file name string, speficied relative to the working dir.
	 */
	public void initialize(File workingDir, String[] arguments) {
		//no action needed (handled by constructor).
		//also this method is never called.
	}

	/*************************************
	 * Time information / Computing
	 *************************************/

	/**
	 * Get the computational time horizon of the model (begin and end time).
	 *
	 * @return ITime containing begin and end time.
	 */
	public ITime getTimeHorizon() {
		return this.timeHorizon;
	}

	/**
	 * Get the model instance's current simulation time stamp.
	 *
	 * @return ITime currentTime.
	 */
	public ITime getCurrentTime() {
		return this.adapter.getCurrentTime(this.timeHorizon);
	}

	/**
	 * Let the model instance compute to the requested target time stamp.
	 * This function can not be used to go back in time. Use saveInternalState and restoreInternalState for that.
	 *
	 * @param targetTime time stamp to compute to.
	 */
	public void compute(ITime targetTime) {
		if (firstTime) {
			firstTime = false;
		} else {//if !firstTime.
			//write output data after analysis (state update).
			if (this.gridAnalysisOutputWriter != null){
				writeAnalysisOutputData();
			}
			if (this.analysisScalarOutputDataObject != null){
				writeAnalysisScalarOutputData();
			}
		}

		//time update.
		ITime endTime = targetTime.getEndTime();
		ITime currentTime = getCurrentTime();
		while (currentTime.getMJD() < endTime.getMJD()) {
			this.currentTimeStep++;

			//compute one timeStep.
			//the call to this.adapter.runDynamic also advances the currentTime returned by method getCurrentTime.
			this.adapter.runDynamic(this.currentTimeStep, this.currentTimeStep);
			currentTime = getCurrentTime();

			//write output data after model run (time update).
			if (this.gridModelOutputWriter != null){
				writeModelOutputData();
			}

            if (this.modelScalarOutputDataObject != null){
                writeModelScalarOutputData();
            }

			//initialize data in boundary condition (forcing) exchange items already for the next timeStep.
			//This cannot be done at the start of this.compute method, because this data
			//is already needed by the noiseModel before this.compute method is called.
			//this code assumes that boundary condition (forcing) exchange items are not part of the state.
			if (this.inputDataObjects.length > 0) {
				putInputDataInBoundaryConditionExchangeItems();
			}
		}
	}

	/**
	 * Get the localization vector.
	 *
	 * @param observationDescriptions
	 * @param distance characteristic distance for Cohn's formula.
	 * @return weight vector for each observation location.
	 */
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new UnsupportedOperationException(getClass().getName() + ": getObservedLocalization not implemented.");
	}

	/*************************************
	 * Save/restore full internal state
	 *************************************/

	/**
	 * Load an internal model state from file.
	 *
	 * Ask the model to internally store the state from the given zip file (with multiple files),
	 * this can be stored in file or in memory. This does not change the model's current state,
	 * this only stores the state and returns a ModelState object that is just an identifier
	 * for the stored state. Updating the current model state can be done after this
	 * by calling the restoreInternalState method.
	 * This method is the inverse of method IModelState.savePersistentState.
	 *
	 * @param persistentStateZipFile file to read state from.
	 * @return modelState object that refers to the saved state.
	 */
	public IModelState loadPersistentState(File persistentStateZipFile) {
		Results.putMessage(this.getClass().getSimpleName() + ": unzipping state files from zip file " + persistentStateZipFile.getAbsolutePath() + " to directory " + this.stateInputDir.getAbsolutePath());

		//unzip the given persistentStateZipFile to the folder with input state files.
		FileBasedModelState persistentState = new WflowModelState(this.stateInputDir);
		persistentState.setZippedStateFile(persistentStateZipFile);
		persistentState.restoreState();
		this.stateStoredOnDisk = persistentState;
		return persistentState;
	}

	/**
	 * Restore a previously saved state of the model.
	 *
	 * Set the model instance's current state to the state identified by the given ModelState object.
	 *
	 * @param savedInternalState handle to a (previously saved) state to be restored.
	 */
	public void restoreInternalState(IModelState savedInternalState) {
		if (savedInternalState != this.stateStoredOnDisk) {
			throw new IllegalStateException("Requested state does not exist anymore. A wflow model instance can only store one ModelState at a time.");
		}

		Results.putMessage(this.getClass().getSimpleName() + ": restoring internal state from input state files for instance " + this.modelRunDir.getAbsolutePath());

		//load state from disk.
		this.adapter.runResume();
	}

	/**
	 * Save the current state of the model to disk.
	 *
	 * Ask the model to store its current state (either to file or in memory) for future reference
	 * and return a ModelState object that is just an identifier for the stored state.
	 *
	 * @return modelState object that refers to the saved state.
	 */
	public IModelState saveInternalState() {
		Results.putMessage(this.getClass().getSimpleName() + ": saving internal state to output state files for instance " + this.modelRunDir.getAbsolutePath());

		//save state to disk.
		this.adapter.runSuspend();
		//create a FileBasedModelState object that refers to the folder with output state files.
		FileBasedModelState persistentState = new WflowModelState(this.stateOutputDir);
		this.stateStoredOnDisk = persistentState;
		return persistentState;
	}

	private class WflowModelState extends FileBasedModelState {
		private final File stateFilesDirectory;

		/**
		 * @param stateFilesDirectory the directory that contains the model state files.
		 */
		public WflowModelState(File stateFilesDirectory) {
			super(stateFilesDirectory);
			this.stateFilesDirectory = stateFilesDirectory;
		}

		/**
		 * Write the algorithm state to file.
		 *
		 * Ask the model to save the state identified by this ModelState object to the given file.
		 * If the state consists of multiple files, these can be zipped to collect them in a single file.
		 *
		 * @param savedStateFile the file to which this state has to be saved.
		 */
		public void savePersistentState(File savedStateFile) {
			if (this != stateStoredOnDisk) {
				throw new IllegalStateException("This state does not exist anymore. A wflow model instance can only store one ModelState at a time.");
			}

			Results.putMessage(this.getClass().getSimpleName() + ": zipping state files from directory " + this.stateFilesDirectory.getAbsolutePath() + " to zip file " + savedStateFile.getAbsolutePath());
			super.savePersistentState(savedStateFile);
		}
	}

	/**
	 * Release resources used to save a state at some earlier time.
	 *
	 * Ask the model to delete the file/memory of the state identified by the given ModelState object.
	 *
	 * @param savedInternalState handle to the (previously saved) state to be released.
	 */
	public void releaseInternalState(IModelState savedInternalState) {
		//do nothing. Only one state stored at a time, so no need to release resources.
	}

	/**
	 * Returns the directory where this model instance runs.
	 *
	 * @return model instance run directory with result files.
	 */
	public File getModelRunDir() {
		return modelRunDir;
	}

	/*************************************
	 * Exchange items
	 *************************************/

	/**
	 * Returns the ids of the exchange items for this model instance.
	 *
	 * @return exchangeItemIds.
	 */
	public String[] getExchangeItemIDs() {
		Set<String> ids = new HashSet<String>();
		ids.addAll(this.modelExchangeItems.keySet());
		return ids.toArray(new String[ids.size()]);
	}

	/**
	 * Returns the ids of the exchange items for this model instance with the given role.
	 *
	 * @param role Input, Output, or InOut.
	 * @return exchangeItemIds.
	 */
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		List<String> ids = new ArrayList<String>();
		for (IExchangeItem exchangeItem : this.modelExchangeItems.values()) {
			if (exchangeItem.getRole() == role) {
				ids.add(exchangeItem.getId());
			}
		}
		return ids.toArray(new String[ids.size()]);
	}

	/**
	 * Returns the exchange item with the given exchangeItemId, if it exists.
	 *
	 * @param exchangeItemId
	 * @return IExchangeItem.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId) {
		IExchangeItem exchangeItem = this.modelExchangeItems.get(exchangeItemId);
		if (exchangeItem == null) {
			throw new RuntimeException("Exchange item with id '" + exchangeItemId + "' not found in " + getClass().getSimpleName());
		}
		return exchangeItem;
	}

	/**
	 * Returns the exchange item with the given exchangeItemId, if it exists.
	 *
	 * @param exchangeItemId
	 * @return IPrevExchangeItem.
	 */
	//TODO this method is only present for backwards compatibility. This should be replaced by the IDataObject.getDataObjectExchangeItem
	//method once all ioObjects and exchange items have been migrated to the new IDataObject/IExchangeItem approach. AK
	@Deprecated
	public IPrevExchangeItem getExchangeItem(String exchangeItemId) {
		//delegate to new getDataObjectExchangeItem method.
		return getDataObjectExchangeItem(exchangeItemId);
	}

	/**
	 * Read the timeStepLength from the given configFile. If not configured, then the default (1 day) is used.
	 *
	 * @param configFile
	 * @return timeStepLength in milliseconds.
	 */
	private long getTimeStepLengthFromIniFile(File configFile) {
		if (!configFile.exists()) {
			throw new RuntimeException("config file " + configFile.getAbsolutePath() + " does not exist.");
		}

		List<String> lines = AsciiFileUtils.readLines(configFile);
		for (String line : lines) {
			if (line.toLowerCase().contains("timestepsecs")) {
				int indexOfEqualsSign = line.indexOf('=');
				if (indexOfEqualsSign == -1) {
					continue;
				}

				String timeStepLengthString = line.substring(indexOfEqualsSign + 1).trim();
				int timeStepLength;
				try {
					timeStepLength = Integer.parseInt(timeStepLengthString);
				} catch (NumberFormatException e) {
					throw new RuntimeException("Invalid value '" + timeStepLengthString
							+ "' configured for timestepsecs in config file " + configFile.getAbsolutePath(), e);
				}

				return timeStepLength * 1000;
			}
		}

		//if not configured, then return 1 day by default.
		return 24*3600*1000;
	}

	/**
	 * Set input data for the current time in the model input exchange items.
	 */
	private void putInputDataInBoundaryConditionExchangeItems() {
		Results.putMessage(getClass().getSimpleName() + ": setting model input data for instance " + this.instanceRunId);

		String[] inputExchangeItemIds = getExchangeItemIDs(Role.Input);
		DataCopier.copyDataFromDataObjectsToDataObject(inputExchangeItemIds, this.inputDataObjects, this);
	}

	/**
	 * Write the current output data to the output exchange items.
	 */
	private void writeModelOutputData() {
		Results.putMessage(getClass().getSimpleName() + ": writing model output data for time " + new Date(Time.mjdToMillies(getCurrentTime().getMJD())) + " for instance " + this.instanceRunId);
		gridModelOutputWriter.writeDataForCurrentTimeStep();
	}

    /**
     * Write the current output data to the output exchange items.
     */
    private void writeModelScalarOutputData() {
        Results.putMessage(getClass().getSimpleName() + ": writing model scalar output data for time "
                + new Date(Time.mjdToMillies(getCurrentTime().getMJD())) + " for instance " + this.instanceRunId);
        DataCopier.copyDataFromDataObjectsToDataObjectWithSelection(this.scalarOutputVectorCollection, new IDataObject[]{this}, this.modelScalarOutputDataObject);
    }

    private void writeAnalysisScalarOutputData() {
        Results.putMessage(getClass().getSimpleName() + ": writing analysis scalar output data for time "
                + new Date(Time.mjdToMillies(getCurrentTime().getMJD())) + " for instance " + this.instanceRunId);
        DataCopier.copyDataFromDataObjectsToDataObjectWithSelection(this.scalarOutputVectorCollection, new IDataObject[]{this}, this.analysisScalarOutputDataObject);
    }

	/**
	 * Write the current output data to the output exchange items.
	 */
	private void writeAnalysisOutputData() {
		Results.putMessage(getClass().getSimpleName() + ": writing analysis output data for time " + new Date(Time.mjdToMillies(getCurrentTime().getMJD())) + " for instance " + this.instanceRunId);
		gridAnalysisOutputWriter.writeDataForCurrentTimeStep();
	}

	/**
	 * Writes the final model state to disk.
	 */
	public void finish() {
		//write output data after last analysis (state update) and clear all data objects.
		if (this.gridAnalysisOutputWriter != null){
			writeAnalysisOutputData();
			this.gridAnalysisOutputWriter.close();
		}
        if (this.analysisScalarOutputDataObject != null){
            writeAnalysisScalarOutputData();
            this.analysisScalarOutputDataObject.finish();
        }

		this.modelExchangeItems.clear();
		if (this.gridModelOutputWriter != null){
			this.gridModelOutputWriter.close();
		}
        if (this.modelScalarOutputDataObject != null){
		    this.modelScalarOutputDataObject.finish();
        }
		for (IDataObject inputDataObject : this.inputDataObjects) {
			//TODO this should be called in modelFactory.finish, but that method is never called. Now this is called for every modelInstance, while they use the same inputDataObject. AK
			inputDataObject.finish();
		}

		this.adapter.runSuspend();
		this.adapter.close();
	}
}
