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
package org.openda.model_efdc_dll;

import java.io.File;
import java.io.IOException;
import java.util.*;


import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IModelState;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.model_efdc_dll.EfdcExchangeItemType.EfdcExchangeItemRole;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.FileBasedModelState;
import org.openda.utils.io.GridExchangeItemNetcdfWriter;

/**
 * Model instance of an EFDC model. This communicates in-memory with the dll version of the EFDC model.
 *
 * For information about the EFDC model see http://www.epa.gov/athens/wwqtsc/html/efdc.html
 *
 * @author Arno Kockx
 */
public class EfdcModelInstance extends Instance implements IModelInstance {
	private static final String[] INPUT_STATE_FILE_NAMES = {"RESTART.INP", "TEMP.RST", "RSTWD.INP", "WQWCRST.INP" };
	private static final String[] OUTPUT_STATE_FILE_NAMES = {"RESTART.OUT", "TEMP.RSTO", "RSTWD.OUT", "WQWCRST.OUT"};
	private static final String[] OPTIONAL_INPUT_STATE_FILE_NAMES = {"WQWCRSTX.INP"};
	private static final String[] OPTIONAL_OUTPUT_STATE_FILE_NAMES = {"WQWCRSTX.OUT"};

	private final int modelInstanceNumber;
	private final EfdcModelFactory parentFactory;

	private final File instanceDir;
	private final File outputStateFilesDirectory;
	private final EfdcDLL modelDll;
	private final double referenceTimePeriodInDays;
	private final ITime timeHorizon;
	private final Map<String, EfdcScalarTimeSeriesExchangeItem> boundaryExchangeItems = new LinkedHashMap<String, EfdcScalarTimeSeriesExchangeItem>();
	private final Map<String, IExchangeItem> stateExchangeItems = new LinkedHashMap<String, IExchangeItem>();

	private IDataObject[] inputDataObjects;
	private GridExchangeItemNetcdfWriter gridModelOutputWriter;
	private GridExchangeItemNetcdfWriter gridAnalysisOutputWriter;
	private boolean firstTime = true;
	private static String XSPECIES = "XSpecies%1$02d";
    private static int XSPECIES_GRID_OFFSET = 1800;
    private static int XSPECIES_OFFSET = 800;


	/**
	 * State stored on disk. The efdc model can only store one state at a time.
	 */
	private FileBasedModelState stateStoredOnDisk = null;

	/**
	 * @param instanceDir path to instance directory
	 * @param inputFilePaths input files path relative to instanceDir
	 * @param modelOutputFilePath path for output file with model results relative to instanceDir
	 * @param analysisOutputFilePath path for output file with analysis relative to instanceDir
	 */
	public EfdcModelInstance(File instanceDir, String[] inputFilePaths, String modelOutputFilePath, String analysisOutputFilePath, int modelInstanceNumber,  EfdcModelFactory parentFactory) {
		this.modelInstanceNumber = modelInstanceNumber;
		this.parentFactory = parentFactory;

		//initialize model.
		this.instanceDir = instanceDir;
		this.outputStateFilesDirectory = new File(this.instanceDir, "output_state_files");
		this.modelDll = EfdcDLL.getForModelInstance(instanceDir);
		this.referenceTimePeriodInDays = this.modelDll.getReferencePeriod();
		this.timeHorizon = new Time(this.modelDll.getStartTime(), this.modelDll.getEndTime(), this.referenceTimePeriodInDays);

		//initialize exchangeItems.
		createModelExchangeItems();

		//initialize dataObjects.
		createDataObjects(inputFilePaths, modelOutputFilePath, analysisOutputFilePath);

		//initialize data in boundary condition exchange items for the entire model run period.
		if (this.inputDataObjects.length > 0) {
			putInputDataInBoundaryConditionExchangeItems();
		}
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

	private void createModelExchangeItems() {
		this.boundaryExchangeItems.clear();
		this.stateExchangeItems.clear();

		//create exchange items for all parameters, locations and layers
		for (EfdcExchangeItemType exchangeItemType : EfdcExchangeItemType.values()) {
			int parameterNumber = exchangeItemType.getParameterNumber();
			String parameterId = exchangeItemType.getParameterId();

			//check if exchangeItem is supported by current EFDC configuration
			if ( ! this.modelDll.supportsExchangeItem(parameterNumber) ) {
				continue;
			}
			
			EfdcExchangeItemRole role = exchangeItemType.getRole();
			switch (role) {
				case FORCING: case BOUNDARY:
					//create a scalar time series exchange item for each location and layer for this parameter.
					int locationCount = this.modelDll.getTimeSeriesCount(parameterNumber);
					int layerCount    = this.modelDll.getLayerCount(parameterNumber);
					for (int locationNumber = 1; locationNumber <= locationCount; locationNumber++) {
						if (layerCount <= 1) {
							EfdcScalarTimeSeriesExchangeItem scalarTimeSeriesExchangeItem =
									new EfdcScalarTimeSeriesExchangeItem(locationNumber, parameterNumber, null, parameterId, IExchangeItem.Role.Input, this.modelDll);
							this.boundaryExchangeItems.put(scalarTimeSeriesExchangeItem.getId(), scalarTimeSeriesExchangeItem);
						} else {//if multiple layers.
							for (int layerNumber = 1; layerNumber <= layerCount; layerNumber++) {
								EfdcScalarTimeSeriesExchangeItem scalarTimeSeriesExchangeItem =
										new EfdcScalarTimeSeriesExchangeItem(locationNumber, parameterNumber, layerNumber, parameterId, IExchangeItem.Role.Input, this.modelDll);
								this.boundaryExchangeItems.put(scalarTimeSeriesExchangeItem.getId(), scalarTimeSeriesExchangeItem);
							}
						}
					}
					break;

				case STATE:
					//create a grid exchange item for this parameter.
					EfdcGridExchangeItem gridExchangeItem = new EfdcGridExchangeItem(parameterNumber, parameterId, Role.InOut, this.modelDll);
					this.stateExchangeItems.put(gridExchangeItem.getId(), gridExchangeItem);
					break;

				default:
					throw new RuntimeException("Unknown EfdcExchangeItemRole type " + role + " for " + parameterNumber);
			}
		}
        //create exchange items for xspecies at all locations and layers
        //check if exchangeItem is supported by current EFDC configuration
        int nXspecies = this.modelDll.getXspeciesCount();

        for (int iXspecies = 1; iXspecies <= nXspecies; ++iXspecies) {
            int parameterGridNumber = XSPECIES_GRID_OFFSET + iXspecies;
            if ( !this.modelDll.supportsExchangeItem(parameterGridNumber) ) {
                continue;
            }
            String parameterId = String.format(XSPECIES,iXspecies);
            EfdcGridExchangeItem gridExchangeItem = new EfdcGridExchangeItem(parameterGridNumber, parameterId, Role.InOut, this.modelDll);
            this.stateExchangeItems.put(gridExchangeItem.getId(), gridExchangeItem);
            int parameterNumber = XSPECIES_OFFSET + iXspecies;
            int locationCount = this.modelDll.getTimeSeriesCount(parameterNumber);
            int layerCount    = this.modelDll.getLayerCount(parameterNumber);
            for (int locationNumber = 1; locationNumber <= locationCount; locationNumber++) {
                if (layerCount <= 1) {
                    EfdcScalarTimeSeriesExchangeItem scalarTimeSeriesExchangeItem =
                        new EfdcScalarTimeSeriesExchangeItem(locationNumber, parameterNumber, null, parameterId, IExchangeItem.Role.Input, this.modelDll);
                    this.boundaryExchangeItems.put(scalarTimeSeriesExchangeItem.getId(), scalarTimeSeriesExchangeItem);
                } else {//if multiple layers.
                    for (int layerNumber = 1; layerNumber <= layerCount; layerNumber++) {
                        EfdcScalarTimeSeriesExchangeItem scalarTimeSeriesExchangeItem =
                            new EfdcScalarTimeSeriesExchangeItem(locationNumber, parameterNumber, layerNumber, parameterId, IExchangeItem.Role.Input, this.modelDll);
                        this.boundaryExchangeItems.put(scalarTimeSeriesExchangeItem.getId(), scalarTimeSeriesExchangeItem);
                    }
                }
            }

        }

    }

	private void createDataObjects(String[] inputFilePaths, String modelOutputFilePath, String analysisOutputFilePath) {
		//check if input files exist.
		for (String inputFilePath : inputFilePaths ) {
			File inputFile = new File(instanceDir, inputFilePath);
			if (!inputFile.exists()) {
				throw new RuntimeException("Cannot find configured input file " + inputFile.getAbsolutePath());
			}
		}

		//create input dataObjects.
		this.inputDataObjects = new IDataObject[inputFilePaths.length];
		for (int n = 0; n < inputFilePaths.length; n++) {
			IDataObject inputDataObject = new NetcdfDataObject();
			inputDataObject.initialize(this.instanceDir, new String[]{inputFilePaths[n], "true", "false"});
			this.inputDataObjects[n] = inputDataObject;
		}
		// START JS
		//inputDataObjects[0].getExchangeItemIDs()
		// STOP JS

		//remove existing output files.
		File modelOutputFile = new File(this.instanceDir, modelOutputFilePath);
		if (modelOutputFile.exists()) {
			if (!modelOutputFile.delete()) {
				throw new RuntimeException("Cannot delete existing output file " + modelOutputFile.getAbsolutePath());
			}
		}
		File analysisOutputFile = new File(this.instanceDir, analysisOutputFilePath);
		if (analysisOutputFile.exists()) {
			if (!analysisOutputFile.delete()) {
				throw new RuntimeException("Cannot delete existing output file " + analysisOutputFile.getAbsolutePath());
			}
		}

		//create output writers.
		this.gridModelOutputWriter = createOutputWriter(modelOutputFile);
		//analysisOutputDataObject can use the same times as modelOutputDataObject, only
		//for the analysisOutputDataObject not all times will be filled with data.
		this.gridAnalysisOutputWriter = createOutputWriter(analysisOutputFile);
	}

	private GridExchangeItemNetcdfWriter createOutputWriter(File outputFile) {
		Collection<IExchangeItem> items = this.stateExchangeItems.values();
		return new GridExchangeItemNetcdfWriter(items.toArray(new IExchangeItem[items.size()]), outputFile);
	}

	/*
	 * Time information / Computing
	 */

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
		return new Time(this.modelDll.getCurrentTime());
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
			writeAnalysisOutputData();
		}

		//time update.
		ITime endTime = targetTime.getEndTime();
		ITime currentTime = getCurrentTime();
		//need tolerance in time comparison, because measuring small times using a double representation is inaccurate.
		while (endTime.getMJD() - currentTime.getMJD() > 1e-5) {
			//compute one referenceTimePeriod (length of referenceTimePeriod can be configured in EFDC.INP config file).
			//Note: In the EFDC model the model run period is divided in a number of referenceTimePeriods.
			//Each referenceTimePeriod is in turn divided in a number of timeSteps.
			//The method EfdcDLL.compute can only be called for a time period that is equal to an integer number of referenceTimePeriods.
			//the call to this.modelDLL.compute also advances the currentTime returned by method getCurrentTime.
			this.modelDll.compute(currentTime, new Time(currentTime.getMJD() + this.referenceTimePeriodInDays));
			currentTime = getCurrentTime();

			//write output data after model run (time update).
			writeModelOutputData();
		}
	}

	/**

	 * Get the localization vector.
	 *
	 * @param observationDescriptions observation descriptions
	 * @param distance characteristic distance for Cohn's formula.
	 * @return weight vector for each observation location.
	 */
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new UnsupportedOperationException(getClass().getName() + ": getObservedLocalization not implemented.");
	}

	/*
	 * Save/restore full internal state
	 */

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
		Results.putMessage(this.getClass().getSimpleName() + ": unzipping state files from zip file "
				+ persistentStateZipFile.getAbsolutePath() + " to directory " + this.instanceDir.getAbsolutePath());

		//unzip the given persistentStateZipFile to the instance folder.
		FileBasedModelState persistentState = new EfdcModelState(this.instanceDir);
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
			throw new IllegalStateException("Requested state does not exist anymore. An efdc model instance can only store one ModelState at a time.");
		}

		Results.putMessage(this.getClass().getSimpleName() + ": restoring internal state from input state files for instance "
				+ this.instanceDir.getAbsolutePath());

		//load state from disk.
		this.modelDll.restoreInstanceFromRestartFiles();
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
		//save state to disk.
		Results.putMessage(this.getClass().getSimpleName() + ": saving internal state to output state files for instance "
				+ this.instanceDir.getAbsolutePath());
		this.modelDll.storeCurrentInstanceToRestartFiles();

		//output state files have different names from input state files.
		//Here need to store the output state files with the names from the input state files,
		//so that they can be used for a new run. However the input state files should
		//not be overwritten, otherwise the run is not reproducible anymore.
		//Therefore copy output state files to a separate folder and rename to names from the input state files.
		Results.putMessage(this.getClass().getSimpleName() + ": copying and renaming output state files to directory "
				+ this.outputStateFilesDirectory.getAbsolutePath() + " for instance "
				+ this.instanceDir.getAbsolutePath());
		if (!this.outputStateFilesDirectory.exists() && !this.outputStateFilesDirectory.mkdir()) {
			throw new RuntimeException("Cannot create output state files directory " + this.outputStateFilesDirectory.getAbsolutePath());
		}
		for (int n = 0; n < OUTPUT_STATE_FILE_NAMES.length; n++) {
			File sourceFile = new File(this.instanceDir, OUTPUT_STATE_FILE_NAMES[n]);
			File targetFile = new File(this.outputStateFilesDirectory, INPUT_STATE_FILE_NAMES[n]);
			if (sourceFile.exists()) {
				try {
					BBUtils.copyFile(sourceFile, targetFile);
				} catch (IOException e) {
					throw new RuntimeException("Cannot copy and rename output state file " + sourceFile.getAbsolutePath()
						+ " to " + targetFile.getAbsolutePath() + " Message was: " + e.getMessage(), e);
				}
			} else {
				throw new RuntimeException("Missing output state file " + sourceFile.getAbsolutePath());
			}
		}

		for (int n = 0; n < OPTIONAL_OUTPUT_STATE_FILE_NAMES.length; n++) {
			File sourceFile = new File(this.instanceDir, OPTIONAL_OUTPUT_STATE_FILE_NAMES[n]);
			File targetFile = new File(this.outputStateFilesDirectory, OPTIONAL_INPUT_STATE_FILE_NAMES[n]);
			if ( sourceFile.exists() ) {
				try {
					BBUtils.copyFile(sourceFile, targetFile);
				} catch (IOException e) {
					throw new RuntimeException("Cannot copy and rename output state file " + sourceFile.getAbsolutePath()
						+ " to " + targetFile.getAbsolutePath() + " Message was: " + e.getMessage(), e);
				}
			}
		}

		//create a FileBasedModelState object that refers to the output state files.
		FileBasedModelState persistentState = new EfdcModelState(this.outputStateFilesDirectory);
		this.stateStoredOnDisk = persistentState;
		return persistentState;
	}

	private class EfdcModelState extends FileBasedModelState {
		private final File stateFilesDirectory;

		/**
		 * @param stateFilesDirectory the directory that contains the model state files
		 */
		public EfdcModelState(File stateFilesDirectory) {
			super(stateFilesDirectory );
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
				throw new IllegalStateException("This state does not exist anymore. An efdc model instance can only store one ModelState at a time.");
			}

			Results.putMessage(this.getClass().getSimpleName() + ": zipping state files from directory "
					+ this.stateFilesDirectory.getAbsolutePath() + " to zip file " + savedStateFile.getAbsolutePath());
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
		return this.instanceDir;
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
		List<String> ids = new ArrayList<String>();
		ids.addAll(this.boundaryExchangeItems.keySet());
		ids.addAll(this.stateExchangeItems.keySet());
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
		for (IExchangeItem exchangeItem : this.boundaryExchangeItems.values()) {
			if (exchangeItem.getRole() == role) {
				ids.add(exchangeItem.getId());
			}
		}
		for (IExchangeItem exchangeItem : this.stateExchangeItems.values()) {
			if (exchangeItem.getRole() == role) {
				ids.add(exchangeItem.getId());
			}
		}
		return ids.toArray(new String[ids.size()]);
	}

	/**
	 * Returns the exchange item with the given exchangeItemId, if it exists.
	 *
	 * @param exchangeItemId Identifier of exchange item
	 * @return IExchangeItem.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId) {
		IExchangeItem exchangeItem = this.boundaryExchangeItems.get(exchangeItemId);
		if (exchangeItem != null) {
			return exchangeItem;
		}
		exchangeItem = this.stateExchangeItems.get(exchangeItemId);
		if (exchangeItem != null) {
			return exchangeItem;
		}
		throw new RuntimeException("Exchange item with id '" + exchangeItemId + "' not found in " + getClass().getSimpleName());
	}

	/**
	 * Returns the exchange item with the given exchangeItemId, if it exists.
	 *
	 * @param exchangeItemId Identifier of exchange item
	 * @return IPrevExchangeItem
	 */
	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all ioObjects and exchange items have been migrated to the new IDataObject/IExchangeItem approach. AK
	@Deprecated
	public IPrevExchangeItem getExchangeItem(String exchangeItemId) {
		//delegate to new getDataObjectExchangeItem method.
		return getDataObjectExchangeItem(exchangeItemId);
	}

	/**
	 * Set input data for the entire run period in the boundary condition exchange items.
	 * If for a given location and parameter there is only one inputExchangeItem and multiple boundaryExchangeItems for multiple layers,
	 * then the values of that inputExchangeItem are copied to the boundaryExchangeItems for all layers.
	 */
	private void putInputDataInBoundaryConditionExchangeItems() {
		Results.putMessage(getClass().getSimpleName() + ": setting model input data for instance " + instanceDir.getAbsolutePath());

		String[] boundaryExchangeItemIds = boundaryExchangeItems.keySet().toArray(new String[boundaryExchangeItems.size()]);
		for (String id : boundaryExchangeItemIds) {
			IExchangeItem boundaryExchangeItem = getDataObjectExchangeItem(id);
			//find corresponding inputExchangeItem.
			IExchangeItem inputExchangeItem = null;
			for (IDataObject inputDataObject : inputDataObjects) {
				inputExchangeItem = inputDataObject.getDataObjectExchangeItem(id);
				if (inputExchangeItem != null) {
					break;
				}
			}
			if (inputExchangeItem == null) {//if item not found.
				String locationId = BBUtils.getLocationFromId(id);
				String parameterId = BBUtils.getParameterFromId(id);
				if (!locationId.contains("_layer") ) {
					if (!EfdcExchangeItemType.findByKey(parameterId).isOptional()) {
					Results.putMessage(getClass().getSimpleName() + ": Exchange item with id '" + id + "' not found in given inputDataObjects. Make sure that the inputData for this ExchangeItem is either supplied by the boundaryProvider or configured in the EFDCModelFactory config file.");
					}
					continue;
				}

				//if boundaryExchangeItem has layers.
				//try to find inputExchangeItem with same location and parameter but without layer.
				String newLocationId = locationId.substring(0, locationId.lastIndexOf('_'));
				String newId = newLocationId + "." + parameterId;
				for (IDataObject inputDataObject : inputDataObjects) {
					inputExchangeItem = inputDataObject.getDataObjectExchangeItem(newId);
					if (inputExchangeItem != null) {
						break;
					}
				}
				if ((inputExchangeItem == null) && EfdcExchangeItemType.findByKey(parameterId).isOptional()) {
					continue;
				}
				else if (inputExchangeItem == null) {//if item not found.
					throw new RuntimeException("Exchange item with id '" + id + "' or id '" + newId + "' not found in given inputDataObjects.");
				}
			}

			//ask the boundaryExchangeItem to copy all value(s) that it currently needs from the inputExchangeItem.
			// TODO: [LOGGING] Move to debug when available as logging option.
			//Results.putMessage(getClass().getSimpleName() + ": copying data from inputExchangeItem '" + id + "' of type " + inputExchangeItem.getClass().getSimpleName()
			//		+ " to boundaryExchangeItem '" + id + "' of type " + boundaryExchangeItem.getClass().getSimpleName());
			boundaryExchangeItem.copyValuesFromItem(inputExchangeItem);
		}
	}

	/**
	 * Write the current output data to the output exchange items.
	 */
	private void writeModelOutputData() {
		Results.putMessage(this.getClass().getSimpleName() + ": writing model output data for time " + new Date(Time.mjdToMillies(getCurrentTime().getMJD())) + " for instance " + this.instanceDir.getAbsolutePath());
		this.gridModelOutputWriter.writeDataForCurrentTimeStep();
	}

	/**
	 * Write the current output data to the output exchange items.
	 */
	private void writeAnalysisOutputData() {
		Results.putMessage(this.getClass().getSimpleName() + ": writing analysis output data for time " + new Date(Time.mjdToMillies(getCurrentTime().getMJD())) + " for instance " + this.instanceDir.getAbsolutePath());
		this.gridAnalysisOutputWriter.writeDataForCurrentTimeStep();
	}

	public void finish() {
		//write output data after last analysis (state update).
		writeAnalysisOutputData();

		this.boundaryExchangeItems.clear();
		this.stateExchangeItems.clear();
		this.gridModelOutputWriter.close();
		this.gridAnalysisOutputWriter.close();
		for (IDataObject inputDataObject : this.inputDataObjects) {
			inputDataObject.finish();
		}
		this.modelDll.finish();

		//TODO this is a hack to free the modelDll. Currently cannot do this in the modelFactory, because the IModelFactory.finish method
		//does not exist yet. Move this call to IModelFactory.finish method. For this need to add and call the following new methods
		//for all implementations: IModelFactory.finish() and IStochModelFactory.finish(). AK
		boolean lastModelInstance = false;
		if (this.parentFactory != null) {
			lastModelInstance = this.modelInstanceNumber == this.parentFactory.getCurrentModelInstanceNumber();
		}
		if (lastModelInstance) {
			//after all modelInstances have been finished, free/reset the dll.
			EfdcDLL.free();
		}
	}
}
