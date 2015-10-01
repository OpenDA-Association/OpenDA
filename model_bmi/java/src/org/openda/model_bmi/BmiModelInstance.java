/* MOD_V2.0
 * Copyright (c) 2015 Netherlands eScience Center
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
package org.openda.model_bmi;

import java.io.File;
import java.util.*;

import bmi.BMIModelException;
import bmi.BMI;
import bmi.EBMI;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.geometry.GeometryUtils;
import org.openda.utils.io.AnalysisDataWriter;
import org.openda.utils.io.FileBasedModelState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Interface to a BMI Model. Passes calls to the BMI interface.
 * 
 * @author Niels Drost
 * 
 */
public class BmiModelInstance extends Instance implements IModelInstance, IModelExtensions {
	private static final Logger LOGGER = LoggerFactory.getLogger(BmiModelInstance.class);

	private final EBMI model;
	private final File modelRunDir;

	private final Map<String, IExchangeItem> exchangeItems;

	/**
	 * Directory where the model reads the input state file(s) from. This is only used if an input state is used.
	 */
	private final File inputStateDir;
	/**
	 * Directory where the model writes the output state file(s) to. This is only used if an output state is used.
	 */
	private final File outputStateDir;

	private final AnalysisDataWriter analysisDataWriter;
	private boolean firstTime = true;

	/**
	 * @param model
	 * @param modelRunDir
	 * @param initFile
	 * @param overrulingTimeHorizon optional, can be null.
	 * @throws BMIModelException
	 */
	public BmiModelInstance(EBMI model, File modelRunDir, File initFile, ITime overrulingTimeHorizon) throws BMIModelException {
		if (model == null) throw new IllegalArgumentException("model == null");
		if (modelRunDir == null) throw new IllegalArgumentException("modelRunDir == null");
		if (initFile == null) throw new IllegalArgumentException("initFile == null");

		this.model = model;
		this.modelRunDir = modelRunDir;
		this.outputStateDir = new File(modelRunDir, "output_state");
		this.inputStateDir = new File(modelRunDir, "input_state");
		if (!inputStateDir.exists()) {
			if (!inputStateDir.mkdirs()) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot create input state directory " + inputStateDir.getAbsolutePath());
			}
		}

		//ask the model to read its config file.
		model.initializeConfig(BBUtils.getFilePathStringForPython(initFile.getAbsoluteFile()));

		// timeHorizon set from 'outside' overrules timeHorizon from model.
		if (overrulingTimeHorizon != null) {
			Results.putMessage(getClass().getSimpleName() + ": using time horizon set from outside: " + overrulingTimeHorizon.toString());
			double startTime = overrulingTimeHorizon.getBeginTime().getMJD();
			double endTime = overrulingTimeHorizon.getEndTime().getMJD();
			model.setStartTime(TimeUtils.mjdToUdUnitsTime(startTime, model.getTimeUnits()));
			model.setEndTime(TimeUtils.mjdToUdUnitsTime(endTime, model.getTimeUnits()));
		} else {
			//if timeHorizon has not been set from outside, then use timeHorizon from the model config file.
			//In this case the startTime and endTime should be configured in the model config file in advance.
			//do nothing.
		}

		//initialize the model.
		model.initializeModel();
		Results.putMessage(getClass().getSimpleName() + ": using time horizon: " + getTimeHorizon().toString());

		exchangeItems = createExchangeItems(model);

		this.analysisDataWriter = new AnalysisDataWriter(exchangeItems.values(), modelRunDir);
	}

	public void initialize(File workingDir, String[] arguments) {
		//no action needed (handled by constructor).
		//also this method is never called.
	}

	private static Map<String, IExchangeItem> createExchangeItems(BMI model) throws BMIModelException {
		Set<String> inputVars = new HashSet<String>();
		Set<String> outputVars = new HashSet<String>();
		Set<String> inoutVars = new HashSet<String>();

		// first fill sets with input and output variables
		Collections.addAll(inputVars, model.getInputVarNames());
		Collections.addAll(outputVars, model.getOutputVarNames());

		// then put duplicates in inout variables.
		// Note: Loop over copy of set to prevent iterator exception
		for (String var : inputVars.toArray(new String[inputVars.size()])) {
			if (outputVars.contains(var)) {
				inputVars.remove(var);
				outputVars.remove(var);
				inoutVars.add(var);
			}
		}

		Map<String, IExchangeItem> result = new HashMap<String, IExchangeItem>();

		for (String variable : inputVars) {
			BmiStateExchangeItem item = new BmiStateExchangeItem(variable, IPrevExchangeItem.Role.Input, model);
			result.put(variable, item);
		}

		for (String variable : outputVars) {
			BmiStateExchangeItem item = new BmiStateExchangeItem(variable, IPrevExchangeItem.Role.Output, model);
			result.put(variable, item);
		}

		for (String variable : inoutVars) {
			BmiStateExchangeItem item = new BmiStateExchangeItem(variable, IPrevExchangeItem.Role.InOut, model);
			result.put(variable, item);
		}
		return result;
	}

	public String[] getExchangeItemIDs() {
		Set<String> ids = exchangeItems.keySet();
		return ids.toArray(new String[ids.size()]);
	}

	/**
	 * Returns the ids of the exchange items for this model instance with the
	 * given role.
	 *
	 * @param role
	 *            Input, Output, or InOut.
	 * @return exchangeItemIds.
	 */
	public String[] getExchangeItemIDs(Role role) {
		List<String> ids = new ArrayList<String>();
		for (IExchangeItem exchangeItem : this.exchangeItems.values()) {
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
		IExchangeItem exchangeItem = this.exchangeItems.get(exchangeItemId);
		if (exchangeItem == null) {
			throw new RuntimeException("Exchange item with id '" + exchangeItemId + "' not found in "
					+ getClass().getSimpleName());
		}
		return exchangeItem;
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return getDataObjectExchangeItem(exchangeItemID);
	}

	public ITime getTimeHorizon() {
		try {
			double startTime = model.getStartTime();
			double endTime = model.getEndTime();
			double timeStepDurationInModelUnits = model.getTimeStep();
			String timeUnitsString = model.getTimeUnits();

			double startTimeMjd = TimeUtils.udUnitsTimeToMjd(startTime, timeUnitsString);
			double endTimeMjd = TimeUtils.udUnitsTimeToMjd(endTime, timeUnitsString);
			//convert time step duration from model time units to MJD.
			double timeStepDurationInDays = timeStepDurationInModelUnits * (endTimeMjd - startTimeMjd) / (endTime - startTime);

			return new Time(startTimeMjd, endTimeMjd, timeStepDurationInDays);
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
	}

	public ITime getCurrentTime() {
		try {
			return new Time(TimeUtils.udUnitsTimeToMjd(model.getCurrentTime(), model.getTimeUnits()));
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
	}

	public void compute(ITime targetTime) {
		if (firstTime) {
			firstTime = false;
		} else {//if !firstTime.
			//write model state data after analysis (state update).
			analysisDataWriter.writeDataAfterAnalysis();
		}

		//time update.
		try {
			model.updateUntil(TimeUtils.mjdToUdUnitsTime(targetTime.getEndTime().getMJD(), model.getTimeUnits()));
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}

		//write model state data before analysis (state update).
		analysisDataWriter.writeDataBeforeAnalysis();
	}

	/**
	 * Returns the localization weights for each observation location. This
	 * method assumes that there is only one state vector.
	 *
	 * @param observationDescriptions
	 *            observation description
	 * @param distance
	 *            characteristic distance for Cohn's formula
	 * @return weight vector for each observation location. The size of the
	 *         returned array must equal the number of observation locations in
	 *         the given observationDescriptions. The size of each vector in the
	 *         returned array must equal the size of the state vector of the
	 *         implementing modelInstance.
	 */
	// this method is never called if this modelInstance implements the
	// IModelExtensions interface.
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new UnsupportedOperationException(this.getClass().getName()
				+ ".getObservedLocalization() not implemented");
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
		//unzip the given persistentStateZipFile to the folder with input state files.
		Results.putMessage(this.getClass().getSimpleName() + ": unzipping state files from zip file " + persistentStateZipFile.getAbsolutePath() + " to directory " + this.inputStateDir.getAbsolutePath());
		FileBasedModelState persistentState = new BmiModelState(this.inputStateDir);
		persistentState.setZippedStateFile(persistentStateZipFile);
		persistentState.restoreState();
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
		//load state from disk.
		Results.putMessage(this.getClass().getSimpleName() + ": restoring internal state from input state files for instance " + this.modelRunDir.getAbsolutePath());
		try {
			model.loadState(BBUtils.getFilePathStringForPython(inputStateDir.getAbsoluteFile()));
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
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
		if (!outputStateDir.exists()) {
			if (!outputStateDir.mkdirs()) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot create output state directory " + outputStateDir.getAbsolutePath());
			}
		}

		//save state to disk.
		Results.putMessage(this.getClass().getSimpleName() + ": saving internal state to output state files for instance " + this.modelRunDir.getAbsolutePath());
		try {
			this.model.saveState(BBUtils.getFilePathStringForPython(outputStateDir.getAbsoluteFile()));
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}

		//create a FileBasedModelState object that refers to the folder with output state files.
		return new BmiModelState(outputStateDir);
	}

	private class BmiModelState extends FileBasedModelState {
		private final File stateFilesDirectory;

		/**
		 * @param stateFilesDirectory the directory that contains the model state files.
		 */
		public BmiModelState(File stateFilesDirectory) {
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
		return this.modelRunDir;
	}

	/**
	 * Get the observed values of the Model. This returns what the observations
	 * would look like, if reality would be equal to the current model state.
	 *
	 * In other words this method returns a grid with values that would be
	 * observed by the satellite if reality would be equal to the current model
	 * state. This is needed, because, to compare the satellite observations
	 * with the model output, they should be defined on the same grid. The grid
	 * of the satellite has a different position, size and orientation than the
	 * grid of the model state. The values of the model state grid are
	 * interpolated to the observations grid using bilinear interpolation. For
	 * satellite observations the interpolation has to be done for each
	 * observation separately, since for each time step the satellite grid can
	 * be different, as the satellite moves along its orbit.
	 *
	 * @param observationDescriptions
	 *            observation description
	 * @return Model prediction interpolated to each observation (location).
	 */
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		validateSingleObservationsExchangeItem(observationDescriptions);

		//TODO if multiple grids for current time, e.g. soilMoisture and evaporation, then the coordinates of these grids are present in sequence in observationDescriptions,
		//in that case need to figure out which of the observations correspond to the given stateExchangeItemID here.
		IVector observationXCoordinates = observationDescriptions.getValueProperties("x");
		IVector observationYCoordinates = observationDescriptions.getValueProperties("y");

		if (exchangeItems.size() > 1) {
			throw new RuntimeException("Can only handle a single exchange item");
		}

		// here only use the model exchangeItem that corresponds to the observed
		// values, e.g. "SoilMoisture".
		IExchangeItem modelExchangeItem = exchangeItems.values().iterator().next();

		IGeometryInfo modelGeometryInfo = modelExchangeItem.getGeometryInfo();
		double[] modelValues = modelExchangeItem.getValuesAsDoubles();

		IVector result = GeometryUtils.getObservedValuesBilinearInterpolation(observationXCoordinates,
				observationYCoordinates, modelGeometryInfo, modelValues);

		int errorCount = 0;
		for (int i = 0; i < result.getSize(); i++) {
			double value = result.getValue(i);

			if (Double.isNaN(value)) {
				errorCount += 1;
			}
		}

		if (errorCount > 0) {
			throw new RuntimeException("The model returned missing values for " + errorCount
					+ " observation locations.");
		}

		return result;
	}

	/**
	 * Check whether the given observationDescriptions contains a single
	 * IExchangeItem, otherwise x and y coordinates will contain coordinates
	 * from multiple exchangeItems.
	 *
	 * @param observationDescriptions
	 */
	private static void validateSingleObservationsExchangeItem(IObservationDescriptions observationDescriptions) {
		// get exchangeItem.
		List<IPrevExchangeItem> exchangeItems = observationDescriptions.getExchangeItems();
		if (exchangeItems == null || exchangeItems.size() != 1) {
			throw new IllegalArgumentException(
					"Given observationDescriptions contains none or multiple exchangeItems, but should contain only one.");
		}
	}

	/**
	 * Returns the localization weights for each observation location.
	 *
	 * @param stateExchangeItemID
	 *            id of the state vector for which the localization weights
	 *            should be returned.
	 * @param observationDescriptions
	 *            observation description
	 * @param distance
	 *            characteristic distance for Cohn's formula
	 * @return weight vector for each observation location. The size of the
	 *         returned array must equal the number of observation locations in
	 *         the given observationDescriptions. The size of each vector in the
	 *         returned array must equal the size of the state vector with the
	 *         given stateExchangeItemID.
	 */
	public IVector[] getObservedLocalization(String stateExchangeItemID,
			IObservationDescriptions observationDescriptions, double distance) {
		// validateSingleObservationsExchangeItem(observationDescriptions);

		//TODO if multiple grids for current time, e.g. soilMoisture and evaporation, then the coordinates of these grids are present in sequence in observationDescriptions,
		//in that case need to figure out which of the observations correspond to the given stateExchangeItemID here.
		IVector observationXCoordinates = observationDescriptions.getValueProperties("x");
		IVector observationYCoordinates = observationDescriptions.getValueProperties("y");

		IExchangeItem stateExchangeItem = getDataObjectExchangeItem(stateExchangeItemID);
		IGeometryInfo stateGeometryInfo = stateExchangeItem.getGeometryInfo();

		return GeometryUtils.getLocalizationWeights(observationXCoordinates, observationYCoordinates,
				stateGeometryInfo, distance);
	}

	public void finish() {
		//write model state data after analysis (state update).
		analysisDataWriter.writeDataAfterAnalysis();
		analysisDataWriter.close();

		this.exchangeItems.clear();

		try {
			model.finalizeModel();
		} catch (BMIModelException e) {
			throw new RuntimeException(e);
		}
	}
}
