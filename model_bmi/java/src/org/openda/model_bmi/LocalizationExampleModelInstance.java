package org.openda.model_bmi;

import org.openda.interfaces.*;
import org.openda.utils.geometry.GeometryUtils;

import java.io.File;


/**
 * Example code for using localization with grids in a model instance.
 *
 * @author Arno Kockx
 */
@SuppressWarnings("UnusedDeclaration")
public class LocalizationExampleModelInstance implements IModelInstance, IModelExtensions {

	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * In other words this method returns a grid with values that would be observed by the satellite
	 * if reality would be equal to the current model state. This is needed, because, to compare the
	 * satellite observations with the model output, they should be defined on the same grid. The grid
	 * of the satellite has a different position, size and orientation than the grid of the model state.
	 * The values of the model state grid are interpolated to the observations grid using bilinear interpolation.
	 * For satellite observations the interpolation has to be done for each observation separately, since for each time step
	 * the satellite grid can be different, as the satellite moves along its orbit.
	 *
	 * @param observationDescriptions observation description
	 * @return Model prediction interpolated to each observation (location).
	 */
	@Override
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		//TODO if multiple grids for current time, e.g. soilMoisture and evaporation, then the coordinates of these grids are present in sequence in observationDescriptions,
		//in that case need to figure out which of the observations correspond to the given stateExchangeItemID here.
		IVector observationXCoordinates = observationDescriptions.getValueProperties("x");
		IVector observationYCoordinates = observationDescriptions.getValueProperties("y");

		//here only use the model exchangeItem that corresponds to the observed values, e.g. "SoilMoisture".
		IExchangeItem modelExchangeItem = getDataObjectExchangeItem("SoilMoisture");
		IGeometryInfo modelGeometryInfo = modelExchangeItem.getGeometryInfo();
		double[] modelValues = modelExchangeItem.getValuesAsDoubles();

		return GeometryUtils.getObservedValuesBilinearInterpolation(observationXCoordinates, observationYCoordinates, modelGeometryInfo, modelValues);
	}

	/**
	 * Returns the localization weights for each observation location.
	 *
	 * @param stateExchangeItemID id of the state vector for which the localization weights should be returned.
	 * @param observationDescriptions observation description
	 * @param distance characteristic distance for Cohn's formula
	 * @return weight vector for each observation location.
	 *         The size of the returned array must equal the number of observation locations in the given observationDescriptions.
	 *         The size of each vector in the returned array must equal the size of the state vector with the given stateExchangeItemID.
	 */
	public IVector[] getObservedLocalization(String stateExchangeItemID, IObservationDescriptions observationDescriptions, double distance) {
		//TODO if multiple grids for current time, e.g. soilMoisture and evaporation, then the coordinates of these grids are present in sequence in observationDescriptions,
		//in that case need to figure out which of the observations correspond to the given stateExchangeItemID here.
		IVector observationXCoordinates = observationDescriptions.getValueProperties("x");
		IVector observationYCoordinates = observationDescriptions.getValueProperties("y");

		IExchangeItem stateExchangeItem = getDataObjectExchangeItem(stateExchangeItemID);
		IGeometryInfo stateGeometryInfo = stateExchangeItem.getGeometryInfo();

		return GeometryUtils.getLocalizationWeights(observationXCoordinates, observationYCoordinates, stateGeometryInfo, distance);
	}

	/**
	 * Returns the localization weights for each observation location.
	 * This method assumes that there is only one state vector.
	 *
	 * @param observationDescriptions observation description
	 * @param distance characteristic distance for Cohn's formula
	 * @return weight vector for each observation location.
	 *         The size of the returned array must equal the number of observation locations in the given observationDescriptions.
	 *         The size of each vector in the returned array must equal the size of the state vector of the implementing modelInstance.
	 */
	@Override
	//this method is never called if this modelInstance implements the IModelExtensions interface.
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new UnsupportedOperationException(getClass().getName() + ".getObservedLocalization() not implemented.");
	}

	@Override
	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getExchangeItem() not implemented.");
	}

	@Override
	public ITime getTimeHorizon() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getTimeHorizon() not implemented.");
	}

	@Override
	public ITime getCurrentTime() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getCurrentTime() not implemented.");
	}

	@Override
	public void compute(ITime targetTime) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".compute() not implemented.");
	}

	@Override
	public IModelState saveInternalState() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".saveInternalState() not implemented.");
	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".restoreInternalState() not implemented.");
	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".releaseInternalState() not implemented.");
	}

	@Override
	public IModelState loadPersistentState(File persistentStateFile) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".loadPersistentState() not implemented.");
	}

	@Override
	public File getModelRunDir() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getModelRunDir() not implemented.");
	}

	@Override
	public IInstance getParent() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getParent() not implemented.");
	}

	@Override
	public String[] getExchangeItemIDs() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getExchangeItemIDs() not implemented.");
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getExchangeItemIDs() not implemented.");
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".getDataObjectExchangeItem() not implemented.");
	}

	@Override
	public void finish() {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".finish() not implemented.");
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		//TODO Auto-generated method stub
		throw new UnsupportedOperationException(getClass().getName() + ".initialize() not implemented.");
	}
}
