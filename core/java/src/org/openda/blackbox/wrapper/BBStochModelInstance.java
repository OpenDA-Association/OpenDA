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


package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.*;
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.utils.*;
import org.openda.utils.Vector;
import org.openda.utils.geometry.GeometryUtils;
import org.openda.utils.io.FileBasedModelState;
import org.openda.utils.performance.OdaGlobSettings;
import org.openda.utils.performance.OdaTiming;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

/**
 * Black box module's implementation of a stochastic model instance
 */
public class BBStochModelInstance extends Instance implements IStochModelInstance {
	private static Logger LOGGER = LoggerFactory.getLogger(BBStochModelInstance.class);

	// In case of parallel runs we use the Distributed counter to generate unique IDs
	static DistributedCounter lastGlobInstanceNr = new DistributedCounter();
	int InstanceNr;
	String ModelID;
	OdaTiming timerAxpyState  = null;
	OdaTiming timerCompute    = null;
	OdaTiming timerGetObs     = null;
	OdaTiming timerGetState   = null;

	List<BBCollectTimeSeriesExchangeItem> collectTimeSeriesBbExchangeItems = new ArrayList<BBCollectTimeSeriesExchangeItem>();

	private File configRootDir;
	protected IModelInstance model;
	protected BBStochModelVectorsConfig bbStochModelVectorsConfig;
	protected LinkedHashMap<String, SelectorInterface> selectors;
	protected LinkedHashMap<BBNoiseModelConfig, IStochModelInstance> noiseModels;
	protected LinkedHashMap<BBUncertOrArmaNoiseConfig, ArmaNoiseModel> armaNoiseModels;
	protected int[] stateNoiseModelsEndIndices = null;
	protected int[] stateVectorsEndIndices = null;
	private boolean doAutomaticNoiseGeneration = false;
	private String savedStatesDirPrefix = null;
	private String savedStatesNoiseModelPrefix = null;
	private String modelSavedStateFile = null;
	private UncertaintyEngine uncertaintyEngine = null;
	private IStochVector parameterUncertainty;
	private ITreeVector paramsTreeVector;
	private HashMap<String,Double> lastNoiseTimes;//avoid adding noise more than once
	private LinkedHashMap<IDataObject, ArrayList<BBBoundaryMappingConfig>> dataObjectBoundaryMappings;
	private int ensembleMemberIndex;
	private HashMap<String,double[]> prevNoiseModelEIValuesForTimeStep = new HashMap<String, double[]>();
	private boolean warningLogged = false;

	/**
	 * For each constraintExchangeItemId for which there is a range validation constraint this map contains
	 * the corresponding RangeValidationConstraint object.
	 */
	private Map<String, RangeValidationConstraint> constraintExchangeItemIdConstraintMap = new LinkedHashMap<String, RangeValidationConstraint>();
	/**
	 * All constraintExchangeItems that have been constructed so far are stored in this map.
	 * This list is filled lazily, i.e. a constraintExchangeItem with constraint(s) is constructed (by wrapping the affected exchangeItem)
	 * when it is first requested in method BBStochModelInstance.getExchangeItem. This is needed because
	 * some (e.g. output) exchangeItems are not initialized when the model has not run yet.
	 */
	private Map<String, IPrevExchangeItem> constraintExchangeItems = new LinkedHashMap<String, IPrevExchangeItem>();
    private IObservationDescriptions observationDescriptions;

	public BBStochModelInstance(File configRootDir, IModelInstance model,
			UncertaintyEngine uncertaintyEngine,
			IStochVector parameterUncertainty,
			ITreeVector paramsTreeVector,
			LinkedHashMap<BBNoiseModelConfig, IStochModelInstance> noiseModels,
			LinkedHashMap<IDataObject, ArrayList<BBBoundaryMappingConfig>> dataObjectBoundaryMappings,
			int ensembleMemberIndex,
			BBStochModelVectorsConfig bbStochModelVectorsConfig,
			String savedStatesDirPrefix,
			String savedStatesNoiseModelPrefix,
			String modelSavedStateFile
			) {
		/* Set modelID and instance Nr */
		synchronized(lastGlobInstanceNr){
			lastGlobInstanceNr.inc();
			InstanceNr = lastGlobInstanceNr.val();
			ModelID="BB Model:"+InstanceNr;
		}

		this.configRootDir = configRootDir;
		this.model = model;
		this.uncertaintyEngine = uncertaintyEngine;
		this.parameterUncertainty = parameterUncertainty;
		this.paramsTreeVector = paramsTreeVector;
		this.noiseModels = noiseModels;
		this.bbStochModelVectorsConfig = bbStochModelVectorsConfig;
		this.savedStatesDirPrefix = savedStatesDirPrefix;
		this.savedStatesNoiseModelPrefix = savedStatesNoiseModelPrefix;
		this.modelSavedStateFile = modelSavedStateFile;
		this.dataObjectBoundaryMappings = dataObjectBoundaryMappings;
		this.ensembleMemberIndex = ensembleMemberIndex;
		selectors = new LinkedHashMap<String, SelectorInterface>();
		armaNoiseModels = new LinkedHashMap<BBUncertOrArmaNoiseConfig, ArmaNoiseModel>();
		createStateNoiseModels();
		this.lastNoiseTimes=new HashMap<String,Double>(); //avoid adding noise more than once

		//prepare index for range constraints
		RangeValidationConstraint[] rangeValidationConstraints = this.bbStochModelVectorsConfig.getRangeValidationConstraints();
		if (rangeValidationConstraints != null) {
			initConstraintsExchangeItemIds(rangeValidationConstraints);
		}

		if (dataObjectBoundaryMappings.size() > 0) {
			processProvidedBoundaries(this.dataObjectBoundaryMappings, this.ensembleMemberIndex);
		}
	}

	private void processProvidedBoundaries(LinkedHashMap<IDataObject, ArrayList<BBBoundaryMappingConfig>> boundaryProviderConfigs, int ensembleMemberIndex) {
		// Process each boundaryProvider in the BlackBoxStochModelConfig.
		for (IDataObject dataObject: boundaryProviderConfigs.keySet()) {
			// Each boundaryProvider specifies 1 or more boundaryMappings.
			for (BBBoundaryMappingConfig mappingConfig: boundaryProviderConfigs.get(dataObject)) {
				// A boundaryMapping specifies the operation with which boundaryExchangeItems are applied to modelExchangeItems.
				int operationType = mappingConfig.getOperationType();

				// A boundaryMapping can (1) specify exchangeItem mappings or (2) imply a 1:1 mapping when absent.
				Map<String, String> mappingExchangeItems = mappingConfig.getMappingExchangeItems();

				if (mappingExchangeItems.size() > 0) {
					// (1) Use the boundaryMapping exchangeItem specification(s).
					for (String boundaryExchangeItemId : mappingExchangeItems.keySet()) {
						String modelExchangeItemId = mappingExchangeItems.get(boundaryExchangeItemId);

						// Retrieve the boundary exchangeItem.
						IPrevExchangeItem boundaryExchangeItem = null;
						if (dataObject instanceof IEnsembleDataObject) {
							for (int aMemberIndex: ((IEnsembleDataObject) dataObject).getEnsembleMemberIndices()){
								if (aMemberIndex == ensembleMemberIndex) {
									boundaryExchangeItem = ((IEnsembleDataObject) dataObject).getDataObjectExchangeItem(boundaryExchangeItemId, ensembleMemberIndex);
									break;
								}
							}
						} else {
							boundaryExchangeItem = dataObject.getDataObjectExchangeItem(boundaryExchangeItemId);
						}
						if (boundaryExchangeItem == null) break;

						// Retrieve the model exchangeItem.
						IPrevExchangeItem modelExchangeItem = model.getExchangeItem(modelExchangeItemId);
						performOperation(operationType, boundaryExchangeItem, modelExchangeItem);
					}
				} else {
					// (2) No boundaryMapping::exchangeItem defined: assume 1:1 name-mapping between boundaryProvider::dataObject.getExchangeItemIDs and modelExchangeItemIDs.
					if (dataObject instanceof IEnsembleDataObject) {
						for (String boundaryExchangeItemId: ((IEnsembleDataObject) dataObject).getEnsembleExchangeItemIds()) {
							if (Arrays.asList(((IEnsembleDataObject) dataObject).getEnsembleMemberIndices()).contains(ensembleMemberIndex)) {
								IPrevExchangeItem boundaryExchangeItem = ((IEnsembleDataObject) dataObject).getDataObjectExchangeItem(boundaryExchangeItemId, ensembleMemberIndex);
								IPrevExchangeItem modelExchangeItem = model.getExchangeItem(boundaryExchangeItemId);
								performOperation(operationType, boundaryExchangeItem, modelExchangeItem);
							}
						}
					} else {
						for (String boundaryExchangeItemId: dataObject.getExchangeItemIDs()) {
							IPrevExchangeItem boundaryExchangeItem = dataObject.getDataObjectExchangeItem(boundaryExchangeItemId);
							IPrevExchangeItem modelExchangeItem = model.getExchangeItem(boundaryExchangeItemId);
							performOperation(operationType, boundaryExchangeItem, modelExchangeItem);
						}
					}
				}
			}
		}
	}

	private void performOperation(int operationType, IPrevExchangeItem boundaryExchangeItem, IPrevExchangeItem modelExchangeItem) {
		// TODO Evaluate assumption that the model run is configured for the same time period as the provided boundaries.
		if (boundaryExchangeItem instanceof IGridTimeSeriesExchangeItem && modelExchangeItem instanceof IGridTimeSeriesExchangeItem) {
			double[] times = boundaryExchangeItem.getTimes();
			for (int aTimeIndex=0; aTimeIndex<times.length; aTimeIndex++){
				double[] boundaryExchangeItemValues = ((IGridTimeSeriesExchangeItem)boundaryExchangeItem).getValuesAsDoublesForSingleTimeIndex(aTimeIndex);
				switch (operationType) {
					case BBRegularisationConstantConfig.OPERATION_ADD:
						((IGridTimeSeriesExchangeItem)modelExchangeItem).axpyOnValuesForSingleTimeIndex(aTimeIndex, 1.0d, boundaryExchangeItemValues);
						break;
					case BBRegularisationConstantConfig.OPERATION_MULTIPLY:
						((IGridTimeSeriesExchangeItem)modelExchangeItem).multiplyValuesForSingleTimeIndex(aTimeIndex, boundaryExchangeItemValues);
						break;
					case BBRegularisationConstantConfig.OPERATION_SET:
						((IGridTimeSeriesExchangeItem)modelExchangeItem).setValuesAsDoublesForSingleTimeIndex(aTimeIndex, boundaryExchangeItemValues);
						break;
				}
			}
		}
		else if (boundaryExchangeItem instanceof IGridTimeSeriesExchangeItem && !(modelExchangeItem instanceof IGridTimeSeriesExchangeItem)) {
			double currentTime = modelExchangeItem.getTimes()[0];
			double[] times = boundaryExchangeItem.getTimes();
			for (int aTimeIndex=0; aTimeIndex<times.length; aTimeIndex++) {
				if (currentTime == times[aTimeIndex]) {
					double[] boundaryExchangeItemValues = ((IGridTimeSeriesExchangeItem)boundaryExchangeItem).getValuesAsDoublesForSingleTimeIndex(aTimeIndex);
					switch (operationType) {
						case BBRegularisationConstantConfig.OPERATION_ADD:
							modelExchangeItem.axpyOnValues(1.0d, boundaryExchangeItemValues);
							break;
						case BBRegularisationConstantConfig.OPERATION_MULTIPLY:
							modelExchangeItem.multiplyValues(boundaryExchangeItemValues);
							break;
						case BBRegularisationConstantConfig.OPERATION_SET:
							modelExchangeItem.setValuesAsDoubles(boundaryExchangeItemValues);
							break;
					}
				}
			}
		}
		else if (!(boundaryExchangeItem instanceof IGridTimeSeriesExchangeItem) && !(modelExchangeItem instanceof IGridTimeSeriesExchangeItem)) {
			double[] boundaryExchangeItemValues = boundaryExchangeItem.getValuesAsDoubles();
			if (modelExchangeItem.getTimes().length != boundaryExchangeItemValues.length) {
				throw new RuntimeException("Unequal number of values in model and boundary exchange items.");
			}
			switch (operationType) {
				case BBRegularisationConstantConfig.OPERATION_ADD:
					modelExchangeItem.axpyOnValues(1.0d, boundaryExchangeItemValues);
					break;
				case BBRegularisationConstantConfig.OPERATION_MULTIPLY:
					modelExchangeItem.multiplyValues(boundaryExchangeItemValues);
					break;
				case BBRegularisationConstantConfig.OPERATION_SET:
					modelExchangeItem.setValuesAsDoubles(boundaryExchangeItemValues);
					break;
			}
		}
		else {
			throw new UnsupportedOperationException(getClass().getSimpleName() + "Model and Boundary ExchangeItems do not match in shape.");
		}
	}

	/**
	 * Initializes this.constraintExchangeItemConstraintMap.
	 */
	private void initConstraintsExchangeItemIds(RangeValidationConstraint[] rangeValidationConstraints) {
		for (RangeValidationConstraint constraint: rangeValidationConstraints) {
			this.constraintExchangeItemIdConstraintMap.put(constraint.getConstraintExchangeItemId(), constraint);
		}
	}

	//
	// ModelInstance Functions
	//

	public ITime getTimeHorizon() {
		return model.getTimeHorizon();
	}

	public ITime getCurrentTime() {
		return model.getCurrentTime();
	}

	public void compute(ITime targetTime) {
		if ( timerCompute == null){
			timerCompute = new OdaTiming(ModelID);
		}
		//System.out.println("Compute from "+Thread.currentThread().getStackTrace()[1].getClassName()+":"+Thread.currentThread().getStackTrace()[2].getClassName());
		timerCompute.start();

		// update times in case of black box model
	    if(model instanceof BBModelInstance) {
			BBModelInstance bbModelInstance = (BBModelInstance) model;
			if (bbModelInstance.getAliasDefinitions().containsKey("currentTime")) {
				bbModelInstance.getAliasDefinitions().setAliasValue(
						"currentTime", TimeUtils.mjdToString(getCurrentTime().getMJD()));
			}
			if (targetTime != null) {
				if (bbModelInstance.getAliasDefinitions().containsKey("targetTime")) {
					bbModelInstance.getAliasDefinitions().setAliasValue(
							"targetTime", TimeUtils.mjdToString(targetTime.getMJD()));
				}
			}
		}

		if (this.bbStochModelVectorsConfig.isCollectPredictorTimeSeries()) {

            double modelDeltaT = model.getTimeHorizon().getStepMJD();
            double tolerance = 1d / 24d / 60d / 2; // half a minute (expressed as MJD). This is consistent with the one used in getObservedValues.
            boolean firstStep = true;

			processProvidedBoundaries(this.dataObjectBoundaryMappings, this.ensembleMemberIndex); // TODO Not necessary for scalar time series.
			boolean addNoiseToExchangeItemsAfterCompute = propagateNoiseModelsAndAddNoiseToExchangeItems(model.getCurrentTime(), targetTime, false);
			while (model.getCurrentTime().getMJD()+tolerance < targetTime.getMJD()) {
				ITime loopTimeStep = new Time(model.getCurrentTime().getMJD() + modelDeltaT);
				model.compute(loopTimeStep);
                if (firstStep && observationDescriptions!=null) {
                    for (IPrevExchangeItem predictorExchangeItem : observationDescriptions.getExchangeItems()) {
                        collectTimeSeriesBbExchangeItems.add(new BBCollectTimeSeriesExchangeItem(predictorExchangeItem));
                    }
                    firstStep = false;
                }

				for (BBCollectTimeSeriesExchangeItem wrappedBbExchangeItem : collectTimeSeriesBbExchangeItems) {
                    BBStochModelVectorConfig vectorConfig = findPredictionVectorConfig(wrappedBbExchangeItem.getId());
                    IPrevExchangeItem sourceExchangeItem = getExchangeItem(vectorConfig.getSourceId());
                    if (sourceExchangeItem == null) {
                        throw new RuntimeException("BBStochModelInstance.setParameters(): parameter not found: " +
                                vectorConfig.getSourceId());
                    }
                    IPrevExchangeItem mappedExchangeItem = new BBExchangeItem(vectorConfig.getId(), vectorConfig,
                            sourceExchangeItem, selectors, configRootDir);
                    double[] computedValues = mappedExchangeItem.getValuesAsDoubles();
					wrappedBbExchangeItem.UpdateTimeStepValue(loopTimeStep,computedValues[0]);
				}
				if (addNoiseToExchangeItemsAfterCompute) propagateNoiseModelsAndAddNoiseToExchangeItems(model.getCurrentTime(), targetTime, true);
			}
		} else
		{
			processProvidedBoundaries(this.dataObjectBoundaryMappings, this.ensembleMemberIndex); // TODO Not necessary for scalar time series.
			boolean addNoiseToExchangeItemsAfterCompute = propagateNoiseModelsAndAddNoiseToExchangeItems(model.getCurrentTime(), targetTime, false);
			model.compute(targetTime);
			if (addNoiseToExchangeItemsAfterCompute) propagateNoiseModelsAndAddNoiseToExchangeItems(model.getCurrentTime(), targetTime, true);
		}

		//ODA-558 Clear exchange items otherwise exchange items from previous compute will be used without newly added noise
		this.constraintExchangeItems.clear();
		timerCompute.stop();
	}

	public String[] getExchangeItemIDs() {
		ArrayList<String> list = new ArrayList<String>();
		list.addAll(this.constraintExchangeItemIdConstraintMap.keySet());
		list.addAll(Arrays.asList(model.getExchangeItemIDs()));
		return list.toArray(new String[list.size()]);
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		ArrayList<String> list = new ArrayList<String>();
		//TODO only add constraintExchangeItems for the given role. AK
		list.addAll(this.constraintExchangeItemIdConstraintMap.keySet());
		list.addAll(Arrays.asList(model.getExchangeItemIDs(role)));
		return list.toArray(new String[list.size()]);
	}

	/**
	 * Get exchangeItems through new IDataObject interface. This is inherited through IModelInstance.
	 * NOTE: all methods in the stochmodel now use the OLD interface.
	 * @param exchangeItemId
	 * @return exchangeItem
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId) {
		IPrevExchangeItem item = this.getExchangeItem(exchangeItemId);
		if(item instanceof IExchangeItem){
			return (IExchangeItem)item;
		}else{
			throw new RuntimeException("BBStochModel.getDataObjectExchangeItem: item "+exchangeItemId+" is not of type IExchangeItem");
		}
	}

	/**
	 * Get ExchangeItems through old interface of IModelInstance. 
	 * NOTE: all methods in the stochmodel now use the OLD interface. TODO move to new interface.
	 */
	public IPrevExchangeItem getExchangeItem(String exchangeItemId) {
		//check if exchangeItem with given id is a constraintExchangeItem and has already been constructed.
		IPrevExchangeItem constraintExchangeItem = this.constraintExchangeItems.get(exchangeItemId);
		if (constraintExchangeItem != null) {
			return constraintExchangeItem;
		}

		//check if exchangeItem with given id is a constraintExchangeItem, but has not been constructed yet.
		RangeValidationConstraint constraint = this.constraintExchangeItemIdConstraintMap.get(exchangeItemId);
		if (constraint != null) {
			//construct constraintExchangeItem.
			//the wrapping constraintExchangeItem always has a different unique id from the original wrapped exchangeItem.
			IPrevExchangeItem affectedExchangeItem = getExchangeItem(constraint.getAffectedExchangeItemId());
			constraintExchangeItem = constraint.wrapAffectedExchangeItem(affectedExchangeItem);
			this.constraintExchangeItems.put(exchangeItemId, constraintExchangeItem);
			return constraintExchangeItem;
		}

		//if no constraints for exchangeItem.
		return getExchangeItemDirectlyFromModel(exchangeItemId);
	}

	private IPrevExchangeItem getExchangeItemDirectlyFromModel(String exchangeItemId) {
		IPrevExchangeItem exchangeItem = this.model.getExchangeItem(exchangeItemId);

		if (exchangeItem == null) {
			String allItems = "";
			for (String availableExchangeItemId : this.getExchangeItemIDs()) {
				allItems += "   " + availableExchangeItemId + "\n";
			}
			throw new RuntimeException(getClass().getSimpleName() + ".getExchangeItem:\n"
					+ " Exchange item with id '" + exchangeItemId + "' not found.\n"
					+ " Existing exchange items are:\n"
					+ allItems + "\n");
		}

		return exchangeItem;
	}

	public ITreeVector getState() {

		if ( timerGetState == null){
			timerGetState = new OdaTiming(ModelID);
		}
		timerGetState.start();

		TreeVector stateTreeVector = new TreeVector("state", "State From Black Box Stoch Model Instance");

		Collection<BBNoiseModelConfig> noiseModelConfigs =
				this.bbStochModelVectorsConfig.getStateConfig().getNoiseModelConfigs();
		Collection<BBUncertOrArmaNoiseConfig> uncertaintyOrArmaNoiseConfigs =
				this.bbStochModelVectorsConfig.getStateConfig().getUncertaintyOrArmaNoiseConfigs();

		stateNoiseModelsEndIndices = new int[noiseModelConfigs.size()+uncertaintyOrArmaNoiseConfigs.size()];

		int i = 0;

		// add external noise model variables to state
		for (BBNoiseModelConfig noiseModelConfig : noiseModelConfigs) {
			IStochModelInstance noiseModel = noiseModels.get(noiseModelConfig);
			IVector noiseModelState = noiseModel.getState();
			//add this part to state
			if(noiseModelState instanceof ITreeVector){
				stateTreeVector.addChild((ITreeVector)noiseModelState);
			}else{
				String id = "noise_part_"+i;
				ITreeVector tv = new TreeVector(id, noiseModelState);
				stateTreeVector.addChild(tv);
			}
			//keep count of lengths for later use
			stateNoiseModelsEndIndices[i] = noiseModelState.getSize();
			if (i > 0) {
				stateNoiseModelsEndIndices[i] += stateNoiseModelsEndIndices[i - 1];
			}
			i++;
		}

		// add blackbox internal noise model contributions to the state vector
		ITime currentTime = getCurrentTime();
		for (BBUncertOrArmaNoiseConfig noiseModelStateNoiseConfig : uncertaintyOrArmaNoiseConfigs) {
			ArmaNoiseModel noiseModel = armaNoiseModels.get(noiseModelStateNoiseConfig);
			if (noiseModelStateNoiseConfig.getNoiseModelType() !=
					BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItem) {
				double[] noiseStateVector = noiseModel.getNoiseStateVector(currentTime);
				stateTreeVector.addChild(noiseModelStateNoiseConfig.getId(), noiseStateVector);

				stateNoiseModelsEndIndices[i] = noiseStateVector.length;
				if (i > 0) {
					stateNoiseModelsEndIndices[i] += stateNoiseModelsEndIndices[i - 1];
				}
			}
			i++;
		}

		// add deterministic mode variables to state vector
		Collection<BBStochModelVectorConfig> vectorCollection =
				this.bbStochModelVectorsConfig.getStateConfig().getVectorCollection();

		stateVectorsEndIndices = new int[vectorCollection.size()];
		i = 0;
		for (BBStochModelVectorConfig vectorConfig : vectorCollection) {
			double[] values = getExchangeItem(vectorConfig.getId()).getValuesAsDoubles();
			stateTreeVector.addChild(vectorConfig.getId(), values);

			stateVectorsEndIndices[i] = values.length;
			if (i > 0) {
				stateVectorsEndIndices[i] += stateVectorsEndIndices[i - 1];
			}
			i++;
		}
		timerGetState.stop();
		return stateTreeVector;
	}

	public void axpyOnState(double alpha, IVector vector) {

		//TODO If vector is a treevector we can look at the children 
		//without asking for values. If not we can ask for the state and
		//overwrite the values. Also then we can look at the children.
		
		
		if ( timerAxpyState == null){
			timerAxpyState = new OdaTiming(ModelID);
		}
		timerAxpyState.start();

		if (stateVectorsEndIndices == null) {
			// no state sizes known, get state first
			getState();
		}

		double[] axpyValues = vector.getValues();

		int i = 0;
		int j = 0;

		Collection<BBNoiseModelConfig> noiseModelConfigs =
				this.bbStochModelVectorsConfig.getStateConfig().getNoiseModelConfigs();
		for (BBNoiseModelConfig noiseModelConfig : noiseModelConfigs) {
			IStochModelInstance noiseModel = noiseModels.get(noiseModelConfig);
			IVector noiseModelState = noiseModel.getState();
			double[] partOfAxpyValues = new double[noiseModelState.getSize()];
			System.arraycopy(axpyValues, j, partOfAxpyValues, 0, partOfAxpyValues.length);
			j+=partOfAxpyValues.length;
			IVector partOfAxpyValuesVector = new Vector(partOfAxpyValues);
			noiseModel.axpyOnState(alpha, partOfAxpyValuesVector);
		}


		ITime currentTime = getCurrentTime();

		Collection<BBUncertOrArmaNoiseConfig> stateNoiseModelCollection =
				this.bbStochModelVectorsConfig.getStateConfig().getUncertaintyOrArmaNoiseConfigs();
		for (BBUncertOrArmaNoiseConfig stateNoiseModelConfig : stateNoiseModelCollection) {
			if (stateNoiseModelConfig.getNoiseModelType() !=
					BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItem) {
				ArmaNoiseModel noiseModel = armaNoiseModels.get(stateNoiseModelConfig);
				if (noiseModel == null) {
					throw new RuntimeException("Noise model not created: " + stateNoiseModelConfig.getId());
				}
				double[] noiseStateVector = noiseModel.getNoiseStateVector(currentTime);
				for (int k = 0; k < noiseStateVector.length; k++) {
					noiseStateVector[k] += alpha * axpyValues[j++];
				}
				if (!(j == stateNoiseModelsEndIndices[i])) {
					throw new RuntimeException("Inconsistent noise model sizes: " + stateNoiseModelConfig.getId());
				}
				noiseModel.setNoiseStateVector(currentTime, noiseStateVector);
			}
			i++;
		}

		Collection<BBStochModelVectorConfig> vectorCollection =
				this.bbStochModelVectorsConfig.getStateConfig().getVectorCollection();
		i = 0;
		for (BBStochModelVectorConfig vectorConfig : vectorCollection) {
			int start = (i > 0) ? stateVectorsEndIndices[i - 1] : 0;
			int subSize = stateVectorsEndIndices[i] - start;
			double[] values = new double[subSize];
			System.arraycopy(axpyValues, j + start, values, 0, subSize);
			getExchangeItem(vectorConfig.getId()).axpyOnValues(alpha, values);
			i++;
		}
		timerAxpyState.stop();
	}

	public ITreeVector getParameters() {
		return paramsTreeVector;
	}

	public void setParameters(IVector parameters) {

		if (model instanceof BBModelInstance) {
			if (!((BBModelInstance) model).isNewDirectory()) return;
		}

		if (!(parameters instanceof ITreeVector)) {
			throw new RuntimeException("BBStochModelInstance.setParameters(): unexpected vector type: " +
					parameters.getClass().getName());
		}

		for (BBRegularisationConstantConfig regularisationConstantConfig : bbStochModelVectorsConfig.getRegularisationConstantCollection()) {
			String parameterId = BBStochModelFactory.composeRelatedParametersId(regularisationConstantConfig);
			IVector paramChild = getAndCheckParamChild(parameters, parameterId);
			double parameterDelta = paramChild.getValue(0) * regularisationConstantConfig.getScale();

			for (BBStochModelVectorConfig stochModelVectorConfig : regularisationConstantConfig.getVectorConfigs()) {
				IPrevExchangeItem sourceExchangeItem = getExchangeItem(stochModelVectorConfig.getSourceId());
				if (sourceExchangeItem == null) {
					throw new RuntimeException("BBStochModelInstance.setParameters(): parameter not found: " +
							stochModelVectorConfig.getSourceId());
				}
				IPrevExchangeItem exchangeItem = new BBExchangeItem(stochModelVectorConfig.getId(), stochModelVectorConfig,
						sourceExchangeItem, selectors, configRootDir);

				addParameterDeltaToExchangeItem(parameterDelta, exchangeItem,
						regularisationConstantConfig.getTransformation());
			}
		}

		for (BBCartesianToPolarConfig cartesianToPolarConfig : bbStochModelVectorsConfig.getCartesianToPolarCollection()) {

			String dxID = BBStochModelFactory.composeCartesionToPolarParameterId(cartesianToPolarConfig, false);
			String dyID = BBStochModelFactory.composeCartesionToPolarParameterId(cartesianToPolarConfig, true);
			IVector paramChildDeltaX = getAndCheckParamChild(parameters, dxID);
			IVector paramChildDeltaY = getAndCheckParamChild(parameters, dyID);

			double deltaX = paramChildDeltaX.getValue(0) * cartesianToPolarConfig.getXScale();
			double deltaY = paramChildDeltaY.getValue(0) * cartesianToPolarConfig.getYScale();

			int vectorConfigNr = 0;
			while (vectorConfigNr < cartesianToPolarConfig.getVectorConfigs().size()) {
				IPrevExchangeItem radiusExchangeItem = getExchangeItem(
						cartesianToPolarConfig.getVectorConfigs().get(vectorConfigNr).getSourceId());
				if (radiusExchangeItem == null) {
					throw new RuntimeException("BBStochModelInstance.setParameters(): radius parameter not found: " +
							cartesianToPolarConfig.getVectorConfigs().get(vectorConfigNr).getSourceId());
				}
				IPrevExchangeItem angleExchangeItem = getExchangeItem(
						cartesianToPolarConfig.getVectorConfigs().get(vectorConfigNr + 1).getSourceId());
				if (angleExchangeItem == null) {
					throw new RuntimeException("BBStochModelInstance.setParameters(): angle parameter not found: " +
							cartesianToPolarConfig.getVectorConfigs().get(vectorConfigNr + 1).getSourceId());
				}

				double radius = radiusExchangeItem.getValuesAsDoubles()[0];
				double angle = angleExchangeItem.getValuesAsDoubles()[0];
				double angleInRadians = angle * Math.PI / 180d;

				if (Double.compare(radius, 0d) == 0) {
					throw new RuntimeException("BBStochModelInstance.setParameters(): " +
							"cartesian to polar transformation allowed for radius == 0: " +
							cartesianToPolarConfig.getVectorConfigs().get(vectorConfigNr).getSourceId());
				}

				double xCoord = Math.cos(angleInRadians) * radius;
				double yCoord = Math.sin(angleInRadians) * radius;

				double adjustedXCoord = xCoord + deltaX;
				double adjustedYCoord = yCoord + deltaY;

				double adjustedRadius = Math.sqrt(Math.pow(adjustedXCoord, 2) + Math.pow(adjustedYCoord, 2));
				double adjustedAngleInRadians = Math.atan2(adjustedYCoord, adjustedXCoord);
				if (adjustedAngleInRadians < 0) adjustedAngleInRadians += 2 * Math.PI;
				double adjustedAngle = adjustedAngleInRadians * 180d / Math.PI;

				radiusExchangeItem.setValuesAsDoubles(new double[]{adjustedRadius});
				angleExchangeItem.setValuesAsDoubles(new double[]{adjustedAngle});

				vectorConfigNr += 2;
			}
		}

		for (BBNoiseModelConfig noiseConfig : bbStochModelVectorsConfig.getParamsUncertaintyModelConfigs()) {
			for (NoiseModelExchangeItemConfig exchangeItemConfig : noiseConfig.getExchangeItemConfigs()) {
				IVector paramChild = getAndCheckParamChild(parameters, exchangeItemConfig.getId());
				for (String modelExchangeItemId : exchangeItemConfig.getModelExchangeItemIds()) {
					IPrevExchangeItem exchangeItem = getExchangeItem(modelExchangeItemId);
					if (exchangeItem == null) {
						throw new RuntimeException("BBStochModelInstance.setParameters(): parameter not found: " +
								modelExchangeItemId);
					}
					addParameterDeltaToExchangeItem(paramChild.getValue(0), exchangeItem,
							exchangeItemConfig.getTransformation());
				}
			}
		}
	}

	public void axpyOnParameters(double alpha, IVector vector) {
		IVector parameters = getParameters();
		parameters.axpy(alpha, vector);
		setParameters(parameters);
	}

	public IModelState saveInternalState() {
		File dirForRestartFiles = checkRestartDir(getCurrentTime(), false);
        FileBasedModelState stochSavedModelState = new FileBasedModelState();
        stochSavedModelState.setDirContainingModelstateFiles(dirForRestartFiles);
		int i = 0;
		for (Map.Entry<BBUncertOrArmaNoiseConfig, ArmaNoiseModel> noiseModelEntry : this.armaNoiseModels.entrySet()) {
			ArmaNoiseModel noiseModel = noiseModelEntry.getValue();
			File noiseModelStateFile = new File(dirForRestartFiles, this.savedStatesNoiseModelPrefix + i + ".txt");
			noiseModel.saveState(getCurrentTime(), noiseModelStateFile);
			stochSavedModelState.addFile(noiseModelStateFile);
			i++;
		}
		for (Map.Entry<BBNoiseModelConfig, IStochModelInstance> noiseModelEntry : noiseModels.entrySet()) {
			IStochModelInstance noiseModel = noiseModelEntry.getValue();
			File noiseModelStateFile = new File(dirForRestartFiles, this.savedStatesNoiseModelPrefix + i + ".txt");
			IModelState noiseModelState = noiseModel.saveInternalState();
			noiseModelState.savePersistentState(noiseModelStateFile);
			stochSavedModelState.addFile(noiseModelStateFile);
			i++;
		}
		IModelState savedModelState = model.saveInternalState();
		File modelRestartStateFile = new File(dirForRestartFiles, this.modelSavedStateFile);
		savedModelState.savePersistentState(modelRestartStateFile);
		model.releaseInternalState(savedModelState);
		stochSavedModelState.addFile(modelRestartStateFile);
        if (modelRestartStateFile.getName().toLowerCase().endsWith(".zip")) {
            stochSavedModelState.setZippedStateFile(modelRestartStateFile);
        }
		return stochSavedModelState;
	}

	public void restoreInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
					" for " + this.getClass().getName() + ".releaseInternalState");
		}
		FileBasedModelState stochModelState = (FileBasedModelState) savedInternalState;
		File dirForRestartFiles = checkRestartDir(getCurrentTime(), false);
        File incomingStateDir = stochModelState.getDirContainingModelStateFiles();

        // the particle filter exchanges full states. Copy a full state dir from another ensemble member
        // if the incoming state is not the state of the current stoch.model instance.
        if (!incomingStateDir.equals(dirForRestartFiles)) {
            if (dirForRestartFiles.exists()) {
                dirForRestartFiles.delete();
            }
            BBUtils.copyDirectory(incomingStateDir, dirForRestartFiles,false);
        }
		stochModelState.setDirContainingModelstateFiles(dirForRestartFiles);
		stochModelState.restoreState();
		int i = 0;
		for (Map.Entry<BBUncertOrArmaNoiseConfig, ArmaNoiseModel> noiseModelEntry : this.armaNoiseModels.entrySet()) {
			ArmaNoiseModel noiseModel = noiseModelEntry.getValue();
			File noiseModelStateFile = new File(dirForRestartFiles, this.savedStatesNoiseModelPrefix + i + ".txt");
			noiseModel.loadState(noiseModelStateFile);
			i++;
		}
		for (Map.Entry<BBNoiseModelConfig, IStochModelInstance> noiseModelEntry : this.noiseModels.entrySet()) {
			IStochModelInstance noiseModel = noiseModelEntry.getValue();
			File noiseModelStateFile = new File(dirForRestartFiles, this.savedStatesNoiseModelPrefix + i + ".txt");
			IModelState noiseModelState = noiseModel.loadPersistentState(noiseModelStateFile);
			noiseModel.restoreInternalState(noiseModelState);
			i++;
		}
		File modelRestartStateFile = new File(dirForRestartFiles, this.modelSavedStateFile);
		model.restoreInternalState(model.loadPersistentState(modelRestartStateFile));
	}

	public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
					" for " + this.getClass().getName() + ".releaseInternalState");
		}
		File dirForRestartFiles = checkRestartDir(getCurrentTime(), false);
		FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
		modelState.releaseState(dirForRestartFiles);
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		File dirForRestartFiles = checkRestartDir(getCurrentTime(), false);
		return FileBasedModelState.loadPersistenState(persistentStateFile, dirForRestartFiles);
	}

	//
	// StochModelInstance Functions
	//

	public IStochVector getStateUncertainty() {

		if (stateVectorsEndIndices == null) {
			// no state sizes known, get state first
			getState();
		}

		// state size is equal to the last sub index
		if (stateNoiseModelsEndIndices.length<1){
			System.out.println("Warning no noise model specified but the method getStateUncertainty is called. Pleae check whether this is correct.");
		   return null;
		}

		int fullStateSize = 0;
		if (stateNoiseModelsEndIndices.length > 0) {
			fullStateSize += stateNoiseModelsEndIndices[stateNoiseModelsEndIndices.length - 1];
		}
		if (stateVectorsEndIndices.length > 0) {
			fullStateSize += stateVectorsEndIndices[stateVectorsEndIndices.length - 1];
		}

		Collection<BBUncertOrArmaNoiseConfig> stateNoiseModelCollection =
				this.bbStochModelVectorsConfig.getStateConfig().getUncertaintyOrArmaNoiseConfigs();
		ArrayList<double[]> stdDevList = new ArrayList<double[]>();
		for (BBUncertOrArmaNoiseConfig stateNoiseModelConfig : stateNoiseModelCollection) {
			int noiseStateSize = 1; // TODO elaborate
			double[] stdDevs = new double[noiseStateSize];
			for (int i = 0; i < stdDevs.length; i++) {
				stdDevs[i] = stateNoiseModelConfig.getStdDev();
			}
			stdDevList.add(stdDevs);
		}
		double[] means = new double[fullStateSize];  // value = 0
		double[] stdDevs = new double[fullStateSize];
		int copyPos = 0;
		for (double[] boundaryStdDevs : stdDevList) {
			int count = boundaryStdDevs.length;
			System.arraycopy(boundaryStdDevs, 0, stdDevs, copyPos, count);
			copyPos += count;
		}
		return new StochVector(means, stdDevs);
	}


	public IStochVector getParameterUncertainty() {
		return parameterUncertainty;
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.getWhiteNoiseUncertainty(): Not implemented yet.");
	}

	public boolean isWhiteNoiseStationary() {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.isWhiteNoiseStationary(): Not implemented yet.");
	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.getWhiteNoiseTimes(): Not implemented yet.");
	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.getWhiteNoise(): Not implemented yet.");
	}

	public void setWhiteNoise(IVector whiteNoise[]) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.setWhiteNoise(): Not implemented yet.");
	}

	public void axpyOnWhiteNoise(double alpha, IVector vector[]) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.axpyOnWhiteNoise(): Not implemented yet.");
	}

	public void setAutomaticNoiseGeneration(boolean value) {
		this.doAutomaticNoiseGeneration = value;
		for (IStochModelInstance noiseModel : noiseModels.values()) {
			noiseModel.setAutomaticNoiseGeneration(value);
		}
	}

	/**
	 * Get model values corresponding to a number of observations
	 * This returns what the observations would look like, if reality would be equal to the current stoch model state.
	 *
     * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
     * @return vector with the model values corresponding to each observation given in the descriptions
	 */
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {

			if (model instanceof IModelExtensions){
				IModelExtensions modelExtended = (IModelExtensions) model;
				IVector retvals=modelExtended.getObservedValues(observationDescriptions);
				if (retvals!=null){
		           return retvals;
				}
				else {
					return getObservedValuesBB(observationDescriptions);
				}
			}
			else {
				return getObservedValuesBB(observationDescriptions);
			}
		}

	/**
	 * Returns the values that would be observed, if reality would be equal to the current model state.
	 */
	private IVector getObservedValuesBB(IObservationDescriptions observationDescriptions) {
		if ( timerGetObs == null){
			timerGetObs = new OdaTiming(ModelID);
		}
		timerGetObs.start();

		TreeVector treeVector = new TreeVector("predictions");
		String errorMessage = "";
		for (IPrevExchangeItem observationExchangeItem : observationDescriptions.getExchangeItems()) {
			//get modelExchangeItem that corresponds to current observationExchangeItem.
			BBStochModelVectorConfig vectorConfig = findPredictionVectorConfig(observationExchangeItem.getId());
            IPrevExchangeItem modelExchangeItem = null;
            if (this.bbStochModelVectorsConfig.isCollectPredictorTimeSeries()) {
                for (int i=0; i< collectTimeSeriesBbExchangeItems.size(); i++){
                    if (collectTimeSeriesBbExchangeItems.get(i).getId().contentEquals(observationExchangeItem.getId())){
                        modelExchangeItem = collectTimeSeriesBbExchangeItems.get(i);
                        vectorConfig = new BBStochModelVectorConfig(modelExchangeItem.getId(),modelExchangeItem.getId(),null,null);
                        break;
                    }
                }
            } else {
			    modelExchangeItem = getExchangeItem(vectorConfig.getSourceId());
            }
			if (modelExchangeItem == null) {
				errorMessage += "\n\tExchange item not found: " + vectorConfig.getSourceId();
				continue;
			}
			String modelExchangeItemId = modelExchangeItem.getId();//only used for log messages.

			//Note: mappedExchangeItem can be a subVector with only a selection of the modelExchangeItem. Therefore below only use mappedExchangeItem.
			IExchangeItem mappedExchangeItem = new BBExchangeItem(vectorConfig.getId(), vectorConfig, modelExchangeItem, selectors, configRootDir);
			//get model values.
			double[] computedValues = mappedExchangeItem.getValuesAsDoubles();

			//get the model values at the observed coordinates.
			if (!GeometryUtils.isScalar(observationExchangeItem)) {//if grid observationExchangeItem.
				String logMessage = "Getting model values at observed coordinates for grid observation exchangeItem with id '" + observationExchangeItem.getId() + "'.";
				if (LOGGER.isInfoEnabled()) LOGGER.info(logMessage);
				Results.putMessage(logMessage);

				//this code only works for grid modelExchangeItems.
				if (GeometryUtils.isScalar(mappedExchangeItem.getGeometryInfo())) {
					throw new IllegalArgumentException(getClass().getSimpleName() + ": Observation exchange item with id '" + observationExchangeItem.getId()
							+ "' is a grid, therefore the corresponding model exchange item must also be a grid. Model exchange item with id '" + modelExchangeItemId + "' is scalar.");
				}
				//grid exchangeItems are always IExchangeItems.
				IVector observedModelValues = getObservedModelValuesForGrid(((IExchangeItem) observationExchangeItem).getGeometryInfo(), mappedExchangeItem.getGeometryInfo(), computedValues,
						observationExchangeItem.getId(), modelExchangeItemId);
				ITreeVector treeVectorLeaf = new TreeVector(mappedExchangeItem.getId(), observedModelValues);
				treeVector.addChild(treeVectorLeaf);
				continue;
			}

			//if scalar observationExchangeItem.
			String logMessage = "Getting model values at observed coordinates for scalar observation exchangeItem with id '" + observationExchangeItem.getId() + "'.";
			if (LOGGER.isInfoEnabled()) LOGGER.info(logMessage);
			Results.putMessage(logMessage);
			//this code assumes that the observationExchangeItem and the mappedExchangeItem have the same coordinates (i.e. their coordinates are not used).
			//If the modelExchangeItem is a grid, then the mappedExchangeItem must be a subVector of the modelExchangeItem with only one selected grid cell.
			ITreeVector treeVectorLeaf;
			double[] observationTimes = observationExchangeItem.getTimes();
			if (observationTimes != null) {
				double[] computedTimes = mappedExchangeItem.getTimes();
				if (computedTimes != null) {
					//this code only works for scalar time series.
					if (computedTimes.length != computedValues.length) {
						errorMessage += "\n\tInconsistency in #times (" +
								computedTimes.length + ") and #values (" + computedValues.length + ") for" +
								vectorConfig.getId();
					}
					double[] sortedValues = computedValues;
					if (computedTimes.length != observationTimes.length) {
						double tolerance = 1d / 24d / 60d / 2; // half a minute (expressed as MJD)
						int[][] indices = SortUtils.mergeDoubleIndex(observationTimes, computedTimes, SortUtils.MergeType.left, tolerance);
						int[] observedIndices = indices[0];
						int[] computedIndices = indices[1];
						String missingTimeStepsString = "";
						String missingTimeStepStringMJD = "";
						for (int i = 0; i < computedIndices.length; i++) {
							if (computedIndices[i] == -1) {
								missingTimeStepStringMJD += ", "+ observationTimes[observedIndices[i]];
								missingTimeStepsString += ", " + TimeUtils.mjdToString(observationTimes[observedIndices[i]]);
							}
						}
						if (missingTimeStepsString.length() > 0) {
							errorMessage += "\n\tNo computed values XX available for " +
									vectorConfig.getId() + missingTimeStepsString+"\n"+
									"MJD :"+missingTimeStepStringMJD+"\n"+
							        "available times are: \n";
							for (int i=0; i<computedTimes.length; i++){
								errorMessage+=computedTimes[i]+" ";
							}
							continue;
						}
						sortedValues = SortUtils.applyIndexToDoubles(computedValues, computedIndices, -999);
					}
					treeVectorLeaf = new TreeVector(mappedExchangeItem.getId(), new Vector(sortedValues));
				} else {
					errorMessage += "\n\tNo times defined for " + vectorConfig.getSourceId();
					continue;
				}
			} else {
				treeVectorLeaf = new TreeVector(mappedExchangeItem.getId(), new Vector(computedValues));
			}
			treeVector.addChild(treeVectorLeaf);
		}

		if (errorMessage.length() > 0) {
			throw new RuntimeException("Error(s) in getting model values at observed locations from black box model " + model.getModelRunDir() + ": " + errorMessage);
		}
		timerGetObs.stop();
		return treeVector;
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
	 * @param observationGeometryInfo
	 * @param modelGeometryInfo
	 * @param modelValues
	 * @param observationExchangeItemId only used for log messages.
	 * @param modelExchangeItemId only used for log messages.
	 * @return model prediction interpolated to each observation (location).
	 */
	private static IVector getObservedModelValuesForGrid(IGeometryInfo observationGeometryInfo, IGeometryInfo modelGeometryInfo, double[] modelValues, String observationExchangeItemId, String modelExchangeItemId) {
		//get the coordinates for the observations.
		//this code assumes that the coordinates are stored in the same order as the values in the exchangeItem.
		//need one coordinate for each grid cell.
		IVector observationXCoordinates = GeometryUtils.getXCoordinates(observationGeometryInfo);
		IVector observationYCoordinates = GeometryUtils.getYCoordinates(observationGeometryInfo);
		logCoordinates(observationXCoordinates, observationYCoordinates, modelGeometryInfo, observationExchangeItemId, modelExchangeItemId);

		//get the model values at the observed coordinates.
		String logMessage = "Using bilinear interpolation to get model values from model exchangeItem '" + modelExchangeItemId
				+ "' at observed coordinates from observation exchangeItem '" + observationExchangeItemId + "'.";
		if (LOGGER.isInfoEnabled()) LOGGER.info(logMessage);
		Results.putMessage(logMessage);
		IVector observedModelValues = GeometryUtils.getObservedValuesBilinearInterpolation(observationXCoordinates, observationYCoordinates, modelGeometryInfo, modelValues);
		validateObservedModelValues(observationXCoordinates, observationYCoordinates, observedModelValues, observationExchangeItemId, modelExchangeItemId);

		return observedModelValues;
	}

	private static void logCoordinates(IVector observationXCoordinates, IVector observationYCoordinates, IGeometryInfo modelGeometryInfo, String observationExchangeItemId, String modelExchangeItemId) {
		IVector modelXCoordinates = GeometryUtils.getXCoordinates(modelGeometryInfo);
		IVector modelYCoordinates = GeometryUtils.getYCoordinates(modelGeometryInfo);

		double xMin = Double.POSITIVE_INFINITY;
		double xMax = Double.NEGATIVE_INFINITY;
		for (double x : modelXCoordinates.getValues()) {
			if (x < xMin) xMin = x;
			if (x > xMax) xMax = x;
		}
		double yMin = Double.POSITIVE_INFINITY;
		double yMax = Double.NEGATIVE_INFINITY;
		for (double y : modelYCoordinates.getValues()) {
			if (y < yMin) yMin = y;
			if (y > yMax) yMax = y;
		}
		String logMessage = "Model exchangeItem with id '" + modelExchangeItemId + "' contains " + GeometryUtils.getGridCellCount(modelGeometryInfo) + " grid cells.\n"
				+ "Range spanned by model grid: x = [" + xMin + ", " + xMax + "], y = [" + yMin + ", " + yMax + "]\n";

		xMin = Double.POSITIVE_INFINITY;
		xMax = Double.NEGATIVE_INFINITY;
		for (double x : observationXCoordinates.getValues()) {
			if (x < xMin) xMin = x;
			if (x > xMax) xMax = x;
		}
		yMin = Double.POSITIVE_INFINITY;
		yMax = Double.NEGATIVE_INFINITY;
		for (double y : observationYCoordinates.getValues()) {
			if (y < yMin) yMin = y;
			if (y > yMax) yMax = y;
		}
		logMessage += "Observation exchangeItem with id '" + observationExchangeItemId + "' contains " + observationYCoordinates.getSize() + " observed locations with coordinates:\n"
				+ "x coordinates: " + observationXCoordinates.printString("") + "\n"
				+ "y coordinates: " + observationYCoordinates.printString("") + "\n"
				+ "Range spanned by observations: x = [" + xMin + ", " + xMax + "], y = [" + yMin + ", " + yMax + "]";

		if (LOGGER.isInfoEnabled()) LOGGER.info(logMessage);
		Results.putMessage(logMessage);
	}

	private static void validateObservedModelValues(IVector observedXCoordinates, IVector observedYCoordinates, IVector observedModelValues, String observationExchangeItemId, String modelExchangeItemId) {
		List<Double> xCoordinatesOfMissingValues = new ArrayList<>();
		List<Double> yCoordinatesOfMissingValues = new ArrayList<>();
		for (int i = 0; i < observedModelValues.getSize(); i++) {
			if (Double.isNaN(observedModelValues.getValue(i))) {
				xCoordinatesOfMissingValues.add(observedXCoordinates.getValue(i));
				yCoordinatesOfMissingValues.add(observedYCoordinates.getValue(i));
			}
		}

		int errorCount = xCoordinatesOfMissingValues.size();
		if (errorCount > 0) {
			String message = BBStochModelInstance.class.getSimpleName() + ".getObservedModelValuesForGrid: Bilinear interpolation returned missing values for " + errorCount + " observed locations."
					+ " Please make sure that all observations with non-missing values from observation exchangeItem '" + observationExchangeItemId
					+ "' are located inside the model domain and coincide with active grid cells in the model for model exchangeItem '" + modelExchangeItemId + "'.\n"
					+ "Observed coordinates for which the model returned missing values:\n"
					+ "x coordinates: " + new VectorDouble(BBUtils.unbox(xCoordinatesOfMissingValues.toArray(new Double[xCoordinatesOfMissingValues.size()]))).printString("") + "\n"
					+ "y coordinates: " + new VectorDouble(BBUtils.unbox(yCoordinatesOfMissingValues.toArray(new Double[yCoordinatesOfMissingValues.size()]))).printString("");
			throw new RuntimeException(message);
		}
	}

	private IVector[] getObservedLocalizationExtended(IObservationDescriptions observationDescriptions, double distance){

	            int nObs=observationDescriptions.getObservationCount();
			    TreeVector localizationVectors[] = new TreeVector[nObs];
			    for (int iObs=0; iObs<nObs; iObs++){
					localizationVectors[iObs] = new TreeVector("state", "State From Black Box Stoch Model Instance");
				}


				Collection<BBNoiseModelConfig> noiseModelConfigs =
						this.bbStochModelVectorsConfig.getStateConfig().getNoiseModelConfigs();
				Collection<BBUncertOrArmaNoiseConfig> uncertaintyOrArmaNoiseConfigs =
						this.bbStochModelVectorsConfig.getStateConfig().getUncertaintyOrArmaNoiseConfigs();

				// add external noise model variables to state
			    int iNoise = 0;
				for (BBNoiseModelConfig noiseModelConfig : noiseModelConfigs) {
					IStochModelInstance noiseModel = noiseModels.get(noiseModelConfig);
					IVector noiseModelState = noiseModel.getState();
					noiseModelState.setConstant(1.0);

					for (int iObs=0; iObs<nObs; iObs++){

						//add this part to state
						if(noiseModelState instanceof ITreeVector){
							localizationVectors[iObs].addChild((ITreeVector)noiseModelState.clone());
						}else{
							String id = "noise_part_"+iNoise;
							ITreeVector tv = new TreeVector(id, noiseModelState);
							localizationVectors[iObs].addChild(tv);
						}
					}
					iNoise++;
				}

				// add blackbox internal noise model contributions to the state vector
				ITime currentTime = getCurrentTime();
				for (BBUncertOrArmaNoiseConfig noiseModelStateNoiseConfig : uncertaintyOrArmaNoiseConfigs) {
					ArmaNoiseModel noiseModel = armaNoiseModels.get(noiseModelStateNoiseConfig);
					if (noiseModelStateNoiseConfig.getNoiseModelType() !=
							BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItem) {
						double[] noiseStateVector = noiseModel.getNoiseStateVector(currentTime);
						for (int iElt=0; iElt<noiseStateVector.length; iElt++){ noiseStateVector[iElt]=1.0;}
						for (int iObs=0; iObs<nObs; iObs++){
							localizationVectors[iObs].addChild(noiseModelStateNoiseConfig.getId(), noiseStateVector);
						}
					}
				}

				// add deterministic mode variables to state vector
				//here bbStochModelStateVectors are listed in the same order as in the configuration.
				Collection<BBStochModelVectorConfig> bbStochModelStateVectors =
						this.bbStochModelVectorsConfig.getStateConfig().getVectorCollection();

				//here localizationVectors contains a treeVector for each observation.
				//for each state vector add a child treeVector with weight values to each localizationVector,
				//i.e. a mapping with weight values is created between each observation and each state vector.
				for (BBStochModelVectorConfig vectorConfig : bbStochModelStateVectors) {
					IModelExtensions modelExtended = (IModelExtensions) model;
					//TODO bug: this always uses vectorConfig.getId(). This should use vectorConfig.getSourceVectorId() if that is configured.
					IVector[] localizationVectorsModel= modelExtended.getObservedLocalization(vectorConfig.getId(),observationDescriptions,distance);
					for (int iObs=0; iObs<nObs; iObs++){
						localizationVectors[iObs].addChild(vectorConfig.getId(), localizationVectorsModel[iObs].getValues());
					}
				}
				return localizationVectors;
		}

		public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
			if (model instanceof IModelExtensions){
				//System.out.println("I implement the extend interface!");
	            return getObservedLocalizationExtended(observationDescriptions, distance);
			}
			int startOfModelState = stateNoiseModelsEndIndices[stateNoiseModelsEndIndices.length - 1];
			IVector[] modelObservedLocalization = model.getObservedLocalization(observationDescriptions, distance);
			int modelStateSize = modelObservedLocalization[0].getSize();
			IVector[] stochModelobservedLocalization = new IVector[modelObservedLocalization.length];
			for (int i = 0; i < stochModelobservedLocalization.length; i++) {
				double[] obsLocalizationValues = new double[startOfModelState + modelStateSize];
				for (int j=0; j<startOfModelState; j++){obsLocalizationValues[j]=1.0;}
				for (int j = 0; j < modelStateSize; j++) {
					obsLocalizationValues[j + startOfModelState] =
							modelObservedLocalization[i].getValue(j);
				}
				stochModelobservedLocalization[i] = new Vector(obsLocalizationValues);
			}
			return stochModelobservedLocalization;
	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
        if (this.bbStochModelVectorsConfig.isCollectPredictorTimeSeries()) {
            this.observationDescriptions = observationDescriptions;
        }
		// else, No action
		if (this.model instanceof IModelExtensions) {
			((IModelExtensions) this.model).announceObservedValues(observationDescriptions);
		}
	}

	public IVector getStateScaling() {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.getStateScaling(): Not implemented yet.");
	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.getStateScaling(): Not implemented yet.");
	}

	public File getModelRunDir() {
		return model.getModelRunDir();
	}

	public void finish() {
		for (IStochModelInstance noiseModel : noiseModels.values()) {
			noiseModel.finish();
		}
		for (ArmaNoiseModel armaNoiseModel : armaNoiseModels.values()) {
			armaNoiseModel.finish();
		}
		model.finish();
	}

	public void initialize(File workingDir, String[] arguments) {
		// no action needed (handled by model factory)
	}

	/**
	 * This method is not used in the java code, but it is in the .Net bridge, so please do not remove it.
 	 * @return The model instance
	 */
	public IModelInstance getModel() {
		return model;
	}

	public String toString() {
		return "BBStochModelInstance(" + model.toString() + ")";
	}

	private void createStateNoiseModels() {
		if (this.bbStochModelVectorsConfig.getStateConfig() != null) {
			for (BBUncertOrArmaNoiseConfig noiseModelStateNoiseConfig :
				this.bbStochModelVectorsConfig.getStateConfig().getUncertaintyOrArmaNoiseConfigs()) {
				ArmaNoiseModel noiseModel = armaNoiseModels.get(noiseModelStateNoiseConfig);
				if (noiseModel == null) {
					int noiseModelStateSize = 1; // TODO elaborate
					boolean doNoiseModelLogging = true; // TODO read from config
					if (noiseModelStateNoiseConfig.getNoiseModelType() == BBUncertOrArmaNoiseConfig.NoiseModelType.ArmaModel) {
						boolean useRandomSeed = false; // TODO read from config
						noiseModel = new ArmaNoiseModel(this.model.getModelRunDir(), noiseModelStateSize,
								noiseModelStateNoiseConfig.getArmaConstants(), noiseModelStateNoiseConfig.getStdDev(),
								useRandomSeed, doNoiseModelLogging);
					} else if (noiseModelStateNoiseConfig.getNoiseModelType() == BBUncertOrArmaNoiseConfig.NoiseModelType.Ar1Model) {
						boolean useRandomSeed = false; // TODO MVL read from config
						noiseModel = new ArmaNoiseModel(this.model.getModelRunDir(), noiseModelStateSize,
								noiseModelStateNoiseConfig.getArmaConstants(), noiseModelStateNoiseConfig.getStdDev(),
								useRandomSeed, doNoiseModelLogging);
					} else {
						noiseModel = new ArmaNoiseModel(this.model.getModelRunDir(), noiseModelStateSize,
								noiseModelStateNoiseConfig.getArmaConstants(), doNoiseModelLogging);
						addWhiteNoiseFromUncertEngineToNoiseModel(noiseModelStateNoiseConfig, noiseModel,
								this.getTimeHorizon().getBeginTime());
					}
					armaNoiseModels.put(noiseModelStateNoiseConfig, noiseModel);
				}
			}
		}
	}

	private BBStochModelVectorConfig findPredictionVectorConfig(String obsId) {

		for (BBStochModelVectorConfig bbStochModelVectorConfig : this.bbStochModelVectorsConfig.getPredictorVectorCollection()) {
			if (bbStochModelVectorConfig.getId().equalsIgnoreCase(obsId)) {
				return bbStochModelVectorConfig;
			}
		}

		String allVectors = "";
		for (BBStochModelVectorConfig bbStochModelVectorConfig :
			this.bbStochModelVectorsConfig.getPredictorVectorCollection()) {
			allVectors += "      " + bbStochModelVectorConfig.getId() + "\n";
		}
		throw new RuntimeException(
				"announceObservedValues:\n" +
						"No prediction subvector found for obs id \"" +
						obsId + "\"\n" +
						"Available subvectors are:\n" +

						allVectors + "\n");
	}

	private void addParameterDeltaToExchangeItem(double parameterDelta, IPrevExchangeItem exchangeItem, int transformation) {
		double[] values = exchangeItem.getValuesAsDoubles();
		if (transformation == BBRegularisationConstantConfig.TRANSFORMATION_IDENTITY) {
			for (int i = 0; i < values.length; i++) {
				values[i] += parameterDelta;
			}
		} else if (transformation == BBRegularisationConstantConfig.TRANSFORMATION_LN) {
			for (int i = 0; i < values.length; i++) {
				values[i] = values[i] * Math.exp(parameterDelta);
			}
		} else if (transformation == BBRegularisationConstantConfig.TRANSFORMATION_SET) {
			for (int i = 0; i < values.length; i++) {
				values[i] = parameterDelta;
			}
		} else {
			throw new RuntimeException("BBStochModelInstance.setParameters(): unexpected transformation typ " +
					exchangeItem.getId());
		}
		exchangeItem.setValuesAsDoubles(values);
	}

	private IVector getAndCheckParamChild(IVector parameters, String parameterId) {
		ITreeVector parametersAsTreeVector = (ITreeVector) parameters;
		IVector paramChild;
		try {
			paramChild = parametersAsTreeVector.getSubTreeVector(parameterId);
		} catch (RuntimeException e) {
			paramChild = null;
			for (String subTreeVectorId : parametersAsTreeVector.getSubTreeVectorIds()) {
				ITreeVector childVector = parametersAsTreeVector.getSubTreeVector(subTreeVectorId);
				try {
					paramChild = childVector.getSubTreeVector(parameterId);
				} catch (Exception e1) {
					paramChild = null;
				}
			}
		}
		if (paramChild != null) {
			if (paramChild.getSize() != 1) {
				throw new RuntimeException("BBStochModelInstance.setParameters(): invalid size for parameter " +
						parameterId + " in incoming parameters");
			}
			return paramChild;
		}
		throw new RuntimeException("BBStochModelInstance.setParameters(): can not find parameter " +
				parameterId + " in incoming parameters");
	}

	private boolean propagateNoiseModelsAndAddNoiseToExchangeItems(ITime currentTime, ITime targetTime, boolean afterCompute) {

		boolean addNoiseAfterComputeForAnyExchangeItem = false;
		// propagate the stochastic noise models
		for (Map.Entry<BBNoiseModelConfig, IStochModelInstance> noiseModelEntry : noiseModels.entrySet()) {

			BBNoiseModelConfig noiseModelConfig = noiseModelEntry.getKey();
			IStochModelInstance noiseModel = noiseModelEntry.getValue();

			System.out.println("noisemodel.compute until "+targetTime);
			//TODO add check on time-span of noise model.
			noiseModel.compute(targetTime);
			if(!this.doAutomaticNoiseGeneration){
				continue;
			}
			for (NoiseModelExchangeItemConfig exchangeItemConfig : noiseModelConfig.getExchangeItemConfigs()) {
				double[] noiseModelEITimes;
				IPrevExchangeItem noiseModelExchangeItem = noiseModel.getExchangeItem(exchangeItemConfig.getId());
				if (noiseModelExchangeItem == null) {
					noiseModelExchangeItem = noiseModel.getDataObjectExchangeItem(exchangeItemConfig.getId());
				}
				if (noiseModelExchangeItem == null) {
					throw new RuntimeException("Could not find noise model item " + exchangeItemConfig.getId());
				}
				noiseModelEITimes = noiseModelExchangeItem.getTimes();
				if (noiseModelEITimes == null) {
					throw new RuntimeException("No times for noise model item " + noiseModelExchangeItem.getId());
				}
				for (String modelExchangeItemId : exchangeItemConfig.getModelExchangeItemIds()) {
					IPrevExchangeItem modelExchangeItem = getExchangeItem(modelExchangeItemId);
					double modelTimes[] = modelExchangeItem.getTimes();
					List<Double> modelTimesInCurrentPeriod;
					if (modelTimes == null) {
						if (!modelExchangeItem.getId().toLowerCase().contains("state")) {
							throw new RuntimeException("No times available for model exchange item " +
									modelExchangeItem.getId());
						}
						double currentModelTime = getCurrentTime().getMJD();
						modelTimes = new double[] {currentModelTime};
						modelTimesInCurrentPeriod = new ArrayList<Double>();
						modelTimesInCurrentPeriod.add(currentModelTime);
					} else {
						modelTimesInCurrentPeriod = determineTimeStampsInInterval(
								modelTimes, currentTime, targetTime, exchangeItemConfig.doSkipFirstTimeStep());
					}
					// The model exchange item has more then one time stamp. Find matching times in
					// both lists.
                    //Note: this loops over the model exchangeItem times. For each model time the same time
                    //must be present in the noise model exchangeItem. For each model time the noise of the
                    //noise model for that time is added to the model values for that time.
					for (int t = 0; t < modelTimesInCurrentPeriod.size(); t++) {
						double time = modelTimesInCurrentPeriod.get(t);
						if(!this.lastNoiseTimes.containsKey(modelExchangeItemId)){
							this.lastNoiseTimes.put(modelExchangeItemId, Double.NEGATIVE_INFINITY);
						}
						double timePrecision = OdaGlobSettings.getTimePrecision();
						if(time>this.lastNoiseTimes.get(modelExchangeItemId)+0.5*timePrecision){
							// we need values for each of the model times in this forecast

							// look for index in ALL times for noise model exchange item
							int ti= TimeUtils.findMatchingTimeIndex(noiseModelEITimes, time, timePrecision);
							if(ti==-1){
                                String message = "Cannot find model time " + time + " in noise model."
                                        + " The noise model must contain the same times as the model. Please adjust the noise model configuration.\n";
                                message += "Noise model times for " + noiseModelExchangeItem.getId() + "\n";
                                for(int j = 0; j < noiseModelEITimes.length; j++){
                                    message += noiseModelEITimes[j] + "\n";
                                }
                                //for historic reasons also write this message to System.out.
                                System.out.println(message);
                                throw new RuntimeException(message);
							}
							double[] noiseModelEIValuesForTimeStep =
									getNoiseModelValuesForTimeStep(noiseModelExchangeItem, ti);
							if (exchangeItemConfig.doAddOnlyNoiseDifference()) {
								int noiseLength = noiseModelEIValuesForTimeStep.length;
								double[] previousValues = prevNoiseModelEIValuesForTimeStep.get(exchangeItemConfig.getId());
								if (previousValues == null) {
									prevNoiseModelEIValuesForTimeStep.put(exchangeItemConfig.getId(), noiseModelEIValuesForTimeStep);
									// only add new noise, no difference needed
								}
								else {
									// substract previous noise from new noise. store resulting noise for next time step
									for (int i = 0; i < noiseLength; i++) {
										double currentNoiseValue = noiseModelEIValuesForTimeStep[i];
										noiseModelEIValuesForTimeStep[i] = noiseModelEIValuesForTimeStep[i] - previousValues[i];
										previousValues[i] = currentNoiseValue;
									}
								}
							}
							int modelTimeIndex= TimeUtils.findMatchingTimeIndex(modelTimes, time, timePrecision);
							boolean addNoiseBefore = !afterCompute && !exchangeItemConfig.isAddStateNoiseAfterCompute();
							boolean addNoiseAfter = afterCompute && exchangeItemConfig.isAddStateNoiseAfterCompute();
							addNoiseAfterComputeForAnyExchangeItem |= !afterCompute && exchangeItemConfig.isAddStateNoiseAfterCompute();
							if (addNoiseAfter || addNoiseBefore) addNoiseToExchangeItemForOneTimeStep(modelExchangeItem, modelTimeIndex,
									noiseModelEIValuesForTimeStep, exchangeItemConfig.getOperation(), exchangeItemConfig.getStateSizeNoiseSizeRatio());
							this.lastNoiseTimes.put(modelExchangeItemId,time);
						}
					}
				}
			}// for noise model
		}


		// Propagate the (deprecated) 'old fashioned' arma models
		// Note: these arma models will become deprecated one the TimeSeriesNoiseModel that implements
		// the IStochModelInstance interface is fully elaborated and tested
		// Configurations that contain old ArmaModels then need to be replaced by a configuration
		// that is based on the new noise model.
		for (Map.Entry<BBUncertOrArmaNoiseConfig, ArmaNoiseModel> armaNoiseModelEntry : armaNoiseModels.entrySet()) {

			BBUncertOrArmaNoiseConfig stateNoiseModelConfig = armaNoiseModelEntry.getKey();
			ArmaNoiseModel armaNoiseModel = armaNoiseModelEntry.getValue();

			int numBcTimeStepsInPeriod = determineNumBcTimeSteps(currentTime, targetTime,
					stateNoiseModelConfig.getVectorConfigs());

			// Update the noise model, and add the noise to the exhange item.
			// TODO: this has to become a time step loop, handling
			// each boundary time step in the interval <currentTime,targetTime]
			double[] noiseForExchangItem = new double[numBcTimeStepsInPeriod];
			double deltaT = (targetTime.getMJD() - currentTime.getMJD()) / (double) numBcTimeStepsInPeriod;
			for (int t = 0; t < numBcTimeStepsInPeriod; t++) {

				// propagate noise one noiseModel timeStep:
				ITime noiseModelCurrentTime = new Time(currentTime.getMJD() + (double) t * deltaT);
				ITime noiseModelTargetTime = new Time(currentTime.getMJD() + ((double) t + 1.0d) * deltaT);
				if (!(stateNoiseModelConfig.getNoiseModelType() ==
						BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItem)) {
					armaNoiseModel.compute(noiseModelCurrentTime, noiseModelTargetTime);
				}

				// store (colored) noise realization at this noiseModelCurrentTime (white noise is not yet added):
				double[] noiseStateVector = armaNoiseModel.getNoiseStateVector(noiseModelCurrentTime);
				if (noiseStateVector.length != 1) {
					throw new RuntimeException("Incompatible noise state vector length for noise model "
							+ stateNoiseModelConfig.getId());
				}
				noiseForExchangItem[t] = noiseStateVector[0];

				if ((stateNoiseModelConfig.getNoiseModelType() ==
						BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItem) ||
						(stateNoiseModelConfig.getNoiseModelType() ==
						BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItemWithArmaConstants)) {
					// Add white noise from UncertainyEngine to noise state
					// (Noise state is empty in case op "UncertainItem", which is pure white noise)
					addWhiteNoiseFromUncertEngineToNoiseModel(stateNoiseModelConfig, armaNoiseModel, noiseModelTargetTime);
				} else {
					if(this.doAutomaticNoiseGeneration){
						armaNoiseModel.updateNoise(noiseModelTargetTime);
					}
				}
			}
			// add colored noise to exchangeItem at targetTime:
			addNoiseToExchangeItem(stateNoiseModelConfig, targetTime.getMJD(),
					numBcTimeStepsInPeriod, noiseForExchangItem);
		}
		return addNoiseAfterComputeForAnyExchangeItem;
	}

	/**
	 * Get values for this time index timeStep=-1 for last timeStep available, ie count negative values from end
	 * @param noiseModelExchangeItem
	 * @param timeStepIndex
	 * @return
	 */
	private double[] getNoiseModelValuesForTimeStep(IPrevExchangeItem noiseModelExchangeItem, int timeStepIndex) {
		if (noiseModelExchangeItem instanceof IExchangeItem) {
			IExchangeItem exchangeItem = (IExchangeItem) noiseModelExchangeItem;
			if (exchangeItem.getTimeInfo() == null) {
				throw new RuntimeException(
						"No time info available in exchange item " + exchangeItem.getId());
			}
			double[] timeStamps = exchangeItem.getTimeInfo().getTimes();
			if (timeStamps == null) {
				throw new RuntimeException(
						"No times set for exchange item " + exchangeItem.getId());
			}
			if(timeStepIndex<0) timeStepIndex+=timeStamps.length; //negative indices count from end

			IGeometryInfo iGeometryInfo = exchangeItem.getGeometryInfo();
			if (iGeometryInfo != null) {
				if (!(iGeometryInfo instanceof ArrayGeometryInfo)) {
					throw new RuntimeException(
							"Unknown geometry info type " + iGeometryInfo.getClass().getName() +
							" for exchange item " + exchangeItem.getId());
				}
				ArrayGeometryInfo geometryInfo = (ArrayGeometryInfo) iGeometryInfo;
				if (geometryInfo.getLatitudeArray().getNumberOfDimensions() != 1 ||
						geometryInfo.getLongitudeArray().getNumberOfDimensions() != 1) {
					throw new RuntimeException(
							"Currently only able to handle 1D long/lat arrays, exchange item " + exchangeItem.getId());
				}
			}
			Object valuesObject = exchangeItem.getValues();
			if (!(valuesObject instanceof IArray)) {
				throw new RuntimeException(
						"Values object type " + valuesObject.getClass().getName() +
						" not support, for exchange item " + exchangeItem.getId());
			}
			// TODO assume time index is at position=0, which is often true
			return ((IArray) valuesObject).getSliceAsDoubles(0, timeStepIndex, timeStepIndex);
		} else {
			// Previous version of exchange item, no spatial and/or time info available. Simply return the value
			// for the index 'timeStep'
			double[] values = noiseModelExchangeItem.getValuesAsDoubles();
			if (timeStepIndex >= values.length) {
				throw new RuntimeException(
						"Not enough values available in noise model exchange item " + noiseModelExchangeItem.getId());
			}
			if(timeStepIndex<0) timeStepIndex+=values.length; //negative to count from end
			return new double[]{values[timeStepIndex]};
		}
	}

	private void addWhiteNoiseFromUncertEngineToNoiseModel(BBUncertOrArmaNoiseConfig stateNoiseModelConfig,
			ArmaNoiseModel noiseModel, ITime noiseModelTargetTime) {
		double[] actualValues = new double[noiseModel.getNoiseStateVectorSize()];
		// TODO: stdDev is Factor
		int realizationCounter = noiseModel.getNextRealizationCounter();
		double[] noise = uncertaintyEngine.getNoise(stateNoiseModelConfig.getUncertainItemId(),
				realizationCounter, actualValues);
		noiseModel.addWhiteNoise(noiseModelTargetTime, noise);
	}

	private void addNoiseToExchangeItem(BBUncertOrArmaNoiseConfig stateNoiseModelConfig,
			double startTime,
			int numBcTimeStepsInPeriod,
			double[] noiseForExchangItem) {

		for (BBStochModelVectorConfig vectorConfig : stateNoiseModelConfig.getVectorConfigs()) {
			String exchangeItemID = vectorConfig.getSourceId();
			IPrevExchangeItem exchangeItem = getExchangeItem(exchangeItemID);
			int tStart = 0;
			double[] exchangeItemTimes = exchangeItem.getTimes();
			if (exchangeItemTimes != null) {
				DoubleArraySearch doubleArraySearch = new DoubleArraySearch(exchangeItemTimes);
				tStart = Math.max(doubleArraySearch.search(startTime), 0);
			}
			for (int t = tStart; t < tStart + numBcTimeStepsInPeriod; t++) {
				addNoiseToExchangeItemForOneTimeStep(exchangeItem, t,
						new double[]{noiseForExchangItem[t-tStart]}, stateNoiseModelConfig.getOperation(), 1);
			}
		}
	}

	private int determineNumBcTimeSteps(ITime currentTime, ITime targetTime, BBStochModelVectorConfig[] vectorConfigs) {
		int numBcTimeStepsInPeriod = Integer.MIN_VALUE;
		for (BBStochModelVectorConfig vectorConfig : vectorConfigs) {
			IPrevExchangeItem exchangeItem = getExchangeItem(vectorConfig.getSourceId());
			if (exchangeItem.getTimes() != null) {
				int exchangeItemNumTimestepsInComputationSpan = determineNumTimeStepsInSpan(exchangeItem.getTimes(), currentTime, targetTime);
				if (numBcTimeStepsInPeriod == Integer.MIN_VALUE) {
					numBcTimeStepsInPeriod = exchangeItemNumTimestepsInComputationSpan;
				} else {
					if (numBcTimeStepsInPeriod != exchangeItemNumTimestepsInComputationSpan) {
						throw new RuntimeException("Incompatible Times in Exchange Items");
					}
				}
			}
		}
		if (numBcTimeStepsInPeriod == Integer.MIN_VALUE) {
			// When no time is available apparently the exchangeItem is a constant.
			numBcTimeStepsInPeriod = 1;
		}
		return numBcTimeStepsInPeriod;
	}

	private List<Double> determineTimeStampsInInterval(double[] exchangeItemTimes, ITime currentTime, ITime targetTime, boolean skipFirstNoiseTimeStep) {
		List<Double> timeStampsInInterval = new ArrayList<Double>();
		for (Double exchangeItemTime : exchangeItemTimes) {
			if (isTimeIncludedInInterval(currentTime.getMJD(), targetTime.getMJD(), exchangeItemTime)) {
				if (!(skipFirstNoiseTimeStep && exchangeItemTime < currentTime.getMJD()+ 1.e-6)) {
					timeStampsInInterval.add(exchangeItemTime);
				}
			}
		}
		return timeStampsInInterval;
	}

	private int determineNumTimeStepsInSpan(double[] exchangeItemTimes, ITime currentTime, ITime targetTime) {
		int numTimeStepsInSpan = 0;
		for (double exchangeItemTime : exchangeItemTimes) {
			if (isTimeIncludedInInterval(currentTime.getMJD(), targetTime.getMJD(), exchangeItemTime)) {
				numTimeStepsInSpan++;
			}
		}
		return numTimeStepsInSpan;
	}

	private boolean isTimeIncludedInInterval(double currentTime, double targetTime, double exchangeItemTime) {
		return ((exchangeItemTime + 1.e-6) >= currentTime) && ((exchangeItemTime - 1.e-6) < targetTime);
	}

	private void addNoiseToExchangeItemForOneTimeStep(IPrevExchangeItem exchangeItem,
													  int timeIndex, double[] noise, BBUncertOrArmaNoiseConfig.Operation operation, int stateSizeNoiseSizeRatio) {
		int numValuesInExchangeItem;
		try {
			// check the number of input values
			//TODO Edwin: use GeometryUtils.isScalar(exchangeItem.getGeometryInfo()) to check if exchangeItem is scalar or grid, do not use instanceof. AK
			if (exchangeItem instanceof NetcdfGridTimeSeriesExchangeItem) {
				numValuesInExchangeItem = ((NetcdfGridTimeSeriesExchangeItem)exchangeItem).getValuesAsDoublesForSingleTimeIndex(0).length;
			} else{
				numValuesInExchangeItem = exchangeItem.getValuesAsDoubles().length;
			}
		} catch (Exception e) {
			// input exchangItem is not able to tell it's #values. assume 1
			// note: this should be handled more intelligent when the
			// IExchangeItem interface is fully elaborated
			numValuesInExchangeItem = 1;
		}
		int numTimeStepsInExchangeItem = -1;
		double[] times = exchangeItem.getTimes();
		if (times == null || times.length == 0) {
			// model EI has no time stamp (is most probable the current state)
			if (noise.length == numValuesInExchangeItem || stateSizeNoiseSizeRatio != 1) {
				numTimeStepsInExchangeItem = 1;
			} else {
				throw new RuntimeException("ExchangeItem " + exchangeItem.getId() + " has no time stamps whereas value size is not equal to noise-size");
			}
		}
		boolean addFullArray = false;
		if (noise.length == numValuesInExchangeItem) {
			if (numTimeStepsInExchangeItem == 1) {
				// Noise is meant for all values
				addFullArray = true;
			}
			//TODO Edwin: use GeometryUtils.isScalar(exchangeItem.getGeometryInfo()) to check if exchangeItem is scalar or grid, do not use instanceof. AK
			if (exchangeItem instanceof NetcdfGridTimeSeriesExchangeItem) {
				addFullArray = true;
			}
		} else {
			if (numTimeStepsInExchangeItem == numValuesInExchangeItem) {
				if (noise.length > 1) {
					// Noise value for all time stamps
					addFullArray = true;
				}
			}
		}

		if (stateSizeNoiseSizeRatio > 1) {
			noise = getSpatialNoise(exchangeItem, noise, operation, stateSizeNoiseSizeRatio);
		}

		if (addFullArray) {
			switch (operation) {
			case Add:
				//TODO Edwin: use GeometryUtils.isScalar(exchangeItem.getGeometryInfo()) to check if exchangeItem is scalar or grid, do not use instanceof. AK
				if (exchangeItem instanceof NetcdfGridTimeSeriesExchangeItem) {
					((NetcdfGridTimeSeriesExchangeItem)exchangeItem).axpyOnValuesForSingleTimeIndex(timeIndex, 1.0d, noise);
				} else {
					exchangeItem.axpyOnValues(1.0d, noise);
				}
				break;
			case Multiply:
				double[] factors = new double[noise.length];
				for (int i = 0; i < noise.length; i++) {
					factors[i] = 1d + noise[i];
				}
				//TODO Edwin: use GeometryUtils.isScalar(exchangeItem.getGeometryInfo()) to check if exchangeItem is scalar or grid, do not use instanceof. AK
				if (exchangeItem instanceof NetcdfGridTimeSeriesExchangeItem) {
					((NetcdfGridTimeSeriesExchangeItem)exchangeItem).multiplyValuesForSingleTimeIndex(timeIndex, factors);
				} else {
					exchangeItem.multiplyValues(factors);
				}
				break;
			case Set:
				throw new RuntimeException("addNoiseToExchangeItemForOneTimeStep on " + exchangeItem.getId() +
						": invalid call for setting values");
			}
		} else {
			int numValuesToBeSet=noise.length;
			if (numValuesInExchangeItem%numValuesToBeSet != 0) {
				String message="The number of values in the exchangeItem is not a multiple of the number of times.\n ";
				message+="noise has length="+noise.length+"\n";
				message+="item with id="+exchangeItem.getId()+" has length="+numValuesInExchangeItem+"\n";
				message+="processing time index="+timeIndex+" for times:"+(new Vector(times)).toString()+"\n";
				throw new RuntimeException(message);
			}
			if(timeIndex>numValuesInExchangeItem/numValuesToBeSet){
				String message="time index out of bounds for "+exchangeItem.getId()+"\n";
				message+="processing time index="+timeIndex+" for times:"+(new Vector(times)).toString()+"\n";
				throw new RuntimeException(message);
			}
			// add noise 'slice'
			int startOfNoise = timeIndex * numValuesToBeSet;
			int endOfNoise = (timeIndex + 1) * numValuesToBeSet;
			switch (operation) {
			case Add:
				double[] values = new double[numValuesInExchangeItem];
				System.arraycopy(noise, 0, values, startOfNoise, endOfNoise - startOfNoise);
				exchangeItem.axpyOnValues(1.0, values);
				break;
			case Multiply:
				double[] factors = new double[numValuesInExchangeItem];
                // initialize factors with ones:
                for (int i = 0; i < numValuesInExchangeItem ; i++){
                    factors[i] = 1d;
                }
				for (int i = 0; i < numValuesToBeSet; i++) {
					factors[startOfNoise+i] += noise[i];
				}
				exchangeItem.multiplyValues(factors);
				break;
			case Set:
				throw new RuntimeException("addNoiseToExchangeItemForOneTimeStep on " + exchangeItem.getId() +
						": invalid call for setting values");
			}
		}
	}

	private double[] getSpatialNoise(IPrevExchangeItem exchangeItem, double[] noise, BBUncertOrArmaNoiseConfig.Operation operation, int stateSizeNoiseSizeRatio) {
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();

		if (valuesAsDoubles.length > stateSizeNoiseSizeRatio * noise.length) {
			throw new RuntimeException("Number of points in state (" + valuesAsDoubles.length + ") should not be more than stateSizeNoiseSizeRatio * noise.length, currently: " + stateSizeNoiseSizeRatio + " * " + noise.length);
		}
		if (valuesAsDoubles.length < stateSizeNoiseSizeRatio * (noise.length - 2) + 1) {
			throw new RuntimeException("Number of points in state (" + valuesAsDoubles.length + ") should not be less than stateSizeNoiseSizeRatio * (noise.length - 2) + 1, currently: " + stateSizeNoiseSizeRatio + " * (" + noise.length + " - 2) + 1");
		}

		int stateSizeMin1 = valuesAsDoubles.length - 1;
		if (!warningLogged && stateSizeMin1 % stateSizeNoiseSizeRatio != 0 && valuesAsDoubles.length > stateSizeNoiseSizeRatio * (noise.length - 1) + 1) {
			System.out.println("Warning: " + stateSizeMin1 + " (Number of points in state - 1) not dividable by noise ratio " + stateSizeNoiseSizeRatio + ", so extrapolation will be used for adding noise to the last state points. Increasing noise points from " + noise.length + " to " + (noise.length + 1) + " will result in interpolation for last state points as well.");
			warningLogged = true;
		}
		double[] spatialNoise = new double[valuesAsDoubles.length];
		for (int i = 0, k = 0; i < valuesAsDoubles.length; i += stateSizeNoiseSizeRatio, k++) {
			double noiseValue = noise[k];

			int nextNoiseValueIndex = k + 1;
			double nextNoiseValue;
			int steps;
			double stepNoiseValue;
			if (nextNoiseValueIndex >= noise.length) {
				nextNoiseValue = noise[noise.length - 1];
				double previousNoiseValue = noise[noise.length - 2];
				steps = valuesAsDoubles.length % stateSizeNoiseSizeRatio;
				stepNoiseValue = (nextNoiseValue - previousNoiseValue) / steps;
			} else {
				nextNoiseValue = noise[k + 1];
				steps = stateSizeNoiseSizeRatio;
				stepNoiseValue = (nextNoiseValue - noiseValue) / steps;
			}
			for (int j = 0; j < steps && i + j < spatialNoise.length; j++) {
				spatialNoise[i + j] = noiseValue + j * stepNoiseValue;
			}
		}
		return spatialNoise;
	}

	private File checkRestartDir(ITime time, boolean mustExist) {
		if (this.savedStatesDirPrefix == null) {
			throw new RuntimeException("Dir for restart files not specified in black box stoch model config file on dir. " +
					configRootDir.getAbsolutePath());
		}
		File savedStatesRootDir = configRootDir;
		String savedStatesDirPrefix = this.savedStatesDirPrefix;
		return BBModelInstance.createDirectoryForSavedState(time, mustExist, savedStatesRootDir, savedStatesDirPrefix);
	}
}
