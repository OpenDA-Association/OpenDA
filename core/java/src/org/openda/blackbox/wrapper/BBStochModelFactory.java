/* OpenDA v2.4.3 
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
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.*;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.uncertainties.UncertaintyStochVector;
import org.openda.utils.*;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import static org.openda.utils.performance.OdaGlobSettings.getTimePrecision;
/**
 * Factory for creating instances of the type BBModelInstance.
 */
public class BBStochModelFactory implements IStochModelFactory, ITimeHorizonConsumer {

	private BBStochModelConfig bbStochModelConfig;
	private IModelFactory modelFactory;
	private File configRootDir;

	private UncertaintyEngine uncertaintyEngine = null;
	private TreeVector paramsInUncertaintyModels = null;
	private List<IStochVector> paramsUncertaintyModelStochVectors = new ArrayList<IStochVector>();
	private IStochVector parameterUncertaintyTotalStochVector = null;

	LinkedHashMap<IDataObject, ArrayList<BBBoundaryMappingConfig>> dataObjectBoundaryMappings = new LinkedHashMap<>();
	LinkedHashMap<BBNoiseModelConfig, IStochModelFactory> noiseModelFactories = new LinkedHashMap<BBNoiseModelConfig, IStochModelFactory>();

    /* Note this model can be use in a parallel environment therefore we use the distributed counter. */
	private DistributedCounter instanceCounter = new DistributedCounter(0);

	private List<String> accessedInputExchangeItemIds = new ArrayList<String>();
	private List<String> accessedOutputExchangeItemIds = new ArrayList<String>();

	public void initialize(File configRootDir, String[] arguments) {

		this.configRootDir = configRootDir;
		String configFileName = arguments[0];
		BBStochModelConfigReader stochModelConfigReader = new BBStochModelConfigReader(new File(configRootDir, configFileName));
		bbStochModelConfig = stochModelConfigReader.getBBStochModelConfig();
		if (bbStochModelConfig.getBbModelConfig() != null) {
			modelFactory = new BBModelFactory(bbStochModelConfig.getBbModelConfig());
		} else {
			Object modelFactoryInstance = ObjectSupport.createNewInstance(
					bbStochModelConfig.getModelFactoryAction().getClassName());
			if (!(modelFactoryInstance instanceof IModelFactory)) {
				throw new IllegalArgumentException(
						"org.openda.blackbox.wrapper.BBStochModelFactory(): unknown uncertainty IModelFactory type");
			}
			modelFactory = (IModelFactory) modelFactoryInstance;
			modelFactory.initialize(bbStochModelConfig.getModelFactoryWorkingDir(),
					bbStochModelConfig.getModelFactoryAction().getArguments());
		}

		BBAction uncertaintyAction = bbStochModelConfig.getUncertaintyAction();
		if (uncertaintyAction != null) {
			Object uncertaintyEngineInstance = ObjectSupport.createNewInstance(uncertaintyAction.getClassName());
			if (!(uncertaintyEngineInstance instanceof UncertaintyEngine)) {
				throw new IllegalArgumentException("org.openda.blackbox.wrapper.BBStochModelFactory(): " +
						"unknown uncertainty engine type: " + uncertaintyEngineInstance.getClass().toString());
			}
			uncertaintyEngine = (UncertaintyEngine) uncertaintyEngineInstance;
			uncertaintyEngine.initialize(bbStochModelConfig.getUncertaintyWorkingDir(), uncertaintyAction.getArguments());
		}

		getDataObjectBoundaryMappings();

		List<BBNoiseModelConfig> paramsUncertaintyModelModelConfigs =
				bbStochModelConfig.getBbStochModelVectorsConfig().getParamsUncertaintyModelConfigs();
		for (BBNoiseModelConfig paramsUncertaintyModelModelConfig : paramsUncertaintyModelModelConfigs) {

			Object paramsUncertaintyModelFactory = ObjectSupport.createNewInstance(paramsUncertaintyModelModelConfig.getClassName());
			if (paramsUncertaintyModelFactory instanceof IStochModelFactory) {
				// Noise Model Config
				((IStochModelFactory) paramsUncertaintyModelFactory).initialize(
						paramsUncertaintyModelModelConfig.getWorkingDir(),
						paramsUncertaintyModelModelConfig.getArgumentsIncludingConfigFile());
				IStochModelInstance paramsUncertaintyModelInstance =
						((IStochModelFactory) paramsUncertaintyModelFactory).getInstance(OutputLevel.ModelDefault);
				paramsUncertaintyModelStochVectors.add(paramsUncertaintyModelInstance.getParameterUncertainty());
			} else if (paramsUncertaintyModelFactory instanceof UncertaintyEngine) {
				// 'old' Uncertainty Engine configuration, create uncertainty stoch vector
				UncertaintyEngine uncertaintyEngine = new UncertaintyEngine();
				uncertaintyEngine.initialize(paramsUncertaintyModelModelConfig.getWorkingDir(),
						paramsUncertaintyModelModelConfig.getArgumentsIncludingConfigFile());
				TreeVector paramsInUE = new TreeVector("Params-UE");
				for (NoiseModelExchangeItemConfig exchangeItemConfig : paramsUncertaintyModelModelConfig.getExchangeItemConfigs()) {
					paramsInUE.addChild(exchangeItemConfig.getId(), new double[]{0});
				}
				UncertaintyStochVector uncertaintyStochVector = new UncertaintyStochVector(uncertaintyEngine, paramsInUE);
				paramsUncertaintyModelStochVectors.add(uncertaintyStochVector);
			} else {
				throw new IllegalArgumentException("org.openda.blackbox.wrapper.BBStochModelFactory(): " +
						"unknown parameter uncertainty model type: " + paramsUncertaintyModelFactory.getClass().toString());
			}

			paramsInUncertaintyModels = new TreeVector("ParamsIn-UMs");
			for (NoiseModelExchangeItemConfig exchangeItemConfig : paramsUncertaintyModelModelConfig.getExchangeItemConfigs()) {
				paramsInUncertaintyModels.addChild(exchangeItemConfig.getId(), new double[]{0});
			}
		}

		determineAccessedModelItems(bbStochModelConfig.getBbStochModelVectorsConfig());
	}

	public void setTimeHorizon(ITime timeHorizon) {
		if (this.modelFactory instanceof ITimeHorizonConsumer) {
			((ITimeHorizonConsumer)this.modelFactory).setTimeHorizon(timeHorizon);
		}
	}

    public IStochModelInstance getInstance(){
        return getInstance(OutputLevel.Suppress);
    }

	public IStochModelInstance getInstance(OutputLevel outputLevel) {

		// TODO MVL check if this makes sense. I think we should just pass the argument instead of the default.
		//IModelInstance modelInstance = modelFactory.getInstance(new String[]{}, OutputLevel.ModelDefault);
		IModelInstance modelInstance = modelFactory.getInstance(new String[]{}, outputLevel);

		instanceCounter.inc();
        String restartStatesDirPrefix = bbStochModelConfig.getRestartStatesDirPrefix();
        if (restartStatesDirPrefix.startsWith("INSTANCE_DIR/")) {
            restartStatesDirPrefix = modelInstance.getModelRunDir() +
                    restartStatesDirPrefix.substring("INSTANCE_DIR".length());
        }

		int ensembleMemberIndex = instanceCounter.val();

        return new BBStochModelInstance(configRootDir, modelInstance,
				uncertaintyEngine,
				getParameterUncertaintyForNewInstance(ensembleMemberIndex),
				getParameters(),
				getNoiseModelsForNewInstance(modelInstance.getTimeHorizon()),
				dataObjectBoundaryMappings,
				ensembleMemberIndex,
				bbStochModelConfig.getBbStochModelVectorsConfig(),
                restartStatesDirPrefix,
				bbStochModelConfig.getRestartStatesNoiseModelPrefix(),
				bbStochModelConfig.getModelRestartStateFile());
	}

	private void getDataObjectBoundaryMappings() {
		for (BBBoundaryProviderConfig config: bbStochModelConfig.getBoundaryProviderConfigs()) {
			IDataObject dataObject = BBUtils.createDataObject(config.getDataObjectWorkDir(), config.getClassName(), config.getDataObjectFileName(), config.getArguments());
			ArrayList<BBBoundaryMappingConfig> boundaryMappingConfigs = config.getBoundaryMappingConfigs();
			dataObjectBoundaryMappings.put(dataObject, boundaryMappingConfigs);
		}
	}

	private LinkedHashMap<BBNoiseModelConfig, IStochModelInstance> getNoiseModelsForNewInstance(ITime modelTimeHorizon) {

		LinkedHashMap<BBNoiseModelConfig, IStochModelInstance> noiseModelForNewInstance =
				new LinkedHashMap<BBNoiseModelConfig, IStochModelInstance>();

		BBStochModelStateConfig stateVectorConfigs = bbStochModelConfig.getBbStochModelVectorsConfig().getStateConfig();
		if (stateVectorConfigs != null) {
			for (BBNoiseModelConfig noiseModelConfig :
					stateVectorConfigs.getNoiseModelConfigs()) {
				IStochModelFactory noiseModelFactory = noiseModelFactories.get(noiseModelConfig);
				if (noiseModelFactory == null) {
					noiseModelFactory = (IStochModelFactory) ObjectSupport.createConfigurable(
							"NoiseModel",
							noiseModelConfig.getClassName(),
							IStochModelFactory.class,
							noiseModelConfig.getWorkingDir(),
							noiseModelConfig.getArgumentsIncludingConfigFile()
					);
					if (noiseModelFactory instanceof ITimeHorizonConsumer) {
						ITime noiseModelTimeHorizon = determineNoiseModelTimeHorizon(modelTimeHorizon);
						((ITimeHorizonConsumer)noiseModelFactory).setTimeHorizon(noiseModelTimeHorizon);
					}
					noiseModelFactories.put(noiseModelConfig, noiseModelFactory);
				}
				noiseModelForNewInstance.put(noiseModelConfig, noiseModelFactory.getInstance(OutputLevel.ModelDefault));
			}
		}

		return noiseModelForNewInstance;
	}

	private ITime determineNoiseModelTimeHorizon(ITime timeHorizon) {
		Time result = new Time(timeHorizon);
		double startAsMJD = timeHorizon.getBeginTime().getMJD();
		double endAsMJD = timeHorizon.getEndTime().getMJD();
		double timeSpan = endAsMJD - startAsMJD;
		if ( Math.abs( timeSpan - timeHorizon.getStepMJD()) < getTimePrecision() ) {
			// make it clear that the timestep was not set and let the noisemodel decide what to do
			result.setStep(Double.NaN);
		}
		return result;
	}

	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelFactory.getPostprocessorInstance(): Not implemented yet.");
	}

	public void finish() {
		// no action needed (yet)
	}

	public ITreeVector getParameters() {

		TreeVector paramsInUE = new TreeVector("Params-UE");

		TreeVector paramsTreeVector = new TreeVector("Params");
		for (BBRegularisationConstantConfig regularisationConstantConfig :
				bbStochModelConfig.getBbStochModelVectorsConfig().getRegularisationConstantCollection()) {
			if (regularisationConstantConfig.useUncertaintyEngine()) {
				paramsInUE.addChild(regularisationConstantConfig.getUncertainItemId(), new double[]{0});
			} else {
				String parameterId = composeRelatedParametersId(regularisationConstantConfig);
				double initialValue = regularisationConstantConfig.getMean();
				paramsTreeVector.addChild(parameterId, new double[]{initialValue});
			}
		}

		for (BBCartesianToPolarConfig cartesianToPolarConfig : bbStochModelConfig.getBbStochModelVectorsConfig().getCartesianToPolarCollection()) {
			String parameterIdDeltaX = composeCartesionToPolarParameterId(cartesianToPolarConfig, false);
			paramsTreeVector.addChild(parameterIdDeltaX, new double[]{0});
			String parameterIdDeltaY = composeCartesionToPolarParameterId(cartesianToPolarConfig, true);
			paramsTreeVector.addChild(parameterIdDeltaY, new double[]{0});
		}

		if (paramsInUncertaintyModels != null) {
			for (String paramId : paramsInUncertaintyModels.getSubTreeVectorIds()) {
				paramsTreeVector.addChild(paramsInUncertaintyModels.getSubTreeVector(paramId));
			}
		}

		for (String paramId : paramsInUE.getSubTreeVectorIds()) {
			paramsTreeVector.addChild(paramsInUE.getSubTreeVector(paramId));
		}

		return paramsTreeVector;
	}

	public List<String> getAccessedInputExchangeItemIds() {
		return accessedInputExchangeItemIds;
	}

	public List<String> getAccessedOutputExchangeItemIds() {
		return accessedOutputExchangeItemIds;
	}

	private IStochVector getParameterUncertaintyForNewInstance(int realizationCounter) {

		if (parameterUncertaintyTotalStochVector == null) {

			List<Double> stdDevs = new ArrayList<Double>();
			TreeVector paramsInUE = new TreeVector("Params-UE");

			// gather in line defined standard deviations
			for (BBRegularisationConstantConfig regularisationConstantConfig : bbStochModelConfig.getBbStochModelVectorsConfig().getRegularisationConstantCollection()) {
				if (regularisationConstantConfig.useUncertaintyEngine()) {
					paramsInUE.addChild(regularisationConstantConfig.getUncertainItemId(), new double[]{0});
				} else {
					double stdDev = regularisationConstantConfig.getStdDev() /
							Math.abs(regularisationConstantConfig.getScale());
					stdDevs.add(stdDev);
				}
			}

			// gather in in line defined standard deviations for "Cartesian to Polar" parameters
			for (BBCartesianToPolarConfig cartesianToPolarConfig : bbStochModelConfig.getBbStochModelVectorsConfig().getCartesianToPolarCollection()) {
				stdDevs.add(cartesianToPolarConfig.getStdDev());  // delta-X
				stdDevs.add(cartesianToPolarConfig.getStdDev());  // delta-Y
			}

			// create stoch vector for the on line defined standard deviations
			IStochVector stdDevsStochVector = null;
			if (stdDevs.size() > 0) {
				double[] params = new double[stdDevs.size()];
				double[] stdDevsVectorArray = new double[stdDevs.size()];
				for (int i = 0; i < stdDevsVectorArray.length; i++) {
					stdDevsVectorArray[i] = stdDevs.get(i);
				}
				stdDevsStochVector = new StochVector(params, stdDevsVectorArray);
			}

			if (paramsInUE.getSubTreeVectorIds().size() > 0) {
				UncertaintyStochVector uncertaintyStochVector = new UncertaintyStochVector(uncertaintyEngine, paramsInUE);
				paramsUncertaintyModelStochVectors.add(uncertaintyStochVector);
			}

			if (stdDevsStochVector != null) {

				if (paramsUncertaintyModelStochVectors.size() == 0) {
					// no uncertainty models defined, return the sdtDevs
					parameterUncertaintyTotalStochVector = stdDevsStochVector;
				} else {
					// add the stoch vectors of the uncertain models to the std.devs. stoch vector
					StochTreeVector combinedVector = new StochTreeVector("paramUncertaintyCombinedVector");
					combinedVector.addChild(stdDevsStochVector);
					for (IStochVector paramsUncertaintyModelStochVector : paramsUncertaintyModelStochVectors) {
						combinedVector.addChild(paramsUncertaintyModelStochVector);
					}
					parameterUncertaintyTotalStochVector = combinedVector;
				}

			} else if (paramsUncertaintyModelStochVectors.size() > 0) {
				// Only uncertainty models defined, return the combined stoch vector
				// (unless there is only one uncertainty model, in that return that stoch vector
				if (paramsUncertaintyModelStochVectors.size() == 1) {
					parameterUncertaintyTotalStochVector = paramsUncertaintyModelStochVectors.get(0);
				} else {
					parameterUncertaintyTotalStochVector = new StochTreeVector("paramUncertaintyCombinedVector");
					for (IStochVector paramsUncertaintyModelStochVector : paramsUncertaintyModelStochVectors) {
						((StochTreeVector) parameterUncertaintyTotalStochVector).addChild(paramsUncertaintyModelStochVector);
					}
				}
			} else {
				return null;
			}
		}

		return cloneUncertaintyEngineStochVectors(parameterUncertaintyTotalStochVector, realizationCounter);
	}

	static private IStochVector cloneUncertaintyEngineStochVectors(
										IStochVector parameterUncertaintyTotalStochVector,
										int realizationCounter) {
		IStochVector clone;
		if (parameterUncertaintyTotalStochVector instanceof StochTreeVector) {
			// Combined stoch vector's, may contain UncertaintyStochVector(s) from UncertaintyEngine.
			// Clone the UncertaintyStochVector(s) for the new realization counter.
			StochTreeVector stochTreeVectorClone = new StochTreeVector("paramUncertaintyCombinedVector");
			for (IStochVector stochVector : ((StochTreeVector) parameterUncertaintyTotalStochVector).getChildren()) {
				if (stochVector instanceof UncertaintyStochVector) {
					UncertaintyStochVector uncertaintyStochVectorClone =
							new UncertaintyStochVector((UncertaintyStochVector) stochVector, realizationCounter);
					stochTreeVectorClone.addChild(uncertaintyStochVectorClone);
				} else {
					stochTreeVectorClone.addChild(stochVector);
				}
			}
			clone = stochTreeVectorClone;
		} else {
			if (parameterUncertaintyTotalStochVector instanceof UncertaintyStochVector) {
				// The parameter uncertainty is an UncertaintyStochVector from UncertaintyEngine.
				// Clone it for the new realization counter.
				clone = new UncertaintyStochVector(
						(UncertaintyStochVector) parameterUncertaintyTotalStochVector, realizationCounter);
			} else {
				// No UncertaintyStochVector present, nothing to be cloned
				clone = parameterUncertaintyTotalStochVector;
			}
		}
		return clone;
	}

    static String composeRelatedParametersId(BBRegularisationConstantConfig regularisationConstantConfig) {
        if (regularisationConstantConfig.useUncertaintyEngine()) {
            return regularisationConstantConfig.getUncertainItemId();
        }
        List<BBStochModelVectorConfig> vectorConfigs = regularisationConstantConfig.getVectorConfigs();
		String parameterId = regularisationConstantConfig.getId();
		if (parameterId == null) {
			parameterId = vectorConfigs.get(0).getId();
			for (int i = 1; i < vectorConfigs.size(); i++) {
				parameterId += "/" + vectorConfigs.get(i).getId();
			}
		}
		return parameterId;
    }

	static String composeCartesionToPolarParameterId(BBCartesianToPolarConfig cartesionToPolarConfig, boolean useYdirection) {

		String parameterId = useYdirection ? cartesionToPolarConfig.getYCaption() : cartesionToPolarConfig.getXCaption();

		if (parameterId == null) {
			// no captions specified, compose it
			String etcText = "";
			if (cartesionToPolarConfig.getVectorConfigs().size() > 3) etcText = "(etc.)";
			String angleId = cartesionToPolarConfig.getVectorConfigs().get(0).getId();
			String radiusId = cartesionToPolarConfig.getVectorConfigs().get(1).getId();
			String componentSuffix = useYdirection ?
					cartesionToPolarConfig.getYSuffixCaption() : cartesionToPolarConfig.getXSuffixCaption();
			parameterId = radiusId + "/" + angleId + etcText + "-" + componentSuffix;
		}
		return parameterId;
	}

	private void determineAccessedModelItems(BBStochModelVectorsConfig bbStochModelVectorsConfig) {
		Collection<BBRegularisationConstantConfig> accessedStochParameters =
				bbStochModelVectorsConfig.getRegularisationConstantCollection();
		for (BBRegularisationConstantConfig accessedStochParameter : accessedStochParameters) {
			for (BBStochModelVectorConfig accessedParameter : accessedStochParameter.getVectorConfigs()) {
				accessedInputExchangeItemIds.add(accessedParameter.getSourceId());
			}
		}
		if (bbStochModelVectorsConfig.getStateConfig() != null) {
			Collection<BBNoiseModelConfig> noiseModelConfigs = bbStochModelVectorsConfig.getStateConfig().getNoiseModelConfigs();
			for (BBNoiseModelConfig noiseModelConfig : noiseModelConfigs) {
				for (NoiseModelExchangeItemConfig exchangeItemConfig : noiseModelConfig.getExchangeItemConfigs()) {
					for (String modelExchangeItemId : exchangeItemConfig.getModelExchangeItemIds()) {
						accessedInputExchangeItemIds.add(modelExchangeItemId);
					}
				}
			}
		}
		Collection<BBStochModelVectorConfig> accessedStochResults =
				bbStochModelVectorsConfig.getPredictorVectorCollection();
		for (BBStochModelVectorConfig accessedStochResult : accessedStochResults) {
			accessedOutputExchangeItemIds.add(accessedStochResult.getSourceId());
		}
	}
}
