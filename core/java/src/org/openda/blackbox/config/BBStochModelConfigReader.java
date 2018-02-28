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


package org.openda.blackbox.config;

import org.exolab.castor.types.Duration;
import org.openda.core.io.castorgenerated.*;
import org.openda.core.io.castorgenerated.types.NoiseOperationTypesXML;
import org.openda.core.io.castorgenerated.types.ParameterTransformationTypesXML;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.utils.DimensionIndex;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.*;

/**
 * Reader for black box stochastic model configuration.
 */
public class BBStochModelConfigReader {

    private BBStochModelConfig bbStochModelConfig;

    public BBStochModelConfigReader(File stochModelConfigFile) {

        BlackBoxStochModelConfigXML bbStochModelConfigXML =
                (BlackBoxStochModelConfigXML) CastorUtils.parse(stochModelConfigFile, BlackBoxStochModelConfigXML.class);

        BlackBoxModelConfigReferenceXML blackBoxModelConfigReferenceXML =
                bbStochModelConfigXML.getBlackBoxStochModelConfigXMLChoice().getModelConfig();

        File modelFactoryWorkingDir = null;
        BBAction modelFactoryActionConfig = null;

        BBModelConfig bbModelConfig = null;

        AliasDefinitions aliasDefinitions = new AliasDefinitions();

        if (blackBoxModelConfigReferenceXML != null) {
            // bb model config, check how it is specified
            String bbModelRefConfigFile = blackBoxModelConfigReferenceXML.getFile();
            if (bbModelRefConfigFile != null) {
                // actual model is defined in separate config file
                File bbModelConfigFile = new File(stochModelConfigFile.getParentFile(),
                        bbModelRefConfigFile);
                if (!bbModelConfigFile.exists()) {
                    throw new RuntimeException("Stoch Model configuration " + stochModelConfigFile.getAbsolutePath() +
                            " refers to non existing model configuration file " + bbModelConfigFile.getAbsolutePath());
                }
                BBModelConfigReader bbModelConfigReader = new BBModelConfigReader(bbModelConfigFile);
                bbModelConfig = bbModelConfigReader.getBBModelConfig();
            } else {
                // actual model is defined "in line"
                BlackBoxModelConfigXML bbModelConfigXML = blackBoxModelConfigReferenceXML.getConfig();
                bbModelConfig = BBModelConfigReader.parseBBModelConfig(stochModelConfigFile, bbModelConfigXML);
            }
            aliasDefinitions = bbModelConfig.getWrapperConfig().getAliasDefinitions();
        } else {
            ActionXML modelFactoryConfig =
                    bbStochModelConfigXML.getBlackBoxStochModelConfigXMLChoice().getModelFactory();
            modelFactoryActionConfig = BBWrapperConfigReader.parseBBAction(stochModelConfigFile,
                    modelFactoryConfig, aliasDefinitions);
            String workingDirName = modelFactoryConfig.getWorkingDirectory();
            if (workingDirName == null) workingDirName = ".";
            modelFactoryWorkingDir = new File(stochModelConfigFile.getParentFile(), workingDirName);
        }

		// get uncertainty module, if specified
		File uncertaintyWorkingDir = null;
		BBAction uncertaintyActionConfig = null;
		if (bbStochModelConfigXML.getUncertaintyModule() != null) {
			// working dir. for unc. module
			String workingDirName = bbStochModelConfigXML.getUncertaintyModule().getWorkingDirectory();
			if (workingDirName == null) workingDirName = ".";
			uncertaintyWorkingDir = new File(stochModelConfigFile.getParentFile(), workingDirName);
			// Unc. module class and arguments
			uncertaintyActionConfig = BBWrapperConfigReader.parseBBAction(stochModelConfigFile,
					bbStochModelConfigXML.getUncertaintyModule(),
					aliasDefinitions);
		}

        RangeValidationConstraint[] rangeValidationConstraints = null;
		String rangeValidationConfigFileName = bbStochModelConfigXML.getRangeValidationConfigFile();
		if (rangeValidationConfigFileName != null) {//if range validation config file configured.
			File rangeValidationConfigFile = new File(stochModelConfigFile.getParentFile(), rangeValidationConfigFileName);
			if (!rangeValidationConfigFile.exists()) {
				throw new RuntimeException("Stoch Model configuration " + stochModelConfigFile.getAbsolutePath() +
						" refers to non existing range validation configuration file " + rangeValidationConfigFile.getAbsolutePath());
			}
            RangeValidationConfigReader rangeValidationConfigReader = new RangeValidationConfigReader(rangeValidationConfigFile);
            rangeValidationConstraints = rangeValidationConfigReader.getRangeValidationConstraints();
		}

		// get boundary provider, if specified
		ArrayList<BBBoundaryProviderConfig> boundaryProviderConfigs = new ArrayList<BBBoundaryProviderConfig>();

		BoundaryProviderXML[] boundaryProviderXMLs = bbStochModelConfigXML.getBoundaryProvider();
		if (boundaryProviderXMLs.length > 0) {
			for (BoundaryProviderXML boundaryProviderXML : boundaryProviderXMLs) {

				BoundaryDataObjectXML dataObjectXML = boundaryProviderXML.getDataObject();

				String dataObjectClassName = dataObjectXML.getClassName();
				String fileName = dataObjectXML.getFile();
				String[] dataObjectArguments = dataObjectXML.getArg();

				ArrayList<BBBoundaryMappingConfig> boundaryMappingConfigs = new ArrayList<BBBoundaryMappingConfig>();
				BoundaryMappingXML[] boundaryMappingXMLs = boundaryProviderXML.getBoundaryMapping();
				for (BoundaryMappingXML boundaryMappingXML : boundaryMappingXMLs) {
					int operationType;
					switch (boundaryMappingXML.getOperation().getType()) {
						case NoiseOperationTypesXML.ADD_TYPE:
							operationType = BBRegularisationConstantConfig.OPERATION_ADD;
							break;
						case NoiseOperationTypesXML.MULTIPLY_TYPE:
							operationType = BBRegularisationConstantConfig.OPERATION_MULTIPLY;
							break;
						case NoiseOperationTypesXML.SET_TYPE:
							operationType = BBRegularisationConstantConfig.OPERATION_SET;
							break;
						default:
							throw new RuntimeException("Unexpected operation type " +
									boundaryMappingXML.getOperation().getType() + " in " +
									stochModelConfigFile.getAbsolutePath());
					}
					Map<String,String> mappingExchangeItems = new HashMap<String, String>();
					for (BoundaryExchangeItemXML exchangeItemXML : boundaryMappingXML.getExchangeItem()) {
						String id = exchangeItemXML.getId();
						String modelExchangeItemId = id;
						if (exchangeItemXML.getModelExchangeItemId() != null) {
							modelExchangeItemId = exchangeItemXML.getModelExchangeItemId();
						}
						mappingExchangeItems.put(id, modelExchangeItemId);
					}
					BBBoundaryMappingConfig boundaryMappingConfig =
							new BBBoundaryMappingConfig(operationType, mappingExchangeItems);
					boundaryMappingConfigs.add(boundaryMappingConfig);

				}
				BBBoundaryProviderConfig boundaryProviderConfig = new BBBoundaryProviderConfig(
						dataObjectClassName, stochModelConfigFile.getParentFile(), fileName, dataObjectArguments, boundaryMappingConfigs);
				boundaryProviderConfigs.add(boundaryProviderConfig);
			}
		}


		BBStochModelVectorsConfig bbStochModelVectorsConfig = null;

        // parse vector specification
        Collection<BBRegularisationConstantConfig> regularisationConstantList = new ArrayList<BBRegularisationConstantConfig>();
        Collection<BBCartesianToPolarConfig> cartesianToPolarList = new ArrayList<BBCartesianToPolarConfig>();
        BBStochModelStateConfig stateConfig = null;
        Collection<BBStochModelVectorConfig> predictorVectorCollection = new ArrayList<BBStochModelVectorConfig>();

        BlackBoxStochModelVectorSpecificationXML vectorSpecificationXML = bbStochModelConfigXML.getVectorSpecification();

		boolean useUncertaintyEngine = false;
		boolean useNoiseModel = false;
		if (vectorSpecificationXML != null) {

			List<BBNoiseModelConfig> paramsUncertaintyModelConfigs = new ArrayList<BBNoiseModelConfig>();

			if (vectorSpecificationXML.getParameters() != null) {

				boolean useInLineDefinedStdDevs = false;
				BlackBoxStochModelParametersXMLItem[] parametersXMLItems = vectorSpecificationXML.getParameters().getBlackBoxStochModelParametersXMLItem();
				for (BlackBoxStochModelParametersXMLItem parametersXMLItem : parametersXMLItems) {

					RegularisationConstantXML regularisationConstantXML = parametersXMLItem.getRegularisationConstant();
					CartesianToPolarXML cartesianToPolarXML = parametersXMLItem.getCartesianToPolar();
					UncertaintyOrNoiseXML uncertaintyModuleXML = parametersXMLItem.getUncertaintyModule();

					if (regularisationConstantXML != null) {

						double stdDevValue = Double.NaN;
						double initialValue = 0.0;
						String uncertainItemId = null;
						int transformationType = BBRegularisationConstantConfig.TRANSFORMATION_IDENTITY;
						int operationType = BBRegularisationConstantConfig.OPERATION_ADD;
						RegularisationConstantXMLChoice stdDevOrUncItemXMLChoice =
								regularisationConstantXML.getRegularisationConstantXMLChoice();
						NoiseOperationTypesXML operationTypesXML = null;
						ParameterTransformationTypesXML transformationTypesXML;

						if (stdDevOrUncItemXMLChoice.getStdDev() != null) {
							// 'in line' uncertainty def., by means of StdDev
							stdDevValue = stdDevOrUncItemXMLChoice.getStdDev().getValue();
							transformationTypesXML = stdDevOrUncItemXMLChoice.getStdDev().getTransformation();
							initialValue = stdDevOrUncItemXMLChoice.getStdDev().getInitial();
							useInLineDefinedStdDevs = true;
						} else {
							BlackBoxStochModelUncertainParameterXML uncertainParameterXML =
									regularisationConstantXML.getRegularisationConstantXMLChoice().getUncertainItem();
							transformationTypesXML = uncertainParameterXML.getTransformation();
							uncertainItemId = uncertainParameterXML.getUncertainItemId();
							if (uncertaintyActionConfig == null) {
								throw new RuntimeException(
										"Uncertain parameter item specified, but no uncertainty module defined, file: " +
												stochModelConfigFile.getAbsolutePath());
							}
						}

						if (operationTypesXML != null) {
							switch (operationTypesXML.getType()) {
								case NoiseOperationTypesXML.ADD_TYPE:
									operationType = BBRegularisationConstantConfig.OPERATION_ADD;
									break;
								case NoiseOperationTypesXML.MULTIPLY_TYPE:
									operationType = BBRegularisationConstantConfig.OPERATION_MULTIPLY;
									break;
								case NoiseOperationTypesXML.SET_TYPE:
									operationType = BBRegularisationConstantConfig.OPERATION_SET;
									break;
							}
						}

						if (transformationTypesXML != null) {
							transformationType = determineTransformationType(transformationTypesXML);
						}

						regularisationConstantList.add(new BBRegularisationConstantConfig(
								regularisationConstantXML.getId(),
                                regularisationConstantXML.getScale(),
                                stdDevValue,
								initialValue,
								uncertainItemId,
								operationType,
								transformationType,
                                parseRegularisationConstantVectorConfigs(stochModelConfigFile, bbModelConfig,
                                        regularisationConstantXML.getRegularisationConstantXMLChoice2())));

					} else if (cartesianToPolarXML != null) {

						if (cartesianToPolarXML.getStdDev() == null) {
							// uncertainty engine can not any longer be used for cartesianToPolar
							throw new RuntimeException(
									"Combination of engine and cartesianToPolar not allowed any more");
						}
						// 'in line' uncertainty def., by means of StdDev
						double stdDevValue = cartesianToPolarXML.getStdDev().getValue();
						useInLineDefinedStdDevs = true;
						cartesianToPolarList.add(new BBCartesianToPolarConfig(
                                cartesianToPolarXML.getXScale(),
                                cartesianToPolarXML.getYScale(),
                                cartesianToPolarXML.getXSuffixCaption(),
                                cartesianToPolarXML.getYSuffixCaption(),
                                cartesianToPolarXML.getXCaption(),
                                cartesianToPolarXML.getYCaption(),
								stdDevValue,
								parseCartesianToPolarVectorConfigs(stochModelConfigFile, bbModelConfig,
                                        cartesianToPolarXML.getCartesianToPolarXMLChoice())));
					} else {
						BBNoiseModelConfig noiseModelConfig = parseNoiseModelConfig(stochModelConfigFile,
								uncertaintyModuleXML,
								aliasDefinitions);
						// If the noise model is en UncertaintyEngine, check if no global one is specific
						if (uncertaintyActionConfig != null) {
							if (noiseModelConfig.getClassName().equals(UncertaintyEngine.class.getName())) {
								throw new RuntimeException(
										"You can not specify both one all over UncertaintyEngine module and one for " +
												"the parameters at the same time, file: " +
												stochModelConfigFile.getAbsolutePath());
							}
						}
						paramsUncertaintyModelConfigs.add(noiseModelConfig);
					}
                }
                if (useInLineDefinedStdDevs && useUncertaintyEngine) {
                    throw new RuntimeException(
                            "You can not mix in line stdDev definitions and uncertain items from uncertainty module, file: "  +
                                    stochModelConfigFile.getAbsolutePath());
                }
				if (useInLineDefinedStdDevs && useNoiseModel) {
					throw new RuntimeException(
							"You can not mix in line stdDev definitions and noise from moise models, file: " +
									stochModelConfigFile.getAbsolutePath());
				}
			}

            if (vectorSpecificationXML.getState() != null) {

				List<BBNoiseModelConfig> stateNoiseModelConfigs = new ArrayList<BBNoiseModelConfig>();

				Collection<BBStochModelVectorConfig> vectorCollection = new ArrayList<BBStochModelVectorConfig>();
				Collection<BBUncertOrArmaNoiseConfig> uncertOrArmaBasedStateNoiseConfigs = new ArrayList<BBUncertOrArmaNoiseConfig>();

                BlackBoxStochModelStateXMLItem[] stateXMLItems =
                        vectorSpecificationXML.getState().getBlackBoxStochModelStateXMLItem();

                for (BlackBoxStochModelStateXMLItem stateXMLItem : stateXMLItems) {

                    StateNoiseXML noiseModelXML = stateXMLItem.getNoiseModel();

                    if (noiseModelXML != null) {

                        // Add a noise model to the state

						if (noiseModelXML.getClassName() != null) {
							// stochModelFactory / stochModelInstance noise model
							BBNoiseModelConfig noiseModelConfig = parseNoiseModelConfig(stochModelConfigFile, noiseModelXML, aliasDefinitions);
							stateNoiseModelConfigs.add(noiseModelConfig);

						} else {

							// non 'formal-stochModelFactory' noise model
							// old 'arma noise model' or reference to uncertain

							// vectors to related to noise
							StateNoiseXMLChoice vectorXMLItems[] =
									noiseModelXML.getStateNoiseXMLChoice();

							// Arma model definition, possible choices
							StateNoiseXMLChoice2 armaModelChoice =
									noiseModelXML.getStateNoiseXMLChoice2();
							StateNoiseUncertainItemXML uncertainItemXML = armaModelChoice.getUncertainItem();
							StateNoiseUncertainItemWithArmaConstantsXML itemWithArmaConstantsXML =
									armaModelChoice.getUncertainItemWithArmaConstants();
							StateNoiseArmaModelXML armaModelXML = armaModelChoice.getArmaModel();
							StateNoiseAr1ModelXML ar1ModelXML = armaModelChoice.getAr1Model();

							if (noiseModelXML.getClassName() != null &&
									(vectorXMLItems == null || vectorXMLItems.length == 0 ||
									uncertainItemXML != null || itemWithArmaConstantsXML != null ||
									armaModelXML != null || ar1ModelXML != null) ) {
								throw new RuntimeException("You can not specify Noise Model class and " +
										"arma model or uncertainty model (and their (sub)vectors) " +
										"at the same time" + stochModelConfigFile.getAbsolutePath());
							}

							// store vector configs for the (sub)vectors to impose the noise on
							BBStochModelVectorConfig vectorConfigs[] = new BBStochModelVectorConfig[vectorXMLItems.length];
							for (int i = 0, vectorXMLItemsLength = vectorXMLItems.length; i < vectorXMLItemsLength; i++) {
								StateNoiseXMLChoice vectorXMLItem = vectorXMLItems[i];
								vectorConfigs[i] = parseStochModelVectorOrSubVector(
										stochModelConfigFile, bbModelConfig,
										vectorXMLItem.getStateNoiseXMLChoiceItem().getSubVector(),
										vectorXMLItem.getStateNoiseXMLChoiceItem().getVector());
							}

							// Arma model definition, determine values based on choice

							BBUncertOrArmaNoiseConfig.NoiseModelType noiseModelType;
							String uncertainItemId = null;
							double[] armaConstants = null;
							double stdDev = Double.NaN;
							BBUncertOrArmaNoiseConfig.Operation operation;
							if (uncertainItemXML != null) {
								noiseModelType = BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItem;
								uncertainItemId = uncertainItemXML.getUncertainItemId();
								operation = determineOperationType(uncertainItemXML.getOperation());
							} else if (itemWithArmaConstantsXML != null) {
								noiseModelType = BBUncertOrArmaNoiseConfig.NoiseModelType.UncertainItemWithArmaConstants;
								uncertainItemId = itemWithArmaConstantsXML.getUncertainItemId();
								operation = determineOperationType(itemWithArmaConstantsXML.getOperation());
								armaConstants = itemWithArmaConstantsXML.getArmaConstant();
							} else if (armaModelXML != null) {
								noiseModelType = BBUncertOrArmaNoiseConfig.NoiseModelType.ArmaModel;
								operation = determineOperationType(armaModelXML.getOperation());
								armaConstants = armaModelXML.getArmaConstant();
								stdDev = armaModelXML.getStdDev().getValue();
							} else {
								noiseModelType = BBUncertOrArmaNoiseConfig.NoiseModelType.Ar1Model;
								operation = determineOperationType(ar1ModelXML.getOperation());
								double stdDevColouredNoised = ar1ModelXML.getStdDevColouredNoised().getValue();
								Duration decorrelationTimeScale = ar1ModelXML.getDecorrelationTimeScale();
								Duration noiseModelTimeStep = ar1ModelXML.getNoiseModelPeriod();
								double tstep = (double) noiseModelTimeStep.toLong();
								double tscale = (double) decorrelationTimeScale.toLong();
								double alpha = Math.exp(-(tstep / tscale));
								double stdDevWhiteNoise = Math.sqrt(1d - Math.pow(alpha, 2d)) * stdDevColouredNoised;
								armaConstants = new double[]{alpha};
								stdDev = stdDevWhiteNoise;
							}
							BBUncertOrArmaNoiseConfig stateNoiseModelConfig =
									new BBUncertOrArmaNoiseConfig(noiseModelXML.getId(),
											noiseModelType, vectorConfigs, uncertainItemId,
											stdDev, armaConstants, operation);
							uncertOrArmaBasedStateNoiseConfigs.add(stateNoiseModelConfig);
						}

					} else {
                        // Add a vector or subvector to the state
                        BBStochModelVectorConfig vectorConfig = parseStochModelVectorOrSubVector(
                                stochModelConfigFile, bbModelConfig,
                                stateXMLItem.getSubVector(), stateXMLItem.getVector());
                        vectorCollection.add(vectorConfig);
                    }
                }
                stateConfig = new BBStochModelStateConfig(stateNoiseModelConfigs,
						uncertOrArmaBasedStateNoiseConfigs, vectorCollection);
            }

            boolean isCollectPredictorTimeSeries = false;
            if (vectorSpecificationXML.getPredictor() != null) {
                BlackBoxStochModelVectorsXMLItem[] vectorXMLItems =
                        vectorSpecificationXML.getPredictor().getBlackBoxStochModelVectorsXMLItem();
                for (BlackBoxStochModelVectorsXMLItem vectorXMLItem : vectorXMLItems) {
                    BBStochModelVectorConfig vectorConfig = parseStochModelVectorOrSubVector(
                            stochModelConfigFile, bbModelConfig,
                            vectorXMLItem.getSubVector(), vectorXMLItem.getVector());
                    predictorVectorCollection.add(vectorConfig);
                }
                isCollectPredictorTimeSeries = vectorSpecificationXML.getPredictor().getCollectTimeSeries();
            }


            bbStochModelVectorsConfig = new BBStochModelVectorsConfig(
					paramsUncertaintyModelConfigs,
                    regularisationConstantList,
                    cartesianToPolarList,
                    stateConfig,
                    predictorVectorCollection,
                    isCollectPredictorTimeSeries,
                    rangeValidationConstraints);
        }

		// get the restart info
		String restartStatesDirPrefix = "./savedStochModelState_";
		String restartStatesNoiseModelPrefix = "./noiseModel_";
		String modelRestartStateFile = "./modelState.zip";
		if (bbStochModelConfigXML.getRestartInfo() != null) {
			restartStatesDirPrefix = bbStochModelConfigXML.getRestartInfo().getDirPrefix();
			restartStatesNoiseModelPrefix = bbStochModelConfigXML.getRestartInfo().getNoiseModelPrefix();
			modelRestartStateFile = bbStochModelConfigXML.getRestartInfo().getModelStateFile();
		}
		bbStochModelConfig = new BBStochModelConfig(bbModelConfig,
                modelFactoryWorkingDir, modelFactoryActionConfig,
                uncertaintyWorkingDir, uncertaintyActionConfig,
				boundaryProviderConfigs,
                bbStochModelVectorsConfig,
				restartStatesDirPrefix,
				restartStatesNoiseModelPrefix,
				modelRestartStateFile,useUncertaintyEngine);
    }

	public BBStochModelConfig getBBStochModelConfig() {
		return bbStochModelConfig;
	}

	private BBNoiseModelConfig parseNoiseModelConfig(File stochModelConfigFile, UncertaintyOrNoiseXML noiseModelXML, AliasDefinitions aliasDefinitions) {
		File noiseModelWorkingDirectory = new File(stochModelConfigFile.getParentFile(), noiseModelXML.getWorkingDirectory());
		UncertaintyOrNoiseExchangeItemsXML exchangeItemsXML = noiseModelXML.getExchangeItems();
		String noiseModelConfigFile = noiseModelXML.getConfigFile();
		String noiseModelClassName = noiseModelXML.getClassName();
		String[] noiseModelArguments = noiseModelXML.getArg();
		return parseNoiseModelConfig(stochModelConfigFile, aliasDefinitions, noiseModelWorkingDirectory,
				exchangeItemsXML, noiseModelConfigFile, noiseModelClassName, noiseModelArguments);
	}

	private BBNoiseModelConfig parseNoiseModelConfig(File stochModelConfigFile, StateNoiseXML noiseModelXML, AliasDefinitions aliasDefinitions) {
		File noiseModelWorkingDirectory = new File(stochModelConfigFile.getParentFile(), noiseModelXML.getWorkingDirectory());
		UncertaintyOrNoiseExchangeItemsXML exchangeItemsXML = noiseModelXML.getExchangeItems();
		String noiseModelConfigFile = noiseModelXML.getConfigFile();
		String noiseModelClassName = noiseModelXML.getClassName();
		return parseNoiseModelConfig(stochModelConfigFile, aliasDefinitions, noiseModelWorkingDirectory,
				exchangeItemsXML, noiseModelConfigFile, noiseModelClassName, new String[]{});
	}

	private BBNoiseModelConfig parseNoiseModelConfig(File stochModelConfigFile, AliasDefinitions aliasDefinitions, File noiseModelWorkingDirectory, UncertaintyOrNoiseExchangeItemsXML exchangeItemsXML, String noiseModelConfigFile, String noiseModelClassName, String[] noiseModelArguments) {
		List<NoiseModelExchangeItemConfig> exchangeItemConfigs = new ArrayList<NoiseModelExchangeItemConfig>();
		if (exchangeItemsXML != null) {
			for (UncertaintyOrNoiseExchangeItemXML exchangeItemXML : exchangeItemsXML.getExchangeItem()) {
				UncertaintyOrNoiseModelExchangeItemXML[] modelExchangeItemXMLs = exchangeItemXML.getModelExchangeItem();
				List<String> modelExchangeItemIds = new ArrayList<String>();
				boolean skipFirstTimeStep = exchangeItemXML.getSkipFirstTimeStep();
				boolean addOnlyNoiseDifference = exchangeItemXML.getAddOnlyNoiseDifference();
				if (modelExchangeItemXMLs.length > 0 && exchangeItemXML.getModelExchangeItemId() != null) {
					throw new RuntimeException(
							"The attribute \"modelExchangeItemId\"and " +
									"the elements <modelExchangeItem> can not be mixed, file: " +
									stochModelConfigFile.getAbsolutePath());
				} else if (modelExchangeItemXMLs.length == 0 && exchangeItemXML.getModelExchangeItemId() != null ) {
					exchangeItemConfigs.add(new NoiseModelExchangeItemConfig(
							exchangeItemXML.getId(),
							exchangeItemXML.getModelExchangeItemId(),
							determineOperationType(exchangeItemXML.getOperation()),
							determineTransformationType(exchangeItemXML.getTransformation() ), skipFirstTimeStep, addOnlyNoiseDifference)
					);
				} else {
					for (UncertaintyOrNoiseModelExchangeItemXML modelExchangeItemXML : modelExchangeItemXMLs) {
						modelExchangeItemIds.add(modelExchangeItemXML.getId());
					}
					exchangeItemConfigs.add(new NoiseModelExchangeItemConfig(
							exchangeItemXML.getId(),
							modelExchangeItemIds,
							determineOperationType(exchangeItemXML.getOperation()),
							determineTransformationType(exchangeItemXML.getTransformation() ), skipFirstTimeStep, addOnlyNoiseDifference, exchangeItemXML.getStateSizeNoiseSizeRatio(), exchangeItemXML.getAddStateNoiseAfterCompute())
					);
				}
			}
		}

		return new BBNoiseModelConfig(
				aliasDefinitions,
				noiseModelWorkingDirectory,
				noiseModelConfigFile,
				noiseModelClassName,
				noiseModelArguments,
				exchangeItemConfigs
		);
	}

	private static BBUncertOrArmaNoiseConfig.Operation determineOperationType(NoiseOperationTypesXML operationXML) {
		BBUncertOrArmaNoiseConfig.Operation operation = BBUncertOrArmaNoiseConfig.Operation.Add;
		if (operationXML.getType() ==
				NoiseOperationTypesXML.ADD_TYPE) {
			operation = BBUncertOrArmaNoiseConfig.Operation.Add;
		} else if (operationXML.getType() ==
				NoiseOperationTypesXML.MULTIPLY_TYPE) {
			operation = BBUncertOrArmaNoiseConfig.Operation.Multiply;
		}
		return operation;
	}

	private static int determineTransformationType(org.openda.core.io.castorgenerated.types.ParameterTransformationTypesXML transformationTypesXML) {
		int transformationType;
		switch (transformationTypesXML.getType()) {
			case ParameterTransformationTypesXML.IDENTITY_TYPE:
				transformationType = BBRegularisationConstantConfig.TRANSFORMATION_IDENTITY;
				break;
			case ParameterTransformationTypesXML.LN_TYPE:
				transformationType = BBRegularisationConstantConfig.TRANSFORMATION_LN;
				break;
			case ParameterTransformationTypesXML.SET_TYPE:
				transformationType = BBRegularisationConstantConfig.TRANSFORMATION_SET;
				break;
			default:
				throw new RuntimeException("Invalid Transformation Type");
		}
		return transformationType;
	}

    private static List<BBStochModelVectorConfig> parseRegularisationConstantVectorConfigs(File stochModelConfigFile, BBModelConfig bbModelConfig,
                                                                                           RegularisationConstantXMLChoice2[] regularisationConstantXMLChoices
    ) {
        List<BBStochModelVectorConfig> stochModelVectorConfigs = new ArrayList<BBStochModelVectorConfig>();
        for (RegularisationConstantXMLChoice2 regularisationConstantXMLChoice2 : regularisationConstantXMLChoices) {
            BlackBoxStochModelSubVectorXML subVectorXML =
					regularisationConstantXMLChoice2.getRegularisationConstantXMLChoice2Item().getSubVector();
            BlackBoxStochModelVectorXML vectorXML =
					regularisationConstantXMLChoice2.getRegularisationConstantXMLChoice2Item().getVector();
            BBStochModelVectorConfig modelVectorConfig = parseStochModelVectorOrSubVector(stochModelConfigFile, bbModelConfig, subVectorXML, vectorXML);
            stochModelVectorConfigs.add(modelVectorConfig);
        }
        return stochModelVectorConfigs;
    }

    private static List<BBStochModelVectorConfig> parseCartesianToPolarVectorConfigs(File stochModelConfigFile, BBModelConfig bbModelConfig, CartesianToPolarXMLChoice[] cartesianToPolarXMLChoices2) {
        List<BBStochModelVectorConfig> stochModelVectorConfigs = new ArrayList<BBStochModelVectorConfig>();
        for (CartesianToPolarXMLChoice cartesianToPolarXMLChoice : cartesianToPolarXMLChoices2) {
            BlackBoxStochModelSubVectorXML subVectorXML =
                    cartesianToPolarXMLChoice.getCartesianToPolarXMLChoiceItem().getSubVector();
            BlackBoxStochModelVectorXML vectorXML =
                    cartesianToPolarXMLChoice.getCartesianToPolarXMLChoiceItem().getVector();
            BBStochModelVectorConfig modelVectorConfig = parseStochModelVectorOrSubVector(stochModelConfigFile, bbModelConfig, subVectorXML, vectorXML);
            stochModelVectorConfigs.add(modelVectorConfig);
        }
        if (!(stochModelVectorConfigs.size() % 2 == 0)) {
            throw new RuntimeException("Stoch Model configuration " + stochModelConfigFile.getAbsolutePath() +
                    " contains a CartesianToPolar configuration that does not have " +
                    " a multiple of 2 source components (angle and radius)");
        }
        return stochModelVectorConfigs;
    }

    private static BBStochModelVectorConfig parseStochModelVectorOrSubVector(File stochModelConfigFile, BBModelConfig bbModelConfig, BlackBoxStochModelSubVectorXML subVectorXML, BlackBoxStochModelVectorXML vectorXML) {
        String id;
        String sourceVectorId;
        DimensionIndex[] selectionIndices = null;
        BBConfigurable selectorConfig = null;
        if (vectorXML != null) {
            // simple vector
            id = vectorXML.getId();
            sourceVectorId = vectorXML.getSourceVectorId();
            if (sourceVectorId == null) {
                sourceVectorId = id;
            }
        } else {
            // subvector (with sub-indices or selector)
            id = subVectorXML.getId();
            sourceVectorId = subVectorXML.getSourceVectorId();
            if (sourceVectorId == null) {
                sourceVectorId = id;
            }
            selectionIndices = BBModelConfigReader.parseSelectionIndices(subVectorXML.getSelection());
			AliasDefinitions aliasDefinitions = new AliasDefinitions();
			if (bbModelConfig != null) {
				aliasDefinitions = bbModelConfig.getWrapperConfig().getAliasDefinitions();
			}
			selectorConfig = BBWrapperConfigReader.parseBbConfigurable(
                    stochModelConfigFile,
					aliasDefinitions,
                    subVectorXML.getSelector());
        }
        return new BBStochModelVectorConfig(
                id, sourceVectorId, selectionIndices, selectorConfig);
    }
}
