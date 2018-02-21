/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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

package org.openda.utils.io;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.ValidationException;
import org.openda.core.io.castorgenerated.*;
import org.openda.core.io.castorgenerated.types.ApplicationInitialSeedTypesXML;
import org.openda.core.io.castorgenerated.types.ResultWriterSourceTypeXML;
import org.openda.interfaces.IResultWriter;
import org.openda.utils.*;

import java.io.File;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;


/**
 * Reader for an OpenDA application file
 */
public class OpenDaConfigurationReader {

    private OpenDaConfiguration openDaConfiguration;

    public OpenDaConfigurationReader(File openDaApplicationFile) {

        OpenDaApplicationXML applicationXML = (OpenDaApplicationXML) CastorUtils.parse(openDaApplicationFile, OpenDaApplicationXML.class);

        File algorithmWorkingDir = composeWorkingDir(openDaApplicationFile, applicationXML.getAlgorithm().getWorkingDirectory());
        boolean algorithmConfigIsFile = false;
        String algorithmClassName = applicationXML.getAlgorithm().getClassName();
        String algorithmConfigString = applicationXML.getAlgorithm().getOpenDaAlgorithmXMLChoice().getConfigString();
        if (algorithmConfigString == null) {
            algorithmConfigString = applicationXML.getAlgorithm().getOpenDaAlgorithmXMLChoice().getConfigFile();
            if (algorithmConfigString != null) {
                algorithmConfigIsFile = true;
            } else {
                Writer xmlStringWriter = new StringWriter();
                try {
                    applicationXML.getAlgorithm().marshal(xmlStringWriter);
                    algorithmConfigString = xmlStringWriter.toString();
                } catch (MarshalException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Algorithm", openDaApplicationFile));
                } catch (ValidationException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Algorithm", openDaApplicationFile));
                }
            }
        }
        String[] algorithmArguments = {algorithmConfigString};
        OpenDaComponentConfig algorithmConfig = new OpenDaComponentConfig(algorithmWorkingDir, algorithmClassName,
                algorithmArguments, algorithmConfigIsFile);


        File stochObserverWorkingDir = composeWorkingDir(openDaApplicationFile, applicationXML.getStochObserver().getWorkingDirectory());
        boolean stochObserverConfigIsFile = false;
        String stochObserverClassName = applicationXML.getStochObserver().getClassName();
        String stochObserverConfigString = applicationXML.getStochObserver().getOpenDaStochObserverXMLChoice().getConfigString();
        String[] stochObserverArguments;
        if (stochObserverConfigString == null) {
            stochObserverConfigString = applicationXML.getStochObserver().getOpenDaStochObserverXMLChoice().getConfigFile();
            if (stochObserverConfigString != null) {
                stochObserverConfigIsFile = true;
            } else {
                Writer xmlStringWriter = new StringWriter();
                try {
                    applicationXML.getStochObserver().marshal(xmlStringWriter);
                    stochObserverConfigString = xmlStringWriter.toString();
                } catch (MarshalException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Stoch. Observer", openDaApplicationFile));
                } catch (ValidationException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Stoch. Observer", openDaApplicationFile));
                }
            }
            stochObserverArguments = new String[]{stochObserverConfigString};
        }  else {
            // (sh) TODO JD: explain usage; usage less frequently used splitting character
            stochObserverArguments = stochObserverConfigString.split(" ");
        }

        OpenDaComponentConfig stochObserverConfig = new OpenDaComponentConfig(stochObserverWorkingDir, stochObserverClassName,
                stochObserverArguments, stochObserverConfigIsFile);

        File stochModelFactoryWorkingDir = composeWorkingDir(openDaApplicationFile, applicationXML.getStochModelFactory().getWorkingDirectory());
        boolean stochModelFactoryConfigIsFile = false;
        String stochModelFactoryClassName = applicationXML.getStochModelFactory().getClassName();
        String stochModelFactoryConfigString = applicationXML.getStochModelFactory().getOpenDaStochModelXMLChoice().getConfigString();
        if (stochModelFactoryConfigString == null) {
            stochModelFactoryConfigString = applicationXML.getStochModelFactory().getOpenDaStochModelXMLChoice().getConfigFile();
            if (stochModelFactoryConfigString != null) {
                stochModelFactoryConfigIsFile = true;
            } else {
                Writer xmlStringWriter = new StringWriter();
                try {
                    applicationXML.getStochModelFactory().marshal(xmlStringWriter);
                    stochModelFactoryConfigString = xmlStringWriter.toString();
                } catch (MarshalException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Stoch. ModelFactory", openDaApplicationFile));
                } catch (ValidationException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Stoch. ModelFactory", openDaApplicationFile));
                }
            }
        }
        String[] stochModelFactoryArguments = {stochModelFactoryConfigString};
        OpenDaComponentConfig stochModelFactoryConfig = new OpenDaComponentConfig(stochModelFactoryWorkingDir, stochModelFactoryClassName,
                stochModelFactoryArguments, stochModelFactoryConfigIsFile);

        List<OpenDaResultWriterConfig> resultWriterConfigs = new ArrayList<OpenDaResultWriterConfig>();
        if (applicationXML.getResultWriter() != null) {
            OpenDaResultWriterXML resultWriterXML = applicationXML.getResultWriter();
            OpenDaResultWriterConfig resultWriterConfig = parseResultWriterConfig(openDaApplicationFile, resultWriterXML);
            resultWriterConfigs.add(resultWriterConfig);
        } else if (applicationXML.getResultWriters() != null) {
            OpenDaResultWriterXML[] openDaResultWriterXMLs = applicationXML.getResultWriters().getResultWriter();
            for (OpenDaResultWriterXML openDaResultWriterXML : openDaResultWriterXMLs) {
                OpenDaResultWriterConfig resultWriterConfig = parseResultWriterConfig(openDaApplicationFile, openDaResultWriterXML);
                resultWriterConfigs.add(resultWriterConfig);
            }
        }

        File restartInFile = null;
        File restartOutFilePrefix = null;

        String restartInFileName = applicationXML.getRestartInFile();
        if (restartInFileName != null && restartInFileName.trim().length() > 0) {
            restartInFile = new File(openDaApplicationFile.getParentFile(), restartInFileName);
            if (!restartInFile.exists()) {
                throw new RuntimeException("Restart In File " + restartInFile.getAbsolutePath() + " does not exists");
            }
            // a file of length=0 is considered as a flag for a cold start, that does not use a restart
            if(restartInFile.canRead() && restartInFile.length()==0){
            	Results.putMessage("Warning: the restart file has zero lenght and will be ignored "+restartInFile.getAbsolutePath());
            	System.out.println("Warning: the restart file has zero lenght and will be ignored "+restartInFile.getAbsolutePath());
            	restartInFile=null;
            }
        }
        String restartOutFilePrefixName = applicationXML.getRestartOutFilePrefix();
        String restartOutFileExtension = null;
        if (restartOutFilePrefixName != null && restartOutFilePrefixName.trim().length() > 0) {
            restartOutFilePrefix = new File(openDaApplicationFile.getParentFile(), restartOutFilePrefixName);
            restartOutFileExtension = applicationXML.getRestartOutFileExtension() != null ?
                    applicationXML.getRestartOutFileExtension() : "xml";
        }
        // allow disabling of time tags in restart filenames
        String restartOutFileTagString = applicationXML.getRestartOutFileTimeTag();
        boolean restartOutFileTimeTag = true;
        if(restartOutFileTagString.equalsIgnoreCase("no")){
            restartOutFileTimeTag = false;        	
        }
        
		String restartOutFileTimes = null;
		String restartOutFileTimeFormat = null;
		if (applicationXML.getRestartOutFileTimes() != null) {
			restartOutFileTimes = applicationXML.getRestartOutFileTimes().getContent();
			restartOutFileTimeFormat = applicationXML.getRestartOutFileTimes().getTimeFormat().toString();
		}

		boolean doTiming = false;
		if (applicationXML.getTimingSettings() != null) {
			doTiming = applicationXML.getTimingSettings().getDoTiming();
		}
		boolean productionRun = false;
		if (applicationXML.getOptimizationSettings() != null) {
			productionRun = applicationXML.getOptimizationSettings().getProductionRun();
		}
		boolean vectorPrecisionIsFloat = false;
		if (applicationXML.getOptimizationSettings() != null) {
			vectorPrecisionIsFloat = applicationXML.getOptimizationSettings().getVectorPrecisionIsFloat();
		}

        boolean vectorIsNative = false;
        if (applicationXML.getOptimizationSettings() != null) {
            vectorIsNative = applicationXML.getOptimizationSettings().getVectorIsNative();
        }


		double timePrecision = 1.0/24.0/60.0/60.0; //1 second
		if (applicationXML.getTimeSettings() != null) {
			timePrecision= Double.parseDouble(applicationXML.getTimeSettings().getPrecision());
			String unit= applicationXML.getTimeSettings().getUnit();

			double factor=1.0;
			if ("hour".equalsIgnoreCase(unit)){
				factor=1.0/24.0;
			}
			else if ("min".equalsIgnoreCase(unit)){
				factor=1.0/24.0/60.0;
			}
			else if ("sec".equalsIgnoreCase(unit)){
				factor=1.0/24.0/60.0/60.0;
			}
			else if ("day".equalsIgnoreCase(unit) || "".equalsIgnoreCase(unit)){
				factor=1.0;
			}
			else {
				throw new RuntimeException("Unit for time precision:"+unit+" is not supported.\n"+
			                           "Allowed values are \"day\",\"hour\",\"sec\" and \"\"");
			}
			timePrecision*=factor;
		}

		ApplicationInitialSeedXML initialSeedXML = applicationXML.getInitialSeed();
		StochVector.InitialSeedType initialSeedType = StochVector.InitialSeedType.fixed; // default
		int initialSeedValue = Integer.MIN_VALUE;
		if (initialSeedXML != null) {
			int type = initialSeedXML.getType().getType();
			switch (type) {
				case ApplicationInitialSeedTypesXML.FIXED_TYPE:
					initialSeedType = StochVector.InitialSeedType.fixed;
					break;
				case ApplicationInitialSeedTypesXML.RANDOM_TYPE:
					initialSeedType = StochVector.InitialSeedType.random;
					break;
				case ApplicationInitialSeedTypesXML.SPECIFY_TYPE:
					initialSeedType = StochVector.InitialSeedType.specify;
					if (!initialSeedXML.hasSeedValue()) {
						throw new RuntimeException("\"specify\" defined, but no initialSeedValue specified in " +
											openDaApplicationFile.getAbsolutePath());
					}
					initialSeedValue = initialSeedXML.getSeedValue();
					break;
			}
		}

		openDaConfiguration = new OpenDaConfiguration(stochObserverConfig, stochModelFactoryConfig,
				algorithmConfig, resultWriterConfigs,
				restartInFile, restartOutFilePrefix, restartOutFileExtension,
				restartOutFileTimes, restartOutFileTimeFormat,restartOutFileTimeTag,
				doTiming, productionRun, timePrecision, vectorPrecisionIsFloat, vectorIsNative,
				initialSeedType, initialSeedValue);
    }

    private OpenDaResultWriterConfig parseResultWriterConfig(File openDaApplicationFile, OpenDaResultWriterXML resultWriterXML) {
        File resultWriterWorkingDir = composeWorkingDir(openDaApplicationFile, resultWriterXML.getWorkingDirectory());
        boolean resultWriterConfigIsFile = false;
        String resultWriterClassName = resultWriterXML.getClassName();
        String resultWriterConfigString = resultWriterXML.getOpenDaResultWriterXMLChoice().getConfigString();
        if (resultWriterConfigString == null) {
            resultWriterConfigString = resultWriterXML.getOpenDaResultWriterXMLChoice().getConfigFile();
            if (resultWriterConfigString != null) {
                resultWriterConfigIsFile = true;
            } else {
                Writer xmlStringWriter = new StringWriter();
                try {
                    resultWriterXML.marshal(xmlStringWriter);
                    resultWriterConfigString = xmlStringWriter.toString();
                } catch (MarshalException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Result Writer", openDaApplicationFile));
                } catch (ValidationException e) {
                    throw new RuntimeException(makeErrorStringForInvalidXML("Result Writer", openDaApplicationFile));
                }
            }
        }
        ResultSelectionConfig resultSelectionConfig = parseResultSelectionConfig(resultWriterXML.getSelection());
        String[] resultWriterArguments = {resultWriterConfigString};
        return new OpenDaResultWriterConfig(resultWriterWorkingDir, resultWriterClassName,
                resultWriterArguments, resultWriterConfigIsFile, resultSelectionConfig);
    }

    public static ResultSelectionConfig parseResultSelectionConfig(ResultSelectionXML resultSelectionXML) {
        ResultSelectionConfig resultSelectionConfig = new ResultSelectionConfig();
		resultSelectionConfig.setWriteNonConfiguredItems(false);
        if (resultSelectionXML != null) {
            //TODO: improve the resultSelection.xsd by providing fixed options for attribute outputLevel,i.e.
            //TODO: None,Essential,Normal/Default,Verbose,All. This will help user understand what the possibilities are.
            ResultItemXML[] resultItemXMLs = resultSelectionXML.getResultItem();
            for (ResultItemXML resultItemXML : resultItemXMLs) {
                ResultWriterSourceTypeXML source = resultItemXML.getSource();
                String sourceAsString = source != null ? source.toString() : null;

                String resultItemXMLId;
                if (resultItemXML.getId()==null){
                    resultItemXMLId = IResultWriter.defaultId;
                } else {
                    resultItemXMLId = resultItemXML.getId();
                }
                int resultItemXMLMinSize;
                if (resultItemXML.hasMinSize()){
                    resultItemXMLMinSize = resultItemXML.getMinSize();
                } else {
                    resultItemXMLMinSize = IResultWriter.defaultMinSize;
                }
                int resultItemXMLMaxSize;
                if (resultItemXML.hasMaxSize()){
                    resultItemXMLMaxSize = resultItemXML.getMaxSize();
                } else {
                    resultItemXMLMaxSize = IResultWriter.defaultMaxSize;
                }
                IResultWriter.OutputLevel resultItemXMLOutputLevel;
                if (resultItemXML.getOutputLevel()==null){
                    resultItemXMLOutputLevel = IResultWriter.defaultOutputLevel;
                } else {
                    resultItemXMLOutputLevel = IResultWriter.OutputLevel.valueOf(resultItemXML.getOutputLevel().toString());
                }
                String resultItemXMLContext;
                if (resultItemXML.getContext()==null){
                    resultItemXMLContext = IResultWriter.defaultContext;
                } else {
                    resultItemXMLContext = resultItemXML.getContext();
                }
                boolean doLog = true;
                resultSelectionConfig.addResultItem(resultItemXMLId, resultItemXMLMinSize,
                        resultItemXMLMaxSize, resultItemXMLOutputLevel,
                        resultItemXMLContext, doLog);
            }
        } else {
			resultSelectionConfig.addResultItem(IResultWriter.defaultId, IResultWriter.defaultMinSize,
					IResultWriter.defaultMaxSize,IResultWriter.defaultOutputLevel,
					IResultWriter.defaultContext, true);
        }
        return resultSelectionConfig;
    }

    private File composeWorkingDir(File openDaApplicationFile, String workingDirectory) {
        File workingDir = new File(openDaApplicationFile.getParentFile(), workingDirectory);
        if (!workingDir.exists()) {
            throw new RuntimeException("Working directory " + workingDir.getAbsolutePath() + " does not exists");
        }
        if (!workingDir.isDirectory()) {
            throw new RuntimeException(workingDir.getAbsolutePath() + " is not a directory");
        }
        return workingDir;
    }

    public OpenDaConfiguration getOpenDaConfiguration() {
        return openDaConfiguration;
    }

    private String makeErrorStringForInvalidXML(String objectTypeName, File openDaApplicationFile) {
        return "Could not process " + objectTypeName +
                " XML configuration in OpenDA application (file: " +
                openDaApplicationFile.getAbsolutePath() + ")";
    }
}
