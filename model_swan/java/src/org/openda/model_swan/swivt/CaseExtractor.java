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

package org.openda.model_swan.swivt;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.model_swan.SwanResults;
import org.openda.model_swan.SwanXMLParameterSettings;
import org.openda.utils.ZipUtils;

import java.io.*;
import java.util.ArrayList;
import java.util.zip.ZipInputStream;

/**
 * Swivt Case Extractor, copies swivt case to the calibration directory, and generates a default calibration configuration
 */
public class CaseExtractor {

    private static final String observationDirName = "observ";
    private static final String modelIoDirName = "model_io";
    private static final String swivtCiPresSetDirName = "swivt_ci_pres_set";

    private File swivtCaseDirectory;

    public CaseExtractor(File swivtCaseDirectory) {

        this.swivtCaseDirectory = swivtCaseDirectory;

        File observationDir = new File(swivtCaseDirectory, observationDirName);
        if (!observationDir.exists() || !observationDir.isDirectory()) {
            throw new RuntimeException("No valid observation directory in swivt case directory " + swivtCaseDirectory.getAbsolutePath());
        }

        File modelIoDir = new File(swivtCaseDirectory, modelIoDirName);
        if (!modelIoDir.exists() || !modelIoDir.isDirectory()) {
            throw new RuntimeException("No valid observation directory in swivt case directory " + swivtCaseDirectory.getAbsolutePath());
        }
    }

    public void extractCase(File calibrationTargetParentDirectory,
                            String windowsSwanExePath, String linuxSwanCalBinDirPath,
                            String linuxSequentialSwanExeName,
                            String linuxParallelSwanExeName
    ) {


        // Create the target directory for this calibration file

        if (calibrationTargetParentDirectory == null) {
            throw new IllegalArgumentException("Calibration target parent directory == null");
        }
        if (!calibrationTargetParentDirectory.exists()) {
            throw new IllegalArgumentException("Calibration target parent directory " + calibrationTargetParentDirectory.getAbsolutePath() + " does not exist");
        }
        File targetDir = new File(calibrationTargetParentDirectory, swivtCaseDirectory.getName());
        if (targetDir.exists()) {
                BBUtils.deleteDirectory(targetDir);
            }

        if (!targetDir.mkdir()) {
            throw new RuntimeException("Could not create calibration case target directory " +
                    targetDir.getAbsolutePath());
        }

        // Unzip the default configuration directory structure.

        this.getClass().getResource("calibrationConfigTemplate.zip");
        InputStream templateZipFile = this.getClass().getResourceAsStream("calibrationConfigTemplate.zip");
        if (templateZipFile == null) {
            throw new RuntimeException("Could not find calibration configuration template " +
                    this.getClass().getName() + "/calibrationConfigTemplate.zip");
        }
        ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(templateZipFile));
        try {
            ZipUtils.unzipFiles(zipInputStream, targetDir);
        } catch (IOException e) {
            throw new RuntimeException("Could not unzip " + this.getClass().getResource("calibrationConfigTemplate.zip") +
                    " to " + targetDir.getAbsolutePath());
        }

        File stochObserverDir = new File(targetDir, "stochObserver");
        File stochModelDir = new File(targetDir, "stochModel");
        if (!stochObserverDir.exists()) {
            if (!stochObserverDir.mkdir()) {
                throw new RuntimeException("Could not create calibration case stoch. observer directory " +
                        stochObserverDir.getAbsolutePath());
            }
        }
        if (!stochModelDir.exists()) {
            if (!stochModelDir.mkdir()) {
                throw new RuntimeException("Could not create calibration case stoch. model directory " +
                        stochModelDir.getAbsolutePath());
            }
        }

        // Copy the swivt case data into the calibration directory structure

        File modelIoDir = new File(swivtCaseDirectory, modelIoDirName);
        File observationDir = new File(swivtCaseDirectory, observationDirName);
        BBUtils.copyDirectory(modelIoDir, new File(stochModelDir, modelIoDir.getName()), false);
        BBUtils.copyDirectory(observationDir, new File(stochObserverDir, observationDir.getName()), false);
        File presentationDir = new File(swivtCaseDirectory, swivtCiPresSetDirName);
        if (presentationDir.exists()) {
            BBUtils.copyDirectory(presentationDir, new File(stochModelDir, presentationDir.getName()), false);
        }

        createAlgorithmConfigurations(targetDir);

        createStochObserverConfig(targetDir);

        createStochModelConfig(targetDir, linuxSwanCalBinDirPath,
                windowsSwanExePath, linuxSequentialSwanExeName, linuxParallelSwanExeName);

    }

    private void createAlgorithmConfigurations(File targetDir) {
        // create the algorithm configs
        AlgConfigCreator algConfigCreator = new AlgConfigCreator(new File(targetDir, "templateSwanOpenDaConfig.xml"));
        algConfigCreator.createConfigFile("dud", "dud");
        algConfigCreator.createConfigFile("dud", "dudWithConstr");
        algConfigCreator.createConfigFile("powell", "powell");
        algConfigCreator.createConfigFile("powell", "powellWithConstr");
        algConfigCreator.createConfigFile("simplex", "simplex");
        algConfigCreator.createConfigFile("simplex", "simplexWithConstr");
    }

    @SuppressWarnings({"ConstantConditions"})
    private void createStochModelConfig(File targetDir,
                                        String calBinDirPath,
                                        String windowsSwanExePath,
                                        String linuxSequentialSwanExeName,
                                        String linuxParallelSwanExeName
                                        ) {

        // create main stoch model config file
        File stochModelConfigDir = new File(targetDir, "stochModel/config");

        final String parameterFileKeyword = "PARAMETER_FILE";
        final String inputTempleFileKeyword = "INPUT_TEMPLATE_FILE";
        final String obsLocationsFileKeyword = "OBS_LOC_FILE";
        final String resultsFileKeyword = "RESULTS_FILE";
        final String swanActionTag = "<swanAction";
        final String linuxSwanScriptName = "runSwan.sh";
        final String linuxSwanScriptKeyword = "LINUX_SWAN_EXECUTABLE_SCRIPT";
        final String windowsSwanExePathKeyword = "WINDOWS_SWAN_EXECUTABLE";

        String parameterFileName = "../" + modelIoDirName + "/" + getCaseName() + ".xml";
        String inputTempleFileName = "../" + modelIoDirName + "/" + getCaseName() + ".swn";
        String obsLocationsFileName = getCaseName() + ".loc";
        String resultsFileName = "swivt_" + getCaseName() + "_loc.tab";

        String linuxSwanScriptPath = "../pleaseSpecifyCalibrationBinDir/" + linuxSwanScriptName;
        if (calBinDirPath != null) {
            if (!calBinDirPath.endsWith("/")) {
                calBinDirPath += "/";
            }
            linuxSwanScriptPath = calBinDirPath + linuxSwanScriptName;
        }

        if (windowsSwanExePath == null || windowsSwanExePath.length() == 0) {
            windowsSwanExePath = "../pleaseSpecifyWindowsSwanExecutable";
        }

        if (linuxSequentialSwanExeName == null || linuxSequentialSwanExeName.length() == 0) {
            linuxSequentialSwanExeName = "pleaseSpecifyLinuxSequentialSwanExeName";
        }

        if (linuxParallelSwanExeName == null || linuxParallelSwanExeName.length() == 0) {
            linuxParallelSwanExeName = "pleaseSpecifyLinuxParallelSwanExeName";
        }

        File templateSwanWrapperConfigFile = new File(stochModelConfigDir, "templateSwanWrapperConfig.xml");
        File swanWrapperConfigFile = new File(stochModelConfigDir, "swanWrapperConfig.xml");
        try {
            PrintWriter writer = new PrintWriter(swanWrapperConfigFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(templateSwanWrapperConfigFile));
            String line = inputFileBufferedReader.readLine();
            while (line != null) {

                String outLine = line;

                boolean writeArguments = false;
                if (outLine.contains(parameterFileKeyword)) {
                    outLine = line.replace(parameterFileKeyword, parameterFileName);
                } else if (outLine.contains(inputTempleFileKeyword)) {
                    outLine = line.replace(inputTempleFileKeyword, inputTempleFileName);
                } else if (outLine.contains(obsLocationsFileKeyword)) {
                    outLine = line.replace(obsLocationsFileKeyword, obsLocationsFileName);
                } else if (outLine.contains(resultsFileKeyword)) {
                    outLine = line.replace(resultsFileKeyword, resultsFileName);
                } else if (outLine.contains(swanActionTag)) {
                    outLine = line.replace(linuxSwanScriptKeyword, linuxSwanScriptPath);
                    outLine = outLine.replace(windowsSwanExePathKeyword, windowsSwanExePath);
                    writeArguments = true;
                }
                writer.println(outLine);
                line = inputFileBufferedReader.readLine();
                if (writeArguments) {
                    writer.println("\t\t<arg>" + getCaseName() + "</arg>");
                    writer.println("\t\t<arg>-linuxSequentialExe</arg>");
                    writer.println("\t\t<arg>" + linuxSequentialSwanExeName + "</arg>");
                    writer.println("\t\t<arg>-linuxParallelExe</arg>");
                    writer.println("\t\t<arg>" + linuxParallelSwanExeName + "</arg>");
                }
            }
            inputFileBufferedReader.close();
            writer.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not produce swan wrapper config file " +
                    swanWrapperConfigFile.getAbsolutePath() + " from template " + templateSwanWrapperConfigFile.getAbsolutePath() +
                    ", error:" + e.getMessage());
        }
        if (!templateSwanWrapperConfigFile.delete()) {
            throw new RuntimeException("Could not delete swan wrapper config template file " +
                    templateSwanWrapperConfigFile.getAbsolutePath());
        }

        //code to handle the config for the swivt presentation module. Not used at the moment.
        //This is to be re-enabled when swivt presentation module is needed. See ODA-119.
//		createVisualizationPartOfStochModelConfig(targetDir, swivtRootDir, stochModelConfigDir);

        // create uncertainties from the active parameter ids and the default uncertainties

        File parameterFile = new File(stochModelConfigDir, parameterFileName);
        ArrayList<String> activeParameterIds = new SwanXMLParameterSettings(parameterFile).getIds();

        File uncertaintyDefaultsFile = new File(stochModelConfigDir, "paramUncertaintyDefaults.csv");
        if (!uncertaintyDefaultsFile.exists()) {
            throw new RuntimeException("Uncertainty defaults file does not exist: " +
                    uncertaintyDefaultsFile.getAbsolutePath());
                    }

		UncertainPars uncertainPars = new UncertainPars(uncertaintyDefaultsFile, activeParameterIds);
		createUncertainties(new File(stochModelConfigDir, "parameterUncertainties.xml"),
				uncertainPars.getParamIds(), uncertainPars.getParamStdDevs(), uncertainPars.getParamStdDevIsFactors(), null);
		if (!uncertaintyDefaultsFile.delete()) {
			throw new RuntimeException("Could not delete uncertainty defaults file " +
					uncertaintyDefaultsFile.getAbsolutePath());
		}
	}

	/**
	 * This method handles the config for the swivt presentation module.
	 *
	 * This method is not used at the moment. See comments in method CaseExtractor.createStochModelConfig and ODA-119.
	 *
	 * @param targetDir
	 * @param swivtRootDir
	 * @param stochModelConfigDir
	 */
	private void createVisualizationPartOfStochModelConfig(File targetDir, File swivtRootDir, File stochModelConfigDir) {
		final String swivtVisualizationExeKeyword = "SWIVT_VISUALIZATION_EXE";
		final String swivtPresentationFileKeyword = "SWIVT_PRESENTATION_FILE";
		String swivtVisualizationExePath;
		try {
			swivtVisualizationExePath =
					new File(swivtRootDir, "swivt_ci_postprocessing.exe").getCanonicalPath();
		} catch (IOException e) {
			throw new RuntimeException("Could not create full path names for visualization executable " +
					swivtRootDir.getAbsolutePath() + "swivt_ci_postprocessing.ex");
		}
		String swivtPresentationDirName = "NONE";
		File presentationSettingsDir = new File(targetDir, "stochModel/" + swivtCiPresSetDirName);
		if (presentationSettingsDir.exists()) {
			swivtPresentationDirName = "../" + swivtCiPresSetDirName;
		}

		File templateSwanVisualizationConfigFile = new File(stochModelConfigDir, "templateSwanVisualizationConfig.xml");
		File swanVisualizationConfigFile = new File(stochModelConfigDir, "swanVisualizationConfig.xml");
		try {
			PrintWriter writer = new PrintWriter(swanVisualizationConfigFile);
			BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(templateSwanVisualizationConfigFile));
			String line = inputFileBufferedReader.readLine();
			while (line != null) {

				String outLine = line;

				if (outLine.contains(swivtVisualizationExeKeyword)) {
					outLine = line.replace(swivtVisualizationExeKeyword, swivtVisualizationExePath);
				} else if (outLine.contains(swivtPresentationFileKeyword)) {
					outLine = line.replace(swivtPresentationFileKeyword, swivtPresentationDirName);
				}
				writer.println(outLine);
				line = inputFileBufferedReader.readLine();
			}
			inputFileBufferedReader.close();
			writer.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not produce swan vizualization config file " +
					swanVisualizationConfigFile.getAbsolutePath() + " from template " + templateSwanVisualizationConfigFile.getAbsolutePath() +
					", error:" + e.getMessage());
		}
		if (!templateSwanVisualizationConfigFile.delete()) {
			throw new RuntimeException("Could not delete swan vizualization config template file " +
					templateSwanVisualizationConfigFile.getAbsolutePath());
		}
	}

	private void createStochObserverConfig(File targetDir) {

        // create uncertainties from observationIDs in stoch observer
        File stochObserverDir = new File(targetDir, "stochObserver");
        String observationsFileName = observationDirName + "/" + "meas_" + getCaseName() + "_loc.tab";
        IDataObject swanObservations = new SwanResults();
        swanObservations.initialize(stochObserverDir, new String[]{observationsFileName});

        File uncertaintiesFile = new File(stochObserverDir, "stochObsUncertainties.xml");

        final String[] activeObsIds = {
                "hsig", "rtpeak", "tm01", "tm02", "tm_10"};

        ArrayList<String> obsIdList = new ArrayList<String>();
        ArrayList<Boolean> obsIdActiveList = new ArrayList<Boolean>();
        String[] obsIds = swanObservations.getExchangeItemIDs();
        for (String obsId : obsIds) {
            obsIdList.add(obsId);
            boolean isActive = false;
            for (String activeObsId : activeObsIds) {
                if (obsId.toLowerCase().startsWith(activeObsId)) {
                    isActive = true;
                    break;
                }
            }
            obsIdActiveList.add(isActive);
        }
        createUncertainties(uncertaintiesFile, obsIdList, null, null, obsIdActiveList);

        // create main stoch observer config file
        File stochObsTemplateFile = new File(stochObserverDir, "templateStochObsConfig.xml");
        File stochObsFile = new File(stochObserverDir, "swanStochObsConfig.xml");
        final String obsFileKeyword = "OBSERVATIONFILE";
        try {
            PrintWriter writer = new PrintWriter(stochObsFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(stochObsTemplateFile));
            String line = inputFileBufferedReader.readLine();
            while (line != null) {
                String outLine = line;
                if (outLine.contains(obsFileKeyword)) {
                    outLine = line.replace(obsFileKeyword, observationsFileName);
                }
                writer.println(outLine);
                line = inputFileBufferedReader.readLine();
            }
            inputFileBufferedReader.close();
            writer.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not produce stoch. obs configuration file " +
                    stochObsFile.getAbsolutePath() + " from template " + stochObsTemplateFile.getAbsolutePath() +
                    ", error:" + e.getMessage());
        }
        if (!stochObsTemplateFile.delete()) {
            throw new RuntimeException("Could not delete stoch. obs configuration template file " +
                    stochObsTemplateFile.getAbsolutePath());
        }
    }

    private static void createUncertainties(File uncertaintiesFile, ArrayList<String> idList, ArrayList<Double> stDevList, ArrayList<Boolean> isFactorList, ArrayList<Boolean> isActiveList) {
        String[] ids = idList.toArray(new String[idList.size()]);
        Double[] stDevs = stDevList == null ? null : stDevList.toArray(new Double[stDevList.size()]);
        Boolean[] isFactors = isFactorList == null ? null : isFactorList.toArray(new Boolean[isFactorList.size()]);
        Boolean[] isActives = isActiveList == null ? null : isActiveList.toArray(new Boolean[isActiveList.size()]);
        createUncertainties(uncertaintiesFile, ids, stDevs, isFactors, isActives);
    }

    private static void createUncertainties(File uncertaintiesFile, String[] ids, Double[] stDevs, Boolean[] isFactors, Boolean[] isActives) {

        String xmlString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
        String uncertaintiesStartString = "<uncertainties xsi:schemaLocation=\"http://www.wldelft.nl " +
                "http://schemas.openda.org/uncertainty/uncertainties.xsd\" " +
                "version=\"1.0\" xmlns=\"http://www.wldelft.nl\" " +
                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n";
        String uncertaintyTypeString = "\t<uncertaintyType>ProbabilityDistributionFunction</uncertaintyType>\n";
        String uncertaintiesEndString = "\n</uncertainties>";

        String pdfStartString = "\t<probabilityDistributionFunction id=\"OBSID\" isActive=\"ISACTIVE\">";
        String pdfValueString = "\t\t<normal mean=\"0\" stdv=\"STDEVVALUE\" stdvIsFactor=\"ISFACTOR\"/>";
        String pdfEndString = "\t</probabilityDistributionFunction>";

        try {
            PrintWriter writer = new PrintWriter(uncertaintiesFile);
            writer.println(xmlString);
            writer.println(uncertaintiesStartString);
            writer.println(uncertaintyTypeString);
            for (int i = 0; i < ids.length; i++) {
                double stdev = stDevs == null ? 0.1 : stDevs[i];
                String isFactor = (isFactors == null || isFactors[i]) ? "true" : "false";
                String isActive = (isActives == null || isActives[i]) ? "true" : "false";
                String startString = pdfStartString.replace("OBSID", ids[i]);
                writer.println(startString.replace("ISACTIVE", isActive));
                String valString = pdfValueString.replace("STDEVVALUE", String.valueOf(stdev));
                writer.println(valString.replace("ISFACTOR", isFactor));
                writer.println(pdfEndString);
            }
            writer.println(uncertaintiesEndString);
            writer.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Could not write uncertainties file " + uncertaintiesFile.getAbsolutePath());
        }
    }

    public String getCaseName() {
        String dirName = swivtCaseDirectory.getName();
        return dirName.substring(0, dirName.lastIndexOf('_'));
    }

    private class AlgConfigCreator {

        private final String algorithmKeyword = "ALGORITHM";
        private final String algorithmConfigKeyword = "CONFIG";

        ArrayList<String> lines = new ArrayList<String>();
        File calibrationDir;

        private AlgConfigCreator(File templateFile) {
            try {
                BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(templateFile));
                String line = inputFileBufferedReader.readLine();
                while (line != null) {
                    lines.add(line);
                    line = inputFileBufferedReader.readLine();
                }
                inputFileBufferedReader.close();
            } catch (IOException e) {
                throw new RuntimeException("Could not read openda configuration template file " +
                        templateFile.getAbsolutePath());
            }
            calibrationDir = templateFile.getParentFile();
            if (!templateFile.delete()) {
                throw new RuntimeException("Could not delete openda configuration template file " +
                        templateFile.getAbsolutePath());
            }
        }

        private void createConfigFile(String algorithm, String algorithmConfig) {

            String algorithmUpper = algorithm.substring(0, 1).toUpperCase() + algorithm.substring(1);
            String algorithmConfigUpper = algorithmConfig.substring(0, 1).toUpperCase() + algorithmConfig.substring(1);
            File configFile = new File(calibrationDir, "swan" + algorithmConfigUpper + ".oda");

            try {
                PrintWriter writer = new PrintWriter(configFile);
                for (String line : lines) {
                    String outLine = line;
                    if (outLine.contains(algorithmKeyword)) {
                        outLine = line.replace(algorithmKeyword, algorithmUpper);
                    } else if (outLine.contains(algorithmConfigKeyword)) {
                        outLine = line.replace(algorithmConfigKeyword, algorithmConfig);
                    }
                    writer.println(outLine);
                }
                writer.close();
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Could not write to configuration file " + configFile.getAbsolutePath());
            }
        }
    }

    private class UncertainPars {
        private ArrayList<String> paramIds = new ArrayList<String>();
        private ArrayList<Double> paramStdDevs = new ArrayList<Double>();
        private ArrayList<Boolean> paramStdDevIsFactors = new ArrayList<Boolean>();

        public UncertainPars(File uncertaintyDefaultsFile, ArrayList<String> activeParameterIds) {
            paramIds = new ArrayList<String>();
            paramStdDevs = new ArrayList<Double>();
            paramStdDevIsFactors = new ArrayList<Boolean>();
            try {
                BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(uncertaintyDefaultsFile));
                String line;
                while ((line = inputFileBufferedReader.readLine()) != null) {
                    String[] fields = line.trim().split(",");
                    if (fields.length != 3) {
                        throw new RuntimeException("Invalid line\n\t" + line.trim() +
                                "\tin uncertainty defaults file " + uncertaintyDefaultsFile.getAbsolutePath());
                    }
                    String paramId = fields[0].trim();
                    Double paramStdDev = Double.valueOf(fields[1].trim());
                    Boolean paramStdDevIsFactor = Boolean.valueOf(fields[2].trim());
                    for (String activeParameterId : activeParameterIds) {
                        if (paramId.equals(activeParameterId)) {
                            paramIds.add(paramId);
                            paramStdDevs.add(paramStdDev);
                            paramStdDevIsFactors.add(paramStdDevIsFactor);
                            break;
                        }
                    }
                }
                inputFileBufferedReader.close();
            } catch (IOException e) {
                throw new RuntimeException("Could not read uncertainty defaults file " + uncertaintyDefaultsFile.getAbsolutePath());
            }
        }

        public ArrayList<String> getParamIds() {
            return paramIds;
        }

        public ArrayList<Double> getParamStdDevs() {
            return paramStdDevs;
        }

        public ArrayList<Boolean> getParamStdDevIsFactors() {
            return paramStdDevIsFactors;
        }
    }
}
