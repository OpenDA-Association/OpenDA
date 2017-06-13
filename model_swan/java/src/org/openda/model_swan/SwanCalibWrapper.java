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

package org.openda.model_swan;

import org.openda.blackbox.config.BBAction;
import org.openda.interfaces.*;
import org.openda.model_swan.SwanParameters;
import org.openda.model_swan.io.SwanCalibWrapperConfigReader;
import org.openda.uncertainties.Uncertainty;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.uncertainties.pdfs.LognormalDistribution;
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.utils.*;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;


public class SwanCalibWrapper extends Instance implements IStochModelInstance{
	private static final String WIND_DIRECTION_PARAMETER_ID = "winddir";
    private static final String WIND_VELOCITY_PARAMETER_ID = "windvel";
    private static final String WAVE_DIRECTION_PARAMETER_ID = "wavedir";

    SwanCalibWrapperConfig swanWrapperConfig = null;
    private SwanXMLParameterSettings swivtParameters = null;
    private File instanceDir = null;

    /**
     * nonSwivtParameters can be null if not configured.
     */
    private SwanNonSwivtParameters nonSwivtParameters = null;

    private UncertaintyEngine uncertaintyEngine =null;

    private SwanResults swanResults = null;
    Time currentTime = new Time(Double.NEGATIVE_INFINITY); // There has to be always one compute step
    private ArrayList<File> swanInputTemplateFiles;

    private ArrayList<String> logNormalParameterIds = null;
    private HashMap<String, Double> logNormalParameterValues = new HashMap<String, Double>();

    /**
     * If windDependentParsHaveBeenGenerated is false, then
     * this means that the wind dependent parameter values
     * are supplied instead of generated.
     */
    private boolean windDependentParsHaveBeenGenerated = false;
    /**
     * windDependentParameters can be null if not configured.
     */
    private SwanWindDepParms windDependentParameters = null;
    private double actualWindDirectionValue = 0;
    private double actualWindVelocityValue = 0;
    private double actualWaveDirectionValue = 0;

    public void initialize(File workingDir, String[] arguments) {
        // TODO: adjust other initialize method
    }

    // OpenDa stochmodel fact.
    public void initialize(File instanceDir, File configDir, String[] args) {

        if (args.length != 1) {
            throw new RuntimeException("Initialize function expects 1 argument: swan wrapper config");
        }

        // set instance dir
        this.instanceDir = instanceDir;

        // read configuration
        File swanWrapperConfigFile = new File(configDir, args[0]);
        SwanCalibWrapperConfigReader swanWrapperConfigReader = new SwanCalibWrapperConfigReader(swanWrapperConfigFile);
        swanWrapperConfig = swanWrapperConfigReader.getSwanCalibWrapperConfig();

        // read the parameters for the swivt xml file, user part
        swivtParameters = new SwanXMLParameterSettings(swanWrapperConfig.getSwivtParametersFile());

        // read additional parameters, not defined in swivt xml file
        File nonSwivtParametersFile = swanWrapperConfig.getNonSwivtParametersFile();
        if (nonSwivtParametersFile != null) {
            nonSwivtParameters = new SwanNonSwivtParameters(nonSwivtParametersFile);
        }

        File windInterpolationTableFile = swanWrapperConfig.getWindInterpolationTableFile();
        if (windInterpolationTableFile != null) {
        	if (nonSwivtParameters == null) {
                throw new RuntimeException("WindInterpolationTableFile configured but nonSwivtParametersFile not configured."
                		+ " Additional non-swivt parameters have to be configured to use the wind dependent parameters.");
        	}
        	windDependentParameters = new SwanWindDepParms(windInterpolationTableFile);
        }

        startVariableAccess();

        swanInputTemplateFiles = new ArrayList<File>();
        if (!swanWrapperConfig.getSwanInputTemplateFile().exists()) {
            final String swnString = ".swn";
            String baseFileName = swanWrapperConfig.getSwanInputTemplateFile().getName();
            int swnPos = baseFileName.indexOf(swnString);
            if (swnPos > 0) {
                baseFileName = baseFileName.substring(0, swnPos);
                Locale locale = new Locale("EN");
                for (int i = 1; i < 10; i++) {
                    File numberedFile = new File(instanceDir, baseFileName + "_" +  String.format(locale, "%02d", i) + swnString);
                    if (numberedFile.exists()) {
                        swanInputTemplateFiles.add(numberedFile);
                    }
                }
            }
        } else {
            swanInputTemplateFiles.add(swanWrapperConfig.getSwanInputTemplateFile());
        }
        if (swanInputTemplateFiles.size() == 0) {
            throw new RuntimeException("Swan input template file not found (nor numbered input files for nesting): " +
                        swanWrapperConfig.getSwanInputTemplateFile().getAbsolutePath());
        }
    }

    public void setStochasticComponents(UncertaintyEngine uncertaintyEngine) {
        this.uncertaintyEngine = uncertaintyEngine;
    }

    public SwanCalibWrapperConfig getSwanCalibWrapperConfig() {
        return swanWrapperConfig;
    }

    public File getModelRunDir() {
        return instanceDir;
    }

	public void finish() {
		// no action needed (yet)
	}

	private double[] getValues(String variableId) {
        double value;
		if (variableId.equals(WAVE_DIRECTION_PARAMETER_ID)) {
			value = actualWaveDirectionValue;
			windDependentParsHaveBeenGenerated = true;

		} else if (windDependentParameters != null && variableId.equals("hs")) {
			value = windDependentParameters.getHS(actualWindDirectionValue, actualWindVelocityValue);
			windDependentParsHaveBeenGenerated = true;

		} else if (windDependentParameters != null && variableId.equals("tp")) {
			value = windDependentParameters.getTp(actualWindDirectionValue, actualWindVelocityValue);
			windDependentParsHaveBeenGenerated = true;

		} else if (swivtParameters.getIds().contains(variableId)) {
            Object valueAsObject = swivtParameters.getValue(variableId);
            double valueAsDouble;
            if (valueAsObject instanceof Double) {
                valueAsDouble = (Double)valueAsObject;
            } else if (valueAsObject instanceof Integer) {
                valueAsDouble = (Integer)valueAsObject;
            } else {
                throw new RuntimeException("Unknown value type for parameter " +
                        variableId);
            }
            value = valueAsDouble;

        } else {
            // not a parameter, getValues was called for a result variable
            if (swanResults == null) {
                throw new RuntimeException("Asking value for result var " +
                        variableId +
                        " while results have not been read yet (perhaps " + variableId +
                        " should have been recognized as parameter?)");
            }
            value = swanResults.getValues(new String[]{variableId})[0];
        }

        return new double[]{value};
    }

    private void setValues(String variableId, double value) {

        if (variableId.equals(WAVE_DIRECTION_PARAMETER_ID)) {
            actualWaveDirectionValue = value;

        } else if (windDependentParameters != null
        		&& (variableId.equals(WIND_VELOCITY_PARAMETER_ID) || variableId.equals(WIND_DIRECTION_PARAMETER_ID))) {

        	if (windDependentParsHaveBeenGenerated) {
                throw new RuntimeException("Adjusting " + variableId + " after generation of wind dependent parameters");
            }

            if (variableId.equals(WIND_VELOCITY_PARAMETER_ID)) {
                actualWindVelocityValue = value;
				nonSwivtParameters.setValue(WIND_VELOCITY_PARAMETER_ID, actualWindVelocityValue);

            } else if (variableId.equals(WIND_DIRECTION_PARAMETER_ID)) {
                actualWindDirectionValue = value;
                actualWaveDirectionValue = actualWindDirectionValue;
				nonSwivtParameters.setValue(WIND_DIRECTION_PARAMETER_ID, actualWindDirectionValue);
            }
        } else if (swivtParameters.getIds().contains(variableId)) {
            double transformedValue = transformParameterValue(variableId, value);
            swivtParameters.setValue(variableId, transformedValue);
        } else {
            throw new RuntimeException("Unexpected parameter: " + variableId);
        }
    }

    public void compute(ITime targetTime) {
        boolean isRunNested = swanInputTemplateFiles.size() > 1;
        for (int i = 0; i < swanInputTemplateFiles.size(); i++) {
            File swanInputTemplateFile = swanInputTemplateFiles.get(i);

            endVariableAccess(swanInputTemplateFile);

            try {
                BBAction swanAction = swanWrapperConfig.getSwanAction();
                if (isRunNested && swanAction.getArguments().length > 0) {
                    // adjust first argument <casename>_0n.swn
                    swanAction.setArgument(0, swanInputTemplateFile.getName().replace(".swn", ""));
                }
                swanAction.run(instanceDir);
                if (new File(instanceDir, "Errfile").exists()) {
                    throw new RuntimeException("Swan run ended with error, dir: " +
                            instanceDir.getAbsolutePath());
                }
                File runEndNormallyFile = new File(instanceDir, "norm_end");
                int milliesToWait = 60000;
                int tryIntervalInMillies = 500;
                if (!fileExistsAfterSomeRetries(runEndNormallyFile, milliesToWait, tryIntervalInMillies)) {
                    throw new RuntimeException("Swan run ended abnormally, dir: " + instanceDir.getAbsolutePath());
                }
                if (swanInputTemplateFiles.size() == 1 && !fileExistsAfterSomeRetries(swanWrapperConfig.getSwanResultsFile(instanceDir), milliesToWait, tryIntervalInMillies)) {
                    throw new RuntimeException(this.getClass() + ": No swan results found: " + swanWrapperConfig.getSwanResultsFile(instanceDir).getAbsolutePath());
                }
            } catch (Exception e) {
                PrintSwanErrors();
                throw new RuntimeException(e.getMessage());
            }
        }
        File swanResultsFile = swanWrapperConfig.getSwanResultsFile(instanceDir);
        if (!swanResultsFile.exists()) {
            // Specified observation file does not exist. It could be a nested run
            SwanResults.composeObservationsForNestedRun(swanResultsFile);
        }

        swanResults = new SwanResults();
//        swanResults.initialize(swanResultsFile.getParentFile(), swanResultsFile.getName(), new String[]{});
        swanResults.initialize(swanResultsFile.getParentFile(), new String[]{swanResultsFile.getName()});
        if (swanWrapperConfig.getCleanupAction() != null) {
            swanWrapperConfig.getCleanupAction().run(instanceDir);
        }

        startVariableAccess();

        if (currentTime.getMJD() == Double.NEGATIVE_INFINITY) {
            currentTime = new Time(0);
        }
    }

    public void startVariableAccess() {
        if (nonSwivtParameters != null) {
        	actualWindDirectionValue = nonSwivtParameters.getValue(WIND_DIRECTION_PARAMETER_ID);
            actualWindVelocityValue = nonSwivtParameters.getValue(WIND_VELOCITY_PARAMETER_ID);
            actualWaveDirectionValue = actualWindDirectionValue;
            windDependentParsHaveBeenGenerated = false;
        }
    }

    public void endVariableAccess(File swanInputTemplateFile) {

        if (swanInputTemplateFile == null) {
            swanInputTemplateFile= swanWrapperConfig.getSwanInputTemplateFile();
        }

        SwanParameters swanInput = new SwanParameters(swanInputTemplateFile);

        if (swivtParameters != null) {
            for (String groupKey : SwanParameters.groupKeys) {
                swanInput.setGroupActive(groupKey, swivtParameters.getGroupActive(groupKey));
            }
            for (String paramID : swivtParameters.getIds()) {
                swanInput.setParameterValue(SwanXMLParameterSettings.parseParamFromID(paramID), swivtParameters.getValue(paramID), false);
            }
        }

        if (nonSwivtParameters != null) {
            for (String paramID : nonSwivtParameters.getIds()) {
                swanInput.setParameterValue(SwanXMLParameterSettings.parseParamFromID(paramID), nonSwivtParameters.getValue(paramID), false);
            }
        }

        try {
            swanInput.write(swanWrapperConfig.getActualSwanInputFile(instanceDir));
        } catch (IOException e) {
            throw new RuntimeException("Could not write swan input file " + swanWrapperConfig.getActualSwanInputFile(instanceDir).getAbsolutePath());
        }
    }


    // OpenDA
    public IVector getParameters() {

        determineLogNormalParameters();
        String[] uncertaintyIDs = uncertaintyEngine.getUncertaintyIDs();
        double[] parameterValues = new double[uncertaintyIDs.length];

        for (int i = 0; i < uncertaintyIDs.length; i++) {
            parameterValues[i] = getValues(uncertaintyIDs[i])[0];
            if (logNormalParameterIds.contains(uncertaintyIDs[i])) {
                logNormalParameterValues.put(uncertaintyIDs[i],  parameterValues[i]);
                parameterValues[i] = 0;
                uncertaintyIDs[i] += ".relChange";  // For display only. Id can be adjusted here, because
                                                    // uncertaintyEngine.getUncertaintyIDs() returned a copy
        }
        }
        return new TreeVector("swan-parameters", uncertaintyIDs, parameterValues);
    }

    // OpenDA
    public void setParameters(IVector parameters) {

        determineLogNormalParameters();
        String[] uncertaintyIDs = uncertaintyEngine.getUncertaintyIDs();
        double[] parameterValues = parameters.getValues();

        for (int i = 0; i < uncertaintyIDs.length; i++) {
            setValues(uncertaintyIDs[i], parameterValues[i]);
        }
    }

    //SwanOpenDAModel
    public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
        if (swanResults == null) {
            throw new RuntimeException("No swan results available on instance dir. " + getModelRunDir());
        }
        String[] observationIDs = observationDescriptions.getStringProperties("id");
        double[] observedValues = swanResults.getValues(observationIDs);
        return new Vector(observedValues);
    }

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper.getObservedLocalization(): Not implemented yet.");
	}

	//SwanOpenDAModel
    public void announceObservedValues(IObservationDescriptions observationDescriptions) {

        double[] allXCoords = observationDescriptions.getValueProperties("Xp").getValues();
        double[] allYCoords = observationDescriptions.getValueProperties("Yp").getValues();

        ArrayList<String> coords = new ArrayList<String>();

        for (int i = 0; i < allXCoords.length; i++) {
            double xCoord = allXCoords[i];
            double yCoord = allYCoords[i];
            String coordString = String.valueOf(xCoord) + " " + String.valueOf(yCoord);
            if (!coords.contains(coordString)) {
                coords.add(coordString);
            }
        }

        File observationLocationsFile = swanWrapperConfig.getObservationLocationsFile(instanceDir);
        try {
            BufferedWriter locationFile = new BufferedWriter(new FileWriter(observationLocationsFile));
            for (String coord : coords) {
                locationFile.write(coord);
                locationFile.newLine();
            }
            locationFile.close();
        } catch (IOException e) {
            throw new RuntimeException("nl.wldelft.openda.SwanCalibWrapper.announceObservedValueLocations(): " +
                    "could not open " + observationLocationsFile.getAbsolutePath() + " for writing");
        }
    }


    public ITime getCurrentTime() {
        return currentTime;
    }

    // OpenDA
    public void axpyOnState(double alpha, IVector vector) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public IVector getState() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public ITime getTimeHorizon() {
        return new Time(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);
    }

    // OpenDA
    public void axpyOnParameters(double alpha, IVector vector) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public String[] getExchangeItemIDs() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        if (role == IPrevExchangeItem.Role.InOut) {
            return getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper.getExchangeItemIDs(): Role selection not implemented yet.");
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper.getDataObjectExchangeItem(): Not implemented yet.");
	}

	// OpenDA
    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public IStochVector getParameterUncertainty() {
        IVector parameters = getParameters();
        String[] uncertaintyIDs = uncertaintyEngine.getUncertaintyIDs();
        double[] medians = new double[uncertaintyIDs.length];
        double[] stds = new double[uncertaintyIDs.length];
        for (int i = 0; i < uncertaintyIDs.length; i++) {
            Uncertainty pdf = uncertaintyEngine.getPdf(uncertaintyIDs[i]);
            if (pdf == null) {
                throw new RuntimeException(
                        "nl.wldelft.openda.models.DefaultOpenDaModel.compute(): PDF not found for parameter " + uncertaintyIDs[i]);
            }
            if (pdf instanceof NormalDistribution) {
            NormalDistribution normalDistribution = (NormalDistribution) pdf;
            medians[i] = parameters.getValue(i) + normalDistribution.getMean();
            stds[i] = normalDistribution.getStd();
            if (normalDistribution.isStdFactor()) {
                stds[i] = Math.abs(medians[i] * stds[i]);
            }
            } else if (pdf instanceof LognormalDistribution) {
                LognormalDistribution logNormalDistribution = (LognormalDistribution) pdf;
                medians[i] = logNormalDistribution.getMean();
                stds[i] = logNormalDistribution.getStd();
            } else {
                throw new RuntimeException(
                        "nl.wldelft.openda.models.DefaultOpenDaModel.compute(): PDF for parameter " + uncertaintyIDs[i] +
                                " is not a normal distribution");
        }
        }
        return new StochVector(medians, stds);
    }

    // OpenDA
    public IVector getStateScaling() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public IVector[] getStateScaling(
            IObservationDescriptions observationDescriptions) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public IStochVector getStateUncertainty() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public IVector[] getWhiteNoise(ITime timeSpan) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public boolean isWhiteNoiseStationary() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public void releaseInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper.loadPersistentState(): Not implemented yet.");
	}

	// OpenDA
    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    // OpenDA
    public void setAutomaticNoiseGeneration(boolean value) {
    	if(value){
    		Results.putMessage("WARNING: Noise for forcings is not implemented for SWAN calibration.");
    	}
    }

    // OpenDA
    public void setWhiteNoise(IVector[] whiteNoise) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanCalibWrapper: Method not implemented yet.");
    }

    private double transformParameterValue(String parameterId, double valueFromAlgorithm) {
        double transformedValue = valueFromAlgorithm;
        if (logNormalParameterIds.contains(parameterId)) {
            if (logNormalParameterValues.size() == 0) {
                // parameters not retrieved yet
                getParameters();
            }
            if (!logNormalParameterValues.containsKey(parameterId)) {
                throw new RuntimeException(
                        "nl.wldelft.openda.models.DefaultOpenDaModel.transformParameterValue(): id not found: " + parameterId);
            }
            transformedValue = logNormalParameterValues.get(parameterId) * Math.exp(valueFromAlgorithm);
        }
        return transformedValue;
    }

    // OpenDA
    private void determineLogNormalParameters() {
        if (logNormalParameterIds == null) {
            logNormalParameterIds = new ArrayList<String>();
            for (String uncertaintyID : uncertaintyEngine.getUncertaintyIDs()) {
                Uncertainty pdf = uncertaintyEngine.getPdf(uncertaintyID);
                if (pdf == null) {
                    throw new RuntimeException(
                            "nl.wldelft.openda.models.DefaultOpenDaModel.determineLogNormalParameters(): PDF not found for parameter " + uncertaintyID);
                }
                if (pdf instanceof LognormalDistribution) {
                    logNormalParameterIds.add(uncertaintyID);
                }
            }
        }
    }

    private static boolean fileExistsAfterSomeRetries(File fileToCheck, int milliesToWait, int tryIntervalInMillies) {
        int maxTries = milliesToWait/tryIntervalInMillies;
        int nTries = 0;
        while (nTries < maxTries && !fileToCheck.exists()) {
            nTries++;
            try {
                Thread.sleep(tryIntervalInMillies);
            } catch (InterruptedException e) {
                throw new RuntimeException("Swan wrapper thread was interrupted, message: "
                        + e.getMessage() + ", dir.: " + fileToCheck.getParentFile().getAbsolutePath());
            }
        }
        return fileToCheck.exists();
    }

    private void PrintSwanErrors() {
        try {
            File errorFile = new File(instanceDir, "Errfile");
            BufferedReader reader = new BufferedReader(new FileReader(errorFile));
            String line;
            Results.putMessage("SWAN error messages:");
            while ((line = reader.readLine()) != null) {
                Results.putMessage("   " + line);
            }
            reader.close();
        } catch (IOException e) {
            Results.putMessage("SWAN error messages: error reading file (\"Errfil\")!");
        }
    }
}

