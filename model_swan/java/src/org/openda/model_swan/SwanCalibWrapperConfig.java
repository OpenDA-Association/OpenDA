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

package org.openda.model_swan;

import org.openda.blackbox.config.BBAction;

import java.io.File;

/**
 * Swan OpenDa wrapper configuration info.
 */
public class SwanCalibWrapperConfig {

    private BBAction swanAction;
    private File swivtParametersFile;
    private File nonSwivtParametersFile;
    private File swanInputTemplateFile;
    private String actualSwanInputFileName;
    private String observationLocationsFileName;
    private String swanResultsFileName;
    /**
     * This is null if no windInterpolationTableFile configured.
     */
    private File windInterpolationTableFile;
    /**
     * This is null if no perturbedValuesValidationFile configured.
     */
    private File perturbedValuesValidationFile;
    private BBAction cleanupAction;

    public  SwanCalibWrapperConfig(File configFile, BBAction swanAction,
                             String swivtParametersFileName,
                             String nonSwivtParametersFileName,
                             String swanInputTemplateFileName,
                             String actualSwanInputFileName,
                             String observationLocationsFileName,
                             String swanResultsFileName,
                             String windInterpolationTableFileName,
                             String perturbedValuesValidationFileName,
                             BBAction cleanupAction) {
        this.swanAction = swanAction;
        this.cleanupAction = cleanupAction;
        this.swivtParametersFile = SwanCalibStochModelFactory.getAndCheckFilePath(configFile.getParentFile(), swivtParametersFileName, true, true);
        this.nonSwivtParametersFile = SwanCalibStochModelFactory.getAndCheckFilePath(configFile.getParentFile(), nonSwivtParametersFileName, true, true);
        if (this.swivtParametersFile == null && this.nonSwivtParametersFile == null) {
            throw new RuntimeException("Specify at least swivtParameters or nonSwivtParameters (config file: "
                    + configFile.getAbsolutePath() + ")");
        }
        this.swanInputTemplateFile = SwanCalibStochModelFactory.getAndCheckFilePath(configFile.getParentFile(), swanInputTemplateFileName);
        this.actualSwanInputFileName = actualSwanInputFileName;
        this.observationLocationsFileName = observationLocationsFileName;
        this.swanResultsFileName = swanResultsFileName;
        this.windInterpolationTableFile = SwanCalibStochModelFactory.getAndCheckFilePath(configFile.getParentFile(), windInterpolationTableFileName, true, true);
        this.perturbedValuesValidationFile = SwanCalibStochModelFactory.getAndCheckFilePath(configFile.getParentFile(), perturbedValuesValidationFileName, true, true);
    }

    public BBAction getSwanAction() {
        return swanAction;
    }

    public File getSwivtParametersFile() {
        return swivtParametersFile;
    }

    public File getNonSwivtParametersFile() {
        return nonSwivtParametersFile;
    }

    public File getSwanInputTemplateFile() {
        return swanInputTemplateFile;
    }

    public File getActualSwanInputFile(File instanceDir) {
        return SwanCalibStochModelFactory.getAndCheckFilePath(instanceDir, actualSwanInputFileName, true, false);
    }

    public File getObservationLocationsFile(File instanceDir) {
        return SwanCalibStochModelFactory.getAndCheckFilePath(instanceDir, observationLocationsFileName, true, false);
    }

    public File getSwanResultsFile(File instanceDir) {
        return SwanCalibStochModelFactory.getAndCheckFilePath(instanceDir, swanResultsFileName);
    }

    /**
     * Can be null if not configured.
     */
    public File getWindInterpolationTableFile() {
        return windInterpolationTableFile;
    }

    /**
     * Can be null if not configured.
     */
    public File getPerturbedValuesValidationFile() {
        return perturbedValuesValidationFile;
    }

    public BBAction getCleanupAction() {
        return cleanupAction;
    }
}
