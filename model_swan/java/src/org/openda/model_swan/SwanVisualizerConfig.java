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

import org.openda.blackbox.config.BBUtils;

import java.io.File;

/**
 * TODO: description
 */
public class SwanVisualizerConfig {

    private File visualizerExe;
    private Boolean active;
    private File figuresDir;
    private File modelDirectory;
    private File observationDirectory;
    private File presentationSettingsDirectory;
    private File outputDirectory;

    public SwanVisualizerConfig(File instanceDir, File configFile, Boolean active,
                                String visualizationExeName, String outputDirectoryParentName,
                                String modelDirectoryName, String observationDirectoryName,
                                String presentationSettingsDirName) {

        String configDir = configFile.getParent();
        String defaultObservationDirectoryName = "../../stochObserver/observ";

        this.active = active;
        this.figuresDir = new File(configDir, outputDirectoryParentName);
        this.outputDirectory = new File(figuresDir, instanceDir.getName());

        this.modelDirectory = modelDirectoryName != null ? new File(configDir, modelDirectoryName) : instanceDir;
        this.observationDirectory = observationDirectoryName != null ? new File(configDir, observationDirectoryName) : new File(configDir, defaultObservationDirectoryName);

        String fullVisExePath = BBUtils.determineExe(configFile.getParentFile(), visualizationExeName);
        visualizerExe = new File(fullVisExePath);
        this.presentationSettingsDirectory = new File(configDir, presentationSettingsDirName);

        if (!modelDirectory.exists()) {
             throw new RuntimeException("Model directory does not exist (" + modelDirectory.getAbsolutePath() + ")");
        }
        if (!observationDirectory.exists()) {
             throw new RuntimeException("Observation directory does not exist (" + observationDirectory.getAbsolutePath() + ")");
        }
        if (!presentationSettingsDirectory.exists()) {
             throw new RuntimeException("Presentation settings directory does not exist (" + presentationSettingsDirectory.getAbsolutePath() + ")");
        }
    }

    public Boolean isActive() {
        return active;
    }

    public File getVisualizerExe() {
        return visualizerExe;
    }

    public File getPresentationSettingsDirectory() {
        return presentationSettingsDirectory;
    }

    public File getFiguresDir() {
        return figuresDir;
    }

    public File getObservationDirectory() {
        return observationDirectory;
    }

    public File getModelDirectory() {
        return modelDirectory;
    }

    public File getOutputDirectory() {
        return outputDirectory;
    }
}
