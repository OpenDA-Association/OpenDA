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

package org.openda.model_swan.io;

import org.openda.model_swan.io.castorgenerated.SwanVisualizationXML;
import org.openda.model_swan.io.castorgenerated.SwivtVisualizationXML;
import org.openda.model_swan.SwanVisualizerConfig;
import org.openda.utils.io.CastorUtils;

import java.io.File;

/**
 * Configuration reader for Swan (i.e. Swivt) visualization config.
 */
public class SwanVisualizerConfigReader {

    private SwanVisualizerConfig swanVisualizerConfig;

    public SwanVisualizerConfigReader(File instanceDir, File configFile) {

        SwanVisualizationXML swanVisualizationXML =
                (SwanVisualizationXML) CastorUtils.parse(configFile, SwanVisualizationXML.class);
        SwivtVisualizationXML swivtVisualizationXML = swanVisualizationXML.getSwivt();

        Boolean isActive = swivtVisualizationXML.getIsActive();

        String executable = swivtVisualizationXML.getExecutable();
        String outputDirectoryParent = swivtVisualizationXML.getOutputDirectoryParent();

        String modelDirectoryName = null;
        String observationDirectoryName = null;
        String presentationSettingsDir  = null;
        if (swivtVisualizationXML.getVisualizationFiles() != null) {
            modelDirectoryName = swivtVisualizationXML.getVisualizationFiles().getModelDirectory();
            observationDirectoryName = swivtVisualizationXML.getVisualizationFiles().getObservationDirectory();
            presentationSettingsDir = swivtVisualizationXML.getVisualizationFiles().getPresentationSettingsDirectory();
        }

        swanVisualizerConfig = new SwanVisualizerConfig(instanceDir, configFile,
                isActive,
                executable,
                outputDirectoryParent,
                modelDirectoryName,
                observationDirectoryName,
                presentationSettingsDir
                );
    }

    public SwanVisualizerConfig getVisualizerConfig() {
        return swanVisualizerConfig;
    }
}
