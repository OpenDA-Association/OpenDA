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

import org.openda.model_swan.SwanCalibStochModelConfig;
import org.openda.model_swan.io.castorgenerated.SwanCalibStochModelConfigXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;

/**
 * Reader for SwanCalibStochModelConfig.
 *
 * Code copied and adapted from DStochModelConfigReader.
 */
public class SwanCalibStochModelConfigReader {

    private SwanCalibStochModelConfig swanCalibStochModelConfig;

    public SwanCalibStochModelConfigReader(File configFile) {
        File configDir = configFile.getParentFile();
        SwanCalibStochModelConfigXML stochModelXML = (SwanCalibStochModelConfigXML) CastorUtils.parse(configFile, SwanCalibStochModelConfigXML.class);
        SwanCalibStochModelConfig.SwanCalibComponentConfig modelConfiguration = null;
        if (stochModelXML.getModel() != null) {
            modelConfiguration = new SwanCalibStochModelConfig.SwanCalibComponentConfig(
                    new File(configDir, stochModelXML.getModel().getWorkingDir()),
                    stochModelXML.getModel().getClassName(),
                    stochModelXML.getModel().getArgument());
        }
        swanCalibStochModelConfig = new SwanCalibStochModelConfig(
                configDir,
                modelConfiguration,
                new SwanCalibStochModelConfig.SwanCalibComponentConfig(
                        new File(configDir, stochModelXML.getUncertaintyModule().getWorkingDir()),
                        stochModelXML.getUncertaintyModule().getClassName(),
                        stochModelXML.getUncertaintyModule().getArgument()));
    }

    public SwanCalibStochModelConfig getSwanCalibStochModelConfig() {
        return swanCalibStochModelConfig;
    }
}
