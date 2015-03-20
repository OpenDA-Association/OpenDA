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

import org.openda.blackbox.config.BBAction;
import org.openda.blackbox.config.BBWrapperConfigReader;
import org.openda.core.io.castorgenerated.SwanCalibWrapperConfigXML;
import org.openda.model_swan.SwanCalibWrapperConfig;
import org.openda.utils.io.CastorUtils;

import java.io.File;

/**
 * Configuration reader for Swan config that uses the dedicated OpenDA Swan wrapper.
 */
public class SwanCalibWrapperConfigReader {

    private SwanCalibWrapperConfig swanCalibWrapperConfig;

    public SwanCalibWrapperConfigReader(File configFile) {
        SwanCalibWrapperConfigXML wrapperConfigXML =
                (SwanCalibWrapperConfigXML) CastorUtils.parse(configFile, SwanCalibWrapperConfigXML.class);

        BBAction swanAction = BBWrapperConfigReader.parseBBAction(configFile, wrapperConfigXML.getSwanAction(), null);
        BBAction cleanupAction = BBWrapperConfigReader.parseBBAction(configFile, wrapperConfigXML.getCleanupAction(), null);
        swanCalibWrapperConfig = new SwanCalibWrapperConfig(configFile,
                swanAction,
                wrapperConfigXML.getSwivtParameters(),
                wrapperConfigXML.getNonSwivtParameters(),
                wrapperConfigXML.getSwanInputTemplate(),
                wrapperConfigXML.getActualSwanInput(),
                wrapperConfigXML.getObservationLocations(),
                wrapperConfigXML.getSwanResults(),
                wrapperConfigXML.getWindInterpolationTable(),
                wrapperConfigXML.getPerturbedValuesValidation(),
                cleanupAction
        );
    }

    public SwanCalibWrapperConfig getSwanCalibWrapperConfig() {
        return swanCalibWrapperConfig;
    }
}
