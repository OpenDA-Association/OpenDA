/* OpenDA v2.4.1 
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
package org.openda.observers;

import org.openda.observers.io.castorgenerated.IoObjectStochObserverXML;
import org.openda.observers.io.castorgenerated.StochObsIoObjectXML;
import org.openda.observers.io.castorgenerated.StochObsUncertaintyModuleXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Reader for IoObject based stochastic observer configuration.
 */
public class IoObjectStochObserverConfigReader {

    private IoObjectStochObserverConfig ioObjectStochObserverConfig;

    public IoObjectStochObserverConfigReader(File configFile) {

        // xml-parse
        org.openda.observers.io.castorgenerated.IoObjectStochObserverXML ioObjectStochObserverXML =
                (IoObjectStochObserverXML) CastorUtils.parse(configFile, IoObjectStochObserverXML.class);

        // uncertainty module
        StochObsUncertaintyModuleXML uncertaintyModuleXML = ioObjectStochObserverXML.getUncertaintyModule();
        IoObjectStochObserverConfig.IoStochObsUncertaintyConfig uncertaintyModuleConfig = new IoObjectStochObserverConfig.IoStochObsUncertaintyConfig(
                new File(configFile.getParentFile(),uncertaintyModuleXML.getWorkingDirectory()),
                uncertaintyModuleXML.getClassName(), uncertaintyModuleXML.getArg());

        // io objects
        ArrayList<IoObjectStochObserverConfig.IoStochObsIoObjectConfig> ioObjectConfigs = new ArrayList<IoObjectStochObserverConfig.IoStochObsIoObjectConfig>();
        for (StochObsIoObjectXML stochObsIoObjectXML : ioObjectStochObserverXML.getIoObject()) {
            double missingValue;
            if (stochObsIoObjectXML.hasMissingValue()){
                missingValue = stochObsIoObjectXML.getMissingValue();
            } else {
                missingValue = Double.NaN;
            }
            ioObjectConfigs.add(new IoObjectStochObserverConfig.IoStochObsIoObjectConfig(
                new File(configFile.getParentFile(),stochObsIoObjectXML.getWorkingDirectory()),
                stochObsIoObjectXML.getClassName(), stochObsIoObjectXML.getFileName(),
                stochObsIoObjectXML.getArg(),missingValue));
        }

        // assimilation and validation station identifiers
		List<String> assimilationObsIds = new ArrayList<String>();
		if (ioObjectStochObserverXML.getAssimilationStations() != null) {
			Collections.addAll(assimilationObsIds, ioObjectStochObserverXML.getAssimilationStations().getObservationId());
		}
		List<String> validationObsIds = new ArrayList<String>();
		if (ioObjectStochObserverXML.getValidationStations() != null) {
			Collections.addAll(validationObsIds, ioObjectStochObserverXML.getValidationStations().getObservationId());
		}

        // should missing values be removed?
        boolean removeMissingValues = ioObjectStochObserverXML.getRemoveMissingValues();

        // store config
		ioObjectStochObserverConfig = new IoObjectStochObserverConfig(uncertaintyModuleConfig, ioObjectConfigs,
				assimilationObsIds, validationObsIds, removeMissingValues);
    }

    public IoObjectStochObserverConfig getIoObjectStochObserverConfig() {
        return ioObjectStochObserverConfig;
    }
}
