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
package org.openda.observers;

import org.openda.observers.io.castorgenerated.*;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Reader for IoObject based stochastic observer configuration.
 */
public class IoObjectStochObserverConfigReader {

    private DataObjectStochObserverConfig dataObjectStochObserverConfig;

    public IoObjectStochObserverConfigReader(File configFile) {

        // xml-parse
        org.openda.observers.io.castorgenerated.DataObjectStochObserverXML dataObjectStochObserverXML =
                (DataObjectStochObserverXML) CastorUtils.parse(configFile, DataObjectStochObserverXML.class);

        // uncertainty module
        StochObsUncertaintyModuleXML uncertaintyModuleXML = dataObjectStochObserverXML.getUncertaintyModule();
        DataObjectStochObserverConfig.DataStochObsUncertaintyConfig uncertaintyModuleConfig = new DataObjectStochObserverConfig.DataStochObsUncertaintyConfig(
                new File(configFile.getParentFile(),uncertaintyModuleXML.getWorkingDirectory()),
                uncertaintyModuleXML.getClassName(), uncertaintyModuleXML.getArg());

        // io objects
        ArrayList<DataObjectStochObserverConfig.IoStochObsIoObjectConfig> ioObjectConfigs = new ArrayList<DataObjectStochObserverConfig.IoStochObsIoObjectConfig>();
		 int dataObjectStochObserverXMLChoiceCount = dataObjectStochObserverXML.getDataObjectStochObserverXMLChoiceCount();
		for (int i = 0; i < dataObjectStochObserverXMLChoiceCount; i++) {
			DataObjectStochObserverXMLChoice dataObjectStochObserverXMLChoice = dataObjectStochObserverXML.getDataObjectStochObserverXMLChoice(i);
			DataObjectStochObserverXMLChoiceItem dataObjectStochObserverXMLChoiceItem = dataObjectStochObserverXMLChoice.getDataObjectStochObserverXMLChoiceItem();
			StochObsDataObjectXML stochObsDataObjectXML = dataObjectStochObserverXMLChoiceItem.getDataObject();
			if (stochObsDataObjectXML == null) stochObsDataObjectXML = dataObjectStochObserverXMLChoiceItem.getIoObject();

			double missingValue;
            if (stochObsDataObjectXML.hasMissingValue()){
                missingValue = stochObsDataObjectXML.getMissingValue();
            } else {
                missingValue = Double.NaN;
            }
            ioObjectConfigs.add(new DataObjectStochObserverConfig.IoStochObsIoObjectConfig(
                new File(configFile.getParentFile(),stochObsDataObjectXML.getWorkingDirectory()),
                stochObsDataObjectXML.getClassName(), stochObsDataObjectXML.getFileName(),
                stochObsDataObjectXML.getArg(),missingValue));
        }

        // assimilation and validation station identifiers
		List<String> assimilationObsIds = new ArrayList<String>();
		if (dataObjectStochObserverXML.getAssimilationStations() != null) {
			Collections.addAll(assimilationObsIds, dataObjectStochObserverXML.getAssimilationStations().getObservationId());
		}
		List<String> validationObsIds = new ArrayList<String>();
		if (dataObjectStochObserverXML.getValidationStations() != null) {
			Collections.addAll(validationObsIds, dataObjectStochObserverXML.getValidationStations().getObservationId());
		}

        // should missing values be removed?
        boolean removeMissingValues = dataObjectStochObserverXML.getRemoveMissingValues();

        // store config
		dataObjectStochObserverConfig = new DataObjectStochObserverConfig(uncertaintyModuleConfig, ioObjectConfigs,
				assimilationObsIds, validationObsIds, removeMissingValues);
    }

    public DataObjectStochObserverConfig getDataObjectStochObserverConfig() {
        return dataObjectStochObserverConfig;
    }
}
