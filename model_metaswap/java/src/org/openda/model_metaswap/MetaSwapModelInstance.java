/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.model_metaswap;
import org.openda.blackbox.config.BBModelConfig;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IOutputModeSetter;
import org.openda.interfaces.ITime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by hummel on 2018-03-28.
 */
public class MetaSwapModelInstance extends BBModelInstance implements IOutputModeSetter {

	public MetaSwapModelInstance(BBModelConfig bbModelConfig, int newInstanceNumber, ITime timeHorizon) {
		super(bbModelConfig, newInstanceNumber, timeHorizon);
	}

	public void setInOutputMode(boolean inOutputMode) {
		// when entering and leaving output mode, clear the data-objects, to avoid problems with the
		// series of ascii-grids (containing the rainfall)
		ArrayList<String> toBeRemovedFromDataObjects = new ArrayList<String>();
		for	(Map.Entry<String, IDataObject> dataObjectEntry : dataObjects.entrySet() ) {
			IDataObject dataObject = dataObjectEntry.getValue();
			if (dataObject instanceof EsriAsciiGridSeriesDataObject) {
				dataObject.finish();
				toBeRemovedFromDataObjects.add(dataObjectEntry.getKey());
			}
		}
		for	(String dataObjectId : toBeRemovedFromDataObjects) {
			dataObjects.remove(dataObjectId);
		}
	}
}
