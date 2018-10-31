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
