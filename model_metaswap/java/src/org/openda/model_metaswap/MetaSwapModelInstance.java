package org.openda.model_metaswap;

import org.openda.blackbox.config.BBModelConfig;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IOutputModeSetter;
import org.openda.interfaces.ITime;

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
		flushAndClearDataObjects(false);
	}
}
