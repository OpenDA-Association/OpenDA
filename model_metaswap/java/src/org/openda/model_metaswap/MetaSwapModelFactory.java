package org.openda.model_metaswap;

import org.openda.blackbox.wrapper.BBModelFactory;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IStochModelFactory;

/**
 * Created by hummel on 2018-03-28.
 */
public class MetaSwapModelFactory extends BBModelFactory {

	public BBModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		int newInstanceNumber=instanceNumber.val();
		instanceNumber.inc();
		return new MetaSwapModelInstance(this.bbModelConfig, newInstanceNumber, this.timeHorizon);
	}
}
