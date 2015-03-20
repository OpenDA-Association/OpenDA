package org.openda.model_nemo;

import org.openda.blackbox.wrapper.BBModelFactory;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IStochModelFactory;

/**
 * We only need to override the getInstance method such that we will get a model implementing
 * the extension for handling observations and localization.
 */
public class NemoModelFactory extends BBModelFactory {

	public BBModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		int newInstanceNumber=instanceNumber.val();
		instanceNumber.inc();
		return new NemoModelInstance(this.bbModelConfig, newInstanceNumber, this.timeHorizon);
	}
}
