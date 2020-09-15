package org.openda.externalsocket;

import org.openda.algorithms.Dud;
import org.openda.blackbox.wrapper.BBStochModelInstance;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelInstance;

public class ExternalSocketDudAlgorithm extends Dud {

	private IStochModelInstance currentBestEstimate;
	private double[] currentBestStdValues;

	@Override
	public void next() {
		super.next();
		if (this.hasNext()) {
			if (this.bestEstimate != currentBestEstimate) {
				currentBestStdValues = optimizer.getStdValues();
				currentBestEstimate = this.bestEstimate;
			}
			return;
		}
		if (this.bestEstimate == null) return;
		if (!(bestEstimate instanceof BBStochModelInstance)) return;
		IModelInstance bestModel = ((BBStochModelInstance) bestEstimate).getModel();
		if (!(bestModel instanceof ExternalSocketModelInstance)) return;
		((ExternalSocketModelInstance) bestModel).sendFinalParameters(currentBestStdValues);
	}


}
