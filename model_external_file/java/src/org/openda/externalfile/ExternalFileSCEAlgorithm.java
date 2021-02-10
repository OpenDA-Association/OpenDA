package org.openda.externalfile;

import org.openda.algorithms.SCE;
import org.openda.interfaces.IStochModelInstance;

public class ExternalFileSCEAlgorithm extends SCE {

	private IStochModelInstance currentBestEstimate;

	@Override
	public void next() {
		super.next();
		if (this.hasNext()) {
			if (this.bestEstimate != currentBestEstimate) {
				currentBestEstimate = this.bestEstimate;
			}
			return;
		}
		if (this.bestEstimate == null) return;
		if (!(bestEstimate instanceof ExternalFileModelInstance)) return;
		((ExternalFileModelInstance) bestEstimate).sendFinalParameters();
		//optimizer.getCurrentCosts()
	}

}
