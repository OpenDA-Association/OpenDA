package org.openda.externalfile;

import org.openda.algorithms.Dud;
import org.openda.interfaces.IStochModelInstance;

public class ExternalSocketDudAlgorithm extends Dud {

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
	}


}
