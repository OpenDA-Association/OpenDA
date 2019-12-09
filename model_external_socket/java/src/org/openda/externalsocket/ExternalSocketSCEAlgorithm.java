package org.openda.externalsocket;

import org.openda.algorithms.SCE;
import org.openda.blackbox.wrapper.BBStochModelInstance;
import org.openda.interfaces.IModelInstance;

public class ExternalSocketSCEAlgorithm extends SCE {
	@Override
	public void next() {
		super.next();
		if (this.hasNext() || this.bestEstimate == null) return;
		if (!(bestEstimate instanceof BBStochModelInstance)) return;
		IModelInstance bestModel = ((BBStochModelInstance) bestEstimate).getModel();
		if (!(bestModel instanceof ExternalSocketModelInstance)) return;
		((ExternalSocketModelInstance) bestModel).sendFinalParameters();
	}
}
