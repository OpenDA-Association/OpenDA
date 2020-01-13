package org.openda.externalsocket;

import org.openda.algorithms.GriddedFullSearch;
import org.openda.blackbox.wrapper.BBStochModelInstance;
import org.openda.interfaces.IModelInstance;

public class ExternalSocketGriddedFullSearchAlgorithm extends GriddedFullSearch {
	@Override
	public void next() {
		super.next();
		if (this.hasNext() || this.bestEstimate == null) return;
		if (!(bestEstimate instanceof BBStochModelInstance)) return;
		IModelInstance bestModel = ((BBStochModelInstance) bestEstimate).getModel();
		if (!(bestModel instanceof ExternalSocketModelInstance)) return;
		((ExternalSocketModelInstance) bestModel).sendFinalParameters(null);
	}
}
