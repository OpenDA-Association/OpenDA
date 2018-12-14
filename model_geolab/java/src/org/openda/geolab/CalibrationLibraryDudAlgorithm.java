package org.openda.geolab;

import org.openda.algorithms.Dud;
import org.openda.interfaces.IStochModelFactory;

public class CalibrationLibraryDudAlgorithm extends Dud implements Runnable {
	@Override
	public void run() {
		this.prepare();
		while(this.hasNext()){
			this.next();
		}
		((CalibrationLibraryStochModelInstance)this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress)).setAlgorithmDoneFlag(true);
	}
}
