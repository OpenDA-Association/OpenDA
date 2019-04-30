package org.openda.geolab;

import org.openda.algorithms.Dud;
import org.openda.interfaces.IStochModelFactory;
import org.apache.commons.lang3.exception.*;

public class CalibrationLibraryDudAlgorithm extends Dud implements Runnable {
	@Override
	public void run() {
		try {
			this.prepare();
			while(this.hasNext()){
				this.next();
			}
			((CalibrationLibraryStochModelInstance)this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress)).
				setAlgorithmDoneFlag(CalibrationLibraryStochModelInstance.ExitStatus.DONE);
		} catch (Exception e) {
			String errorString = e.getMessage() + "\n" + ExceptionUtils.getStackTrace(e);
			System.out.println(errorString);
			((CalibrationLibraryStochModelInstance)this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress)).
				setAlgorithmDoneFlag(CalibrationLibraryStochModelInstance.ExitStatus.ERROR, errorString);
		}
	}
}
