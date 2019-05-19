package org.openda.geolab;

import org.openda.interfaces.IStochModelInstance;

public interface ICalLibAlgorithm extends Runnable {
	void run();
	IStochModelInstance getBestEstimate();
}

