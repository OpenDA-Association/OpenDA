package org.openda.algorithms;

import junit.framework.TestCase;

public class SimulationKwadraticCostFunctionTest extends TestCase {

	public void testClone() {
		SimulationKwadraticCostFunction function = new SimulationKwadraticCostFunction();
		function.factor = 2.0;
		function.addBackgroundTerm = true;
		function.tryParallel = true;
		function.biasRemoval = true;
		function.stdRemoval = true;

		LeastSquaresCostFunction leastSquaresClone = function.clone();
		assertEquals(function.getClass(), leastSquaresClone.getClass());
		SimulationKwadraticCostFunction clone = (SimulationKwadraticCostFunction) leastSquaresClone;

		assertEquals(2.0, clone.factor);
		assertTrue(clone.addBackgroundTerm);
		assertTrue(clone.tryParallel);
		assertTrue(clone.biasRemoval);
		assertTrue(clone.stdRemoval);
	}
}
