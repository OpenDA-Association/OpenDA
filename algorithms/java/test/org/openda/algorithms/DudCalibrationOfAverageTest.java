/*
 * Copyright (c) 2019 OpenDA Association
 * All rights reserved.
 *
 * This file is part of OpenDA.
 *
 * OpenDA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * OpenDA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.models.river1.River1StochModelFactory;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.*;

import java.io.File;
import java.io.IOException;

public class DudCalibrationOfAverageTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;
    private long initSeed=1234567890;

    protected void setUp() throws IOException {
       	testData = new OpenDaTestSupport(CalibrationTest.class,"algorithms");
        testRunDataDir = testData.getTestRunDataDir();
   }

    public void testOscillBiasedCostFunction() {
        System.out.println("========================================================");
        System.out.println(" Test cost function with removal of temporal variation.");
        System.out.println("========================================================");

        //generate observations
        StochVector.setSeed(initSeed);
        IStochModelFactory factory = new River1StochModelFactory();
        factory.initialize(null, new String[]{"<riverConfig>"
        +"<simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>"
        +"<parameters names=\"alpha_a,alpha_b\">[0.0,0.0]</parameters>"
        +"<parameterUncertainty names=\"alpha_a,alpha_b\">[0.1,0.1]</parameterUncertainty>"
        +"<systemNoise>{[0.0,0.0],[0.1,0.1]}</systemNoise>"
        +"<initialState>[0.0,0.0]</initialState><initialStateUncertainty>[0.0,0.0]</initialStateUncertainty>"
        +"</riverConfig>"});
        IStochObserver obs = new NoosTimeSeriesStochObserver();
        obs.initialize(new File(testRunDataDir, "river1"), new String[]{"noosObservations.xml"});

        // run model:
        IStochModelInstance riverModel = factory.getInstance(IStochModelFactory.OutputLevel.ModelDefault);
        //riverModel.announceObservedValues(obs.getObservationDescriptions());
        ITime time = riverModel.getTimeHorizon();
        riverModel.compute(time.getEndTime());

        // check cost at optimum, with bias correction:
        SimulationKwadraticCostFunction J = new SimulationKwadraticCostFunction(factory,obs);
        J.stdRemoval = true;
		IVector p0 = new Vector(riverModel.getParameters());
		System.out.println("p0 = "+p0);
		System.out.println("Should be p0 = [0.0,0.0]");
		assertEquals("p0",p0.toString(),"[0.0,0.0]");
		double Jp0 = J.evaluate(p0,"initialization");
		System.out.println("Initial cost = "+Jp0);
		System.out.println("Should be Initial cost = 0.0");
		assertEquals("J(p0) = ",25.198,Jp0,1e-3);
		IVector predVec = J.getLastPredictions();
		IVector obsVec  = obs.getExpectations();
		System.out.println("predVec="+predVec);
		System.out.println(" obsVec="+obsVec);
		IVector resVec=obsVec.clone();
		resVec.axpy(-1.0, predVec);
		System.out.println(" resVec="+resVec);

		riverModel.finish();
    }

    public void tstOscillBiasedDud() {
        System.out.println("========================================================");
        System.out.println(" Test Dud with and without bias correction.");
        System.out.println("========================================================");

        IStochModelFactory factory = new OscillatorStochModelFactory();
        factory.initialize(null, new String[]{"<oscillatorConfig><simulationTimespan>[0.0,0.05,10.0]</simulationTimespan><parameters names=\"t_damp,omega\">[8.1,1.56]</parameters><parameterUncertainty names=\"t_damp,omega\">[1.0,0.1257]</parameterUncertainty><systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise><initialState>[0.8,0.1]</initialState><initialStateUncertainty>[0.8,0.8]</initialStateUncertainty></oscillatorConfig>"});
        IStochObserver obs = new NoosTimeSeriesStochObserver();
        obs.initialize(new File(testRunDataDir, "oscillator_with_noos_obs"), new String[]{"noosObservationsBiasedDud.xml"});

        IVector trueParams = new Vector(new double[]{8.0,1.5708});
        IVector initParams = new Vector(new double[]{8.1,1.56});

        // with bias correction:
//        DudNoBias dudAlgorithm = new DudNoBias();
        Dud dudAlgorithm = new Dud();
        dudAlgorithm.initialize(new File(testRunDataDir, "oscillator_with_noos_obs"), new String[]{"dudAlgorithmBiased.xml"});
        dudAlgorithm.setStochComponents(obs,factory);
        dudAlgorithm.prepare();
        dudAlgorithm.run();
        IStochModelInstance bestModel = dudAlgorithm.getBestEstimate();
        IVector bestParams = bestModel.getParameters();
        dudAlgorithm.finish();

        IVector bestDelta = bestParams.clone();
        bestDelta.axpy(-1.0, trueParams);
        double best = bestDelta.norm2();

        IVector initDelta = initParams.clone();
        initDelta.axpy(-1.0, trueParams);
        double init = initDelta.norm2();

        assertEquals("Initial norm2 difference: ", init, 0.1,1E-2);
        assertEquals("Best norm2 difference: ", best, 0.0,1E-3);
        //i.e. with bias correction, Dud is able to reproduce the true parameters

        // without bias correction:
        Dud dudAlgorithmNormal = new Dud();
        dudAlgorithmNormal.initialize(new File(testRunDataDir, "oscillator_with_noos_obs"), new String[]{"dudAlgorithm.xml"});
        dudAlgorithmNormal.setStochComponents(obs,factory);
        dudAlgorithmNormal.prepare();
        dudAlgorithmNormal.run();
        bestModel = dudAlgorithmNormal.getBestEstimate();
        bestParams = bestModel.getParameters();
        dudAlgorithmNormal.finish();

        bestDelta = bestParams.clone();
        bestDelta.axpy(-1.0, trueParams);
        best = bestDelta.norm2();

        initDelta = initParams.clone();
        initDelta.axpy(-1.0, trueParams);
        init = initDelta.norm2();

        assertEquals("Initial norm2 difference: ", init, 0.1,1E-2);
        assertEquals("Best norm2 difference: ", best, 3.06,1E-2);
        //best cost > init cost i.e. without bias correction, Dud fails to reproduce the true parameters

    }

}
