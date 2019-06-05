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
package org.openda.blackbox.wrapper;
import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.ITime;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;

/**
 * Testing for the stoch model configuration of coloured noise on 2d maps
 */
public class BBStochModelMaps2dNoiseModelTest extends TestCase
{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(BBStochModelMaps2dNoiseModelTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testNoiseModel() {
		
		StochVector.setSeed(1234); //fix seedvalue to get repeatable results.

		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_1.xml"});

		int instanceCount = 4;
		IStochModelInstance[] stochModelInstances = new IStochModelInstance[instanceCount];
		for (int i = 0; i < instanceCount; i++) {
			stochModelInstances[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
			stochModelInstances[i].setAutomaticNoiseGeneration(true);
		}

		int timeStepCount = 5;
		ITime timeHorizon = stochModelInstances[0].getTimeHorizon();
		ITime startTime = timeHorizon.getBeginTime();
		double deltaTasMJD = 1d / 24d / 60d * 30d; // 30 minutes (3 bc.-timeseries steps)
		for (int t = 1; t <= timeStepCount; t++) {
			ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
			for (int i = 0; i < instanceCount; i++) {
				stochModelInstances[i].compute(targetTime);
			}
			if (t == 3) {
				assertEquals("instance 2, model-item-A-1-b, value 6", -0.790808846271576d, //-0.43694452d, values get summed!!!
						stochModelInstances[2].getExchangeItem("model-item-A-1-b").getValuesAsDoubles()[6], 1e-6d);
			}
			if (t == 4) {
				assertEquals("instance 0 model-item-A-2, value 4", -0.34063953d, //0.36426269d, values get summed!!!
						stochModelInstances[0].getExchangeItem("model-item-A-2").getValuesAsDoubles()[4], 1e-6d);
			}
		}
	}

	public void testNoiseModel2() {
		
		StochVector.setSeed(1234); //fix seedvalue to get repeatable results.

		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_1.xml"});

		IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		stochModelInstance.setAutomaticNoiseGeneration(true);

		int timeStepCount = 5;
		ITime timeHorizon = stochModelInstance.getTimeHorizon();
		ITime startTime = timeHorizon.getBeginTime();
		double deltaTasMJD = 1d / 24d / 60d * 30d; // 30 minutes (3 bc.-timeseries steps)
		for (int t = 1; t <= timeStepCount; t++) {
			ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
			stochModelInstance.compute(targetTime);
			if (t == 1) {
				double[] values = stochModelInstance.getExchangeItem("model-item-A-1-b").getValuesAsDoubles();
				assertEquals("instance, model-item-A-1-b, value 6", 0.0d, values[6], 1e-6d);
			}
			if (t == 2) {
				double[] values = stochModelInstance.getExchangeItem("model-item-A-1-b").getValuesAsDoubles();
				assertEquals("instance, model-item-A-1-b, value 6", -0.341638266805d, values[6], 1e-6d);
			}
		}
	}

    public void testStateSizeNoiseSizeRatio() {
        DummyStateDataObject.setStateSize(1061);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        for (int i = 0; i < 3; i++) {
            stochModelInstance.compute(new Time(i + 1));
        }
    }

    public void testStateSizeNoiseSizeRatioExact() {
        DummyStateDataObject.setStateSize(1061);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        for (int i = 0; i < 3; i++) {
            stochModelInstance.compute(new Time(i + 1));
        }
    }

    public void testStateSizeNoiseSizeRatioBigger() {
        DummyStateDataObject.setStateSize(1065);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        for (int i = 0; i < 3; i++) {
            stochModelInstance.compute(new Time(i + 1));
        }
    }

    public void testStateSizeNoiseSizeRatioSmaller() {
        DummyStateDataObject.setStateSize(1055);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        for (int i = 0; i < 3; i++) {
            stochModelInstance.compute(new Time(i + 1));
        }
    }

    public void testStateSizeNoiseSizeRatioTooSmall() {
        DummyStateDataObject.setStateSize(1050);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        try {
            stochModelInstance.compute(new Time(1));
        } catch (RuntimeException e) {
            return;
        }
        assert false;
    }

    public void testStateSizeNoiseSizeRatioTooBig() {
        DummyStateDataObject.setStateSize(1071);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        try {
            stochModelInstance.compute(new Time(1));
        } catch (RuntimeException e) {
            return;
        }
        assert false;
    }

	public void testStateSizeNoiseSizeRatio2DSmaller() {
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps/noiseRatio2DMappingSmaller");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio2DMappingSmaller.xml"});

		IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		stochModelInstance.setAutomaticNoiseGeneration(true);
		stochModelInstance.compute(new Time(stochModelInstance.getCurrentTime().getMJD()+1.0));
	}

	public void testStateSizeNoiseSizeRatio2DBigger() {
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps/noiseRatio2DMappingBigger");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio2DMappingBigger.xml"});

		IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		stochModelInstance.setAutomaticNoiseGeneration(true);
		stochModelInstance.compute(new Time(stochModelInstance.getCurrentTime().getMJD()+1.0));
	}

	public void testStateSizeNoiseSizeRatio2DExact() {
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps/noiseRatio2DMapping");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseRatio2DMapping.xml"});

		IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		stochModelInstance.setAutomaticNoiseGeneration(true);
		stochModelInstance.compute(new Time(stochModelInstance.getCurrentTime().getMJD()+1.0));
	}

    public void testAddNoiseAfterCompute() {
        DummyStateDataObject.setStateSize(1061);
        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel2DMaps/noiseAfterCompute");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_noiseAfterCompute.xml"});

        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        stochModelInstance.setAutomaticNoiseGeneration(true);
        stochModelInstance.compute(new Time(1));
    }

}
