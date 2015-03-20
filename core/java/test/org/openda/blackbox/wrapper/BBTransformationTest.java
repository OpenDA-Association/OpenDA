/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Tests for black box model parameter transformations
 */
public class BBTransformationTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(BBTransformationTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testTransformations() {

        File stochModelConfigDir = new File(testRunDataDir,"transformations");

        IStochModelFactory stochModelFactory = new BBStochModelFactory();
        stochModelFactory.initialize(stochModelConfigDir, new String[]{"simpleBbStochModelConfig.xml"});
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);

        IVector standardDeviations = stochModelInstance.getParameterUncertainty().getStandardDeviations();
        assertEquals("stDev locA.DX", .05, standardDeviations.getValue(0));
        assertEquals("stDev locA.DY", .05, standardDeviations.getValue(1));
        assertEquals("stDev locB.DX", .03, standardDeviations.getValue(2));
        assertEquals("stDev locB.DY", .03, standardDeviations.getValue(3));
        assertEquals("stDev locC.DX", .1, standardDeviations.getValue(4));
        assertEquals("stDev locC.DY", .1, standardDeviations.getValue(5));
        assertEquals("stDev locD.DX", 2.0, standardDeviations.getValue(6));
        assertEquals("stDev locD.DY", 2.0, standardDeviations.getValue(7));

        ITreeVector parameters = (ITreeVector) stochModelInstance.getParameters();
        assertEquals("stDev locA.Phase/locA.Ampl-dX", 0d, parameters.getSubTreeVector("locA.Phase/locA.Ampl-dX").getValue(0));
        assertEquals("stDev locA.Phase/locA.Ampl-dY", 0d, parameters.getSubTreeVector("locA.Phase/locA.Ampl-dY").getValue(0));
        assertEquals("stDev locB.Phase/locB.Ampl-dX", 0d, parameters.getSubTreeVector("locB.Phase/locB.Ampl-dX").getValue(0));
        assertEquals("stDev locB.Phase/locB.Ampl-dY", 0d, parameters.getSubTreeVector("locB.Phase/locB.Ampl-dY").getValue(0));
        assertEquals("stDev locC.Phase/locC.Ampl-dX", 0d, parameters.getSubTreeVector("locC.Phase/locC.Ampl-dX").getValue(0));
        assertEquals("stDev locC.Phase/locC.Ampl-dY", 0d, parameters.getSubTreeVector("locC.Phase/locC.Ampl-dY").getValue(0));
        assertEquals("stDev locD.Phase/locD.Ampl(etc.)-dX", 0d, parameters.getSubTreeVector("locD.Phase/locD.Ampl(etc.)-dX").getValue(0));
        assertEquals("stDev locD.Phase/locD.Ampl(etc.)-dY", 0d, parameters.getSubTreeVector("locD.Phase/locD.Ampl(etc.)-dY").getValue(0));

        ArrayList<String> subTreeVectorIds = parameters.getSubTreeVectorIds();
        assertEquals("adjust caption param 8", "locD.Phase/locD.Ampl-H", subTreeVectorIds.get(8));
        assertEquals("adjust caption param 9", "locD.Phase/locD.Ampl-G", subTreeVectorIds.get(9));
        assertEquals("adjust caption param 10", "SpecificXCaption", subTreeVectorIds.get(10));
        assertEquals("adjust caption param 11", "locD.Phase/locD.Ampl-dY", subTreeVectorIds.get(11));
        assertEquals("adjust caption param 12", "locD.H", subTreeVectorIds.get(12));
        assertEquals("adjust caption param 13", "locD.G", subTreeVectorIds.get(13));
        assertEquals("adjust caption param 14", "locD/locE.H", subTreeVectorIds.get(14));
        assertEquals("adjust caption param 15", "locD/locE.G", subTreeVectorIds.get(15));

        IVector adjustedParameters = parameters.clone();
        adjustedParameters.setValue(0, 0.02); // locA.Phase/locA.Ampl-dX
        adjustedParameters.setValue(1, 0.02); // locA.Phase/locA.Ampl-dY
        adjustedParameters.setValue(2, 0.01); // locB.Phase/locB.Ampl-dX
        adjustedParameters.setValue(3, 0.02); // locB.Phase/locB.Ampl-dY
        adjustedParameters.setValue(4, 0.05); // locC.Phase/locC.Ampl-dX
        adjustedParameters.setValue(5, 0.06); // locC.Phase/locC.Ampl-dY
        adjustedParameters.setValue(6, 0.00); // locD/locE.Phase/locD/locE-dX
        adjustedParameters.setValue(7, 1   ); // locD/locE.Phase/locD/locE-dY
        stochModelInstance.setParameters(adjustedParameters);

        double phaseLocA = stochModelInstance.getExchangeItem("locA.Phase").getValuesAsDoubles()[0];
        double amplLocA = stochModelInstance.getExchangeItem("locA.Ampl").getValuesAsDoubles()[0];
        double phaseLocB = stochModelInstance.getExchangeItem("locB.Phase").getValuesAsDoubles()[0];
        double amplLocB = stochModelInstance.getExchangeItem("locB.Ampl").getValuesAsDoubles()[0];
        double phaseLocC = stochModelInstance.getExchangeItem("locC.Phase").getValuesAsDoubles()[0];
        double amplLocC = stochModelInstance.getExchangeItem("locC.Ampl").getValuesAsDoubles()[0];
        double phaseLocD = stochModelInstance.getExchangeItem("locD.Phase").getValuesAsDoubles()[0];
        double amplLocD = stochModelInstance.getExchangeItem("locD.Ampl").getValuesAsDoubles()[0];
        double phaseLocE = stochModelInstance.getExchangeItem("locE.Phase").getValuesAsDoubles()[0];
        double amplLocE = stochModelInstance.getExchangeItem("locE.Ampl").getValuesAsDoubles()[0];

        assertEquals("adjust locA.Phase", 141.56669, phaseLocA, 1e-5);
        assertEquals("adjust locA.Ampl", 0.6019, amplLocA, 1e-5);
        assertEquals("adjust locB.Phase", 260.46914, phaseLocB, 1e-5);
        assertEquals("adjust locB.Ampl", 0.77859, amplLocB, 1e-5);
        assertEquals("adjust locC.Phase", 323.72467, phaseLocC, 1e-5);
        assertEquals("adjust locC.Ampl", 1.20227, amplLocC, 1e-5);
        assertEquals("adjust locD.Phase", 0.73869, phaseLocD, 1e-5);
        assertEquals("adjust locD.Ampl", 9.96277, amplLocD, 1e-5);
        assertEquals("adjust locE.Phase", 179.26131, phaseLocE, 1e-5);
        assertEquals("adjust locE.Ampl", 9.96277, amplLocE, 1e-5);

        stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        adjustedParameters = stochModelInstance.getParameters().clone();
        adjustedParameters.setValue(14, 1); // locD/locE.Phase/locD/locE-dX
        adjustedParameters.setValue(15, 1   ); // locD/locE.Phase/locD/locE-dY
        stochModelInstance.setParameters(adjustedParameters);

        phaseLocD = stochModelInstance.getExchangeItem("locD.Phase").getValuesAsDoubles()[0];
        amplLocD = stochModelInstance.getExchangeItem("locD.Ampl").getValuesAsDoubles()[0];
        phaseLocE = stochModelInstance.getExchangeItem("locE.Phase").getValuesAsDoubles()[0];
        amplLocE = stochModelInstance.getExchangeItem("locE.Ampl").getValuesAsDoubles()[0];

        assertEquals("adjust locD.Phase", 0.671311, phaseLocD, 1e-5);
        assertEquals("adjust locD.Ampl", 10.9627, amplLocD, 1e-5);
        assertEquals("adjust locE.Phase", 179.17889, phaseLocE, 1e-5);
        assertEquals("adjust locE.Ampl", 8.96287, amplLocE, 1e-5);

        stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        adjustedParameters = stochModelInstance.getParameters().clone();
        adjustedParameters.setValue(12, 1); // locD/locE.Phase/locD/locE-dX
        adjustedParameters.setValue(13, 1); // locD/locE.Phase/locD/locE-dY
        stochModelInstance.setParameters(adjustedParameters);

        phaseLocD = stochModelInstance.getExchangeItem("locD.Phase").getValuesAsDoubles()[0];
        amplLocD = stochModelInstance.getExchangeItem("locD.Ampl").getValuesAsDoubles()[0];

        assertEquals("adjust locD.Phase", 0.671311, phaseLocD, 1e-5);
        assertEquals("adjust locD.Ampl", 10.9627, amplLocD, 1e-5);

        stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        adjustedParameters = stochModelInstance.getParameters().clone();
        adjustedParameters.setValue(10, 1); // locD/locE.Phase/locD/locE-dX
        adjustedParameters.setValue(11, 1); // locD/locE.Phase/locD/locE-dY
        stochModelInstance.setParameters(adjustedParameters);

        phaseLocD = stochModelInstance.getExchangeItem("locD.Phase").getValuesAsDoubles()[0];
        amplLocD = stochModelInstance.getExchangeItem("locD.Ampl").getValuesAsDoubles()[0];

        assertEquals("adjust locD.Phase", 0.671311, phaseLocD, 1e-5);
        assertEquals("adjust locD.Ampl", 10.9627, amplLocD, 1e-5);

        stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        adjustedParameters = stochModelInstance.getParameters().clone();
        adjustedParameters.setValue(8, 1); // locD/locE.Phase/locD/locE-dX
        adjustedParameters.setValue(9, 1); // locD/locE.Phase/locD/locE-dY
        stochModelInstance.setParameters(adjustedParameters);

        phaseLocD = stochModelInstance.getExchangeItem("locD.Phase").getValuesAsDoubles()[0];
        amplLocD = stochModelInstance.getExchangeItem("locD.Ampl").getValuesAsDoubles()[0];

        assertEquals("adjust locD.Phase", 0.671311, phaseLocD, 1e-5);
        assertEquals("adjust locD.Ampl", 10.9627, amplLocD, 1e-5);

    }
}
