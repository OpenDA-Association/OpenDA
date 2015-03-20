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
package org.openda.model_swan;

import junit.framework.TestCase;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IStochObserver;
import org.openda.observers.IoObjectStochObserver;
import org.openda.utils.OpenDaTestSupport;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Locale;

/**
 * Tests for Swan Stochastic Observer
 */
public class SwanStochObserverRegexTest extends TestCase {

    private enum OutputType {
        Realizations, StandardDeviations, Values
    }

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(SwanStochObserverRegexTest.class, "model_swan");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testRealizations_1() throws IOException {

        File stochObsWorkingDir = new File(testRunDataDir, "swanObsRegexTest1");
        IStochObserver stochObserver = new IoObjectStochObserver();
        stochObserver.initialize(stochObsWorkingDir, new String[]{"swanStochObsConfig.xml"});

        IObservationDescriptions observationDescriptions = stochObserver.getObservationDescriptions();
        String[] observationIDs = observationDescriptions.getStringProperties("id");

        assertTrue("observationIDs", observationIDs != null);
        assertEquals("#observationIDs", 55, observationIDs.length);
        assertEquals("observationIDs[36]", "RTpeak @ 13.5,0.0", observationIDs[36]);

        final int ensembleCount = 50;
        retrieveAndWriteValues(stochObserver, OutputType.Realizations,
                "realizations-out.txt", ensembleCount, stochObsWorkingDir);
    }

    public void testValues_1() throws IOException {

        File stochObsWorkingDir = new File(testRunDataDir, "swanObsRegexTest1");
        IStochObserver stochObserver = new IoObjectStochObserver();
        stochObserver.initialize(stochObsWorkingDir, new String[]{"swanStochObsConfig.xml"});

        final int ensembleCount = 2;
        retrieveAndWriteValues(stochObserver, OutputType.Values,
                "values-out.txt", ensembleCount, stochObsWorkingDir);
    }

    public void testStandardDeviations_1() throws IOException {

        File stochObsWorkingDir = new File(testRunDataDir, "swanObsRegexTest1");
        IStochObserver stochObserver = new IoObjectStochObserver();
        stochObserver.initialize(stochObsWorkingDir, new String[]{"swanStochObsConfig.xml"});

        final int ensembleCount = 2;
        retrieveAndWriteValues(stochObserver, OutputType.StandardDeviations,
                "stdevs-out.txt", ensembleCount, stochObsWorkingDir);
    }

    public void testRegexVersusIndividual() {

        File stochObsWorkingDir = new File(testRunDataDir, "swanObsRegexTest1");

        IStochObserver stochObserver = new IoObjectStochObserver();
        stochObserver.initialize(stochObsWorkingDir, new String[]{"swanStochObsConfig.xml"});

        IStochObserver stochObserverRegex = new IoObjectStochObserver();
        stochObserverRegex.initialize(stochObsWorkingDir, new String[]{"swanStochObsRegexConfig.xml"});

        double[] standardDeviations = stochObserver.getStandardDeviations().getValues();
        double[] standardDeviationsRegex = stochObserverRegex.getStandardDeviations().getValues();
        assertEquals("StdDevs.length", standardDeviations.length, standardDeviationsRegex.length);
        for (int i = 0; i < stochObserverRegex.getCount(); i++) {
            assertEquals("StdDevs.getValue(i)",
                stochObserverRegex.getStandardDeviations().getValue(i),
                stochObserverRegex.getStandardDeviations().getValue(i));
            assertEquals("values[i]",standardDeviations[i], standardDeviationsRegex[i]);
        }
    }

    private void retrieveAndWriteValues(IStochObserver stochObserver, OutputType outputType, String outputFileName, int ensembleCount, File stochObsWorkingDir) throws IOException {
        Locale locale = new Locale("EN");
        BufferedWriter realizationsFile = new BufferedWriter(new FileWriter(new File(stochObsWorkingDir, outputFileName)));

        realizationsFile.write("Ensemble members 1 to " + ensembleCount);
        realizationsFile.newLine();
        for (int iEnsemble = 1; iEnsemble <= ensembleCount; iEnsemble++) {
            double[] retrievedValues;
            if (outputType == OutputType.Values) {
                retrievedValues = stochObserver.getValues().getValues();
            } else if (outputType == OutputType.Realizations) {
                retrievedValues = stochObserver.getRealizations().getValues();
            } else if (outputType == OutputType.StandardDeviations) {
                retrievedValues = stochObserver.getStandardDeviations().getValues();
            } else {
                throw new RuntimeException("Unexpected output type");
            }
            assertEquals("#retrievedValues", 55, retrievedValues.length);
            realizationsFile.write(String.format(locale, "%16.5f", retrievedValues[0]));
            for (int i = 1; i < retrievedValues.length; i++) {
                realizationsFile.write(String.format(locale, ", %16.5f", retrievedValues[i]));
            }
            realizationsFile.newLine();
        }
        realizationsFile.close();
    }
}
