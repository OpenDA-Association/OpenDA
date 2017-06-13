/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
import org.openda.interfaces.*;
import org.openda.observers.IoObjectStochObserver;
import org.openda.utils.ObjectSupport;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Locale;

/**
 * Tests for Swan Stochastic Observer
 */
public class SwanStochObserverTest extends TestCase {

    private OpenDaTestSupport testData;

    final String[] checkIDs = {
            "Hsig @ 206767.0,622696.0", "RTpeak @ 206767.0,622696.0",
            "Tm-2-1 @ 196992.0,612714.0", "Tpeq @ 200738.0,607693.0",
            "RTpeak @ 205996.0,604802.0", "Tpm @ 205996.0,604802.0"};
    final boolean[] idMustBeThere = { true, true, true, false, true, false };
    final double[] checkValues = { 2.261, 7.143, 6.701, 4.736, 2.174, 3.794 };
    final double[] checkStdDevs = { 0.2261, 0.9876, .75, -1.0,  0.4444,  -1.0, };


    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanStochObserverTest.class,"model_swan");
    }

    public void testGetIdsAndValues() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTest1");
        IStochObserver swanStochObs = (IStochObserver) ObjectSupport.createConfigurable("IoObjectStochObserver",
                IoObjectStochObserver.class.getName(), test_1_dir, new String[]{"swanStochObsConfig.xml"});
        checkValues(swanStochObs);
    }

    public void testGetStdDevsAsMethod() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTest1");
        IStochObserver swanStochObs = (IStochObserver) ObjectSupport.createConfigurable("IoObjectStochObserver",
                IoObjectStochObserver.class.getName(), test_1_dir, new String[]{"swanStochObsConfig.xml"});
        checkStdDevs(swanStochObs, swanStochObs.getStandardDeviations());
    }

    public void testGetStdDevsAsProperty() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTest1");
        IStochObserver swanStochObs = (IStochObserver) ObjectSupport.createConfigurable("IoObjectStochObserver",
                IoObjectStochObserver.class.getName(), test_1_dir, new String[]{"swanStochObsConfig.xml"});
        checkStdDevs(swanStochObs, swanStochObs.getObservationDescriptions().getValueProperties("stdDev"));
    }

    public void testRealizations() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTest1");
        IStochObserver swanStochObs = (IStochObserver) ObjectSupport.createConfigurable("IoObjectStochObserver",
                IoObjectStochObserver.class.getName(), test_1_dir, new String[]{"swanStochObsConfig.xml"});

        Locale locale = new Locale("EN");
        BufferedWriter realizationsFile = new BufferedWriter(new FileWriter(new File(testData.getTestRunDataDir(), "realizations-out.txt")));

        final int ensembleCount = 50;
        for (int iEnsemble = 1; iEnsemble <= ensembleCount; iEnsemble++) {
            ITreeVector realizations = (ITreeVector) swanStochObs.getRealizations();
            if (iEnsemble == 1) {
                boolean writeComma=false;
                for (String subTreeVectorId : realizations.getSubTreeVectorIds()) {
                    if (writeComma) realizationsFile.write(",");
                    realizationsFile.write(String.format(locale, "%16s", subTreeVectorId));
                    writeComma = true;
                }
                realizationsFile.newLine();
            }
            double[] retrievedValues = realizations.getValues();
            assertEquals("#retrievedValues", 13, retrievedValues.length);
            realizationsFile.write(String.format(locale, "%16.5f", retrievedValues[0]));
            for (int i = 1; i < retrievedValues.length; i++) {
                realizationsFile.write(String.format(locale, ", %16.5f", retrievedValues[i]));
            }
            realizationsFile.newLine();
        }
        realizationsFile.close();
    }

    public void testGetIdsAndValuesTimeDependent() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTimeDependent");
        IStochObserver swanStochObs = (IStochObserver) ObjectSupport.createConfigurable("IoObjectStochObserver",
                IoObjectStochObserver.class.getName(), test_1_dir, new String[]{"swanStochObsConfig.xml"});
        ITime selectedTimes = new Time(55197.0,55197.2,0.042);
        swanStochObs.createSelection(selectedTimes);
        ITime[] times = swanStochObs.getTimes();
        assertEquals("#observationIDs[0]", "Hsig @ 5000.,1000.", swanStochObs.getObservationDescriptions().getStringProperties("id")[0]);
        assertEquals("#observationIDs[1]", "Hsig @ 3000.,2000.", swanStochObs.getObservationDescriptions().getStringProperties("id")[1]);
        assertEquals("times[0]",55197.0,times[0].getMJD());
        assertEquals("times[4]",55197.166666666664,times[4].getMJD());
        assertEquals("values[0]",0.02508,swanStochObs.getValues().getValue(0));
        assertEquals("values[4]",0.61598,swanStochObs.getValues().getValue(4));
    }

    private void checkValues(IStochObserver observer) {

        IObservationDescriptions observationDescriptions = observer.getObservationDescriptions();
        String[] observationIDs = observationDescriptions.getStringProperties("id");

        assertEquals("#observationIDs", 13, observationIDs.length);

        double[] observationValues = observer.getValues().getValues();
        assertEquals("#observationValues", 13, observationValues.length);

        for (int i = 0; i < checkIDs.length; i++) {
            if (idMustBeThere[i]) {
                assertTrue(i + ": " + checkIDs[i] + " present", findIndex(observationIDs, checkIDs[i]) >= 0);
                assertEquals(i + ": " + checkIDs[i] + " value",
                        checkValues[i], observationValues[findIndex(observationIDs, checkIDs[i])]);
            } else {
                assertTrue(i + ": " + checkIDs[i] + " must be skipped", findIndex(observationIDs, checkIDs[i]) < 0);
            }
        }

        double[] xCoords = observationDescriptions.getValueProperties("Xp").getValues();
        double[] yCoords = observationDescriptions.getValueProperties("Yp").getValues();

        assertEquals(checkIDs[0] + " xCoord", 206767.0, xCoords[findIndex(observationIDs, checkIDs[0])]);
        assertEquals(checkIDs[2] + " xCoord", 196992.0, xCoords[findIndex(observationIDs, checkIDs[2])]);
        assertEquals(checkIDs[4] + " yCoord", 604802.0, yCoords[findIndex(observationIDs, checkIDs[4])]);
    }

    private void checkStdDevs(IStochObserver observer, IVector standardDeviations) {

        assertEquals("#standardDeviations", 13, standardDeviations.getSize());

        IObservationDescriptions observationDescriptions = observer.getObservationDescriptions();
        String[] observationIDs = observationDescriptions.getStringProperties("id");

        for (int i = 0; i < checkIDs.length; i++) {
            if (idMustBeThere[i]) {
                assertEquals(i + ": " + checkIDs[i] + " value",
                        checkStdDevs[i], standardDeviations.getValue(findIndex(observationIDs, checkIDs[i])), 1e-7);
            }
        }
    }

    private static int findIndex(String[] observationIDs, String checkID) {
        for (int i = 0; i < observationIDs.length; i++) {
            if (observationIDs[i].equals(checkID)) return i;
        }
        return -1;
    }
}
