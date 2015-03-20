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


package org.openda.utils.io;

import junit.framework.TestCase;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Test for writing and reading a calibration's restart settings
 */
public class CalRestartSettingsTest extends TestCase {

    private final IVector[] paramsToFile = new IVector[]{
            new Vector(new double[]{3.2, 4.3, 5.4, 6.5}),
            new Vector(new double[]{30.2, 40.3, 50.4, 60.5})
    };
    private final IVector[] predictionsToFile = new IVector[]{
            new Vector(new double[]{66, 55, 44, 33, 22, 11}),
            new Vector(new double[]{166, 155, 144, 133, 122, 111})
    };
    private final double[] costsToFile = new double[]{.1, .2};

    ITreeVector[] treeVectorParamsToFile = new ITreeVector[]{
            new TreeVector("cal.par.",
                    new String[]{"first par.", "second parameter", "third param."}, new double[]{11, 22, 33}),
            new TreeVector("cal.par.",
                    new String[]{"first par.", "second parameter", "third param."}, new double[]{111, 222, 333})
    };

    ITreeVector[] treeVectorPredictionsToFile = new ITreeVector[]{
            new TreeVector("predictions",
                    new String[]{"pred.1.", "pred.2."}, new double[]{444, 555}),
            new TreeVector("predictions",
                    new String[]{"pred.1.", "pred.2."}, new double[]{4444, 5555})
    };

     
    private File testRunDataDir;
    private OpenDaTestSupport testData;
    private File restartFile;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(CalRestartSettingsTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
        restartFile = new File(testRunDataDir, "restartSettings.xml");
    }

    public void testWriteAndReadDud() {

        final String commentString = "general comment string";
        final String methodName = "dud";

        // write restart info
        CalRestartSettings restartSettings = new CalRestartSettings(methodName);
        restartSettings.setParameters(paramsToFile);
        restartSettings.setCostValues(costsToFile);
        restartSettings.setPredictions(predictionsToFile);
        restartSettings.setComment(commentString);
        restartSettings.writeToFile(restartFile);

        // retrieve restart info
        CalRestartSettings restartSettingsFromFile = new CalRestartSettings(methodName, restartFile);

        // check content
        IVector[] paramsFromFile = restartSettingsFromFile.getParameters();
        double [] costsFromFile = restartSettingsFromFile.getCosts();
        IVector[] predictionsFromFile = restartSettingsFromFile.getPredictions();
        for (int i = 0; i < paramsToFile.length; i++) {
            assertEquals("Check params to/from file", paramsToFile[i].toString(), paramsFromFile[i].toString());
            assertEquals("Check costs to/from file", costsToFile[i], costsFromFile[i]);
            assertEquals("Check predictions to/from file", predictionsToFile[i].toString(), predictionsFromFile[i].toString());
        }
        assertEquals("Check method to/from file", methodName, restartSettingsFromFile.getMethodName());
        assertEquals("Check comment to/from file", commentString, restartSettingsFromFile.getComment());

        // try to retrieve wrong restart info
        try {
            new CalRestartSettings("powell", restartFile);
        } catch (Exception e) {
            assertTrue(e.getMessage().startsWith("Invalid calibration methodName in"));
        }
    }

    public void testWriteAndReadPowell() {

        final String methodName = "powell";

        final IVector baseParamsToFile = new Vector(new double[]{3.2, 4.3, 5.4, 6.5});
        final IVector[] searchDirParamsToFile = new IVector[]{
                new Vector(new double[]{44, 33, 22, 11})
        };

        CalRestartSettings restartSettings = new CalRestartSettings(methodName);
        restartSettings.setParameters(baseParamsToFile, searchDirParamsToFile);
        double costToFile = 12;
        restartSettings.setCostValue(costToFile);
        restartSettings.writeToFile(restartFile);

        CalRestartSettings restartSettingsFromFile = new CalRestartSettings(methodName, restartFile);
        // check content
        IVector baseParamsFromFile = restartSettingsFromFile.getBaseParameters();
        assertEquals("Check base params to/from file", baseParamsToFile.toString(), baseParamsFromFile.toString());

        IVector[] searchDirParamsFromFile = restartSettingsFromFile.getSearchDirParameters();
        for (int i = 0; i < searchDirParamsToFile.length; i++) {
            assertEquals("Check search dir params to/from file", searchDirParamsToFile[i].toString(), searchDirParamsFromFile[i].toString());
        }
        assertEquals("Check cost to/from file", costToFile, restartSettingsFromFile.getCost());
    }

    public void testWriteAndReadSimplex() {

        final String methodName = "simplex";

        CalRestartSettings restartSettings = new CalRestartSettings(methodName);
        restartSettings.setParameters(paramsToFile);
        restartSettings.setCostValues(costsToFile);
        restartSettings.writeToFile(restartFile);

        CalRestartSettings restartSettingsFromFile = new CalRestartSettings(methodName, restartFile);
        // check content
        IVector[] paramsFromFile = restartSettingsFromFile.getParameters();
        double [] costsFromFile = restartSettingsFromFile.getCosts();
        for (int i = 0; i < paramsToFile.length; i++) {
            assertEquals("Check params to/from file", paramsToFile[i].toString(), paramsFromFile[i].toString());
            assertEquals("Check costs to/from file", costsToFile[i], costsFromFile[i]);
        }
    }

    public void testWriteAndReadTreeVectorDud() {
        final String methodName = "dud";
        CalRestartSettings restartSettings = new CalRestartSettings(methodName);
        restartSettings.setParameters(treeVectorParamsToFile);
        restartSettings.setCostValues(costsToFile);
        restartSettings.setPredictions(treeVectorPredictionsToFile);
        restartSettings.writeToFile(restartFile);

        CalRestartSettings restartSettingsFromFile = new CalRestartSettings(methodName, restartFile);
        IVector[] paramsFromFile = restartSettingsFromFile.getParameters();
        IVector[] treeVectorPredictionsFromFile = restartSettingsFromFile.getPredictions();
        for (int i = 0; i < treeVectorParamsToFile.length; i++) {
            assertEquals("Check params to/from file", treeVectorParamsToFile[i].toString(), paramsFromFile[i].toString());
            assertEquals("Check predictions to/from file", treeVectorPredictionsToFile[i].toString(), treeVectorPredictionsFromFile[i].toString());
        }
    }

    public void testWriteAndReadTreeVectorPowell() {
        final String methodName = "powell";

        final IVector baseParamsToFile = new Vector(new double[]{3.2, 4.3, 5.4, 6.5});
        final IVector[] searchDirParamsToFile = new IVector[]{
                new Vector(new double[]{44, 33, 22, 11})
        };

        CalRestartSettings restartSettings = new CalRestartSettings(methodName);
        restartSettings.setParameters(baseParamsToFile, searchDirParamsToFile);
        restartSettings.setCostValue(12);
        restartSettings.writeToFile(restartFile);

        CalRestartSettings restartSettingsFromFile = new CalRestartSettings(methodName, restartFile);
        // check content
        IVector baseParamsFromFile = restartSettingsFromFile.getBaseParameters();
        assertEquals("Check base params to/from file", baseParamsToFile.toString(), baseParamsFromFile.toString());
        IVector[] searchDirParamsFromFile = restartSettingsFromFile.getSearchDirParameters();
        for (int i = 0; i < searchDirParamsToFile.length; i++) {
            assertEquals("Check search dir params to/from file", searchDirParamsToFile[i].toString(), searchDirParamsFromFile[i].toString());
        }
    }

    public void testWriteAndReadTreeVectorSimplex() {
        final String methodName = "simplex";
        CalRestartSettings restartSettings = new CalRestartSettings(methodName);
        restartSettings.setParameters(treeVectorParamsToFile);
        restartSettings.setCostValues(costsToFile);
        restartSettings.writeToFile(restartFile);

        CalRestartSettings restartSettingsFromFile = new CalRestartSettings(methodName, restartFile);
        IVector[] paramsFromFile = restartSettingsFromFile.getParameters();
        for (int i = 0; i < treeVectorParamsToFile.length; i++) {
            assertEquals("Check params to/from file", treeVectorParamsToFile[i].toString(), paramsFromFile[i].toString());
        }
    }
}
