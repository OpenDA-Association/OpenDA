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


package org.openda.resultwriters;

import junit.framework.TestCase;
import org.openda.core.io.castorgenerated.ResultSelectionXML;
import org.openda.interfaces.*;
import org.openda.utils.*;
import org.openda.utils.io.CastorUtils;
import org.openda.utils.io.OpenDaConfigurationReader;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;


/**
 * Tests for the configuration of resultWriters
 */
public class ResultWriterTest extends TestCase {

    //File testDir = null;
    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(ResultWriterTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
        Results.reset();
    }
    
    //private void setTestDir() {
    //    testDir = OpenDaTestSupport.getUnitTestDir("openda/test/org/openda/resultwriters");
    //}

    private void doAlgorithmSteps() {
        final int maxStep = 3;
        final int maxOuterIter = 2;
        final int maxInnerIter = 2;
        Results.putMessage("starting test");

        IVector v = new Vector(3);
        v.setConstant(0);
        
        ITreeVector v2_part1 = new TreeVector("first_part", new Vector("[1,2,3]"));
        ITreeVector v2_part2 = new TreeVector("second_part", new Vector("[4,5,6]"));
        TreeVector v2 = new TreeVector("all");
        v2.addChild(v2_part1);
        v2.addChild(v2_part2);
        Results.putValue("treevector", v2, v2.getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);


        Results.putProgression("*** START doAlgorithmSteps ***");
        for (int i = 0; i < maxStep; i++) {
            Results.putProgression("Algorithm Step");
            Results.putProgression(IResultWriter.MessageType.Step);
            Results.putMessage("Here is an algorithm step");
            Results.putValue("v1", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
            Results.putValue("someValues", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
            for (int j = 0; j < maxOuterIter; j++) {
                Results.putProgression("Outer Iteration");
                Results.putProgression(IResultWriter.MessageType.OuterIteration);
                for (int k = 0; k < maxInnerIter; k++) {
                    Results.putProgression("Inner Iteration");
                    Results.putProgression(IResultWriter.MessageType.InnerIteration);
                    Results.putValue("modelValues", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.InnerIteration);
                    Results.putValue("someValues", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.InnerIteration);
                    Results.putValue("costEvaluation", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.InnerIteration);
                }
                Results.putValue("Outer result", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.OuterIteration);
            }
            Object dummy = null;
            Results.putIterationReport((IInstance) dummy,i,nextValues(v).getValue(i),v);
        }
    }

    public void testCalibrationCostFunctionResults() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-costs-out.m");
        Results.addResultWriter(matlabWriter, new ResultSelectionConfig("costEvaluation"));
        doAlgorithmSteps();
        Results.reset();
    }

    public void testCalibrationTreeVectorOutput() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-treevector-out.m");
        Results.addResultWriter(matlabWriter, new ResultSelectionConfig("treevector"));
        doAlgorithmSteps();
        Results.reset();
    }

    
    public void testMatlabResultsAlgorithmUnknown() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-out.m");

        Results.addResultWriter(matlabWriter);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsFiltering() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-filtering-out.m");

        Results.addResultWriter(matlabWriter);
        Results.setAlgorithmType(IAlgorithm.Type.Filtering);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsFilteringWithSelection_1() {

        //setTestDir();
        //ResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-filtering-select-1-out.m");
        //ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDataDir, "result_selection_1.xml"));
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-filtering-select-1-out.m");
        ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDataDir, "result_selection_1.xml"));

        Results.addResultWriter(matlabWriter, selectionConfig);
        Results.setAlgorithmType(IAlgorithm.Type.Filtering);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsFilteringWithSelection_2() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-filtering-select-2-out.m");
        //ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDir, "result_selection_2.xml"));
        ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDataDir, "result_selection_2.xml"));

        Results.addResultWriter(matlabWriter, selectionConfig);
        Results.setAlgorithmType(IAlgorithm.Type.Filtering);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsCalibration() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-calibration-out.m");

        Results.addResultWriter(matlabWriter);
        Results.setAlgorithmType(IAlgorithm.Type.Calibration);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsCalibrationWithSelection_1() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-calibration-select-1-out.m");
        ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDataDir, "result_selection_1.xml"));

        Results.addResultWriter(matlabWriter, selectionConfig);
        Results.setAlgorithmType(IAlgorithm.Type.Calibration);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsCalibrationWithSelection_2() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-calibration-select-2-out.m");
        ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDataDir, "result_selection_2.xml"));

        Results.addResultWriter(matlabWriter, selectionConfig);
        Results.setAlgorithmType(IAlgorithm.Type.Calibration);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMatlabResultsCalibrationWithWildCard() {

        //setTestDir();
        IResultWriter matlabWriter = new MatlabResultWriter(testRunDataDir, "matlab-calibration-wildcard-out.m");
        ResultSelectionConfig selectionConfig = parseSelectionConfig(new File(testRunDataDir, "result_wildcard.xml"));

        Results.addResultWriter(matlabWriter, selectionConfig);
        Results.setAlgorithmType(IAlgorithm.Type.Calibration);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testCalibrationCostFunctionOutput() {

        //setTestDir();
        IResultWriter asciiWriter = new AsciiResultWriter(testRunDataDir, "output-costs.txt");
        Results.addResultWriter(asciiWriter, new ResultSelectionConfig("costEvaluation"));
        doAlgorithmStepsLevels();
        Results.reset();
    }

    public void testTextTableResultsAlgorithmUnknown() {

        //setTestDir();
        IResultWriter textTableWriter = new TextTableWriter(testRunDataDir, "textTable-out.asc");

        Results.addResultWriter(textTableWriter);
        doAlgorithmSteps();
        Results.reset();
    }

    public void testMcCsvResultWriterAlgorithmUnknown() {

        //setTestDir();
        IResultWriter mcCsvWriter = new McCsvResultWriter(testRunDataDir, "McResult-parameters.csv");

        Results.addResultWriter(mcCsvWriter);
        doAlgorithmMcSteps();
        Results.reset();
    }

    private void doAlgorithmMcSteps() {
        final int maxStep = 3;
        final int maxOuterIter = 2;
        final int maxInnerIter = 2;
        Results.putMessage("starting test");

        IVector v = new Vector(3);
        v.setConstant(0);

        ITreeVector params = new TreeVector("model_parameters",new String[]{"param1","param2"},new double[]{0.02,0.03});
        ITreeVector residual1 = new TreeVector("residual1", new Vector("[1,2,3]"));
        ITreeVector residual2 = new TreeVector("residual2", new Vector("[4,5,6]"));
        TreeVector v2 = new TreeVector("allResidual");
        v2.addChild(residual1);
        v2.addChild(residual2);

        Results.putProgression("*** START doAlgorithmSteps ***");
        for (int i = 0; i < maxStep; i++) {
            params.scale((double)(i+1));
            v2.scale((double) (i+1)*0.6);
            Results.putValue("evaluatedParameters", params, params.getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
            Results.putValue("residuals", v2, v2.getSize(), "any", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);
        }
    }

    private ResultSelectionConfig parseSelectionConfig(File selectionFile) {
        if (!selectionFile.exists()) {
            throw new RuntimeException("Selection file " + selectionFile.getAbsolutePath() + "does not exist");
        }
        ResultSelectionXML resultSelectionXML = (ResultSelectionXML) CastorUtils.parse(selectionFile, ResultSelectionXML.class);
        return OpenDaConfigurationReader.parseResultSelectionConfig(resultSelectionXML);
    }

    private IVector nextValues(IVector prevVector) {
        prevVector.setConstant(prevVector.getValue(0)+1);
        return prevVector;
    }

    private void doAlgorithmStepsLevels() {
        final int maxStep = 3;
        final int maxOuterIter = 2;
        final int maxInnerIter = 2;
        TestInstance testRunner = new TestInstance(null);
        TestInstance testAlgorithm = new TestInstance(testRunner);
        Results.putMessage(testRunner, "starting test");

        IVector v = new Vector(3);
        v.setConstant(0);

        for (int i = 0; i < maxStep; i++) {
            Results.putProgression(testAlgorithm, "Algorithm Step");
            Results.putProgression(IResultWriter.MessageType.Step);
            Results.putMessage(testAlgorithm, "Here is an algorithm step");
            Results.putValue("someValues", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
            TestInstance testOuterLoop = new TestInstance(testAlgorithm);
            for (int j = 0; j < maxOuterIter; j++) {
                Results.putProgression(testOuterLoop, "Outer Iteration");
                TestInstance testInnerLoop = new TestInstance(testOuterLoop);
                for (int k = 0; k < maxInnerIter; k++) {
                    Results.putProgression(testInnerLoop, "Inner Iteration");
                    Results.putValue("modelValues", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.InnerIteration);
                    Results.putValue("someValues", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.InnerIteration);
                    Results.putValue("costEvaluation", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.InnerIteration);
                }
                Results.putValue("Outer result", nextValues(v), nextValues(v).getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.OuterIteration);
            }
        }
    }


}
