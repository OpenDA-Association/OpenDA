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
package org.openda.model_wflow;
import junit.framework.TestCase;
import org.openda.blackbox.wrapper.BBStochModelFactory;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;
import org.openda.resultwriters.MatlabResultWriter;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.ResultSelectionConfig;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.io.IOException;

/**
 * For testing the model.axpyOnState of OpenStreams wrapper.
 * The following environment variables should be set before running the unit test:
 * PATH=openda_bin\win64_ifort\;c:\Python27_64\anaconda\;c:\ jre\bin\server\;d: \pcraster-4.0.1-beta-20140301_x86-64_msvs-9\bin\
 * PYTHONPATH=openda_bin\jep-3.1.0\;d:\public\model_wflow\java\test\org\openda\model_wflow\testData\wflow_bin\;d:\\pcraster-4.0.1-beta-20140301_x86-64_msvs-9\python\
 * PYTHONHOME=c:\Python27_64\anaconda\
 * The examples above show what each environment variable should contain.
 *
 * This test only works with a 64-bit JDK version 1.7.
 */
public class WflowStochModelFactoryTest extends TestCase {
    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(WflowStochModelFactoryTest.class, "model_wflow");
    }

    public static void testDummy() {
        // No action. Test only exist to avoid warnings on empty test class when
        //            the test below is de-activated by renaming it to tst...()
    }

    public void _testAxpyOnState_1(){
        IStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        bbStochModelFactory.initialize(testData.getTestRunDataDir(),new String[]{"Ourthe/Ourthe_StochModelConfig.xml"});
        IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        IVector orgState = stochModelInstance.getState();
        IVector onesVector = new Vector(orgState.getSize());
        onesVector.setConstant(1.0);
        stochModelInstance.axpyOnState(1.0,onesVector);
        IVector newState = stochModelInstance.getState();
        IResultWriter matlabWriter = new MatlabResultWriter(testData.getTestRunDataDir(), "axpyonstate_results.m");
        Results.addResultWriter(matlabWriter, new ResultSelectionConfig("test"));
        Results.putValue("orgState", orgState, orgState.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
        Results.putValue("newState", newState, newState.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

        assertEquals("Value of arbitrary element i=34: ", newState.getValue(34), orgState.getValue(34)+1.0d);
        assertEquals("Value of arbitrary element i=1635: ", newState.getValue(1635), orgState.getValue(1635)+1.0d);
        assertEquals("Value of arbitrary element i=52789: ", newState.getValue(52789), orgState.getValue(52789)+1.0d);

    }

    public void _testAxpyOnState_2(){
        // case of multiple instances
        IStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        bbStochModelFactory.initialize(testData.getTestRunDataDir(),new String[]{"Ourthe/Ourthe_StochModelConfig.xml"});
        int nInstance = 5;
        IStochModelInstance[] stochModelInstances = new IStochModelInstance[nInstance];
        for (int i=0; i<nInstance; i++){
            stochModelInstances[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        }

        IVector orgState = stochModelInstances[nInstance-1].getState();
        IVector onesVector = new Vector(orgState.getSize());
        onesVector.setConstant(1.0);
        stochModelInstances[nInstance-1].axpyOnState(1.0d,onesVector);

        IVector newState = stochModelInstances[nInstance-1].getState();
        assertEquals("Value of arbitrary element i=34: ", newState.getValue(34), orgState.getValue(34)+1.0d);
        assertEquals("Value of arbitrary element i=1635: ", newState.getValue(1635), orgState.getValue(1635)+1.0d);
        assertEquals("Value of arbitrary element i=52789: ", newState.getValue(52789), orgState.getValue(52789)+1.0d);

        IResultWriter matlabWriter = new MatlabResultWriter(testData.getTestRunDataDir(), "axpyonstate2_results.m");
        Results.addResultWriter(matlabWriter, new ResultSelectionConfig("test"));
        for (int i=0; i<nInstance; i++){
            Results.putValue("orgState_"+i, stochModelInstances[i].getState(), stochModelInstances[i].getState().getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
            stochModelInstances[i].axpyOnState(1.0,onesVector);
            Results.putValue("newState_" + i, stochModelInstances[i].getState(), stochModelInstances[i].getState().getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
        }
    }
}
