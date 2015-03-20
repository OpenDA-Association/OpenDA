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
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.wrapper.BBModelFactory;
import org.openda.blackbox.wrapper.BBStochModelFactory;
import org.openda.interfaces.*;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Test of SWAN as a Black Box model.
 */
public class SwanBBTest extends TestCase {

    private File testRunDataDir;

    static String[] expectedExchangeItemIDs = new String[]{
            "start_time","end_time","state", "wind.x", "wind.y", "openboundary" , "obHs" , "obPeriod" , "obPeakDir" , "obDirSpread"
    };

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(SwanBBTest.class, "model_swan");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testDUMMYSWAN() {
        IStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File testDir = new File(testRunDataDir,"simpleBBW"+File.separator+"simpleBBStochModel");
        bbStochModelFactory.initialize(testDir, new String[]{"bbSwanForEnkfStochModelConfig.xml"});
    }

    public void testSimpleBB() {

        File testData = new File(new File(testRunDataDir,"simpleBBW"),"simpleBBStochModel");
        IModelFactory bbModelFactory = new BBModelFactory();
        bbModelFactory.initialize(testData,new String[]{"bbSwanModel.xml"});

        for (int i = 0; i < 1; i++) {
            IModelInstance bbModel = bbModelFactory.getInstance(new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);
            String[] exchangeItemIDs = bbModel.getExchangeItemIDs();
            for (int j = 0; j < exchangeItemIDs.length; j++) {
                assertEquals("exch.item.ID[" + j + "]", expectedExchangeItemIDs[j], exchangeItemIDs[j]);
                IPrevExchangeItem exchangeItem = bbModel.getExchangeItem(exchangeItemIDs[j]);
                if (exchangeItem.getId().contentEquals("state")) {
                    int iLoc = 3;
                    int iFreq = 6;
                    int iDir = 0;
                    int nLoc = 2397;
                    int nFreq = 31;
                    int nDir = 36;
                    int intValue = 248;
                    int index = iDir+iFreq*nDir+iLoc*nDir*nFreq;
                    double factor = 0.14506797E-06;
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("State value: ",factor*intValue,values[index]);
                    System.out.println("factor*intValue: "+factor*intValue);
                    System.out.println("values[index]: "+values[index]);

                    iLoc = 12;
                    iFreq = 21;
                    iDir = 34;
                    index = iDir+iFreq*nDir+iLoc*nDir*nFreq;
                    factor = 0.19009338E-07;
                    intValue = 4143;
                    values = exchangeItem.getValuesAsDoubles();
                    assertEquals("State value: ",factor*intValue,values[index]);
                    System.out.println("factor*intValue: "+factor*intValue);
                    System.out.println("values[index]: "+values[index]);
                } else if (exchangeItem.getId().contentEquals("wind.x")) {
                    double[] windx1 = new double[] {0.0,0.0,0.0,0.0};
                    double[] windx2 = exchangeItem.getValuesAsDoubles();
                    for (int k=0; k<windx1.length; k++) {
                        assertEquals("wind.x: ",windx1[k],windx2[k]);
                    }
                }  else if (exchangeItem.getId().contentEquals("wind.y")) {
                    double[] windy1 = new double[] {-15.0,-15.0,-15.0,-15.0};
                    double[] windy2 = exchangeItem.getValuesAsDoubles();
                    for (int k=0; k<windy1.length; k++) {
                        assertEquals("wind.y: ",windy1[k],windy2[k]);
                    }
                }  else if (exchangeItem.getId().contentEquals("openboundary")) {
                    int iLoc = 2;
                    int iFreq = 4;
                    int iDir = 23;
                    int nLoc = 4;
                    int nFreq = 30;
                    int nDir = 24;
                    int intValue = 159;
                    int index = iDir+iFreq*nDir+iLoc*nDir*nFreq;
                    double factor = 1.0E-06;
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("Openboundary value: ",factor*intValue,values[index]);
                    System.out.println("Openboundary, factor*intValue: "+factor*intValue);
                    System.out.println("Openboundary, values[index]: "+values[index]);
                }  else if (exchangeItem.getId().contentEquals("obHs")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("Hs: ",0.0,values[1]);
                }  else if (exchangeItem.getId().contentEquals("obPeriod")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("period: ",20.0,values[1]);
                }  else if (exchangeItem.getId().contentEquals("obPeakDir")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("peak direction: ",0.0,values[1]);
                }  else if (exchangeItem.getId().contentEquals("obDirSpread")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("directional spread: ",2.0,values[1]);
                }
            }
            bbModel.compute(new Time(0));
        }
    }

    public void testSimpleStochBB() {

        IStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File testData = new File(new File(testRunDataDir,"simpleBBW"),"simpleBBStochModel");
        bbStochModelFactory.initialize(testData,new String[]{"bbSwanForEnkfStochModelConfig.xml"});

        for (int i = 0; i < 1; i++) {
            IStochModelInstance bbStochModel = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            String[] exchangeItemIDs = bbStochModel.getExchangeItemIDs();

            ITime starttime = bbStochModel.getTimeHorizon().getBeginTime();
            ITime endtime = bbStochModel.getTimeHorizon().getEndTime();
            System.out.println("starttime: "+starttime+", endtime: "+endtime);

            for (int j = 0; j < exchangeItemIDs.length; j++) {
                assertEquals("exch.item.ID[" + j + "]", expectedExchangeItemIDs[j], exchangeItemIDs[j]);
                IPrevExchangeItem exchangeItem = bbStochModel.getExchangeItem(exchangeItemIDs[j]);
                if (exchangeItem.getId().contentEquals("state")) {
                    int iLoc = 3;
                    int iFreq = 6;
                    int iDir = 0;
                    int nLoc = 2397;
                    int nFreq = 31;
                    int nDir = 36;
                    int intValue = 248;
                    int index = iDir+iFreq*nDir+iLoc*nDir*nFreq;
                    double factor = 0.14506797E-06;
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("State value: ",factor*intValue,values[index]);
                    System.out.println("factor*intValue: "+factor*intValue);
                    System.out.println("values[index]: "+values[index]);

                    iLoc = 12;
                    iFreq = 21;
                    iDir = 34;
                    index = iDir+iFreq*nDir+iLoc*nDir*nFreq;
                    factor = 0.19009338E-07;
                    intValue = 4143;
                    values = exchangeItem.getValuesAsDoubles();
                    assertEquals("State value: ",factor*intValue,values[index]);
                    System.out.println("factor*intValue: "+factor*intValue);
                    System.out.println("values[index]: "+values[index]);
                } else if (exchangeItem.getId().contentEquals("wind.x")) {
                    double[] windx1 = new double[] {0.0,0.0,0.0,0.0};
                    double[] windx2 = exchangeItem.getValuesAsDoubles();
                    for (int k=0; k<windx1.length; k++) {
                        assertEquals("wind.x: ",windx1[k],windx2[k]);
                    }
                }  else if (exchangeItem.getId().contentEquals("wind.y")) {
                    double[] windy1 = new double[] {-15.0,-15.0,-15.0,-15.0};
                    double[] windy2 = exchangeItem.getValuesAsDoubles();
                    for (int k=0; k<windy1.length; k++) {
                        assertEquals("wind.y: ",windy1[k],windy2[k]);
                    }
                }  else if (exchangeItem.getId().contentEquals("openboundary")) {
                    int iLoc = 2;
                    int iFreq = 4;
                    int iDir = 23;
                    int nLoc = 4;
                    int nFreq = 30;
                    int nDir = 24;
                    int intValue = 159;
                    int index = iDir+iFreq*nDir+iLoc*nDir*nFreq;
                    double factor = 1.0E-06;
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("Openboundary value: ",factor*intValue,values[index]);
                    System.out.println("Openboundary, factor*intValue: "+factor*intValue);
                    System.out.println("Openboundary, values[index]: "+values[index]);
                }  else if (exchangeItem.getId().contentEquals("obHs")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("Hs: ",0.0,values[1]);
                }  else if (exchangeItem.getId().contentEquals("obPeriod")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("period: ",20.0,values[1]);
                }  else if (exchangeItem.getId().contentEquals("obPeakDir")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("peak direction: ",0.0,values[1]);
                }  else if (exchangeItem.getId().contentEquals("obDirSpread")) {
                    double[] values = exchangeItem.getValuesAsDoubles();
                    assertEquals("directional spread: ",2.0,values[1]);
                }
            }
            IVector state = bbStochModel.getState();
            assertTrue("TreeVector expected",state instanceof ITreeVector);
            ITreeVector stateAsTree = (ITreeVector) state;
            assertTrue("stateAsTree.numChild",stateAsTree.getSubTreeVectorIds().size() == 8);

            bbStochModel.compute(new Time(0));
        }
    }

    public void testUncertaintyEngine() {
        UncertaintyEngine uncertaintyEngine = new UncertaintyEngine();
        File uncertaintyEngineWorkingDir = new File(new File(new File(
                testRunDataDir,"simpleBBW"),"simpleBBStochModel"),"uncertainties");
        String[] uncertaintyEngineArguments = {"uncertaintySpecification.xml"};
        uncertaintyEngine.initialize(uncertaintyEngineWorkingDir, uncertaintyEngineArguments);
        IVector mannUsectionAslice1 = new Vector("[0,3,4,5,6]");
        uncertaintyEngine.useRandomSeed=false;
        IVector mannUsectionAslice1Draw = uncertaintyEngine.getRealization("state", mannUsectionAslice1);
        assertEquals("mannUsectionAslice1Draw",
                "[0.0,2.944941926521162,3.9150411070968616,5.12281182129511,5.733099908281949]",
                mannUsectionAslice1Draw.toString());
    }
}
