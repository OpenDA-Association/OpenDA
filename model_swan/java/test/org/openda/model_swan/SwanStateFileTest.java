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
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class SwanStateFileTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanStateFileTest.class, "model_swan");
    }

    public void testSwanStateFile_1() throws IOException {
		File stateFilesTestDir = new File(testData.getTestRunDataDir(), "StateFiles");

        File swnFile = new File(stateFilesTestDir, "swantest_sector.swn");
        SwanParameters swanParameters = new SwanParameters(swnFile);

        SwanStateFile swanStateFile = new SwanStateFile();
        swanStateFile.initialize(stateFilesTestDir, new String[]{"swantest_sector.HOT", "swantest_sector.swn"});

        String exchangeItemID = swanStateFile.getExchangeItemIDs()[0];
        IExchangeItem swnStateExchItem = swanStateFile.getDataObjectExchangeItem(exchangeItemID);
//        assertEquals("swnStateExchItems.length", 1, swnStateExchItems.length);
        assertEquals("swnStateExchItems[0].id", "swanstate", swnStateExchItem.getId());

        // test getValuesAsDoubles:
        double[] allStateValues = swnStateExchItem.getValuesAsDoubles();
        int nDir = swanParameters.getCDir();
        int nFreq = swanParameters.getRFreq();
        int nGrid = swanParameters.getNMMax();
        int iGrid = 2;
        int iDir = 14;
        int iFreq = 4;
        double factor = 0.23458892E-05;
        int valThisDirThisFreq = 53341;
        int index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(factor*valThisDirThisFreq,allStateValues[index]);
        iGrid = 0;
        iDir = 14;
        iFreq = 0;
        factor = 0.23445664E-05;
        valThisDirThisFreq = 1;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(factor*valThisDirThisFreq,allStateValues[index]);

        // store unmodified state values:
        double[] orgState = new double[allStateValues.length];
        System.arraycopy(allStateValues,0,orgState,0,allStateValues.length);

        // test axpyOnValues:
        double[] axpyVal = new double[nGrid*nFreq*nDir];
        System.arraycopy(allStateValues, 0, axpyVal, 0, axpyVal.length);
        double alpha = 1.5;
        swnStateExchItem.axpyOnValues(alpha, axpyVal);
        allStateValues = swnStateExchItem.getValuesAsDoubles();
        iGrid = 2;
        iDir = 14;
        iFreq = 4;
        factor = 0.23458892E-05;
        valThisDirThisFreq = 53341;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(alpha*axpyVal[index]+factor*valThisDirThisFreq,allStateValues[index],1e-7);
        iGrid = 0;
        iDir = 14;
        iFreq = 0;
        factor = 0.23445664E-05;
        valThisDirThisFreq = 1;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(alpha*axpyVal[index]+factor*valThisDirThisFreq,allStateValues[index],1e-7);

        // test setValuesAsDoubles:
        swnStateExchItem.setValuesAsDoubles(orgState);
        allStateValues = swnStateExchItem.getValuesAsDoubles();
        iGrid = 2;
        iDir = 14;
        iFreq = 4;
        factor = 0.23458892E-05;
        valThisDirThisFreq = 53341;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(factor*valThisDirThisFreq,allStateValues[index],1e-7);
        iGrid = 0;
        iDir = 14;
        iFreq = 0;
        factor = 0.23445664E-05;
        valThisDirThisFreq = 1;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(factor*valThisDirThisFreq,allStateValues[index],1e-7);

        // test multiplyValues:
        alpha=2;
        swnStateExchItem.multiplyValues(new double[]{alpha});
        allStateValues = swnStateExchItem.getValuesAsDoubles();
        iGrid = 2;
        iDir = 14;
        iFreq = 4;
        factor = 0.23458892E-05;
        valThisDirThisFreq = 53341;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(alpha*factor*valThisDirThisFreq,allStateValues[index],1e-8);
        iGrid = 0;
        iDir = 14;
        iFreq = 0;
        factor = 0.23445664E-05;
        valThisDirThisFreq = 1;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(alpha*factor*valThisDirThisFreq,allStateValues[index],1e-7);

        // test write HOT file:
        swanStateFile.finish();

        // reread HOT file:
        SwanStateFile modSwanStateFile = new SwanStateFile();
        modSwanStateFile.initialize(stateFilesTestDir,new String[]{"swantest_sector.HOT","swantest_sector.swn"});
        exchangeItemID = swanStateFile.getExchangeItemIDs()[0];
        IExchangeItem modSwnStateExchItem = modSwanStateFile.getDataObjectExchangeItem(exchangeItemID);
        double[] modAllStateValues = modSwnStateExchItem.getValuesAsDoubles();
        iGrid = 2;
        iDir = 14;
        iFreq = 4;
        factor = 0.23458892E-05;
        valThisDirThisFreq = 53341;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(alpha*factor*valThisDirThisFreq,modAllStateValues[index],1e-7);
        iGrid = 0;
        iDir = 14;
        iFreq = 0;
        factor = 0.23445664E-05;
        valThisDirThisFreq = 1;
        index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals(alpha*factor*valThisDirThisFreq,modAllStateValues[index],1e-7);

    }

    public void testSwanStateFileUnstruct_1() throws IOException {
		File stateFilesTestDir = new File(testData.getTestRunDataDir(), "StateFiles");

        File swnFile = new File(stateFilesTestDir, "swantestunstruct_circle.swn");
        SwanParameters swanParameters = new SwanParameters(swnFile);

        SwanStateFile swanStateFile = new SwanStateFile();
        swanStateFile.initialize(stateFilesTestDir,new String[]{"s08.HOT","swantestunstruct_circle.swn"});

        String exchangeItemID = swanStateFile.getExchangeItemIDs()[0];
        IExchangeItem swnStateExchItem = swanStateFile.getDataObjectExchangeItem(exchangeItemID);

        // test getValuesAsDoubles:
        double[] orgStateValues = swnStateExchItem.getValuesAsDoubles();
        int nDir = swanParameters.getCDir();
        int nFreq = swanParameters.getRFreq();
        int nmmax = swanParameters.getNMMax();
        assertEquals("number of nodes (nmmax): ",5856,nmmax);
        double[] stateVal = swnStateExchItem.getValuesAsDoubles();
        int iGrid = 2;
        int iDir = 0;
        int iFreq = 5;
        int index=iDir+iFreq*nDir+iGrid*nDir*nFreq;
        assertEquals("stateVal: ",0.22405792E-10*3933,stateVal[index]);

        // test multiplyValues:
        double alpha=2;
        swnStateExchItem.multiplyValues(new double[]{alpha});
        stateVal = swnStateExchItem.getValuesAsDoubles();
        assertEquals("stateVal: ",alpha*0.22405792E-10*3933,stateVal[index],1e-6);

        // test axpyOnValues:
        swnStateExchItem.setValuesAsDoubles(orgStateValues);
        swnStateExchItem.axpyOnValues(alpha,orgStateValues);
        stateVal = swnStateExchItem.getValuesAsDoubles();
        assertEquals("stateVal: ",stateVal[index],(alpha+1)*orgStateValues[index],1e-6);

        // test write HOT file:
        swanStateFile.finish();

    }

}