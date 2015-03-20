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

/**
 * Test of exchange item for reading and writing SWAN open boundary file in spectral format.
 */
public class SwanOpenBoundarySpectralFileTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanOpenBoundarySpectralFileTest.class, "model_swan");
    }

    public void testSwanOpenBoundarySpectralFile_1() {
        File openBoundaryFilesTestDir = new File(testData.getTestRunDataDir(), "SwanOpenBoundaryFiles");
        File swnFile = new File(openBoundaryFilesTestDir, "s12.swn");

        SwanParameters swanParam = new SwanParameters(swnFile);
        String openBoundaryFile = swanParam.getFileNameOpenBoundary();
        File obFile = new File(openBoundaryFilesTestDir.getPath(),"INPUT_files");

        SwanOpenBoundarySpectralFile obSpectral = new SwanOpenBoundarySpectralFile();
        obSpectral.initialize(obFile, new String[]{"wam_adapted.bnd"});

        String exchangeItemId = obSpectral.getExchangeItemIDs()[0];
        IExchangeItem swnOBExchItems = obSpectral.getDataObjectExchangeItem(exchangeItemId);
//        assertEquals("swnOBExchItems.length", 1, swnOBExchItems.length);
        assertEquals("swnOBExchItems.id", "openboundary", swnOBExchItems.getId());

        int iGrid = 0;
        double factor = 1e-06;
        int iFreq = 4;
        int iDir = 0;
        int nLoc= 4;
        int nFreq= 30;
        int nDir= 24;
        int index = iGrid*(nDir*nFreq)+iFreq*nDir+iDir;
        int iTime = 0;
        ((SwanOpenBoundarySpectralFileExchangeItem)swnOBExchItems).resetTimeCounter();
        double[] dblValues = new double[nLoc*nFreq*nDir];
        for (int i=0;i<=iTime;i++){
            dblValues = swnOBExchItems.getValuesAsDoubles();
        }
        assertEquals("dblValues: ",69*factor,dblValues[index]);

        iTime = 5;
        factor = 1e-06;
        iFreq = 3;
        iDir = 23;
        index = iGrid*(nDir*nFreq)+iFreq*nDir+iDir;
        ((SwanOpenBoundarySpectralFileExchangeItem)swnOBExchItems).resetTimeCounter();
        for (int i=0;i<=iTime;i++){
           dblValues = swnOBExchItems.getValuesAsDoubles();
        }
        assertEquals("dblValues: ",33*factor,dblValues[index]);

        // keep original values:
        SwanOpenBoundarySpectralFile obSpectralOrg = new SwanOpenBoundarySpectralFile();
        obSpectralOrg.initialize(obFile, new String[]{"wam_adapted.bnd"});
        IExchangeItem swnOBExchItemsOrg = obSpectralOrg.getDataObjectExchangeItem(obSpectralOrg.getExchangeItemIDs()[0]);

//        double gamma = 2.0;
        double gamma = 1.0;
        for (int i=0; i<dblValues.length; i++){
            dblValues[i] = gamma*dblValues[i];
        }
        swnOBExchItems.setValuesAsDoubles(dblValues);
        ((SwanOpenBoundarySpectralFileExchangeItem)swnOBExchItems).resetTimeCounter();
        for (int i=0;i<=iTime;i++){
           dblValues = swnOBExchItems.getValuesAsDoubles();
        }
        assertEquals("dblValues: ",gamma*33*factor,dblValues[index],1e-5);

        obSpectral.finish();
    }
}
