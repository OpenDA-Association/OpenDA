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

import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test for putting parameters in the swan input file
 */
public class SwanSpectralFileTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanSpectralFileTest.class, "model_swan");
    }

    public void testSwanSpectralFile_1a() throws IOException {
		File spectralFilesTestDir = new File(testData.getTestRunDataDir(), "StateFiles");
        File spectralFile = new File(spectralFilesTestDir,"s08.HOT");

        SwanSpectralFile spcFile = new SwanSpectralFile(spectralFile);
        int nLocations = spcFile.getNLocations();
        int nFreq = spcFile.getNFreq();
        int nDir = spcFile.getNDir();
        int iTime = 0;
        String quantity = spcFile.getQuantity();
        String unit = spcFile.getUnit();
        double exceptionValues = spcFile.getExceptionValue();
        String[] times = spcFile.getTimesStr();

        assertEquals("isStationary: ",true,spcFile.isStationary());
        assertEquals("nLocations: ",5856,nLocations);
        assertEquals("nFreq: ",31,nFreq);
        assertEquals("nDir: ",36,nDir);
        assertEquals("quantity: ","AcDens",quantity);
        assertEquals("unit: ","m2s/Hz/deg",unit);
        assertEquals("exceptionValues : ",0.0,exceptionValues);
        assertEquals("times : ",null,times);

        int iGrid = 2;
        double factor = 0.22405792E-10;
        int iFreq = 3;
        int iDir = 0;
        int index = iGrid*(nDir*nFreq)+iFreq*nDir+iDir;
        double[] dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",3*factor,dblValues[index]);

        factor=0.11535097E-05;
        iGrid=4;
        iFreq=6;
        iDir=21;
        index = iGrid*(nDir*nFreq)+iFreq*nDir+iDir;
        assertEquals("dblValues: ",537*factor,dblValues[index]);

        // keep original values:
        SwanSpectralFile spcFileOrg = new SwanSpectralFile(spectralFile);

        double gamma = 2.0;
        for (int i=0; i<dblValues.length; i++){
            dblValues[i] = gamma*dblValues[i];
        }
        spcFile.setSpectralValuesAsDouble(0,dblValues);
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",gamma*537*factor,dblValues[index],1e-6);

        double[] alpha = new double[1];
        alpha[0] = 0.8;
        spcFile.multiplySpectralValues(0,alpha);
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",alpha[0]*gamma*537*factor,dblValues[index],1e-6);

        double beta = 2.0;
        spcFile.axpyOnSpectralValues(0,beta,dblValues);
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",(beta+1.0)*alpha[0]*gamma*537*factor,dblValues[index],1e-6);

        // write spectral file:
        spcFile.writeSpectralFile();

        // reread spectral file:
        SwanSpectralFile spcFile2 = new SwanSpectralFile(spectralFile);
        double[] dblValues2;
        dblValues2 = spcFile2.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",dblValues2[index],dblValues[index],1e-6);
        System.out.println("dblValues old: "+dblValues[index]+", dblValues new: "+dblValues2[index]);
        double[] dblValuesOrg;
        dblValuesOrg = spcFileOrg.getSpectralValuesAsDouble(iTime);
        System.out.println("dblValues org: "+dblValuesOrg[index]+", dblValues new: "+dblValues2[index]);

    }

    
    public void testSwanSpectralFile_1b() throws IOException {
    	System.out.println("===========================================");
    	System.out.println("Hot file with ZERO cells");
    	System.out.println("===========================================");
		File spectralFilesTestDir = new File(testData.getTestRunDataDir(), "StateFiles");
        File spectralFile = new File(spectralFilesTestDir,"restart_with_zero.hot");
        
        SwanSpectralFile spcFile = new SwanSpectralFile(spectralFile);
        int nLocations = spcFile.getNLocations();
        int nFreq = spcFile.getNFreq();
        int nDir = spcFile.getNDir();
        int iTime = 0;
        String quantity = spcFile.getQuantity();
        String unit = spcFile.getUnit();
        double exceptionValues = spcFile.getExceptionValue();
        String[] times = spcFile.getTimesStr();

        assertEquals("isStationary: ",false,spcFile.isStationary());
        assertEquals("nLocations: ",567,nLocations);
        assertEquals("nFreq: ",32,nFreq);
        assertEquals("nDir: ",36,nDir);
        assertEquals("quantity: ","AcDens",quantity);
        assertEquals("unit: ","m2s/Hz/deg",unit);
        assertEquals("exceptionValues : ",0.0,exceptionValues);
        assertEquals("times : ",null,times);

        // write spectral file:
        spcFile.writeSpectralFile();

    }

    public void testSwanSpectralFile_2() throws IOException {
		File spectralFilesTestDir = new File(testData.getTestRunDataDir(), "SwanOpenBoundaryFiles/INPUT_files");
        File spectralFile = new File(spectralFilesTestDir,"wam_adapted.bnd");

        SwanSpectralFile spcFile = new SwanSpectralFile(spectralFile);
        int nLocations = spcFile.getNLocations();
        int nFreq = spcFile.getNFreq();
        int nDir = spcFile.getNDir();
System.out.println("nLoc: "+nLocations+", nFreq: "+nFreq+", nDir: "+nDir);
        String quantity = spcFile.getQuantity();
        String unit = spcFile.getUnit();
        double exceptionValues = spcFile.getExceptionValue();
        String[] timesStr = new String[spcFile.getNTimes()];
        double[] timesDbl = new double[spcFile.getNTimes()];

        assertEquals("isStationary: ",false,spcFile.isStationary());
        assertEquals("nLocations: ",4,nLocations);
        assertEquals("nFreq: ",30,nFreq);
        assertEquals("nDir: ",24,nDir);
        assertEquals("quantity: ","VaDens",quantity);
        assertEquals("unit: ","m2/Hz/degr",unit);
        assertEquals("exceptionValues : ",9999.0,exceptionValues);
        timesStr = spcFile.getTimesStr();
        assertEquals("dblValues: ","20100101.000000",timesStr[0]);
        assertEquals("dblValues: ","20100101.060000",timesStr[1]);
        assertEquals("dblValues: ","20100101.120000",timesStr[2]);
        assertEquals("dblValues: ","20100101.180000",timesStr[3]);
        assertEquals("dblValues: ","20100102.000000",timesStr[4]);
        assertEquals("dblValues: ","20100102.060000",timesStr[5]);
        assertEquals("dblValues: ","20100102.120000",timesStr[6]);
        assertEquals("dblValues: ","20100103.000000",timesStr[7]);

        timesDbl = spcFile.getTimesDbl();
        for (int i=0; i<timesDbl.length; i++){
            System.out.println("timesDbl["+i+"]= "+timesDbl[i]);
        }

        int iGrid = 0;
        double factor = 1e-06;
        int iFreq = 4;
        int iDir = 0;
        int index = iGrid*(nDir*nFreq)+iFreq*nDir+iDir;
        int iTime = 0;
        double[] dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",69*factor,dblValues[index]);

        iTime = 5;
        factor = 1e-06;
        iFreq = 3;
        iDir = 23;
        index = iGrid*(nDir*nFreq)+iFreq*nDir+iDir;
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",33*factor,dblValues[index]);

        // keep original values:
        SwanSpectralFile spcFileOrg = new SwanSpectralFile(spectralFile);

        double gamma = 2.0;
        for (int i=0; i<dblValues.length; i++){
            dblValues[i] = gamma*dblValues[i];
        }
        spcFile.setSpectralValuesAsDouble(iTime,dblValues);
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",gamma*33*factor,dblValues[index],1e-5);

        double[] alpha = new double[1];
        alpha[0] = 0.5;
        spcFile.multiplySpectralValues(iTime,alpha);
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",alpha[0]*gamma*33*factor,dblValues[index],1e-5);

        double beta = 1.1;
        spcFile.axpyOnSpectralValues(iTime,beta,dblValues);
        dblValues = spcFile.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",(beta+1.0)*alpha[0]*gamma*33*factor,dblValues[index],1e-5);

        // write spectral file:
        spcFile.writeSpectralFile();

        // reread spectral file:
        SwanSpectralFile spcFile2 = new SwanSpectralFile(spectralFile);
        double[] dblValues2;
        dblValues2 = spcFile2.getSpectralValuesAsDouble(iTime);
        assertEquals("dblValues: ",dblValues2[index],dblValues[index],1e-6);
        System.out.println("dblValues old: "+dblValues[index]+", dblValues new: "+dblValues2[index]);
        double[] dblValuesOrg;
        dblValuesOrg = spcFileOrg.getSpectralValuesAsDouble(iTime);
        System.out.println("dblValues org: "+dblValuesOrg[index]+", dblValues new: "+dblValues2[index]);

    }

}