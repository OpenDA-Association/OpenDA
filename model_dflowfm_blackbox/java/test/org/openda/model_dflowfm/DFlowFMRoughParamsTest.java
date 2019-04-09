/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association
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
package org.openda.model_dflowfm;

import junit.framework.TestCase;

import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Started with a copy from from model_delft3d
 */
public class DFlowFMRoughParamsTest  extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(DFlowFMRoughParamsTest.class,"model_dflowfm_blackbox");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testExistingfile(){

        // First write a test file
    	File dataDir=testRunDataDir;
        DFlowFMTrachytopeFile roughFile=new DFlowFMTrachytopeFile();
        roughFile.initialize(testRunDataDir,new String[]{"ruw.ttd"});

        System.out.println(roughFile.toString());
        
        String[] ids = roughFile.getExchangeItemIDs();
        int n=ids.length;
        double delta=0.00001;
        assertEquals(10, n);
        for(int i=0;i<n;i++){
        	String id=ids[i];
        	IExchangeItem item=roughFile.getDataObjectExchangeItem(id);
        	double[] values =item.getValuesAsDoubles();
        	assertEquals(1, values.length);
        	System.out.println("Id="+id+" value="+values[0]);

        	String code=id.substring(id.length()-1);
        	if(code.matches("A")){
        		assertEquals(0.1, values[0], delta);
        	}else{
        		assertEquals(2.5, values[0], delta);
        	}
        }

        // change some parameter
        String firstId="RoughNr_411_FormulaNr101_A";
        DFlowFMRougnessFileExchangeItem item= (DFlowFMRougnessFileExchangeItem)roughFile.getDataObjectExchangeItem(firstId);
        //items[0].setValuesAsDoubles(new double[]{100.});
        item.setValue(100.);
        String secondId="RoughNr_411_FormulaNr101_B";
        item=(DFlowFMRougnessFileExchangeItem)roughFile.getDataObjectExchangeItem(secondId);
        //items[1].setValuesAsDoubles(new double[]{4.5});
        item.setValue(4.5);
        // flush to file
        roughFile.finish();

        // check output
        File outputFile=new File(dataDir,"ruw.ttd");
        File referenceFile=new File(dataDir,"ruw.ttd.ref");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());

        assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

    }

    public void testExistingfile_2(){

        // First write a test file
    	File dataDir=testRunDataDir;
        DFlowFMTrachytopeFile roughFile=new DFlowFMTrachytopeFile();
        roughFile.initialize(testRunDataDir,new String[]{"ruwQ7020.ttd"});

        System.out.println(roughFile.toString());
        
        String[] ids = roughFile.getExchangeItemIDs();
        int n=ids.length;

        double delta=0.00001;
        assertEquals(137, n);
        for(int i=0;i<n;i++){
        	String id=ids[i];
        	DFlowFMRougnessFileExchangeItem item = (DFlowFMRougnessFileExchangeItem)roughFile.getDataObjectExchangeItem(id);
        	assertEquals(id, item.getId());
        	double value =item.getValue();
        	System.out.println("Id="+id+" value="+value);
        }

        // change some parameter
        String id13="RoughNr_21_FormulaNr51_A";
    	DFlowFMRougnessFileExchangeItem item13 = (DFlowFMRougnessFileExchangeItem)roughFile.getDataObjectExchangeItem(id13);
        item13.setValue(100.);
        // flush to file
        roughFile.finish();

        // check output
        File outputFile=new File(dataDir,"ruwQ7020.ttd");
        File referenceFile=new File(dataDir,"ruwQ7020.ttd.ref");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());

        assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

    }



    public void testDischargeDependentFile(){

        // First open existing test file
    	File dataDir=testRunDataDir;
        DFlowFMTrachytopeFile roughFile=new DFlowFMTrachytopeFile();
        roughFile.initialize(dataDir,new String[]{"ruw_disch.ttd"});

        
        System.out.println(roughFile.toString());
        
        String[] ids = roughFile.getExchangeItemIDs();
        int n=ids.length;
        assertEquals(240, n);

        String id1="RoughNr_612_DISCHARGE2700_FormulaNr101_A"; 
        double value1_check=0.0229;
        DFlowFMRougnessFileExchangeItem item1 = (DFlowFMRougnessFileExchangeItem)roughFile.getDataObjectExchangeItem(id1);
        double value1=item1.getValue();
        double delta=1e-6;
        assertEquals(value1_check, value1, delta);
        item1.setValue(2700.029);

        String id2="RoughNr_612_DISCHARGE4450_FormulaNr101_A"; 
        double value2_check=0.0262;
        DFlowFMRougnessFileExchangeItem item2 = (DFlowFMRougnessFileExchangeItem)roughFile.getDataObjectExchangeItem(id2);
        double value2=item2.getValue();
        assertEquals(value2_check, value2, delta);
        item2.setValue(4450.0262);
        //flush to file
        roughFile.finish();
        
        // check output
        File outputFile=new File(dataDir,"ruw_disch.ttd");
        File referenceFile=new File(dataDir,"ruw_disch.ttd.ref");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());

        assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

}

}
