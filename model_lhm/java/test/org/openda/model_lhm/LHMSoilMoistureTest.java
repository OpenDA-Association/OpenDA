/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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

package org.openda.model_lhm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test of exchange item for reading and writing LHM input file.
 */
public class LHMSoilMoistureTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(LHMTimeSettingsTest.class, "model_lhm");
    }

    public void testLhmSoilMoisture(){
        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "LHMSoilMoisture");
        //System.out.println(testRunDataParamFilesDir);
        LHMSoilMoisture lhmSoilMoisture = new LHMSoilMoisture();
        lhmSoilMoisture.initialize(testRunDataParamFilesDir,new String[]{"init_svat.out","init_svat_config.csv","satmoist_mm.csv"});

        IExchangeItem lhmSoilMoistureExchangeItem = lhmSoilMoisture.getExchangeItem(lhmSoilMoisture.getExchangeItemIDs()[0]);

        Object values = lhmSoilMoistureExchangeItem.getValues();
        double soilMoist = Double.NaN;
        if(values instanceof double[]){
            double[] temp = (double[]) values;
            soilMoist = temp[0];
        }else{
            System.out.println("Output is not a double array.");
        }
        double avMoistDefIn = lhmSoilMoisture.getAverage(lhmSoilMoisture.getMoistDef());
        System.out.println("Moisture deficit in init_svat.out: "+avMoistDefIn);
        assertEquals("Existing moisture deficit: ",0.02615074850897391, avMoistDefIn);
        System.out.println("Volumetric soil moisture derived from init_svat.out: "+soilMoist);
        assertEquals("Average existing soilmoisture: ",0.49446761513414855, soilMoist);

        System.out.println("Writing new deficit based on 10% higher volumetric");
        double newSoilMoist = 1.1 * soilMoist;
        double[] outValues = {newSoilMoist};
        lhmSoilMoistureExchangeItem.setValues(outValues);

        File smFile = new File(testRunDataParamFilesDir,"temp_init_svat.inp" );
        File configFile = new File(testRunDataParamFilesDir,"init_svat_config.csv" );
        File satSmFile = new File(testRunDataParamFilesDir,"satmoist_mm.csv" );
        lhmSoilMoisture.ReadFiles(smFile,configFile,satSmFile);

        double avMoistDefOut = lhmSoilMoisture.getAverage(lhmSoilMoisture.getMoistDef());
        Object testValues = lhmSoilMoistureExchangeItem.getValues();
        double testSoilMoist = Double.NaN;
        if(testValues instanceof double[]){
            double[] temp2 = (double[]) testValues;
            testSoilMoist = temp2[0];
        }else{
            System.out.println("Output is not a double array.");
        }
        System.out.println("Moisture deficit read from adapted file: "+avMoistDefOut);
        assertEquals("Average new soil moisture deficit: ",0.007420296651765089,avMoistDefOut);
        System.out.println("Volumetric soil moisture derived from adapted file: "+testSoilMoist);
        assertEquals("Average new volumetric soil moisture: ",String.format("%5.2f",soilMoist*1.1), String.format("%5.2f",testSoilMoist));
    }
}

