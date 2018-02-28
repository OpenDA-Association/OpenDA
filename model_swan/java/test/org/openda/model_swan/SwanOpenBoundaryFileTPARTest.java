/* OpenDA v2.4.3 
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

import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Test of exchange item for reading and writing SWAN open boundary TPAR file.
 */
public class SwanOpenBoundaryFileTPARTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanOpenBoundaryFileTPARTest.class, "model_swan");
    }

    public void testSwanOpenBoundaryFile_1() {
        File openBoundaryFilesTestDir = new File(testData.getTestRunDataDir(), "SwanOpenBoundaryFiles");
        SwanOpenBoundaryTPARFile swanOBF = new SwanOpenBoundaryTPARFile();
        swanOBF.initialize(openBoundaryFilesTestDir,new String[]{"INPUT_files/increase_25m_50m.RVW"});
        swanOBF.periodCorrection=false;

        String[] ids = new String[] {"Hs","period","peakdir","dirspread"};

        String[] exchangeItemIDs = swanOBF.getExchangeItemIDs();
        IExchangeItem[] swanOBFExchItem = new IExchangeItem[4];
        for (int i=0; i<swanOBFExchItem.length; i++){
            swanOBFExchItem[i] = swanOBF.getDataObjectExchangeItem(exchangeItemIDs[i]);
        }
        System.out.println("swanOBFExchItem.length: "+swanOBFExchItem.length);
        for (int i=0;i<swanOBFExchItem.length;i++) {
           assertEquals("swanOBFExchItem["+i+"].getId(): ",ids[i],swanOBFExchItem[i].getId());
        }
        double[] Hs = swanOBFExchItem[0].getValuesAsDoubles();
        double[] period = swanOBFExchItem[1].getValuesAsDoubles();
        double[] peakDirection = swanOBFExchItem[2].getValuesAsDoubles();
        double[] directSpread = swanOBFExchItem[3].getValuesAsDoubles();
        double[] times = swanOBFExchItem[0].getTimes();

        // keep original values:
        double[] orgHs = new double[Hs.length];
        double[] orgPeriod = new double[period.length];
        double[] orgPeakDirection = new double[peakDirection.length];
        double[] orgDirectSpread = new double[directSpread.length];
        System.arraycopy(Hs,0,orgHs,0,Hs.length);
        System.arraycopy(period,0,orgPeriod,0,period.length);
        System.arraycopy(peakDirection,0,orgPeakDirection,0,peakDirection.length);
        System.arraycopy(directSpread,0,orgDirectSpread,0,directSpread.length);
        System.out.println("Original...");
        for (int i=0;i<Hs.length;i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs[i]+", T: "+ period[i]+", pDir= "+ peakDirection[i]+", dirSpread:"+ directSpread[i]);
        }

        double c = 1.1;
        for (int i=0;i<Hs.length;i++){
            Hs[i] = c*Hs[i];
            period[i] = c*period[i];
            peakDirection[i] = c*peakDirection[i];
            directSpread[i] = c*directSpread[i];
        }
        swanOBFExchItem[0].setValuesAsDoubles(Hs);
        swanOBFExchItem[1].setValuesAsDoubles(period);
        swanOBFExchItem[2].setValuesAsDoubles(peakDirection);
        swanOBFExchItem[3].setValuesAsDoubles(directSpread);

        double[] alpha0 = new double[]{0.5};
        double[] alpha1 = new double[]{2.0};
        double[] alpha2 = new double[]{1.7};
        double[] alpha3 = new double[]{1.5};
        swanOBFExchItem[0].multiplyValues(alpha0);
        swanOBFExchItem[1].multiplyValues(alpha1);
        swanOBFExchItem[2].multiplyValues(alpha2);
        swanOBFExchItem[3].multiplyValues(alpha3);

        swanOBF.finish();

        // reload and check against original values:
        SwanOpenBoundaryTPARFile swanOBF2 = new SwanOpenBoundaryTPARFile();
        swanOBF2.initialize(openBoundaryFilesTestDir,new String[]{"INPUT_files/increase_25m_50m.RVW","s04.swn"});
        swanOBF2.periodCorrection=false;

        exchangeItemIDs = swanOBF2.getExchangeItemIDs();
        IExchangeItem[] swanOBFExchItem2 = new IExchangeItem[4];
        for (int i=0; i<swanOBFExchItem2.length; i++){
            swanOBFExchItem2[i] = swanOBF2.getDataObjectExchangeItem(exchangeItemIDs[i]);
        }
        double[] Hs2 = swanOBFExchItem2[0].getValuesAsDoubles();
        double[] period2 = swanOBFExchItem2[1].getValuesAsDoubles();
        double[] peakDirection2 = swanOBFExchItem2[2].getValuesAsDoubles();
        double[] directSpread2 = swanOBFExchItem2[3].getValuesAsDoubles();
        System.out.println("After setValues and multiplication...");
        for (int i=0; i<Hs2.length; i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs2[i]+", T: "+ period2[i]+", pDir= "+ peakDirection2[i]+", dirSpread:"+ directSpread2[i]);
            assertEquals("Hs :",c*alpha0[0]*orgHs[i],Hs2[i],1e-2);
            assertEquals("Period :",c*alpha1[0]*orgPeriod[i], period2[i],1e-2);
            assertEquals("PeakDirection :",c*alpha2[0]*orgPeakDirection[i],peakDirection2[i],1e-2);
            assertEquals("DirectSpread :",c*alpha3[0]*orgDirectSpread[i],directSpread2[i],1e-2);
        }


        // test axpy:
        double[] axpyValues = new double[Hs2.length];
        for (int i=0; i<Hs2.length; i++){
            axpyValues[i] = 1.0;
        }
        double beta = 1.0;
        swanOBFExchItem2[0].axpyOnValues(beta,axpyValues);
        swanOBFExchItem2[1].axpyOnValues(beta,axpyValues);
        swanOBFExchItem2[2].axpyOnValues(beta,axpyValues);
        swanOBFExchItem2[3].axpyOnValues(beta,axpyValues);

        System.out.println("After reload and axpyOnValues, but not yet finished...");
        for (int i=0; i<Hs2.length; i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs2[i]+", T: "+ period2[i]+", pDir= "+ peakDirection2[i]+", dirSpread:"+ directSpread2[i]);
        }
        swanOBF2.finish();

        SwanOpenBoundaryTPARFile swanOBF3 = new SwanOpenBoundaryTPARFile();
        swanOBF3.initialize(openBoundaryFilesTestDir,new String[]{"INPUT_files/increase_25m_50m.RVW","s04.swn"});
        exchangeItemIDs = swanOBF3.getExchangeItemIDs();
        IExchangeItem[] swanOBFExchItem3 = new IExchangeItem[4];
        for (int i=0; i<swanOBFExchItem2.length; i++){
            swanOBFExchItem3[i] = swanOBF3.getDataObjectExchangeItem(exchangeItemIDs[i]);
        }
        double[] Hs3 = swanOBFExchItem3[0].getValuesAsDoubles();
        double[] period3 = swanOBFExchItem3[1].getValuesAsDoubles();
        double[] peakDirection3 = swanOBFExchItem3[2].getValuesAsDoubles();
        double[] directSpread3 = swanOBFExchItem3[3].getValuesAsDoubles();
        System.out.println("Final reload...");
        for (int i=0; i<Hs3.length; i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs3[i]+", T: "+ period3[i]+", pDir= "+ peakDirection3[i]+", dirSpread:"+ directSpread3[i]);
            assertEquals("Hs :",beta*axpyValues[i]+Hs2[i],Hs3[i],1e-2);
            assertEquals("Period :",beta*axpyValues[i]+period2[i], period3[i],1e-2);
            assertEquals("PeakDirection :",beta*axpyValues[i]+peakDirection2[i],peakDirection3[i],1e-2);
            assertEquals("DirectSpread :",beta*axpyValues[i]+directSpread2[i],directSpread3[i],1e-2);
        }
        swanOBF3.finish();
    }

    public void testSwanOpenBoundaryFile_2() {
        File openBoundaryFilesTestDir = new File(testData.getTestRunDataDir(), "SwanOpenBoundaryFiles");
        SwanOpenBoundaryTPARFile swanOBF = new SwanOpenBoundaryTPARFile();
//        swanOBF.initialize(openBoundaryFilesTestDir,"swan_rvw_p1.dat",new String[]{"swan_nautboom.swn"});
        swanOBF.initialize(openBoundaryFilesTestDir,new String[]{"swan_rvw_p1.dat"});
        swanOBF.periodCorrection=false;

        String[] ids = new String[] {"Hs","period","peakdir","dirspread"};

        String[] exchangeItemIDs = swanOBF.getExchangeItemIDs();
        IExchangeItem[] swanOBFExchItem = new IExchangeItem[4];
        for (int i=0; i<swanOBFExchItem.length; i++){
            swanOBFExchItem[i] = swanOBF.getDataObjectExchangeItem(exchangeItemIDs[i]);
        }
        System.out.println("swanOBFExchItem.length: "+swanOBFExchItem.length);
        for (int i=0;i<swanOBFExchItem.length;i++) {
           assertEquals("swanOBFExchItem["+i+"].getId(): ",ids[i],swanOBFExchItem[i].getId());
        }
        double[] Hs = swanOBFExchItem[0].getValuesAsDoubles();
        double[] period = swanOBFExchItem[1].getValuesAsDoubles();
        double[] peakDirection = swanOBFExchItem[2].getValuesAsDoubles();
        double[] directSpread = swanOBFExchItem[3].getValuesAsDoubles();
        double[] times = swanOBFExchItem[0].getTimes();

        // keep original values:
        double[] orgHs = new double[Hs.length];
        double[] orgPeriod = new double[period.length];
        double[] orgPeakDirection = new double[peakDirection.length];
        double[] orgDirectSpread = new double[directSpread.length];
        System.arraycopy(Hs,0,orgHs,0,Hs.length);
        System.arraycopy(period,0,orgPeriod,0,period.length);
        System.arraycopy(peakDirection,0,orgPeakDirection,0,peakDirection.length);
        System.arraycopy(directSpread,0,orgDirectSpread,0,directSpread.length);
        System.out.println("Original...");
        for (int i=0;i<Hs.length;i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs[i]+", T: "+ period[i]+", pDir= "+ peakDirection[i]+", dirSpread:"+ directSpread[i]);
        }

        double c = 3.4;
        for (int i=0;i<Hs.length;i++){
            Hs[i] = c*Hs[i];
            period[i] = c*period[i];
            peakDirection[i] = c*peakDirection[i];
            directSpread[i] = c*directSpread[i];
        }
        swanOBFExchItem[0].setValuesAsDoubles(Hs);
        swanOBFExchItem[1].setValuesAsDoubles(period);
        swanOBFExchItem[2].setValuesAsDoubles(peakDirection);
        swanOBFExchItem[3].setValuesAsDoubles(directSpread);

        double[] alpha0 = new double[]{0.5};
        double[] alpha1 = new double[]{2.0};
        double[] alpha2 = new double[]{1.7};
        double[] alpha3 = new double[]{1.5};
        swanOBFExchItem[0].multiplyValues(alpha0);
        swanOBFExchItem[1].multiplyValues(alpha1);
        swanOBFExchItem[2].multiplyValues(alpha2);
        swanOBFExchItem[3].multiplyValues(alpha3);

        swanOBF.finish();

        // reload and check against original values:
        SwanOpenBoundaryTPARFile swanOBF2 = new SwanOpenBoundaryTPARFile();
        swanOBF2.initialize(openBoundaryFilesTestDir,new String[]{"swan_rvw_p1.dat","swan_nautboom.swn"});
        swanOBF2.periodCorrection=false;

        exchangeItemIDs = swanOBF2.getExchangeItemIDs();
        IExchangeItem[] swanOBFExchItem2 = new IExchangeItem[4];
        for (int i=0; i<swanOBFExchItem.length; i++){
            swanOBFExchItem2[i] = swanOBF2.getDataObjectExchangeItem(exchangeItemIDs[i]);
        }

        double[] Hs2 = swanOBFExchItem2[0].getValuesAsDoubles();
        double[] period2 = swanOBFExchItem2[1].getValuesAsDoubles();
        double[] peakDirection2 = swanOBFExchItem2[2].getValuesAsDoubles();
        double[] directSpread2 = swanOBFExchItem2[3].getValuesAsDoubles();
        System.out.println("After setValues and multiplication...");
        for (int i=0; i<Hs2.length; i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs2[i]+", T: "+ period2[i]+", pDir= "+ peakDirection2[i]+", dirSpread:"+ directSpread2[i]);
            assertEquals("Hs :",c*alpha0[0]*orgHs[i],Hs2[i],1e-2);
            assertEquals("Period :",c*alpha1[0]*orgPeriod[i], period2[i],1e-2);
            assertEquals("PeakDirection :",c*alpha2[0]*orgPeakDirection[i],peakDirection2[i],1e-2);
            assertEquals("DirectSpread :",c*alpha3[0]*orgDirectSpread[i],directSpread2[i],1e-2);
        }


        // test axpy:
        double[] axpyValues = new double[Hs2.length];
        for (int i=0; i<Hs2.length; i++){
            axpyValues[i] = 1.0;
        }
        double beta = 1.3;
        swanOBFExchItem2[0].axpyOnValues(beta,axpyValues);
        swanOBFExchItem2[1].axpyOnValues(beta,axpyValues);
        swanOBFExchItem2[2].axpyOnValues(beta,axpyValues);
        swanOBFExchItem2[3].axpyOnValues(beta,axpyValues);

        System.out.println("After reload and axpyOnValues, but not yet finished...");
        for (int i=0; i<Hs2.length; i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs2[i]+", T: "+ period2[i]+", pDir= "+ peakDirection2[i]+", dirSpread:"+ directSpread2[i]);
        }
        swanOBF2.finish();

        SwanOpenBoundaryTPARFile swanOBF3 = new SwanOpenBoundaryTPARFile();
        swanOBF3.initialize(openBoundaryFilesTestDir,new String[]{"swan_rvw_p1.dat","swan_nautboom.swn"});
        exchangeItemIDs = swanOBF3.getExchangeItemIDs();
        IExchangeItem[] swanOBFExchItem3 = new IExchangeItem[4];
        for (int i=0; i<swanOBFExchItem.length; i++){
            swanOBFExchItem3[i] = swanOBF3.getDataObjectExchangeItem(exchangeItemIDs[i]);
        }
        double[] Hs3 = swanOBFExchItem3[0].getValuesAsDoubles();
        double[] period3 = swanOBFExchItem3[1].getValuesAsDoubles();
        double[] peakDirection3 = swanOBFExchItem3[2].getValuesAsDoubles();
        double[] directSpread3 = swanOBFExchItem3[3].getValuesAsDoubles();
        System.out.println("Final reload...");
        for (int i=0; i<Hs3.length; i++){
            System.out.println("For i="+i+", time: "+times[i]+", Hs: "+ Hs3[i]+", T: "+ period3[i]+", pDir= "+ peakDirection3[i]+", dirSpread:"+ directSpread3[i]);
            assertEquals("Hs :",beta*axpyValues[i]+Hs2[i],Hs3[i],1e-2);
            assertEquals("Period :",beta*axpyValues[i]+period2[i], period3[i],1e-2);
            assertEquals("PeakDirection :",beta*axpyValues[i]+peakDirection2[i],peakDirection3[i],1e-2);
            assertEquals("DirectSpread :",beta*axpyValues[i]+directSpread2[i],directSpread3[i],1e-2);
        }
        swanOBF3.finish();
    }
    
    public void testSwanOpenBoundaryFile_3() {
        File openBoundaryFilesTestDir = new File(testData.getTestRunDataDir(), "SwanOpenBoundaryFiles");
        SwanOpenBoundaryTPARFile swanOBF = new SwanOpenBoundaryTPARFile();
        swanOBF.initialize(openBoundaryFilesTestDir,new String[]{"period_correction.RVW"});

        String[] ids = swanOBF.getExchangeItemIDs();
        assertEquals(4, ids.length);
        System.out.println("id[0]="+ids[0]);
        System.out.println("id[1]="+ids[1]);
        System.out.println("id[2]="+ids[2]);
        System.out.println("id[3]="+ids[3]);
        
        IExchangeItem Hs_item=swanOBF.getDataObjectExchangeItem("Hs");
        IExchangeItem period_item=swanOBF.getDataObjectExchangeItem("period");
        double[] Hs_original=Hs_item.getValuesAsDoubles();
        double[] period_original=period_item.getValuesAsDoubles();
        int n=Hs_original.length;
        System.out.println("Hs_old="+new Vector(Hs_original));
        System.out.println("Tp_old="+new Vector(period_original));
        
        double[] Hs_changes=new double[n];
        for(int i=0;i<n;i++){Hs_changes[i]=1.0;}
        Hs_item.axpyOnValues(1.0, Hs_changes);
        
        swanOBF.finish();
        
        //check changes to file
        swanOBF.initialize(openBoundaryFilesTestDir,new String[]{"period_correction.RVW"});
        Hs_item=swanOBF.getDataObjectExchangeItem("Hs");
        period_item=swanOBF.getDataObjectExchangeItem("period");
        double[] Hs_new=Hs_item.getValuesAsDoubles();
        double[] period_new=period_item.getValuesAsDoubles();
        System.out.println("Hs_new="+new Vector(Hs_new));
        System.out.println("Tp_new="+new Vector(period_new));
        
        assertTrue(testData.FilesAreIdentical(new File(openBoundaryFilesTestDir,"period_correction.RVW"), new File(openBoundaryFilesTestDir,"period_correction.RVW.ref")));

    }
    
}
