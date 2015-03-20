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
package org.openda.utils;
import junit.framework.TestCase;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Test for Statistics class, which computes certain statistics of a residual timeseries.
 */
public class StatisticsTest extends TestCase {
    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(StatisticsTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testStatisticsOscill_Grouped(){
        IStochObserver obs1 = new CsvStochObserver();
        obs1.initialize(testRunDataDir,new String[]{"observations_oscillator_generated_2.csv"});
        IVector allValues = obs1.getExpectations();
        IObservationDescriptions descr = obs1.getObservationDescriptions();

        List<IPrevExchangeItem> items = descr.getExchangeItems();
        double[] thisBias = new double[items.size()];
        double[] thisSTD = new double[items.size()];
        double[] thisRMS = new double[items.size()];
        int indFirst = 0;
        int indLast = 0;
        int j=0;
        for (IPrevExchangeItem item : items){
            String id = item.getId();
            int n = 1;
            double times[] = item.getTimes();
            if (times != null) {
                n = times.length;
            }
            indFirst = indLast;
            indLast = indFirst + n;

            int nVal = indLast - indFirst;
            double bias=0.0;
            double rms=0.0;
            for (int i=indFirst; i<indLast; i++){
                bias += 1/(double)nVal * allValues.getValue(i);
                rms += 1/(double)nVal * allValues.getValue(i) * allValues.getValue(i);
//                System.out.println("values["+i+"]: "+allValues.getValue(i));
            }
            rms = Math.pow(rms,0.5);
            double std = 0.0;
            for (int i=indFirst; i<indLast; i++){
                std += 1 / (double) (nVal - 1) * (allValues.getValue(i) - bias) * (allValues.getValue(i) - bias);
            }
            std = Math.pow(std,0.5);
            thisBias[j]=bias;
            thisSTD[j]=std;
            thisRMS[j]=rms;
            System.out.println("bias: "+bias+", std: "+std+", rms: "+rms);
            j++;
        }
        Statistics stat1 = new Statistics(allValues,descr);
        System.out.println(stat1.toString());
        for (int k=0; k<items.size(); k++){
            assertEquals("bias: ",thisBias[k],stat1.asTreeVector(stat1.getBias()).getValue(k));
            assertEquals("STD: ",thisSTD[k],stat1.asTreeVector(stat1.getSTD()).getValue(k),1E-15);
            assertEquals("RMS: ",thisRMS[k],stat1.asTreeVector(stat1.getRMS()).getValue(k),1E-15);
        }
    }
}
