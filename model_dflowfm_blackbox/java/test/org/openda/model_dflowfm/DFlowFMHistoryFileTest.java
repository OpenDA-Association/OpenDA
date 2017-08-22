/* OpenDA v2.4.1 
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
package org.openda.model_dflowfm;

import junit.framework.TestCase;

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.NoosDataObject;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Array;
import java.io.File;
import java.io.IOException;

/**
 * Tests for DFlowFM Results DataObject *_his.nc
 */
public class DFlowFMHistoryFileTest extends TestCase {

    private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(DFlowFMRestartTest.class,"model_dflowfm_blackbox");
	}

    public void testWaterlevelHistories() throws Exception {
//		System.out.println("==============================================================================");
//		System.out.println(" Test for reading timeseries from *_his.nc and export to *.noos.");
//		System.out.println("==============================================================================");

        // specify which variable to export
		String Outputvar = "sea_surface_height";
        File inputDir = new File(testData.getTestRunDataDir(), "Historyfile");
		File outputDir = new File(inputDir, "noos_output");
		if(!outputDir.exists()){
			if (!outputDir.mkdir()) {
				throw new RuntimeException("Problem creating directory"+outputDir);
			}

		}else{
			throw new RuntimeException("Output directory already exists. This should not be possible");
		}

        String fileName="simple_waal_his.nc";
		IDataObject hisfile = new NetcdfDataObject();
		hisfile.initialize(inputDir, new String[]{fileName,"true","false"}); //The first option 'true' is crucial to get timeseries!

		String[] exchangeItemIDs = hisfile.getExchangeItemIDs();

//		for (String id : exchangeItemIDs) {
//			// find exchangeItem to export to NOOS format
//			if (id.matches(Outputvar)) {
//				IExchangeItem ex = hisfile.getDataObjectExchangeItem(id);
//				// get time
//				double[] timevals = ex.getTimes();
//				// get values
//				Array vals = (Array) ex.getValues();
//				int[] dims = vals.getDimensions();
//				double[] vec = new double[dims[0]];
//				int idx = 0;
//				while (idx < dims[1]) {
//					// this presumes that values are stored as (time,station) in the his-file
//					for (int i = 0; i < dims[0] ; i++) {
//						vec[i] = vals.getValueAsDouble(idx+i*dims[1]);
//					}
//					// create a timeserie for this slice
//					TimeSeries series = new TimeSeries(timevals,vec);
//					series.setLocation("station" + idx);
//					series.setQuantity(Outputvar);
//					series.setSource("twin experiment DFlowFM");
//					// create NoosDataObject, add the new timeseries to it and write to file.
//					NoosDataObject noos = new NoosDataObject();
//					noos.initialize(outputDir,new String[]{""});
//					noos.addExchangeItem(series);
//					noos.finish();
//
//					idx++;
//				}
//			}
//		}
		IExchangeItem obs1 = hisfile.getDataObjectExchangeItem("Obs01.waterlevel");
		//System.out.println("obs1="+obs1.toString());
		String id1=obs1.getId();
		assertEquals("Obs01.waterlevel", id1);
		double[] times1=obs1.getTimes();
		assertEquals(2881, times1.length);
		assertEquals(48865.0,times1[0],0.001);
		assertEquals(48875.0,times1[2880],0.001);
		double[] values1=obs1.getValuesAsDoubles();
		assertEquals(2881, values1.length);
		assertEquals(0.0,values1[0],0.001);
		assertEquals(1.013,values1[2880],0.001);

		IExchangeItem obs3 = hisfile.getDataObjectExchangeItem("Obs03.waterlevel");
		//System.out.println("obs3="+obs3.toString());
		String id3=obs3.getId();
		assertEquals("Obs03.waterlevel", id3);
		double[] times3=obs3.getTimes();
		assertEquals(2881, times3.length);
		assertEquals(48865.0,times3[0],0.001);
		assertEquals(48875.0,times3[2880],0.001);
		double[] values3=obs3.getValuesAsDoubles();
		assertEquals(2881, values3.length);
		assertEquals(0.0,values3[0],0.001);
		assertEquals(0.999,values3[2880],0.001);
    }
    
    public void testDischargeHistories() throws Exception {
//		System.out.println("==============================================================================");
//		System.out.println(" Test for reading discharge timeseries from *_his.nc.");
//		System.out.println("==============================================================================");

        File inputDir = new File(testData.getTestRunDataDir(), "Historyfile");
        String fileName="river1D_his.nc";
		IDataObject hisfile = new NetcdfDataObject();
		hisfile.initialize(inputDir, new String[]{fileName,"true","false"}); //The first option 'true' is crucial to get timeseries!

		String[] exchangeItemIDs = hisfile.getExchangeItemIDs();
		for(int i=0;i<exchangeItemIDs.length;i++){
			System.out.println("id = "+exchangeItemIDs[i]);
		}
        // check a waterlevel series
		IExchangeItem obs1 = hisfile.getDataObjectExchangeItem("M20.waterlevel");
		System.out.println("obs1="+obs1.toString());
		String id1=obs1.getId();
		assertEquals("M20.waterlevel", id1);
		double[] times1=obs1.getTimes();
		assertEquals(721, times1.length);
		assertEquals(55927.0,times1[0],0.001);
		assertEquals(55932.0,times1[720],0.001);
		double[] values1=obs1.getValuesAsDoubles();
		assertEquals(721, values1.length);
		assertEquals(10.0,values1[0],0.001);
		assertEquals(-2.3180,values1[720],0.001);
        // check a discharge series
		IExchangeItem obs2 = hisfile.getDataObjectExchangeItem("Q-M60.cross_section_discharge");
		System.out.println("obs2="+obs2.toString());
		String id2=obs2.getId();
		assertEquals("M20.waterlevel", id1);
		double[] times2=obs2.getTimes();
		assertEquals(721, times2.length);
		assertEquals(55927.0,times2[0],0.001);
		assertEquals(55932.0,times2[720],0.001);
		double[] values2=obs2.getValuesAsDoubles();
		assertEquals(721, values2.length);
		assertEquals(0.0,values2[0],0.001);
		assertEquals(-251.018,values2[720],0.001);
    }
}
