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
package org.openda.model_gotm;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IDataObject;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;

public class GotmNetcdfFileTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(GotmNetcdfFileTest.class,"model_gotm");
        testRunDataDir = testData.getTestRunDataDir();
    }
    
    public void testDummy(){
    	

    /**
     * test the netcdf file for waterlevel station data
     */
//    public void testSeriesWaterlevel1() {
//
//        IDataObject ioObject = new GotmNetcdfFile();
//        String args[] = {};
//        ioObject initialize(testRunDataDir, "SimonaNetcdfFile/waterlevelseries.nc", args);
//
//        TimeSeries[] exchangeItems = ioObject.getExchangeItems();
//        int n = exchangeItems.length;
//        assertEquals("exchangeItems.length", 275, exchangeItems.length);
//
//        TimeSeries ex = exchangeItems[0];
//        //String getId();
//        String id = ex.getId();
//        assertEquals("ex.getId()", "Shetland-East.waterlevel", id);
//
//        //String getQuantityId();
//        String quantityId = ((TimeSeries) ex).getQuantityId();
//        assertEquals("ex.getQuantityId()", "waterlevel", quantityId);
//
//        //String getUnitId();
//        String unitId = ((TimeSeries) ex).getUnitId();
//        assertEquals("ex.getUnitId()", "m", unitId);
//
//        //public Type getObjectType(); //
//        Type valueType = ex.getValueType();
//        assertTrue(valueType == org.openda.utils.Array.class);
//
//        //public Object getValues();
//        Array valuesArray = (Array) ex.getValues();
//        double[] valuesFromArray = valuesArray.getValuesAsDoubles();
//        assertEquals("values[1]", 0.6419042348861694, valuesFromArray[1], 0.0001);
//
//        //public double[] getValuesAsDoubles();
//        double[] valuesCopy = ex.getValuesAsDoubles();
//        assertEquals("values[1]", 0.6419042348861694, valuesCopy[1], 0.0001);
//
//    }

    /**
     * test the netcdf file for the variable SEP containing waterlevel field data
     */
//    public void tstMapsWaterlevel() {
//        IoObjectInterface ioObject = new SimonaNetcdfFile();
//        String args[] = {};
//        File inputFile = new File(testRunDataDir, "SimonaNetcdfFile/waqua.nc");
//        ioObject.initialize(testRunDataDir, "SimonaNetcdfFile/waqua.nc", args);
//
//        IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();
//        int n = exchangeItems.length;
//        assertEquals("exchangeItems.length", 32, n);
//
//        IPrevExchangeItem lastExchangeItem = exchangeItems[n-1];
//        assertEquals("last exchangeItems id", "SEP", lastExchangeItem.getId());

//		TODO (SV): Fix test. Test gives error
//		junit.framework.ComparisonFailure:netcdf output file
//		Expected: D:\src\openda_1\deltares\..\public\opendaTestRuns\core\org\openda\blackbox\io\SimonaNetcdfFile\waqua.nc
// 		Actual: D:\src\openda_1\public\opendaTestRuns\core\org\openda\blackbox\io\SimonaNetcdfFile\waqua.nc
//        Object values = lastExchangeItem.getValues();
//        String outputFile = "";
//        if (values instanceof NetcdfFile) {
//            NetcdfFile ncFile = (NetcdfFile) values;
//            outputFile = ncFile.getLocation();
//        }
//        outputFile = outputFile.replaceFirst("trunk", "public");
//        assertEquals("netcdf output file",inputFile.getAbsolutePath(), outputFile);
	}

}
