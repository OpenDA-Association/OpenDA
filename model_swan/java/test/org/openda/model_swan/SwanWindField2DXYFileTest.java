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

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IExchangeItem.ValueType;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.Array;
import org.openda.utils.IMyObservable;
import org.openda.utils.IMyObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Test of reading and writing wind vector data file of SWAN.
 */
public class SwanWindField2DXYFileTest extends TestCase{

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanWindField2DXYFileTest.class, "model_swan");
    }
    
    public void test_read() throws IOException {
    	// swantest_sector.swn :
    	//INPGRID WIND REG 0. 0. 0 1 1 2000 2000 NONSTAT 20081121.0000 12 HR 20081125.0000 
    	//READINP WIND 1.0 'rotating.WND' 1 1 1 1 FREE
    	// rotating.WND :
    	// wave modelling - wind
    	//20081121.0000   
    	//X-comp 
    	//    0.00     0.00 
    	//    0.00     0.00 
    	//Y-comp 
    	//  -15.00   -15.00 
    	//  -15.00   -15.00 
    	//20081121.1200   
    	//X-comp 
    	//  -10.61   -10.61 
    	//  -10.61   -10.61 
    	//Y-comp 
    	//  -10.61   -10.61 
    	//  -10.61   -10.61 
        File windFilesTestDir = new File(testData.getTestRunDataDir(), "SwanWindFiles");

        SwanField2DXYFile swanWindFile = new SwanField2DXYFile();
        swanWindFile.initialize(windFilesTestDir,new String[] {"swantest_sector.swn","WIND"});

        String[] Ids = swanWindFile.getExchangeItemIDs();
        assertEquals("number of exchangeItems",2,Ids.length);
        assertTrue("look for wind.x",Arrays.asList(Ids).contains("wind.x"));
        assertTrue("look for wind.y",Arrays.asList(Ids).contains("wind.y"));
        
        // get windX
        IExchangeItem windX = swanWindFile.getDataObjectExchangeItem("wind.x");
        System.out.println("windX="+windX.toString());
        
        //check windX 
        String windXId = windX.getId();
        assertEquals("wind.x",windXId);
        ITimeInfo xTimeInfo = windX.getTimeInfo();
        double times[] = xTimeInfo.getTimes();
        assertEquals("times.length",9,times.length);
        assertEquals("times[0]",54791.,times[0]);
        assertEquals("times[1]",54791.5,times[1]);
        assertEquals("times[end]",54795.,times[times.length-1]);
        
        IGeometryInfo xGeometryInfo = windX.getGeometryInfo();
        if(xGeometryInfo instanceof ArrayGeometryInfo){
        	IArray lat = ((ArrayGeometryInfo)xGeometryInfo).getLatitudeArray();
        	assertEquals("{0.0,2000.0}", lat.toString());
        	IArray lon = ((ArrayGeometryInfo)xGeometryInfo).getLongitudeArray();
        	assertEquals("{0.0,2000.0}", lon.toString());
        }
        
        String windXQuantity = windX.getQuantityInfo().getQuantity();
        assertEquals("x_wind", windXQuantity);
        String windXUnit = windX.getQuantityInfo().getUnit();
        assertEquals("m/s", windXUnit);
        
        //get windY
        IExchangeItem windY = swanWindFile.getDataObjectExchangeItem("wind.y");        
        System.out.println("windY="+windY.toString());

        //check windY 
        String windYId = windY.getId();
        assertEquals("wind.y",windYId);
        ITimeInfo yTimeInfo = windY.getTimeInfo();
        double ytimes[] = yTimeInfo.getTimes();
        assertEquals("times.length",9,ytimes.length);
        assertEquals("times[0]",54791.,ytimes[0]);
        assertEquals("times[1]",54791.5,ytimes[1]);
        assertEquals("times[end]",54795.,ytimes[ytimes.length-1]);
        
        IGeometryInfo yGeometryInfo = windY.getGeometryInfo();
        if(xGeometryInfo instanceof ArrayGeometryInfo){
        	IArray lat = ((ArrayGeometryInfo)yGeometryInfo).getLatitudeArray();
        	assertEquals("{0.0,2000.0}", lat.toString());
        	IArray lon = ((ArrayGeometryInfo)yGeometryInfo).getLongitudeArray();
        	assertEquals("{0.0,2000.0}", lon.toString());
        }
        
        String windYQuantity = windY.getQuantityInfo().getQuantity();
        assertEquals("y_wind", windYQuantity);
        String windYUnit = windY.getQuantityInfo().getUnit();
        assertEquals("m/s", windYUnit);

        swanWindFile.finish();
    }

    public void test_read_series() throws IOException {
    	// swantest_sector.swn :
    	//INPGRID WIND REG 0. 0. 0 1 1 2000 2000 NONSTAT 20081121.0000 12 HR 20081125.0000 
    	//READINP WIND 1.0 'rotating.WND' 1 1 1 1 FREE
    	// rotating.WND :
    	// wave modelling - wind
    	//20081121.0000   
    	//X-comp 
    	//    0.00     0.00 
    	//    0.00     0.00 
    	//Y-comp 
    	//  -15.00   -15.00 
    	//  -15.00   -15.00 
    	//20081121.1200   
    	//X-comp 
    	//  -10.61   -10.61 
    	//  -10.61   -10.61 
    	//Y-comp 
    	//  -10.61   -10.61 
    	//  -10.61   -10.61 
        File windFilesTestDir = new File(testData.getTestRunDataDir(), "SwanWindFiles");

        SwanField2DXYFile swanWindFile = new SwanField2DXYFile();
        swanWindFile.initialize(windFilesTestDir,new String[] {"swantest_windseries.swn","WIND"});

        String[] Ids = swanWindFile.getExchangeItemIDs();
        assertEquals("number of exchangeItems",2,Ids.length);
        assertTrue("look for wind.x",Arrays.asList(Ids).contains("wind.x"));
        assertTrue("look for wind.y",Arrays.asList(Ids).contains("wind.y"));
        
        // get windX
        IExchangeItem windX = swanWindFile.getDataObjectExchangeItem("wind.x");
        System.out.println("windX="+windX.toString());
        
        //check windX 
        String windXId = windX.getId();
        assertEquals("wind.x",windXId);
        ITimeInfo xTimeInfo = windX.getTimeInfo();
        double times[] = xTimeInfo.getTimes();
        assertEquals("times.length",9,times.length);
        assertEquals("times[0]",54791.,times[0]);
        assertEquals("times[1]",54791.5,times[1]);
        assertEquals("times[end]",54795.,times[times.length-1]);
        
        IGeometryInfo xGeometryInfo = windX.getGeometryInfo();
        if(xGeometryInfo instanceof ArrayGeometryInfo){
        	IArray lat = ((ArrayGeometryInfo)xGeometryInfo).getLatitudeArray();
        	assertEquals("{0.0,2000.0}", lat.toString());
        	IArray lon = ((ArrayGeometryInfo)xGeometryInfo).getLongitudeArray();
        	assertEquals("{0.0,2000.0}", lon.toString());
        }
        
        String windXQuantity = windX.getQuantityInfo().getQuantity();
        assertEquals("x_wind", windXQuantity);
        String windXUnit = windX.getQuantityInfo().getUnit();
        assertEquals("m/s", windXUnit);
        
        ValueType windxType = windX.getValuesType();
        assertEquals(ValueType.IArrayType,windxType);
        IArray xValues = (IArray)windX.getValues();
        int xDims[] = xValues.getDimensions();
        assertEquals("xDims[0]",9,xDims[0]);
        assertEquals("xDims[1]",2,xDims[1]);
        assertEquals("xDims[2]",2,xDims[2]);
        
        //get windY
        IExchangeItem windY = swanWindFile.getDataObjectExchangeItem("wind.y");        
        System.out.println("windY="+windY.toString());

        //check windY 
        String windYId = windY.getId();
        assertEquals("wind.y",windYId);
        ITimeInfo yTimeInfo = windY.getTimeInfo();
        double ytimes[] = yTimeInfo.getTimes();
        assertEquals("times.length",9,ytimes.length);
        assertEquals("times[0]",54791.,ytimes[0]);
        assertEquals("times[1]",54791.5,ytimes[1]);
        assertEquals("times[end]",54795.,ytimes[ytimes.length-1]);
        
        IGeometryInfo yGeometryInfo = windY.getGeometryInfo();
        if(xGeometryInfo instanceof ArrayGeometryInfo){
        	IArray lat = ((ArrayGeometryInfo)yGeometryInfo).getLatitudeArray();
        	assertEquals("{0.0,2000.0}", lat.toString());
        	IArray lon = ((ArrayGeometryInfo)yGeometryInfo).getLongitudeArray();
        	assertEquals("{0.0,2000.0}", lon.toString());
        }
        
        String windYQuantity = windY.getQuantityInfo().getQuantity();
        assertEquals("y_wind", windYQuantity);
        String windYUnit = windY.getQuantityInfo().getUnit();
        assertEquals("m/s", windYUnit);

        swanWindFile.finish();
    }

    public void test_change() throws IOException {
        File windFilesTestDir = new File(testData.getTestRunDataDir(), "SwanWindFiles");

        SwanField2DXYFile swanWindFile = new SwanField2DXYFile();
        swanWindFile.initialize(windFilesTestDir,new String[] {"northsea1d_nonstationary.swn","WIND"});

        String[] Ids = swanWindFile.getExchangeItemIDs();
        assertEquals("number of exchangeItems",2,Ids.length);
        assertTrue("look for wind.x",Arrays.asList(Ids).contains("wind.x"));
        assertTrue("look for wind.y",Arrays.asList(Ids).contains("wind.y"));
        
        // get windX
        IExchangeItem windX = swanWindFile.getDataObjectExchangeItem("wind.x");
        System.out.println("windX="+windX.toString());
        
        //modify windX
        IArray xValues=(IArray)windX.getValues();
        double x[] = xValues.getValuesAsDoubles();
        for(int i=0;i<x.length;i++){
        	x[i]+=100.0;
        }
        xValues.setValuesAsDoubles(x);
        windX.setValuesAsDoubles(x); //change values the old way
        
        //get windY
        IExchangeItem windY = swanWindFile.getDataObjectExchangeItem("wind.y");        
        System.out.println("windY="+windY.toString());

        //modify windY using new method
        windY.copyValuesFromItem(windX);
        
        // write changes
        swanWindFile.finish();
        
        //check files
        File outFile = new File(windFilesTestDir, "hirlam_wind_k13.WND");
        File refFile = new File(windFilesTestDir, "hirlam_wind_k13_ref.WND");
		assertTrue(testData.FilesAreIdentical(outFile, refFile));			

    }

}

