/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.exchange.dataobjects;
import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by werner on 25/03/16.
 */
public class AsciiKeywordDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(AsciiKeywordDataObjectTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testInitializeControlDict() {
		IDataObject dataObject = new AsciiKeywordDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"asciiKeywordDataobject/config.yaml" });
	}

	public void testGetControlDictExchangeItems() {
		IDataObject dataObject = new AsciiKeywordDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"asciiKeywordDataobject/config.yaml" });
		String[] ids = dataObject.getExchangeItemIDs();
		assertEquals( 4, ids.length  );
		Arrays.sort(ids);
		assertEquals("reaction_time", ids[0]);
		assertEquals("time_control@1", ids[1]);
		assertEquals("time_control@2", ids[2]);
		assertEquals("time_control@3", ids[3]);

		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("reaction_time");
		assertEquals(2000.0, (Double) exchangeItem.getValues(), Math.ulp(2000.0) );
		exchangeItem = dataObject.getDataObjectExchangeItem("time_control@1");
		assertEquals(0.0, (Double) exchangeItem.getValues(), Math.ulp(0.0));
		exchangeItem = dataObject.getDataObjectExchangeItem("time_control@2");
		assertEquals(60.0, (Double) exchangeItem.getValues(), Math.ulp(60.0));
		exchangeItem = dataObject.getDataObjectExchangeItem("time_control@3");
		assertEquals(15000.0, (Double) exchangeItem.getValues(), Math.ulp(15000.0));
	}

	public void testSetControlDictExchangeItems() {
    	// Set exchange items
		IDataObject dataObject = new AsciiKeywordDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"asciiKeywordDataobject/template_config.yaml"});
        IExchangeItem item = dataObject.getDataObjectExchangeItem("reaction_time");
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("reaction_time");
		assertEquals(0.0, (Double) exchangeItem.getValues(), Math.ulp(0.0) );
		exchangeItem = dataObject.getDataObjectExchangeItem("time_control@1");
		assertEquals(0.0, (Double) exchangeItem.getValues(), Math.ulp(0.0));
		exchangeItem = dataObject.getDataObjectExchangeItem("time_control@2");
		assertEquals(0.0, (Double) exchangeItem.getValues(), Math.ulp(0.0));
		exchangeItem = dataObject.getDataObjectExchangeItem("time_control@3");
		assertEquals(0.0, (Double) exchangeItem.getValues(), Math.ulp(0.0));
		
        item.setValues(2000.0);
        item = dataObject.getDataObjectExchangeItem("time_control@1");
        item.setValues(0.0);
		item = dataObject.getDataObjectExchangeItem("time_control@2");
		item.setValues(60.0);
		item = dataObject.getDataObjectExchangeItem("time_control@3");
		item.setValues(15000.0);
        dataObject.finish();

		// Open modified with new dataObject
		IDataObject modifiedDataObject = new AsciiKeywordDataObject();
		modifiedDataObject.initialize(testRunDataDir, new String[]{"asciiKeywordDataobject/template_config.yaml"});

		IExchangeItem modifiedExchangeItem = modifiedDataObject.getDataObjectExchangeItem("reaction_time");
		assertEquals(2000.0, (Double) modifiedExchangeItem.getValues(), Math.ulp(2000.0) );
		modifiedExchangeItem = dataObject.getDataObjectExchangeItem("time_control@1");
		assertEquals(0.0, (Double) modifiedExchangeItem.getValues(), Math.ulp(0.0));
		modifiedExchangeItem = dataObject.getDataObjectExchangeItem("time_control@2");
		assertEquals(60.0, (Double) modifiedExchangeItem.getValues(), Math.ulp(60.0));
		modifiedExchangeItem = dataObject.getDataObjectExchangeItem("time_control@3");
		assertEquals(15000.0, (Double) modifiedExchangeItem.getValues(), Math.ulp(15000.0));
		modifiedDataObject.finish();

	}

}
