/* OpenDA v2.4.4 
* Copyright (c) 2018 OpenDA Association 
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
package org.openda.model_openfoam;
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
public class DictionaryTimeSeriesDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(DictionaryTimeSeriesDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testInitialize() {
		IDataObject dataObject = new DictionaryTimeSeriesDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"system/HEATING_ALLEY01" });
	}

	public void testGetExchangeItems() {
		IDataObject dataObject = new DictionaryTimeSeriesDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"system/HEATING_ALLEY01" });
		String[] ids = dataObject.getExchangeItemIDs();
		assertEquals( 2, ids.length);
		Arrays.sort(ids);
		assertEquals("1.oda:HeatAG04", ids[0]);
		assertEquals("2.oda:HeatAG04", ids[1]);
	}

	public void testSetExchangeItems() {
		IDataObject dataObject = new DictionaryTimeSeriesDataObject();
		File original = new File(testRunDataDir, "system/HEATING_ALLEY01");
        File modified = new File(testRunDataDir, "output/HEATING_ALLEY01");
        dataObject.initialize(testRunDataDir, new String[]{"output/HEATING_ALLEY01" });
        IExchangeItem item = dataObject.getDataObjectExchangeItem("1.oda:HeatAG04");
        double[] value = new double[1];
        value[0] = 1.273;
        item.setValues(value);
        item = dataObject.getDataObjectExchangeItem("2.oda:HeatAG04");
        value[0] = 1.0;
        item.setValues(value);
        dataObject.finish();
        testData.FilesAreIdentical(original, modified, 0);
    }


}
