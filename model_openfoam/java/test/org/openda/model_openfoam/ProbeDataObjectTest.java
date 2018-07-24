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
public class ProbeDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(ProbeDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testInitializeScalarTimeSeries() {
        IDataObject dataObject = new ProbeDataObject();
        dataObject.initialize(testRunDataDir, new String[]{"probes/0/p", "2015-12-01T00:00:00Z"});
    }

    public void testGetScalarExchangeItemIDs() {
        IDataObject scalarObject = new ProbeDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"probes/0/p", "2015-12-01T00:00:00Z"} );
        String[] ids = scalarObject.getExchangeItemIDs();
		assertEquals( 2, ids.length);
		Arrays.sort(ids);
		assertEquals("probes#0.p", ids[0]);
		assertEquals("probes#1.p", ids[1]);
    }

    public void testGetScalarDataObjectExchangeItem() {
        IDataObject scalarObject = new ProbeDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"probes/0/p", "2015-12-01T00:00:00Z"} );
        IExchangeItem item = scalarObject.getDataObjectExchangeItem("probes#1.p");
		assertEquals("probes#1.p", item.getId());
    }

	public void testInitializeVectorTimeSeries() {
		IDataObject dataObject = new ProbeDataObject();
		dataObject.initialize(testRunDataDir, new String[]{"probes/0/U", "2015-12-01T00:00:00Z"});
	}

	public void testGetVectorExchangeItemIDs() {
		IDataObject scalarObject = new ProbeDataObject();
		scalarObject.initialize(testRunDataDir, new String[]{"probes/0/U", "2015-12-01T00:00:00Z"} );
		String[] ids = scalarObject.getExchangeItemIDs();
		assertEquals( 6, ids.length);
		Arrays.sort(ids);
		assertEquals("probes#0.U1", ids[0]);
		assertEquals("probes#0.U2", ids[1]);
		assertEquals("probes#0.U3", ids[2]);
		assertEquals("probes#1.U1", ids[3]);
		assertEquals("probes#1.U2", ids[4]);
		assertEquals("probes#1.U3", ids[5]);
	}


}
