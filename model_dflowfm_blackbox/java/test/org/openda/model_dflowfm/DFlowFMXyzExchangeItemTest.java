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
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IVector;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;


public class DFlowFMXyzExchangeItemTest extends TestCase{
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMXyzExchangeItem.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "Forcingfile");
	}

	public void testDflowfmFrcitionCoefExchangeItem() {

		File dataDir=testRunDataDir;
		File testfile=new File(dataDir,"frcfact.xyz");
		int lineNum = 1;
		DFlowFMXyzExchangeItem item = new DFlowFMXyzExchangeItem("id",testfile, 2.0 ,lineNum);

		String id = item.getId();
		assertEquals("id", id);

		String unitId = item.getUnitId();
		assertEquals("-",unitId);

		
		IVector vector = (IVector) item.getValues();
		vector.setConstant(2.0);
		item.setValues(vector);
		assertEquals(item.getValuesType(), IExchangeItem.ValueType.IVectorType);
		String value = item.getValues().toString();
		assertEquals("[2.0]", value);
	}
}
