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
package org.openda.model_gotm;
import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Locale;

public class GotmProfileFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(GotmProfileFileTest.class,"model_gotm");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		IDataObject ioObject = new GotmProfileFile();
		String args[] = {"vel_prof_file.dat"};
		ioObject.initialize(testRunDataDir, args);

		// dump interpreted data to screen
		System.out.println(ioObject.toString());
		
		String[] exchangeItemIds = ioObject.getExchangeItemIDs();

		for(int item=0;item<exchangeItemIds.length;item++){

			IExchangeItem ex = ioObject.getDataObjectExchangeItem(exchangeItemIds[item]);
			System.out.println(ex.toString());

			if(exchangeItemIds[item].equalsIgnoreCase("model_setup_dt")){
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()", "model_setup_dt", id);

				//public Type getObjectType(); //
				IExchangeItem.ValueType valueType = ex.getValuesType();
				assertTrue(valueType == IExchangeItem.ValueType.doubleType);

				//public Object getValues();
				//double value = (double) ex.getValues();
				//assertEquals("value", 100.0, value, 0.0001);
			}
		}
	}

	public void testWriteInput() {
		//First read input
		
		IDataObject ioObject = new GotmProfileFile();
		String args[] = {"vel_prof_file.dat"};
		File original = new File(testRunDataDir,"vel_prof_file.dat");
		ioObject.initialize(testRunDataDir, args);

		//String[] exchangeItemIds = ioObject.getExchangeItemIDs();
		// System.out.println(exchangeItemIds);source
		IExchangeItem item = ioObject.getDataObjectExchangeItem("vel_prof_file_1_0");
		//double[] values = new double[1];
		//values[0] = 1.0;
		//item.setValues(1.0);

		//write to file
		ioObject.finish();
		File reference = new File(testRunDataDir,"vel_prof_file.ref.dat");
		boolean identical = testData.FilesAreIdentical(original,reference);
		assertTrue(identical);
	}
}
