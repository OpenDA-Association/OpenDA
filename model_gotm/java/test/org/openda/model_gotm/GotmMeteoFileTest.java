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

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;

public class GotmMeteoFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(GotmMeteoFileTest.class,"model_gotm");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		IDataObject ioObject = new GotmMeteoFile();
		String args[] = {"meteo_test.dat"};
		ioObject.initialize(testRunDataDir, args);

		// dump interpreted data to screen
		// System.out.println(ioObject.toString());
		
		String[] exchangeItemIds = ioObject.getExchangeItemIDs();

		for(int item=0;item<exchangeItemIds.length;item++){

			IExchangeItem ex = ioObject.getDataObjectExchangeItem(exchangeItemIds[item]);
			System.out.println(ex.toString());

			if(exchangeItemIds[item].equalsIgnoreCase("1.u10")){
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()", "1.u10", id);

				//public Type getObjectType(); //
				IExchangeItem.ValueType valueType = ex.getValuesType();
				
				System.out.println("Type" + valueType ); 
				assertTrue(valueType == IExchangeItem.ValueType.IArrayType);

				//public Object getValues();
				//double value = (double) ex.getValues();
				//assertEquals("value", 100.0, value, 0.0001);
			}
		}
	}


	public void testWriteInput() {
		//First read input
		IDataObject ioObject = new GotmMeteoFile();
		String args[] = {"meteo_test.dat"};
		File original = new File(testRunDataDir,"meteo_test.dat");
		File copy = new File(testRunDataDir,"meteo_file_copy.dat");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		ioObject.initialize(testRunDataDir, args);

		//String[] exchangeItemIds = ioObject.getExchangeItemIDs();
		// System.out.println(exchangeItemIds);source
		IExchangeItem item = ioObject.getDataObjectExchangeItem("1.u10");
		double[] values = new double[3];
		values[0] = 1.0;
		values[1] = 2.0;
		values[2] = 3.0;
		item.setValues( values );
		
		//change some things

		//write to file
		ioObject.finish();
		File reference = new File(testRunDataDir,"meteo_file_ref.dat");
		//boolean containsLocA =testData.FileContains(reference, "102030");
		//assertTrue(containsLocA);
		boolean identical = testData.FilesAreIdentical(original,reference);
		//assertTrue(identical);
	}
}
