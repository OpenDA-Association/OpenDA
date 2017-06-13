/* OpenDA v2.4 
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
package org.openda.model_glm;
import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;

public class GlmNmlFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(GlmNmlFileTest.class,"model_glm");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		IDataObject ioObject = new GlmNmlFile();
		String args[] = {"fabm.nml"};
		ioObject.initialize(testRunDataDir, args);

		// dump interpreted data to screen
		System.out.println(ioObject.toString());
		
		String[] exchangeItemIds = ioObject.getExchangeItemIDs();

		for(int item=0;item<exchangeItemIds.length;item++){

			IExchangeItem ex = ioObject.getDataObjectExchangeItem(exchangeItemIds[item]);
			System.out.println(ex.toString());

			if(exchangeItemIds[item].equalsIgnoreCase("gotm_npzd_rmax")){
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()", "gotm_npzd_rmax", id);

				//public Type getObjectType(); //
				IExchangeItem.ValueType valueType = ex.getValuesType();
				assertTrue(valueType == IExchangeItem.ValueType.doubleType);

				//public Object getValues();
				double value = (double) ex.getValues();
				assertEquals("value", 2.0, value, 0.0001);
			}
		}
	}


	public void testWriteInput() {
		//First read input
		IDataObject ioObject = new GlmNmlFile();
		String args[] = {"fabm.nml"};
		File original = new File(testRunDataDir,"fabm.nml");
		File copy = new File(testRunDataDir,"fabm_copy.nml");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		ioObject.initialize(testRunDataDir, args);

		String[] exchangeItemIds = ioObject.getExchangeItemIDs();

		//change some things


		//write to file
		ioObject.finish();
		//File reference = new File(testRunDataDir,"pollution_model_changed.input");
		//boolean containsLocA =testData.FileContains(reference, "102030");
		//assertTrue(containsLocA);
		//boolean identical = testData.FilesAreIdentical(copy,reference);
		//assertTrue(identical);
	}
}
