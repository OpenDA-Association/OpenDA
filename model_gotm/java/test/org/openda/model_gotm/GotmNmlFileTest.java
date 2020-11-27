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
package org.openda.model_gotm;
import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;

public class GotmNmlFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(GotmNmlFileTest.class,"model_gotm");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadInput() {

		IDataObject dataObject = new GotmNmlFile();
		String args[] = {"gotmrun.nml"};
		dataObject.initialize(testRunDataDir, args);

		// dump interpreted data to screen
		System.out.println(dataObject.toString());
		
		String[] exchangeItemIds = dataObject.getExchangeItemIDs();

		for(int item=0;item<exchangeItemIds.length;item++){

			IExchangeItem ex = dataObject.getDataObjectExchangeItem(exchangeItemIds[item]);
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
		IDataObject dataObject = new GotmNmlFile();
		String args[] = {"gotmrun.nml"};
		File original = new File(testRunDataDir,"gotmrun.nml");
		File copy = new File(testRunDataDir,"gotmrun_copy.nml");
		try {
			BBUtils.copyFile(original,copy);
		} catch (IOException e) {
			throw new RuntimeException("Could not copy file "+original.getAbsolutePath()+" to "+copy.getAbsolutePath());
		}
		dataObject.initialize(testRunDataDir, args);

		//String[] exchangeItemIds = dataObject.getExchangeItemIDs();
		// System.out.println(exchangeItemIds);source
		IExchangeItem item = dataObject.getDataObjectExchangeItem("eqstate_p0");
		double[] values = new double[1];
		values[0] = 1.0;
		item.setValues(1.0);
		
		//change some things

		//write to file
		dataObject.finish();
		File reference = new File(testRunDataDir,"gotmrun_ref.nml");
		//boolean containsLocA =testData.FileContains(reference, "102030");
		//assertTrue(containsLocA);
		boolean identical = testData.FilesAreIdentical(original,reference);
		//assertTrue(identical);
	}
}
