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
package org.openda.blackbox.wrapper;

import junit.framework.TestCase;
import org.openda.interfaces.IModelState;
import org.openda.interfaces.IStochModelFactory;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.io.FileBasedModelState;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Created by pelgrim on 03-Jun-16.
 */
public class BBModelRestartTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(BBStochmodelEnsembleRestartTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadAndWriteFromBlackBox() {
		BBModelFactory factory = new BBModelFactory();
		factory.initialize(new File(testRunDataDir, "restart"), new String[]{"bbModelConfig.xml"});
		BBModelInstance instance = factory.getInstance(new String[0], IStochModelFactory.OutputLevel.ModelDefault);
		Time targetTime = new Time(57542.375);
		instance.compute(targetTime);
		IModelState modelState = instance.saveInternalState();
		File modelRunDir = instance.getModelRunDir();
		File savedStateDir = new File(modelRunDir, "savedState");
		savedStateDir.mkdir();
		File savedStateZip = new File(savedStateDir, "savedState.zip");
		modelState.savePersistentState(savedStateZip);
		assertTrue(savedStateZip.exists());
		String[] expectedEntryNames = {"dflow1d/extra/extra.txt", "dflow1d/sobek.rda", "dflow1d/sobek.rdf", "dflow1d/wlevStateFileIn.xyz", "rtc/state_export.xml"};
		try {
			ZipInputStream zipInputStream = new ZipInputStream(new FileInputStream(savedStateZip));
			ZipEntry entry;
			int i = 0;
			while ((entry = zipInputStream.getNextEntry()) != null) {
				assertEquals(expectedEntryNames[i], entry.getName());
				i++;
			}
			assertEquals(i, expectedEntryNames.length);
		} catch (IOException e) {
			assert false;
		}
		for (String expectedEntryName : expectedEntryNames) {
			File file = new File(modelRunDir, expectedEntryName);
			assert file.exists();
			assert file.delete();
		}

		File rtcDir = new File(modelRunDir, "rtc");
		assert rtcDir.delete();
		File dflow1dDir = new File(modelRunDir, "dflow1d");
		File extraDir = new File(dflow1dDir, "extra");
		assert extraDir.delete();
		assert dflow1dDir.delete();
		instance.restoreInternalState(modelState);
		for (String expectedEntryName : expectedEntryNames) {
			File file = new File(modelRunDir, expectedEntryName);
			assert file.exists();
		}
	}

	public void testReleaseStateFromBlackBox() {
		BBModelFactory factory = new BBModelFactory();
		factory.initialize(new File(testRunDataDir, "restart"), new String[]{"bbModelConfig.xml"});
		BBModelInstance instance = factory.getInstance(new String[0], IStochModelFactory.OutputLevel.ModelDefault);
		Time targetTime = new Time(57542.375);
		instance.compute(targetTime);
		IModelState modelState = instance.saveInternalState();
		File modelRunDir = instance.getModelRunDir();
		File savedStateDir = new File(modelRunDir, "savedState");
		savedStateDir.mkdir();
		File savedStateZip = new File(savedStateDir, "savedState.zip");
		modelState.savePersistentState(savedStateZip);
		assertTrue(savedStateZip.exists());
		String[] expectedEntryNames = {"dflow1d/extra/extra.txt", "dflow1d/sobek.rda", "dflow1d/sobek.rdf", "dflow1d/wlevStateFileIn.xyz", "rtc/state_export.xml"};
		try {
			ZipInputStream zipInputStream = new ZipInputStream(new FileInputStream(savedStateZip));
			ZipEntry entry;
			int i = 0;
			while ((entry = zipInputStream.getNextEntry()) != null) {
				assertEquals(expectedEntryNames[i], entry.getName());
				i++;
			}
			assertEquals(i, expectedEntryNames.length);
		} catch (IOException e) {
			assert false;
		}
		instance.releaseInternalState(modelState);
		File dirContainingModelStateFiles = ((FileBasedModelState) modelState).getDirContainingModelStateFiles();
		assertFalse(dirContainingModelStateFiles.exists());
		for (String expectedEntryName : expectedEntryNames) {
			File file = new File(modelRunDir, expectedEntryName);
			assert file.exists();
			assert file.delete();
		}

		File rtcDir = new File(modelRunDir, "rtc");
		assert rtcDir.delete();
		File dflow1dDir = new File(modelRunDir, "dflow1d");
		File extraDir = new File(dflow1dDir, "extra");
		assert extraDir.delete();
		assert dflow1dDir.delete();
	}

}
