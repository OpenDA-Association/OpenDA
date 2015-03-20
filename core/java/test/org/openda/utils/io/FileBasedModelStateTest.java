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
package org.openda.utils.io;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Test for file based model state storage.
 */
public class FileBasedModelStateTest extends TestCase {

	OpenDaTestSupport testData;
	private File testRunDataDir;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(FileBasedModelStateTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testSaveAndRestore() {
		// prepare directories
		File dirForInitialAlgorithm = new File(testRunDataDir, "fbModelState/toBeSaved");
		File dirForRestartingAlgorithm = new File(testRunDataDir, "fbModelState/restarted");
		dirForRestartingAlgorithm.mkdir();
		for (File directory : dirForInitialAlgorithm.listFiles()) {
			File restoreSubDir = new File(dirForRestartingAlgorithm, directory.getName());
			restoreSubDir.mkdir();
		}

		// algorithm that produces the state (dummy alg. assumes that the model instances are already there)
		IAlgorithm initialAlgorithm = new DummyAlgorithm();
		initialAlgorithm.initialize(dirForInitialAlgorithm, new String[]{});
		IModelState savedState = initialAlgorithm.saveInternalState();
		File savedAlgorithmStateFile = new File(testRunDataDir, "algorithmModelState.zip");
		savedState.savePersistentState(savedAlgorithmStateFile);

		File dirWithSavedStates = new File(dirForInitialAlgorithm, DummyAlgorithm.zippedModelStatesDirName);
		assertTrue("zip1 exists", new File(dirWithSavedStates, "model1_state.zip").exists());
		assertTrue("zip2 exists", new File(dirWithSavedStates, "model2_state.zip").exists());
		assertTrue("zip3 exists", new File(dirWithSavedStates, "model3_state.zip").exists());

		initialAlgorithm.releaseInternalState(savedState);

		assertFalse("zip1 removed", new File(dirWithSavedStates, "model1_state.zip").exists());
		assertFalse("zip2 removed", new File(dirWithSavedStates, "model2_state.zip").exists());
		assertFalse("zip3 removed", new File(dirWithSavedStates, "model3_state.zip").exists());

		// algorithm that uses the state for restart (dummy alg. assumes that the model instances are already there)
		IAlgorithm algorithmThatRestarts = new DummyAlgorithm();
		algorithmThatRestarts.initialize(dirForRestartingAlgorithm, new String[]{});
		IModelState restartState = algorithmThatRestarts.loadPersistentState(savedAlgorithmStateFile);

		File dirWithRestoredStates = new File(dirForRestartingAlgorithm, DummyAlgorithm.zippedModelStatesDirName);
		assertFalse("restored zip1 not yet there", new File(dirWithRestoredStates, "model1_state.zip").exists());
		assertFalse("restored zip2 not yet there", new File(dirWithRestoredStates, "model2_state.zip").exists());
		assertFalse("restored zip3 not yet there", new File(dirWithRestoredStates, "model3_state.zip").exists());

		algorithmThatRestarts.restoreInternalState(restartState);

		assertTrue("zip1  restored", new File(dirWithRestoredStates, "model1_state.zip").exists());
		assertTrue("zip2  restored", new File(dirWithRestoredStates, "model2_state.zip").exists());
		assertTrue("zip3  restored", new File(dirWithRestoredStates, "model3_state.zip").exists());

		algorithmThatRestarts.releaseInternalState(restartState);

		assertFalse("zip1 removed", new File(dirWithRestoredStates, "model1_state.zip").exists());
		assertFalse("zip2 removed", new File(dirWithRestoredStates, "model2_state.zip").exists());
		assertFalse("zip3 removed", new File(dirWithRestoredStates, "model3_state.zip").exists());

		String[] modelStateFiles = {
				"model1/modelStatePartA.txt",
				"model1/modelStatePartB.txt",
				"model2/modelState.txt",
				"model3/modelStatePart-I.txt",
				"model3/modelStatePart-II.txt",
				"model3/modelStatePart-III.txt"
		};
		for (String modelStateFile : modelStateFiles) {
			testData.FilesAreIdentical(new File(dirForInitialAlgorithm, modelStateFile),
					new File(dirForRestartingAlgorithm, modelStateFile));
		}
	}

	private class DummyAlgorithm implements IAlgorithm {

		File workingDir = null;
		File dirForPersistentModelStates = null;
		String zippedModelStateFileExtension = "_state.zip";
		public static final String zippedModelStatesDirName = "zippedModelStates";

		public void initialize(File workingDir, String[] arguments) {
			this.workingDir = workingDir;
			dirForPersistentModelStates = new File(workingDir, zippedModelStatesDirName);
			if (!dirForPersistentModelStates.mkdir()) {
				throw new RuntimeException("Could not create dir " + dirForPersistentModelStates.getAbsolutePath());
			}
		}

		public IModelState saveInternalState() {


			// Create model instance state files, and add them state files to model state
			FileBasedModelState algorithmState = new FileBasedModelState();
			algorithmState.setDirContainingModelstateFiles(dirForPersistentModelStates);

			ArrayList<File> modelInstanceDirs = listModelInstanceDirs();
			for (int i = 0; i < modelInstanceDirs.size(); i++) {
				File modelInstanceDir = modelInstanceDirs.get(i);
				IModelState modelInstanceState = modelInstance_saveInternalState(modelInstanceDir);
				File modelStateFile = new File(dirForPersistentModelStates, modelInstanceDir.getName() + zippedModelStateFileExtension);
				File fileToBeAddedToState;
				if (i == 1) {
					// test absolute path
					fileToBeAddedToState = modelStateFile.getAbsoluteFile();
				} else {
					// test relative path
					fileToBeAddedToState = modelStateFile;
				}
				modelInstanceState.savePersistentState(fileToBeAddedToState);
				algorithmState.addFile(fileToBeAddedToState);
			}
			return algorithmState;
		}

		public void restoreInternalState(IModelState savedInternalState) {

			// first unzip the algoritm state file
			if (!(savedInternalState instanceof FileBasedModelState)) {
				throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
						" for " + this.getClass().getName() + ".releaseInternalState");
			}
			FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
			modelState.setDirContainingModelstateFiles(dirForPersistentModelStates);
			modelState.restoreState();

			// now for each model instance, unzip its state
			for (File modelInstanceDir : listModelInstanceDirs()) {
				File modelStateFile = new File(dirForPersistentModelStates, modelInstanceDir.getName() + "_state.zip");
				if (modelStateFile.exists()) {
					modelInstance_restoreInternalState(modelInstanceDir, modelStateFile);
				}
			}
		}

		public void releaseInternalState(IModelState savedInternalState) {
			if (!(savedInternalState instanceof FileBasedModelState)) {
				throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
						" for " + this.getClass().getName() + ".releaseInternalState");
			}
			// now for each model instance, unzip its state
			for (File modelInstanceDir : listModelInstanceDirs()) {
				File modelStateFile = new File(dirForPersistentModelStates, modelInstanceDir.getName() + "_state.zip");
				if (modelStateFile.exists()) {
					if (!modelStateFile.delete()) {
						throw new RuntimeException("Could not deleted zipped model instance state: " +
								modelStateFile.getAbsolutePath());
					}
				}
			}
			if (!dirForPersistentModelStates.delete()) {
				throw new RuntimeException("Could not delete dir " + dirForPersistentModelStates.getAbsolutePath());
			}
		}

		public IModelState loadPersistentState(File algorithmStateFile) {
			FileBasedModelState modelState = new FileBasedModelState();
			modelState.setZippedStateFile(algorithmStateFile);
			return modelState;
		}

		private IModelState modelInstance_saveInternalState(File modelInstanceDir) {

			FileBasedModelState modelState = new FileBasedModelState();
			modelState.setDirContainingModelstateFiles(modelInstanceDir.getAbsoluteFile());
			File[] modelStateFiles = modelInstanceDir.listFiles();
			for (int i = 0; i < modelStateFiles.length; i++) {
				File modelStateFile;
				if (i == 0) {
					// test absolute path
					modelStateFile = modelStateFiles[i];
				} else {
					// test relative path
					modelStateFile = new File(modelStateFiles[i].getName());
				}
				modelState.addFile(modelStateFile);
			}
			return modelState;
		}

		private void modelInstance_restoreInternalState(File modelInstanceDir, File modelStateFile) {
			FileBasedModelState modelState = new FileBasedModelState();
			modelState.setDirContainingModelstateFiles(modelInstanceDir.getAbsoluteFile());
			modelState.loadState(modelStateFile);
			modelState.restoreState();
		}

		private ArrayList<File> listModelInstanceDirs() {

			ArrayList<File> files = new ArrayList<File>();
			for (File file : workingDir.listFiles()) {
				if (file.getName().startsWith("model")) {
					files.add(file);
				}
			}
			return files;
		}

		public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
			// no action needed for this test
		}

		public void prepare() {
			// no action needed for this test
		}

		public void run() {
			// no action needed for this test
		}

		public boolean hasNext() {
			// nothing needed for this test
			return false;
		}

		public void next() {
			// no action needed for this test
		}

		public void finish() {
			// no action needed for this test
		}

		public IVector getState() {
			// nothing needed for this test
			return null;
		}

		public IInstance getParent() {
			// nothing needed for this test
			return null;
		}

		public ITime getTimeHorizon() {
			// TODO Auto-generated method stub
			return null;
		}

		public ITime getCurrentTime() {
			// TODO Auto-generated method stub
			return null;
		}

		public void compute(ITime targetTime) {
			// TODO Auto-generated method stub
		}
	}
}
