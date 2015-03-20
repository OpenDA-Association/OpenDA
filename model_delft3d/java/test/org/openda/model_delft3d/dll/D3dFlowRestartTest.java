package org.openda.model_delft3d.dll;

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.ITime;
import org.openda.model_delft3d.D3dResults;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.RelativePath;
import org.openda.utils.Time;
import org.openda.utils.io.FileBasedModelState;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Delft3D flow DLL restart functionality
 */
public class D3dFlowRestartTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dFlowRestartTest.class, "public", "model_delft3d");
	}

	public void tstDummy(){
		//nothing here on purposes
	}

	public void testRestart() { //TODO MVL Some problem with removing existing files.
	                        	//This fails when running multiple tests

		if (!BBUtils.RUNNING_ON_WINDOWS) {
			return;
		}

		File restartTestRunDir = new File(testData.getTestRunDataDir(), "restartTest");

		double deltaTasMJD = 10d / 86400d; // 10 seconds

		File fullRunModelDir = new File(restartTestRunDir, "fullRun");

		IModelFactory d3dFlowModelFactory = new D3dFlowModelFactory();
		d3dFlowModelFactory.initialize(fullRunModelDir, new String[]{"d3dFact.xml"});
		IModelInstance fullRunModelInstance = d3dFlowModelFactory.getInstance(
				new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);

		ITime timeHorizon = fullRunModelInstance.getTimeHorizon();
		double beginTimeAsMJD = timeHorizon.getBeginTime().getMJD();
		double endTimeAsMJD = timeHorizon.getEndTime().getMJD();
		int nTimes = (int) (Math.floor((endTimeAsMJD - beginTimeAsMJD + deltaTasMJD/2d)/deltaTasMJD));
		int timeStepForRestart = nTimes/3;
		ITime restartTimeStamp = new Time(beginTimeAsMJD + timeStepForRestart*deltaTasMJD);

		File savedStateFile = new File("restartFromFullRun.zip");

		fullRunModelInstance.compute(restartTimeStamp);
		fullRunModelInstance.saveInternalState().savePersistentState(savedStateFile);
		fullRunModelInstance.compute(timeHorizon.getEndTime());
		fullRunModelInstance.finish();
		d3dFlowModelFactory.finish();

		IModelFactory d3dFlowModelFactoryRestart = new D3dFlowModelFactory();
		File secondPartModelDir = new File(restartTestRunDir, "secondPart");
		d3dFlowModelFactoryRestart.initialize(secondPartModelDir, new String[]{"d3dFact.xml"});
		IModelInstance secondPartModelInstance = d3dFlowModelFactoryRestart.getInstance(
				new String[]{}, IStochModelFactory.OutputLevel.ModelDefault);

		FileBasedModelState modelState = new FileBasedModelState();
		modelState.loadState(savedStateFile);
		secondPartModelInstance.restoreInternalState(modelState);
		secondPartModelInstance.compute(timeHorizon.getEndTime());
		secondPartModelInstance.finish();
		d3dFlowModelFactoryRestart.finish();

		File nativeDLLDir = new File(testData.getProjectRootDir(), "bin");
		String nativeDLLDirRelativePath = RelativePath.getRelativePath(fullRunModelDir, nativeDLLDir);

		D3dResults fullRunResults = new D3dResults();
		fullRunResults.initialize(fullRunModelDir, "trih-Est1D.dat", new String[]{nativeDLLDirRelativePath});
		IPrevExchangeItem[] fullRunExchangeItems = fullRunResults.getExchangeItems();

		D3dResults secondPartResults = new D3dResults();
		secondPartResults.initialize(secondPartModelDir, "trih-Est1D.dat", new String[]{nativeDLLDirRelativePath});
		IPrevExchangeItem[] secondPartExchangeItems = secondPartResults.getExchangeItems();

		assertEquals("#exchange items", fullRunExchangeItems.length, secondPartExchangeItems.length);
		for (int i = 0; i < secondPartExchangeItems.length; i++) {
			double[] firstPartValues = secondPartExchangeItems[i].getValuesAsDoubles();
			double [] secondPartValues = secondPartExchangeItems[i].getValuesAsDoubles();
			int startIndex = firstPartValues.length - secondPartValues.length;
			for (int j = startIndex; j < firstPartValues.length; j++) {
				assertEquals(secondPartExchangeItems[i].getId() + "[" + j + "]",
						firstPartValues[j], secondPartValues[j]);
			}
		}

		fullRunResults.finish();
		secondPartResults.finish();
	}
}
