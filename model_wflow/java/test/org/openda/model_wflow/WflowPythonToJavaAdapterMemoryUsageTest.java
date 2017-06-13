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

package org.openda.model_wflow;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;

import jep.JepException;
import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.ITime;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

/**
 * Tests the WflowPythonToJavaAdapter class.
 *
 * For this test to work, CPython and PCRaster need to be installed.
 * For this test to work, the following folders need to be present in the corresponding environment variables:
 * PATH: folder containing python executable (e.g. C:\Anaconda), folder with jep.dll file (e.g. model_wflow\bin_external\win64_ifort), folder with PCRaster dll files (e.g. C:\pcraster-4.0.1_x86-64\bin)
 * PYTHONPATH: folder with WFLOW python scripts (e.g. model_wflow\java\test\org\openda\model_wflow\testData\wflow_bin), folder with PCRaster python scripts (e.g. C:\pcraster-4.0.1_x86-64\python).
 *
 * This test only works with a 64-bit JDK version 1.7.
 *
 * @author Arno Kockx
 */
public class WflowPythonToJavaAdapterMemoryUsageTest extends TestCase {

	private OpenDaTestSupport testData;
	private File testRunDataDir;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(WflowPythonToJavaAdapterTest.class, "model_wflow");
		testRunDataDir = testData.getTestRunDataDir();
	}

	/**
	 * This test class only contains methods for manual testing.
	 * Added this empty test method to avoid causing JUnit
	 * to throw Exception "No runnable methods".
	 */
	public void testEmpty() {
	}

	/**
	 * Manual test used to monitor heap space size to find memory leaks
	 * in WflowPythonToJavaAdapter by calling all its methods many times.
	 *
	 * @throws JepException
	 * @throws InterruptedException 
	 */
	public void _testMemoryUsage() throws JepException, InterruptedException {
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeZone(TimeUtils.createTimeZoneFromDouble(0));
		calendar.set(2012, 0, 1, 0, 0, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		double startDate = Time.milliesToMjd(calendar.getTimeInMillis());
		calendar.set(2012, 0, 21, 0, 0, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		double endDate = Time.milliesToMjd(calendar.getTimeInMillis());
		ITime timeHorizon = new Time(startDate, endDate, 1);
		int numberOfTimeSteps = (int) ((Time) timeHorizon).getStepCount();

		WflowPythonToJavaAdapter adapter = new WflowPythonToJavaAdapter();

		//initialize model.
		String pythonModuleNameOfModelToUse = "wflow_hbv";
		adapter.performPythonImports(pythonModuleNameOfModelToUse);
		//working directory (testRunDataDir) is public/opendaTestRuns/model_wflow/org/openda/model_wflow
		File caseDir = new File(testRunDataDir, "wflowPythonToJavaAdapterTest/case_default_hbv");
		String runId = "work0";
		String configFileName = "wflow_hbv.ini";
		String cloneMapFileName = "wflow_subcatch.map";
		adapter.createWflowModel(pythonModuleNameOfModelToUse, caseDir, runId, configFileName, cloneMapFileName);
		adapter.createWfDynamicFramework(pythonModuleNameOfModelToUse, numberOfTimeSteps);
		adapter.createRunId(pythonModuleNameOfModelToUse);
		adapter.runInitial();
		adapter.runResume();

		String variableName = "SoilMoisture";
		for (int i = 0; i < 100; i++) {
			System.out.println(i);

			@SuppressWarnings("unused")
			ITime currentTime = adapter.getCurrentTime(timeHorizon);
			@SuppressWarnings("unused")
			int columnCount = adapter.getColumnCount();
			@SuppressWarnings("unused")
			int rowCount = adapter.getRowCount();
			@SuppressWarnings("unused")
			String[] variableNames = adapter.getVariableNames();
			@SuppressWarnings("unused")
			int[] variableUnits = adapter.getVariableUnits();
			@SuppressWarnings("unused")
			int[] variableRoles = adapter.getVariableRoles();

			adapter.runInitial();
			adapter.runResume();
			adapter.runDynamic(1, 1);
			adapter.runSuspend();
			adapter.runQuickResume();
			adapter.runQuickSuspend();

			//test set values (here create a new double array inside the loop, otherwise memory leaks may not become apparent).
			double[] newValues = new double[31603];
			for (int n = 0; n < newValues.length; n++) {
				newValues[n] = n;
			}
			adapter.setValues(variableName, newValues);

			@SuppressWarnings("unused")
			double[] values = adapter.getMapAsList(variableName);
		}

		//finalize model.
		adapter.runSuspend();
		adapter.close();
	}
}
