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
package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.utils.OpenDaTestSupport;
import ucar.ma2.DataType;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFiles;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;

public class NetcdfFileConcatenaterTest extends TestCase {
	private File testRunDataDir;

	@Override
	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(NetcdfFileConcatenaterTest.class, "core");
		this.testRunDataDir = testData.getTestRunDataDir();
	}

	public void testDelft3dHisFileConcatenation() throws IOException {
		File firstFile = new File(this.testRunDataDir, "westerscheldt_part1_his.nc");
		File targetFile = new File(this.testRunDataDir, "westerscheldt_his.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		File secondFile = new File(this.testRunDataDir, "westerscheldt_part2_his.nc");
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});
		// find size of time array for original files and concatenated file
		int[] size1 = Objects.requireNonNull(NetcdfFiles.open(firstFile.toString()).findVariable("time")).getShape();
		int[] size2 = Objects.requireNonNull(NetcdfFiles.open(secondFile.toString()).findVariable("time")).getShape();
		int[] size3 = Objects.requireNonNull(NetcdfFiles.open(targetFile.toString()).findVariable("time")).getShape();
		for (int i = 0; i < size3.length; ++i) {
			assertEquals(size3[i], size1[i] + size2[i]);
		}
	}

	public void testDflowfmHisfileConcatenation() throws IOException {
		File firstFile = new File(this.testRunDataDir, "simple_waal_part1_his.nc");
		File targetFile = new File(this.testRunDataDir, "simple_waal_his.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		File secondFile = new File(this.testRunDataDir, "simple_waal_part2_his.nc");
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});
		// find size of time array for original files and concatenated file
		int[] size1 = Objects.requireNonNull(NetcdfFiles.open(firstFile.toString()).findVariable("time")).getShape();
		int[] size2 = Objects.requireNonNull(NetcdfFiles.open(secondFile.toString()).findVariable("time")).getShape();
		int[] size3 = Objects.requireNonNull(NetcdfFiles.open(targetFile.toString()).findVariable("time")).getShape();
		for (int i = 0; i < size3.length; ++i) {
			assertEquals(size3[i], size1[i] + size2[i]);
		}
	}

	public void testNetcdfFixedTimeDimensionConcatenation() {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateFixedTimeDim");
		File firstFile = new File(testRunDataSubDir, "toAdd.nc");
		File targetFile = new File(testRunDataSubDir, "concatenated.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
	}

	public void testNetcdfFixedTimeDimensionInt() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateTimeInt");
		File firstFile = new File(testRunDataSubDir, "rrunoff_201250_timeInt.nc");
		File targetFile = new File(testRunDataSubDir, "concatenated_timeInt.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		File secondFile = new File(testRunDataSubDir, "rrunoff_201257_timeInt.nc");
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});
		assertTrue(targetFile.exists());

		checkConcatenatedValues(firstFile, targetFile, secondFile, 6);
	}

	public void testNetcdfFixedTimeDimensionUseNewValueOnOverlapConcatenation() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateNewValueOnOverlap");
		File firstFile = new File(testRunDataSubDir, "rrunoff_201250.nc");
		File targetFile = new File(testRunDataSubDir, "concatenated_rrrunoff.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		File secondFile = new File(testRunDataSubDir, "rrunoff_201257.nc");
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});
		assertTrue(targetFile.exists());

		checkConcatenatedValues(firstFile, targetFile, secondFile, 6);
	}

	public void testNetcdfFloats() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateFloatValues");
		File firstFile = new File(testRunDataSubDir, "rrunoff_201250_floats.nc");
		File targetFile = new File(testRunDataSubDir, "concatenated_rrrunoff_floats.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		File secondFile = new File(testRunDataSubDir, "rrunoff_201257_floats.nc");
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});
		assertTrue(targetFile.exists());

		checkConcatenatedValues(firstFile, targetFile, secondFile, 6);
	}

	public void testNetcdf3Dhis() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenate3dVariables");
		File firstFile = new File(testRunDataSubDir, "first_FlowFM_his.nc");
		File targetFile = new File(testRunDataSubDir, "full_FlowFM_his.nc");
		File secondFile = new File(testRunDataSubDir, "FlowFM_his.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});

		checkConcatenated3dVariable(firstFile, targetFile, secondFile, "temperature", 18000);
	}

	public void testNetcdf3Dmap() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenate3dVariables");
		File firstFile = new File(testRunDataSubDir, "first_FlowFM_map.nc");
		File targetFile = new File(testRunDataSubDir, "full_FlowFM_map.nc");
		File secondFile = new File(testRunDataSubDir, "FlowFM_map.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath()});

		checkConcatenated3dVariable(firstFile, targetFile, secondFile, "tem1", 3000);
	}

	private static void checkConcatenated3dVariable(File firstFile, File targetFile, File secondFile, String variableName, int split) throws IOException {
		try (NetcdfFile firstNetcdf = NetcdfFiles.open(firstFile.toString());
			 NetcdfFile secondNetcdf = NetcdfFiles.open(secondFile.toString());
			 NetcdfFile concatenatedNetcdf = NetcdfFiles.open(targetFile.toString())) {
			double[] firstValues = (double[]) Objects.requireNonNull(firstNetcdf.findVariable(variableName)).read().get1DJavaArray(DataType.DOUBLE);
			double[] secondValues = (double[]) Objects.requireNonNull(secondNetcdf.findVariable(variableName)).read().get1DJavaArray(DataType.DOUBLE);
			double[] concatenatedValues = (double[]) Objects.requireNonNull(concatenatedNetcdf.findVariable(variableName)).read().get1DJavaArray(DataType.DOUBLE);
			assertEquals(firstValues.length + secondValues.length - 125, concatenatedValues.length);
			for (int i = 0; i < split; i++) {
				assertEquals(firstValues[i], concatenatedValues[i]);
			}
			for (int i = split; i < concatenatedValues.length; i++) {
				assertEquals(secondValues[i - split], concatenatedValues[i]);
			}
		}
	}

	public void testNetcdfFixedTimeDimensionUseOldValueOnOverlapConcatenation() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateOldValueOnOverlap");
		File firstFile = new File(testRunDataSubDir, "rrunoff_201250.nc");
		File targetFile = new File(testRunDataSubDir, "concatenated_rrrunoff_oldValueOverlap.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath(), "useOldValueOnOverlap=true"});
		assertTrue(targetFile.exists());
		File secondFile = new File(testRunDataSubDir, "rrunoff_201257.nc");
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), secondFile.getAbsolutePath(), "useOldValueOnOverlap=true"});
		assertTrue(targetFile.exists());

		checkConcatenatedValues(firstFile, targetFile, secondFile, 7);
	}

	public void testConcatenationOfManyFiles() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateStress");
		File inputDirectory = new File(testRunDataSubDir, "input");
		File targetFile = new File(testRunDataSubDir, "concatenated_averaged.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		File[] files = Objects.requireNonNull(inputDirectory.listFiles());
		Arrays.sort(files);
		for(File file : files) {
			NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), file.getAbsolutePath(), "useOldValueOnOverlap=true"});
		}
		File expectedFile = new File(testRunDataSubDir, "expected_averaged.nc");
		NetcdfFile expectedNetcdf = null;
		NetcdfFile targetNetcdf = null;
		try {
			expectedNetcdf = NetcdfFiles.open(expectedFile.toString());
			long expected = Objects.requireNonNull(expectedNetcdf.findVariable("time")).read().getSize();
			targetNetcdf = NetcdfFiles.open(targetFile.toString());
			long target = Objects.requireNonNull(targetNetcdf.findVariable("time")).read().getSize();
			assertEquals("Time data matches", expected, target);
		} finally {
			if (expectedNetcdf != null) expectedNetcdf.close();
			if (targetNetcdf != null) targetNetcdf.close();
		}
	}

	public void testConcatenationOfManyFilesInOneGo() throws IOException {
		File testRunDataSubDir = new File(this.testRunDataDir, "concatenateStress");
		File inputDirectory = new File(testRunDataSubDir, "input");
		File targetFile = new File(testRunDataSubDir, "concatenated_averaged.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);

		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), inputDirectory.getAbsolutePath(), "useOldValueOnOverlap=true"});
		assertTrue(targetFile.exists());
		File expectedFile = new File(testRunDataSubDir, "expected_averaged.nc");
		NetcdfFile expectedNetcdf = null;
		NetcdfFile targetNetcdf = null;
		try {
			expectedNetcdf = NetcdfFiles.open(expectedFile.toString());
			long expected = Objects.requireNonNull(expectedNetcdf.findVariable("time")).read().getSize();
			targetNetcdf = NetcdfFiles.open(targetFile.toString());
			long target = Objects.requireNonNull(targetNetcdf.findVariable("time")).read().getSize();
			assertEquals("Time data matches", expected, target);
		} finally {
			if (expectedNetcdf != null) expectedNetcdf.close();
			if (targetNetcdf != null) targetNetcdf.close();
		}
	}

	private void checkConcatenatedValues(File firstFile, File targetFile, File secondFile, int split) throws IOException {
		try (NetcdfFile firstNetcdf = NetcdfFiles.open(firstFile.toString());
			 NetcdfFile secondNetcdf = NetcdfFiles.open(secondFile.toString());
			 NetcdfFile concatenatedNetcdf = NetcdfFiles.open(targetFile.toString())) {
			double[] firstValues = (double[]) Objects.requireNonNull(firstNetcdf.findVariable("Runoff")).read().get1DJavaArray(DataType.DOUBLE);
			double[] secondValues = (double[]) Objects.requireNonNull(secondNetcdf.findVariable("Runoff")).read().get1DJavaArray(DataType.DOUBLE);
			double[] concatenatedValues = (double[]) Objects.requireNonNull(concatenatedNetcdf.findVariable("Runoff")).read().get1DJavaArray(DataType.DOUBLE);
			assertEquals(firstValues.length + secondValues.length - 1, concatenatedValues.length);
			for (int i = 0; i < split; i++) {
				assertEquals(firstValues[i], concatenatedValues[i]);
			}
			for (int i = split; i < concatenatedValues.length; i++) {
				assertEquals(secondValues[i - 6], concatenatedValues[i]);
			}
		}
	}
}
