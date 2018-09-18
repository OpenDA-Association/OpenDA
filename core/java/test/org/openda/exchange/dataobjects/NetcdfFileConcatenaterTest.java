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
import ucar.nc2.NetcdfFile;

import java.io.File;
import java.io.IOException;

public class NetcdfFileConcatenaterTest extends TestCase {
	private File testRunDataDir;

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
		long size1 = NetcdfFile.open(firstFile.toString()).findVariable("time").read().getSize();
		long size2 = NetcdfFile.open(secondFile.toString()).findVariable("time").read().getSize();
		long size3 = NetcdfFile.open(targetFile.toString()).findVariable("time").read().getSize();
		assertEquals(size3, size1 + size2);
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
		long size1 = NetcdfFile.open(firstFile.toString()).findVariable("time").read().getSize();
		long size2 = NetcdfFile.open(secondFile.toString()).findVariable("time").read().getSize();
		long size3 = NetcdfFile.open(targetFile.toString()).findVariable("time").read().getSize();
		assertEquals(size3, size1 + size2);
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

	private void checkConcatenatedValues(File firstFile, File targetFile, File secondFile, int split) throws IOException {
		NetcdfFile firstNetcdf = null;
		NetcdfFile secondNetcdf = null;
		NetcdfFile concatenatedNetcdf = null;
		try {
			firstNetcdf = NetcdfFile.open(firstFile.toString());
			secondNetcdf = NetcdfFile.open(secondFile.toString());
			concatenatedNetcdf = NetcdfFile.open(targetFile.toString());
			double[] firstValues = (double[]) firstNetcdf.findVariable("Runoff").read().copyTo1DJavaArray();
			double[] secondValues = (double[]) secondNetcdf.findVariable("Runoff").read().copyTo1DJavaArray();
			double[] concatenatedValues = (double[]) concatenatedNetcdf.findVariable("Runoff").read().copyTo1DJavaArray();
			assertEquals(firstValues.length + secondValues.length - 1, concatenatedValues.length);
			for (int i = 0; i < split; i++) {
				assertEquals(firstValues[i], concatenatedValues[i]);
			}
			for (int i = split; i < concatenatedValues.length; i++) {
				assertEquals(secondValues[i - 6], concatenatedValues[i]);
			}
		} finally {
			if (firstNetcdf != null) firstNetcdf.close();
			if (secondNetcdf != null) secondNetcdf.close();
			if (concatenatedNetcdf != null) concatenatedNetcdf.close();
		}
	}
}
