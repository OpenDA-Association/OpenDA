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

	public void testNetcdfFixedTimeDimensionConcatenation() throws IOException {
		File firstFile = new File(this.testRunDataDir, "toAdd.nc");
		File targetFile = new File(this.testRunDataDir, "concatenated.nc");
		if (targetFile.exists()) BBUtils.deleteFileOrDir(targetFile);
		assertFalse(targetFile.exists());
		NetcdfFileConcatenater.main(new String[]{targetFile.getAbsolutePath(), firstFile.getAbsolutePath()});
		assertTrue(targetFile.exists());
	}
}
