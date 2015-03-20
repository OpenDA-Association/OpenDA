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
package org.openda.exchange.iotools;
import java.io.File;
import java.io.IOException;

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.iotools.DataDumper;
import org.openda.interfaces.IDataObject;
import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class DataDumperTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(DataDumperTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testDumpNetcdfFromObject(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for NetcdfDataObject with DataDumper constructor.");
		System.out.println("==============================================================================");

		IDataObject netcdfDataObject = new NetcdfDataObject();
		netcdfDataObject.initialize(this.testRunDataDir, new String[]{"fews_wind_small.nc"});
		DataDumper dumper = new DataDumper(netcdfDataObject);
		dumper.setOutputDir(testRunDataDir);
		dumper.dump();
	}

	public void testDumpNetcdfFromMain(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for NetcdfDataObject with DataDumper main method.");
		System.out.println("==============================================================================");

		File file = new File(testRunDataDir, "fews_wind_small.nc");
		DataDumper.main(new String[]{file.getAbsolutePath(), "org.openda.exchange.dataobjects.NetcdfDataObject"});
	}
}
