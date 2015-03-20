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
package org.openda.exchange.ioobjects;
import java.io.File;
import java.io.IOException;
import org.openda.exchange.iotools.ioDumper;
import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class ioDumperTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(ioDumperTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testIoDumpSeriesFromObject(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for NoosTimeSeriesIoObject with ioObject constructor");
		System.out.println("==============================================================================");
		NoosTimeSeriesIoObject noosIO = new NoosTimeSeriesIoObject();
		noosIO.initialize(this.testRunDataDir, "NoosTimeSeriesIoObjectTestData1.txt");

		ioDumper dumper = new ioDumper(noosIO);
		dumper.dump();
	}

	public void testIoDumpSeriesFromFile(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for NoosTimeSeriesIoObject with initialize and className");
		System.out.println("==============================================================================");
		ioDumper dumper = new ioDumper(testRunDataDir,"NoosTimeSeriesIoObjectTestData1.txt",
		"org.openda.exchange.ioobjects.NoosTimeSeriesIoObject",new String[0]);
		dumper.dump();
	}

	public void testIoDumpSeriesFromMain(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for NoosTimeSeriesIoObject with the static main");
		System.out.println("==============================================================================");
		File file = new File(testRunDataDir,"NoosTimeSeriesIoObjectTestData1.txt");
		try{
			ioDumper.main(new String[]{file.getAbsolutePath(),"org.openda.exchange.ioobjects.NoosTimeSeriesIoObject"});
		}catch (Exception e) {
			assertTrue(e instanceof RuntimeException);
		}
	}
}
