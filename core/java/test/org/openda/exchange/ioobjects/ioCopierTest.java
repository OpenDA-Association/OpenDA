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

import org.openda.exchange.iotools.ioCopier;
import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class ioCopierTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(ioCopierTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testIoCopySeriesFromObject(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for NoosTimeSeriesIoObject with ioObject constructor");
		System.out.println("==============================================================================");
		NoosTimeSeriesIoObject input = new NoosTimeSeriesIoObject();
		input.initialize(this.testRunDataDir, "NoosTimeSeriesInput.txt");
		NoosTimeSeriesIoObject output = new NoosTimeSeriesIoObject();
		output.initialize(this.testRunDataDir, "NoosTimeSeriesOutput.txt");

		ioCopier copier = new ioCopier(input,output);
		copier.copyValuesForNamedItems("hoekvanholland.waterlevel", "hoekvanholland.waterlevel");
		output.finish();
		
		assertTrue(this.testData.FilesAreIdentical(new File(this.testRunDataDir, "NoosTimeSeriesOutput.txt"),
				new File(this.testRunDataDir, "NoosTimeSeriesOutput_ref.txt"),4));
		
		//ioDumper dumper = new ioDumper(input);
		//dumper.dump();
	}
}
