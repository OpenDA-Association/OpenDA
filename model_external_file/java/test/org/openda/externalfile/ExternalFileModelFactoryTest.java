/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.externalfile;
import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class ExternalFileModelFactoryTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() {
		OpenDaTestSupport testData = new OpenDaTestSupport(ExternalFileModelFactoryTest.class, "model_external_file");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testReadConfig() {
		ExternalFileModelFactory externalModelStochModelFactory = new ExternalFileModelFactory();
		externalModelStochModelFactory.initialize(testRunDataDir, new String[]{"ExternalFileModelFactory.xml"});
		ExternalFileModelInstance modelInstance = externalModelStochModelFactory.getInstance(new String[0], IStochModelFactory.OutputLevel.Suppress);

		IVector parameters = modelInstance.getParameters();
		assertEquals(3, parameters.getSize());
		assertEquals(10.0, parameters.getValue(0));
		assertEquals(20.0, parameters.getValue(1));
		assertEquals(30.0, parameters.getValue(2));
	}

}
