/* OpenDA v2.4.3 
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

import jep.Jep;
import jep.JepException;
import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

/**
 * Test using JEPP to run Python code from Java.
 * This class uses the JEPP (Java Embedded Python) framework, see http://jepp.sourceforge.net/
 *
 * For this test to work, CPython needs to be installed.
 * For this test to work, the following folders need to be present in the corresponding environment variables:
 * PATH: folder with jep.dll file (e.g. openda_bin\win64_ifort), folder containing python executable (e.g. c:\Python27_64\anaconda\)
 * PYTHONHOME: folder containing python executable (e.g. c:\Python27_64\anaconda\)
 *
 * This test only works with a 64-bit JDK version 1.7.
 *
 * @author Arno Kockx
 */
public class JeppTest extends TestCase {

	private OpenDaTestSupport testData;
	private File testRunDataDir;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(JeppTest.class, "model_wflow");
		testRunDataDir = testData.getTestRunDataDir();
	}

	private class TestObject {
		String string = null;

		@SuppressWarnings("unused")
		public void setString(String string) {
			this.string = string;
		}

		public String getString() {
			return this.string;
		}
	}

    /**
     * This test class only contains methods for manual testing.
     * Added this empty test method to avoid causing JUnit
     * to throw Exception "No runnable methods".
     */
    public void testEmpty() {
    }

    /**
	 * Test calling a Python script from Java using JEPP.
	 *
	 * @throws JepException
	 */
	//TODO linux
	public void _testPythonScript() throws JepException {
		//new Jep(boolean interactive, String includePath, ClassLoader classLoader). 
		Jep jep = new Jep(false);

		//set a variable that can be accessed from Python.
		jep.set("string1", "Text from Java");

		//set an object that can be accessed from Python as well as from Java.
		TestObject testObject = new TestObject();
		jep.set("object1", testObject);

		//run the script (this should be located in the current directory, or you can use the full path to the script).
		jep.runScript(testRunDataDir.getAbsolutePath() + "/jeppTest/pythonScripts/test_script.py");
		assertEquals("Text from Python", testObject.getString());
		assertEquals("Text from Python", ((TestObject) jep.getValue("object1")).getString());
		jep.close();
	}

	/**
	 * Test calling a Python function from Java using JEPP.
	 *
	 * @throws JepException
	 */
	public void _testPythonFunction() throws JepException, IOException {
		//new Jep(boolean interactive, String includePath, ClassLoader classLoader).
        //Important note: Jep cannot handle parent dir ".." in includePath, so need to pass canonical path.
		Jep jep = new Jep(false, testRunDataDir.getCanonicalPath() + "/jeppTest/pythonScripts");
		jep.eval("from test_function import *");
		//evaluate an expression in Python and return result.
		Integer integer = (Integer) jep.getValue("getInt() + 1");
		jep.close();
		assertEquals(1501, integer.intValue());
	}
}
