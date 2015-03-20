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
package org.openda.blackbox.config;
import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Unit test for testing BBCheckOutput
 */
public class BBCheckActionTest extends TestCase {
	private static String LOG_FILENAME = "action/output.log";
	private static String EXPECT_STRING = "test";
	private static String EXPECT_RETURN = "0";
	private OpenDaTestSupport testData;
	private File testRunDataDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(BBCheckActionTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testCheckFileExists() {
		AliasDefinitions aliasDefinitions = new AliasDefinitions();
		BBCheckOutput checkOutput = new BBCheckOutput(LOG_FILENAME,"",aliasDefinitions);
		String message = "";
		try {
			checkOutput.performCheck(testRunDataDir);
		} catch (IllegalStateException e) {
			message = e.getMessage();
		}
		assertTrue("PerformCheck not correct.","".equals(message));
	}

	public void testCheckInFile() {
		AliasDefinitions aliasDefinitions = new AliasDefinitions();
		BBCheckOutput checkOutput = new BBCheckOutput(LOG_FILENAME,EXPECT_STRING,aliasDefinitions);
		String message = "";
		try {
			checkOutput.performCheck(testRunDataDir);
		} catch (IllegalStateException e) {
			message = e.getMessage();
		}
		assertTrue("PerformCheck not correct.","".equals(message));
	}

	public void testCheckReturnStatus() {
		AliasDefinitions aliasDefinitions = new AliasDefinitions();
		BBCheckReturnStatus checkReturn = new BBCheckReturnStatus(EXPECT_RETURN,aliasDefinitions);
		String message = "";
		try {
			checkReturn.performCheck(0);
		} catch (IllegalStateException e) {
			message = e.getMessage();
		}
		assertTrue("PerformCheck not correct.","".equals(message));
	}

	public void testCheckReturnNullStatus() {
		AliasDefinitions aliasDefinitions = new AliasDefinitions();
		BBCheckReturnStatus checkReturn = new BBCheckReturnStatus(EXPECT_RETURN,aliasDefinitions);
		String message = "";
		try {
			checkReturn.performCheck(null);
		} catch  (IllegalStateException e) {
			message = e.getMessage();
		}
		assertTrue(message.contains("action has no return value"));
	}

}
