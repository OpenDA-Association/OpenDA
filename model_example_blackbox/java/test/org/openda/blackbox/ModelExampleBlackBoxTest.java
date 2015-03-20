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
package org.openda.blackbox;

import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;

import java.io.File;
import java.io.IOException;

public class ModelExampleBlackBoxTest extends TestCase {

	protected void setUp() throws IOException {
	}

	public static void testDummy() {
		// No action. Test only exist to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	public void tstModelExampleBlackBox_1() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\src\\openda_1\\public\\model_example_blackbox\\tests\\blackbox_example\\Dud.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}
}
