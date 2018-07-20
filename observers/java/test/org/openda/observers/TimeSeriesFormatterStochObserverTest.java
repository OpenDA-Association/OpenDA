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
package org.openda.observers;

import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class TimeSeriesFormatterStochObserverTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(TimeSeriesFormatterStochObserverTest.class,"observers");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testStochObserver() {
		IStochObserver obs1 = new TimeSeriesFormatterStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"TimeSeriesFormatter.xml"});
	}

}


