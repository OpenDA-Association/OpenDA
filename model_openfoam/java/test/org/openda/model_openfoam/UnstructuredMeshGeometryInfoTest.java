/*
 * Copyright (c) 2019 OpenDA Association
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
package org.openda.model_openfoam;
import junit.framework.TestCase;
import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class UnstructuredMeshGeometryInfoTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(UnstructuredMeshGeometryInfoTest.class,"model_openfoam");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testGetCoordinates()  {

		IQuantityInfo[] coordinates = new QuantityInfo[3];
		coordinates[0] = new QuantityInfo("x","m");
		coordinates[1] = new QuantityInfo("y","m");
		coordinates[2] = new QuantityInfo("z","m");

		//IArray points = Array( );
		IGeometryInfo geometryInfo = new UnstructuredMeshGeometryInfo(coordinates);
	}

	public void testSetCoordinates()  {

	}

	public void testGetPoints()  {

	}

	public void testSetPoints() {

	}
}
