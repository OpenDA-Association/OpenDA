/* MOD_V2.0
* Copyright (c) 2013 OpenDA Association
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
package org.openda.model_dflowfm;

import junit.framework.TestCase;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IDataObject;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.OpenDaTestSupport;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Tests reading from and writing to the D-Flow FM restart file dflowfm_map.nc.
 */
public class DFlowFMRestartTest extends TestCase {

	OpenDaTestSupport testData = null;
    private File testRunDataRestartFileDir;
	private File testCopyDir;


	protected void setUp() throws IOException {
	    testData = new OpenDaTestSupport(DFlowFMRestartTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "Restartfile");
		testCopyDir = new File(testRunDataRestartFileDir,"copy");
	}

// Commented out because no small restart file available, but below test should work with any
	public void testRestartFileInsteadOfMap() {
		IDataObject initialRestartFile = new DFlowFMRestartFileWrapper();
		String[] args = new String[]{"dcsmv5_20070104_000000_rst.nc"};

		initialRestartFile.initialize(testRunDataRestartFileDir, args);

		DFlowFMExchangeItem initialS1 = (DFlowFMExchangeItem) initialRestartFile.getDataObjectExchangeItem("s1");
		IGeometryInfo geometryInfoS1 = initialS1.getGeometryInfo();
		assertNotNull(geometryInfoS1);
		assertTrue(geometryInfoS1 instanceof DFlowFMMapExchangeItemGeometryInfo);
		assertEquals(17843, ((DFlowFMMapExchangeItemGeometryInfo) geometryInfoS1).getSize());
		double[] initialValuesAsDoubles = initialS1.getValuesAsDoubles().clone();
		double[] axpyValues = new double[initialValuesAsDoubles.length];
		Arrays.fill(axpyValues, 1);
		initialS1.axpyOnValues(0.1, axpyValues);

		DFlowFMExchangeItem initialUnorm = (DFlowFMExchangeItem) initialRestartFile.getDataObjectExchangeItem("unorm");
		IGeometryInfo geometryInfoUnorm = initialUnorm.getGeometryInfo();
		assertNotNull(geometryInfoUnorm);
		assertTrue(geometryInfoUnorm instanceof DFlowFMMapExchangeItemGeometryInfo);
		assertEquals(34921, ((DFlowFMMapExchangeItemGeometryInfo) geometryInfoUnorm).getSize());
		
		initialRestartFile.finish();

		IDataObject finishedRestartFile = new DFlowFMRestartFileWrapper();
		finishedRestartFile.initialize(testRunDataRestartFileDir, args);
		DFlowFMExchangeItem finishedS1 = (DFlowFMExchangeItem) finishedRestartFile.getDataObjectExchangeItem("s1");
		double[] finishedS1ValuesAsDoubles = finishedS1.getValuesAsDoubles();
		for (int i = 0; i < initialValuesAsDoubles.length; i++) {
			assertEquals(initialValuesAsDoubles[i] + 0.1, finishedS1ValuesAsDoubles[i], 0.000001);
		}
	}

// Commented out because no small restart file available, but below test should work with any
	public void testRestartFileTranspose3DVariableDimensions() {
		IDataObject initialRestartFile = new DFlowFMRestartFileWrapper();
		String[] transposeArgs = new String[]{"FlowFM3D_00000000_000000_rst.nc", "transposeDimensions=nFlowElem,laydim"};

		initialRestartFile.initialize(testRunDataRestartFileDir, transposeArgs);

		IExchangeItem tem1Transposed = initialRestartFile.getDataObjectExchangeItem("tem1");
		double[] transposedValuesAsDoubles = tem1Transposed.getValuesAsDoubles().clone();
		DFlowFMMapExchangeItemGeometryInfo transposedFmGeometry = (DFlowFMMapExchangeItemGeometryInfo) tem1Transposed.getGeometryInfo();
		int transposedFmGeometrySize = transposedFmGeometry.getSize();
		assertEquals(transposedValuesAsDoubles.length, transposedFmGeometrySize);
		assertEquals(47.915, transposedFmGeometry.getXCoord(25), 0.0001);
		assertEquals(29.425, transposedFmGeometry.getYCoord(25), 0.0001);
		assertEquals(47.915, transposedFmGeometry.getXCoord(2), 0.0001);
		assertEquals(29.475, transposedFmGeometry.getYCoord(2), 0.0001);

		initialRestartFile.finish();

		IDataObject finishedRestartFile = new DFlowFMRestartFileWrapper();
		finishedRestartFile.initialize(testRunDataRestartFileDir, transposeArgs);
		double[] finishedTransposedValuesAsDoubles = finishedRestartFile.getDataObjectExchangeItem("tem1").getValuesAsDoubles();
		for (int i = 0; i < transposedValuesAsDoubles.length; i++) {
			assertEquals(transposedValuesAsDoubles[i], finishedTransposedValuesAsDoubles[i], 0.000001);
		}

		IDataObject normalRestartFile = new DFlowFMRestartFileWrapper();
		String[] normalArgs = new String[]{"FlowFM3D_00000000_000000_rst.nc"};
		normalRestartFile.initialize(testRunDataRestartFileDir, normalArgs);
		IExchangeItem tem1Normal = normalRestartFile.getDataObjectExchangeItem("tem1");
		DFlowFMMapExchangeItemGeometryInfo normalGeometryInfo = (DFlowFMMapExchangeItemGeometryInfo) tem1Normal.getGeometryInfo();
		int normalGeometrySize = normalGeometryInfo.getSize();
		assertEquals(transposedValuesAsDoubles.length, normalGeometrySize);
		assertEquals(47.915, normalGeometryInfo.getXCoord(1), 0.001);
		assertEquals(29.425, normalGeometryInfo.getYCoord(1), 0.001);
		assertEquals(47.915, normalGeometryInfo.getXCoord(10), 0.001);
		assertEquals(29.475, normalGeometryInfo.getYCoord(10), 0.001);
		double[] normalValuesAsDoubles = tem1Normal.getValuesAsDoubles();
		for (int i = 0; i < 25; i++) {
			for (int j = 0; j < 5; j++) {
				int normalIndex = i * 5 + j;
				int transposedIndex = j * 25 + i;
				assertEquals(transposedValuesAsDoubles[transposedIndex], normalValuesAsDoubles[normalIndex], 0.000001);
			}
		}
	}

	public void testReadInput() {

		IDataObject RestartFile = new DFlowFMRestartFileWrapper();
		String[] args = new String[]{"dflowfm_map.nc"};

		RestartFile.initialize(testRunDataRestartFileDir, args);

		String[] exchangeItemIDs= RestartFile.getExchangeItemIDs();

		// loop over all exchangeItems and check some for Quantity and Unit
		for (String id : exchangeItemIDs) {
			if (id.matches("time")) {
			    DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assert(ex.getUnitId().startsWith("seconds since"));
			}
			if (id.matches("timestep")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()",ex.getQuantityId(),"timestep");
				assert(ex.getUnitId().matches("seconds"));
			}

			if (id.matches("s1")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_surface_height");
                assert(ex.getUnitId().matches("m"));
			}

			if (id.matches("s0")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_surface_height");
				assert(ex.getUnitId().matches("m"));
			}

			if (id.matches("taus")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"taucurrent");
				assert(ex.getUnitId().matches("N/m2"));
			}

			if (id.matches("unorm")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_water_speed");
				assert(ex.getUnitId().matches("m s-1"));

			}

			if (id.matches("u0")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				assertEquals("ex.getQuantityId()", ex.getQuantityId(),"sea_water_speed_old");
				assert(ex.getUnitId().matches("m s-1"));
			}

		}
	}

	public void testWriteInput() {

		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "Restartfile");
		IDataObject RestartFile = new DFlowFMRestartFileWrapper();

		// Copy the original file to a "copy" directory to prevent overwriting
		File original = new File(testRunDataRestartFileDir,"dflowfm_map.nc");
		File copy = new File(testCopyDir,"dflowfm_map.nc");
		try {
			BBUtils.copyFile(original, copy);
			} catch (IOException e) {
				throw new RuntimeException("Could not copy files");
		}
		String[] args = new String[]{"dflowfm_map.nc","1200"};
	    RestartFile.initialize(testRunDataRestartFileDir, args);

		String[] exchangeItemIDs= RestartFile.getExchangeItemIDs();

		//change a value
		for (String id : exchangeItemIDs) {
//			System.out.println("id = "+id);
			if (id.matches("s1")) {
				DFlowFMExchangeItem ex = (DFlowFMExchangeItem) RestartFile.getDataObjectExchangeItem(id);
				double values[] = ex.getValuesAsDoubles();
				for (int i=0; i < values.length ; i++) {
					values[i] = 100.0;
				}
				RestartFile.getDataObjectExchangeItem(id).setValuesAsDoubles(values);
				IGeometryInfo geometryInfo = ex.getGeometryInfo();
				assertNotNull(geometryInfo);
				assertTrue(geometryInfo instanceof DFlowFMMapExchangeItemGeometryInfo);
				DFlowFMMapExchangeItemGeometryInfo dFlowFMGeometry = (DFlowFMMapExchangeItemGeometryInfo) geometryInfo;
				assertEquals(87, dFlowFMGeometry.getSize());
				assertFalse(dFlowFMGeometry.is3D());
				assertEquals(0, dFlowFMGeometry.getZCoord(0), 0.001);
				double xCoord0 = dFlowFMGeometry.getXCoord(0);
				assertEquals(157080.05234218697, xCoord0, 0.001);
				assertEquals(170668.49413182843, dFlowFMGeometry.getXCoord(55), 0.001);
				assertEquals(179393.00276083976, dFlowFMGeometry.getXCoord(86), 0.001);
				double yCoord0 = dFlowFMGeometry.getYCoord(0);
				assertEquals(429554.3158721777, yCoord0, 0.001);
				assertEquals(434611.9683101297, dFlowFMGeometry.getYCoord(55), 0.001);
				assertEquals(433143.09523854207, dFlowFMGeometry.getYCoord(86), 0.001);
				IArray distanceToPointArray = dFlowFMGeometry.distanceToPoint(xCoord0, yCoord0, 0);
				assertEquals(0.0, distanceToPointArray.getValueAsDouble(0), 0.01);
				assertEquals(14499.158542949863, distanceToPointArray.getValueAsDouble(55), 0.01);
			}
		}
		// write file
		RestartFile.finish();

		// Reread file and compare with copy
		RestartFile.initialize(testRunDataRestartFileDir, args);

		IDataObject RestartCopy = new DFlowFMRestartFileWrapper();
		RestartCopy.initialize(testCopyDir, args);

		double orig[] = RestartFile.getDataObjectExchangeItem("s1").getValuesAsDoubles();
		double[] newval = RestartCopy.getDataObjectExchangeItem("s1").getValuesAsDoubles();
		assertTrue(Math.abs(orig[0]-newval[0])>1.e-20);
		RestartFile.finish();
		RestartCopy.finish();
	}
}
