/* OpenDA v2.4.1 
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

package org.openda.utils.geometry;

import junit.framework.TestCase;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.IrregularGridGeometryInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Array;
import org.openda.utils.Vector;

import java.util.Arrays;

public class GeometryUtilsTest extends TestCase {

	public void testGetObservedValuesBilinearInterpolationRectangularGrid() {
		//square grid.
		IVector lon = new Vector(new double[]{0, 1, 2});
		IVector lat = new Vector(new double[]{50, 51, 52});
		IArray modelLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray modelLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		ArrayGeometryInfo modelGeometryInfo = new ArrayGeometryInfo(modelLat, new int[]{0}, null, modelLon, new int[]{1}, null, null, null, null);
		double[] modelValues = new double[]{1, 2, 3, 4, 5, 6, 7, 8, 9};

		//square grid, shifted.
		lon = new Vector(new double[]{0.5, 1.5, 2.5});
		lat = new Vector(new double[]{49.5, 50.5, 51.5});
		IArray obsLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray obsLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		//need one coordinate for each grid cell.
		ArrayGeometryInfo observationsGeometryInfo = new ArrayGeometryInfo(obsLat, new int[]{0}, null, obsLon, new int[]{1}, null, null, null, null).toCurvilinearGeometryInfo();

		IVector observedModelValues = GeometryUtils.getObservedValuesBilinearInterpolation(observationsGeometryInfo.getCellXCoordinates(), observationsGeometryInfo.getCellYCoordinates(), modelGeometryInfo, modelValues);
		assertEquals(modelValues.length, observedModelValues.getSize());
		assertTrue(Double.isNaN(observedModelValues.getValue(0)));
		assertTrue(Double.isNaN(observedModelValues.getValue(1)));
		assertTrue(Double.isNaN(observedModelValues.getValue(2)));
		assertEquals(3, observedModelValues.getValue(3), 0.01);
		assertEquals(4, observedModelValues.getValue(4), 0.01);
		assertTrue(Double.isNaN(observedModelValues.getValue(5)));
		assertEquals(6, observedModelValues.getValue(6), 0.01);
		assertEquals(7, observedModelValues.getValue(7), 0.01);
		assertTrue(Double.isNaN(observedModelValues.getValue(8)));
	}

	public void testGetObservedValuesBilinearInterpolationIrregularGrid() {
		//square grid.
		IVector lon = new Vector(new double[]{0, 1, 2});
		IVector lat = new Vector(new double[]{50, 51, 52});
		IArray modelLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray modelLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		IGeometryInfo modelGeometryInfo = new ArrayGeometryInfo(modelLat, new int[]{0}, null, modelLon, new int[]{1}, null, null, null, null);
		double[] modelValues = new double[]{1, 2, 3, 4, 5, 6, 7, 8, 9};

		//list of observation points.
		double[] x = new double[]{-0.5, -0.5, -0.5, -0.5, -0.5,   0,   0,   0,   0,   0,   1,  1,  1,  1,  1,   2,  2,  2,  2,  2,  90, 90, 90, 90, 90};
		double[] y = new double[]{ -10,   50,   51,   52,   90, -10,  50,  51,  52,  90, -10, 50, 51, 52, 90, -10, 50, 51, 52, 90, -10, 50, 51, 52, 90};
		IrregularGridGeometryInfo observationsGeometryInfo = new IrregularGridGeometryInfo(x.length, x, y);

		IVector observedModelValues = GeometryUtils.getObservedValuesBilinearInterpolation(observationsGeometryInfo.getXCoordinates(), observationsGeometryInfo.getYCoordinates(), modelGeometryInfo, modelValues);
		assertEquals(x.length, observedModelValues.getSize());

		assertTrue(Double.isNaN(observedModelValues.getValue(0)));
		assertTrue(Double.isNaN(observedModelValues.getValue(1)));
		assertTrue(Double.isNaN(observedModelValues.getValue(2)));
		assertTrue(Double.isNaN(observedModelValues.getValue(3)));
		assertTrue(Double.isNaN(observedModelValues.getValue(4)));

		assertTrue(Double.isNaN(observedModelValues.getValue(5)));
		assertEquals(1, observedModelValues.getValue(6), 0.01);
		assertEquals(4, observedModelValues.getValue(7), 0.01);
		assertTrue(Double.isNaN(observedModelValues.getValue(8)));
		assertTrue(Double.isNaN(observedModelValues.getValue(9)));

		assertTrue(Double.isNaN(observedModelValues.getValue(10)));
		assertEquals(2, observedModelValues.getValue(11), 0.01);
		assertEquals(5, observedModelValues.getValue(12), 0.01);
		assertTrue(Double.isNaN(observedModelValues.getValue(13)));
		assertTrue(Double.isNaN(observedModelValues.getValue(14)));

		assertTrue(Double.isNaN(observedModelValues.getValue(15)));
		assertTrue(Double.isNaN(observedModelValues.getValue(16)));
		assertTrue(Double.isNaN(observedModelValues.getValue(17)));
		assertTrue(Double.isNaN(observedModelValues.getValue(18)));
		assertTrue(Double.isNaN(observedModelValues.getValue(19)));

		assertTrue(Double.isNaN(observedModelValues.getValue(20)));
		assertTrue(Double.isNaN(observedModelValues.getValue(21)));
		assertTrue(Double.isNaN(observedModelValues.getValue(22)));
		assertTrue(Double.isNaN(observedModelValues.getValue(23)));
		assertTrue(Double.isNaN(observedModelValues.getValue(24)));
	}

	public void testDistanceInMeters() {
		assertEquals(20000 * 1000, GeometryUtils.distanceInMeters(0, 0, 0, 180), 5 * 1000);
		assertEquals(5000 * 1000, GeometryUtils.distanceInMeters(0, 0, 45, 0), 2 * 1000);
		//Note: near the pole the distance around the pole is calculated, instead of the distance across the pole. This error seems to be smaller than 50%.
		assertEquals(10000 * 1000, GeometryUtils.distanceInMeters(45, 0, 45, 180), 5000 * 1000);
	}

	public void testGetLocalizationWeights1CurvilinearGrid() {
		//diagonal line from southwest to northeast.
		IVector lon = new Vector(new double[]{0, 1, 2});
		IVector lat = new Vector(new double[]{50, 51, 52});

		//square grid.
		IArray stateLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray stateLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		ArrayGeometryInfo stateGeometryInfo = new ArrayGeometryInfo(stateLat, new int[]{0}, null, stateLon, new int[]{1}, null, null, null, null).toCurvilinearGeometryInfo();
		assertTrue(stateGeometryInfo.isCurvilinear());

		IVector[] weightVectors = GeometryUtils.getLocalizationWeights(lon, lat, stateGeometryInfo, 100 * 1000);
		assertEquals(lon.getSize(), weightVectors.length);
		assertEquals(lon.getSize()*lat.getSize(), weightVectors[0].getSize());
		assertEquals(1.0, weightVectors[0].getValue(0), 0.01);
		assertEquals(0.46, weightVectors[0].getValue(1), 0.01);
		assertEquals(0.027, weightVectors[0].getValue(2), 0.001);
		assertEquals(0.14, weightVectors[0].getValue(3), 0.01);
		assertEquals(0.053, weightVectors[0].getValue(4), 0.001);
		assertEquals(0.0, weightVectors[0].getValue(5), 0.01);
		assertEquals(0.0, weightVectors[0].getValue(6), 0.01);
		assertEquals(0.0, weightVectors[0].getValue(7), 0.01);
		assertEquals(0.0, weightVectors[0].getValue(8), 0.01);
	}

	public void testGetLocalizationWeights1RectangularGrid() {
		//diagonal line from southwest to northeast.
		IVector lon = new Vector(new double[]{0, 1, 2});
		IVector lat = new Vector(new double[]{50, 51, 52});

		//square grid.
		IArray stateLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray stateLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		ArrayGeometryInfo stateGeometryInfo = new ArrayGeometryInfo(stateLat, new int[]{0}, null, stateLon, new int[]{1}, null, null, null, null);
		assertTrue(stateGeometryInfo.isRectangular());

		IVector[] weightVectors = GeometryUtils.getLocalizationWeights(lon, lat, stateGeometryInfo, 100 * 1000);
		assertEquals(lon.getSize(), weightVectors.length);
		assertEquals(lon.getSize()*lat.getSize(), weightVectors[0].getSize());
		assertEquals(1.0, weightVectors[0].getValue(0), 0.01);
		assertEquals(0.46, weightVectors[0].getValue(1), 0.01);
		assertEquals(0.027, weightVectors[0].getValue(2), 0.001);
		assertEquals(0.14, weightVectors[0].getValue(3), 0.01);
		assertEquals(0.053, weightVectors[0].getValue(4), 0.001);
		assertEquals(0.0, weightVectors[0].getValue(5), 0.01);
		assertEquals(0.0, weightVectors[0].getValue(6), 0.01);
		assertEquals(0.0, weightVectors[0].getValue(7), 0.01);
		assertEquals(0.0, weightVectors[0].getValue(8), 0.01);
	}

	public void testGetLocalizationWeights2CurvilinearGrid() {
		//diagonal line from southwest to northeast.
		IVector lon = new Vector(new double[]{0, 0.5, 1});
		IVector lat = new Vector(new double[]{-0.5, 0, 0.5});

		//square grid.
		IArray stateLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray stateLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		ArrayGeometryInfo stateGeometryInfo = new ArrayGeometryInfo(stateLat, new int[]{0}, null, stateLon, new int[]{1}, null, null, null, null).toCurvilinearGeometryInfo();
		assertTrue(stateGeometryInfo.isCurvilinear());

		IVector[] weightVectors = GeometryUtils.getLocalizationWeights(lon, lat, stateGeometryInfo, 100 * 1000);
		assertEquals(lon.getSize(), weightVectors.length);
		assertEquals(lon.getSize()*lat.getSize(), weightVectors[0].getSize());
		assertEquals(0.390, weightVectors[1].getValue(0), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(1), 0.001);
		assertEquals(0.390, weightVectors[1].getValue(2), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(3), 0.001);
		assertEquals(1.0, weightVectors[1].getValue(4), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(5), 0.001);
		assertEquals(0.390, weightVectors[1].getValue(6), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(7), 0.001);
		assertEquals(0.390, weightVectors[1].getValue(8), 0.001);
	}

	public void testGetLocalizationWeights2RectangularGrid() {
		//diagonal line from southwest to northeast.
		IVector lon = new Vector(new double[]{0, 0.5, 1});
		IVector lat = new Vector(new double[]{-0.5, 0, 0.5});

		//square grid.
		IArray stateLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray stateLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		ArrayGeometryInfo stateGeometryInfo = new ArrayGeometryInfo(stateLat, new int[]{0}, null, stateLon, new int[]{1}, null, null, null, null);
		assertTrue(stateGeometryInfo.isRectangular());

		IVector[] weightVectors = GeometryUtils.getLocalizationWeights(lon, lat, stateGeometryInfo, 100 * 1000);
		assertEquals(lon.getSize(), weightVectors.length);
		assertEquals(lon.getSize()*lat.getSize(), weightVectors[0].getSize());
		assertEquals(0.390, weightVectors[1].getValue(0), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(1), 0.001);
		assertEquals(0.390, weightVectors[1].getValue(2), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(3), 0.001);
		assertEquals(1.0, weightVectors[1].getValue(4), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(5), 0.001);
		assertEquals(0.390, weightVectors[1].getValue(6), 0.001);
		assertEquals(0.627, weightVectors[1].getValue(7), 0.001);
		assertEquals(0.390, weightVectors[1].getValue(8), 0.001);
	}

	public void testCalculateCohnWeight() {
		assertEquals(1.0, GeometryUtils.calculateCohnWeight(0, 1), 0.01);
		assertEquals(0.91, GeometryUtils.calculateCohnWeight(0.25, 1), 0.01);
		assertEquals(0.68, GeometryUtils.calculateCohnWeight(0.5, 1), 0.01);
		assertEquals(0.43, GeometryUtils.calculateCohnWeight(0.75, 1), 0.01);
		assertEquals(0.21, GeometryUtils.calculateCohnWeight(1.0, 1), 0.01);
		assertEquals(0.075, GeometryUtils.calculateCohnWeight(1.25, 1), 0.001);
		assertEquals(0.016, GeometryUtils.calculateCohnWeight(1.5, 1), 0.001);
		assertEquals(0.0011, GeometryUtils.calculateCohnWeight(1.75, 1), 0.0001);
		assertEquals(0.0, GeometryUtils.calculateCohnWeight(2.0, 1), 1e-6);
		assertEquals(0.0, GeometryUtils.calculateCohnWeight(2.25, 1), 1e-6);
		assertEquals(0.0, GeometryUtils.calculateCohnWeight(2.5, 1), 1e-6);
		assertEquals(0.0, GeometryUtils.calculateCohnWeight(1e6, 1), 1e-6);
	}

	public void testToCurvilinearGeometryInfo() {
		//diagonal line from southwest to northeast.
		IVector lon = new Vector(new double[]{0, 1, 2});
		IVector lat = new Vector(new double[]{50, 51, 52});

		//square grid.
		IArray stateLon = new Array(lon.getValues(), new int[]{lon.getSize()}, true);
		IArray stateLat = new Array(lat.getValues(), new int[]{lat.getSize()}, true);
		ArrayGeometryInfo geometryInfo = new ArrayGeometryInfo(stateLat, new int[]{0}, null, stateLon, new int[]{1}, null, null, null, null);
		assertTrue(geometryInfo.isRectangular());
		assertFalse(geometryInfo.isCurvilinear());
		assertTrue(Arrays.equals(lon.getValues(), geometryInfo.getColumnXCoordinates().getValues()));
		assertTrue(Arrays.equals(lat.getValues(), geometryInfo.getRowYCoordinates().getValues()));

		geometryInfo = geometryInfo.toCurvilinearGeometryInfo();
		assertTrue(geometryInfo.isCurvilinear());
		assertFalse(geometryInfo.isRectangular());
		assertTrue(Arrays.equals(new double[]{0, 1, 2, 0, 1, 2, 0, 1, 2}, geometryInfo.getCellXCoordinates().getValues()));
		assertTrue(Arrays.equals(new double[]{50, 50, 50, 51, 51, 51, 52, 52, 52}, geometryInfo.getCellYCoordinates().getValues()));

		//if geometryInfo is already curvilinear.
		assertEquals(geometryInfo, geometryInfo.toCurvilinearGeometryInfo());
	}
}
