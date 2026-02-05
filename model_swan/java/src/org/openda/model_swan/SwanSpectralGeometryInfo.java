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

package org.openda.model_swan;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.Array;

/**
 * Geometry info for SWAN spectral state files.
 * Maps the 4D spectral structure (nLocations × nFreq × nDir) to spatial coordinates.
 * All spectral bins at the same location return the same distance.
 *
 * @author korte_es
 *
 */
public class SwanSpectralGeometryInfo implements IGeometryInfo {

	private final int nLocations;
	private final double[] locationX;
	private final double[] locationY;

	public SwanSpectralGeometryInfo(int nLocations, double[] locationX, double[] locationY) {
		this.nLocations = nLocations;
		this.locationX = locationX;
		this.locationY = locationY;
	}

	@Override
	public IArray distanceToPoint(double x, double y, double z) {
		double[] distances = new double[locationX.length];

		for (int i = 0; i < locationX.length; i++) {
			double lon = locationX[i];
			double lat = locationY[i];

			double distMeters = haversineDistance(lat, lon, y, x);
			distances[i] = distMeters;
		}

		return new Array(distances);
	}

	/**
	 * Haversine formula for distance between two lat/lon points.
	 * @param lat1 Latitude of point 1 (degrees)
	 * @param lon1 Longitude of point 1 (degrees)
	 * @param lat2 Latitude of point 2 (degrees)
	 * @param lon2 Longitude of point 2 (degrees)
	 * @return distance in meters
	 */
	private double haversineDistance(double lat1, double lon1, double lat2, double lon2) {
		final double R = 6371000; // Earth radius in meters

		double lat1Rad = Math.toRadians(lat1);
		double lat2Rad = Math.toRadians(lat2);
		double dLatRad = Math.toRadians(lat2 - lat1);
		double dLonRad = Math.toRadians(lon2 - lon1);

		double a = Math.sin(dLatRad / 2) * Math.sin(dLatRad / 2) +
			Math.cos(lat1Rad) * Math.cos(lat2Rad) *
				Math.sin(dLonRad / 2) * Math.sin(dLonRad / 2);
		double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

		return R * c;
	}

	@Override
	public IGeometryInfo clone() {
		return new SwanSpectralGeometryInfo(nLocations, locationX.clone(), locationY.clone());
	}

	public int getNLocations() { return nLocations; }

	public double getLocationX(int locIndex) { return locationX[locIndex]; }

	public double getLocationY(int locIndex) { return locationY[locIndex]; }

	@Override
	public String toString() {
		return String.format("SwanSpectralGeometryInfo[nLoc=%d]", nLocations);
	}
}
