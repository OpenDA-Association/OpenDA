package org.openda.model_dflowfm;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.Array;

/**
 * Created by Theo on 07.11.2016.
 */
public class DFlowFMMapExchangeItemGeometryInfo implements IGeometryInfo {

	private final int[] dimensions;
	private final double[] xCoords;
	private final double[] yCoords;
	private double[] zCoords;
	private final boolean is3D;
	private static final double METERS_PER_DEGREE = 60. * 1852.27;
	private final boolean unitInMeters;

	public DFlowFMMapExchangeItemGeometryInfo(double[] xCoords, double[] yCoords, double[] zCoords, boolean unitInMeters) {
		this.unitInMeters = unitInMeters;

		if (xCoords.length != yCoords.length) {
			throw new RuntimeException("x and y coordinates are not uniform!");
		}
		//dimensions = new int[] {xCoords.length, yCoords.length, zCoords.length};
		this.xCoords = xCoords;
		this.yCoords = yCoords;
		this.zCoords = zCoords;

		is3D = (zCoords != null);
		if (!is3D) {
			this.zCoords = new double[1];
			this.zCoords[0] = 0;
		}

		dimensions = new int[]{this.xCoords.length};
	}

	private static double distanceInMetersFromLatLon(double lat1, double lon1, double lat2, double lon2) {
		double dy = (lat2 - lat1) * METERS_PER_DEGREE;
		double phi = (lat2 + lat1) * 0.017453 / 2;
		double dx = (lon2 - lon1) * METERS_PER_DEGREE * Math.cos(phi);

		return Math.hypot(dx, dy);
	}

	private static double distance(double y1, double x1, double y2, double x2) {
		double dy = (y2 - y1);
		double dx = (x2 - x1);

		return Math.hypot(dx, dy);
	}

	public IArray distanceToPoint(double x, double y, double z) {

		Array arrayDistances = new Array(dimensions);

		for (int i = 0; i < dimensions[0]; i++) {
			double distance = unitInMeters ? distance(y, x, this.yCoords[i], this.xCoords[i]) : distanceInMetersFromLatLon(y, x, this.yCoords[i], this.xCoords[i]);
			arrayDistances.setValueAsDouble(i, distance);
		}
		return arrayDistances;
	}

	public int getSize() {
		if (xCoords == null) return 0;
		return xCoords.length;
	}

	public double getXCoord(int index) {
		return xCoords[index];
	}

	public double getYCoord(int index) {
		return yCoords[index];
	}

	public double getZCoord(int index) {
		return zCoords[index];
	}

	public boolean is3D() {
		return is3D;
	}
}
