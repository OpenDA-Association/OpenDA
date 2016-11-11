package org.openda.model_delft3d;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.Array;

import static java.lang.Math.sqrt;

/**
 * Created by Theo on 07.11.2016.
 */
public class NetcdfD3dMapExchangeItemGeometryInfo implements IGeometryInfo {

	private final int[] dimensions;
	private double[] xCoords;
	private double[] yCoords;
	private double[] zCoords;
	boolean is3D;

	public NetcdfD3dMapExchangeItemGeometryInfo(double[] xCoords, double[] yCoords, double[] zCoords) {

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

		dimensions = new int[] {this.xCoords.length, this.zCoords.length};

	}

	public IArray distanceToPoint(double x, double y, double z) {

		Array arrayDistances = new Array(dimensions);
		//double[] distances = new double[xCoords.length*zCoords.length];

		int k=0;
		for (int lay=0; lay < dimensions[1];lay++){
			for (int xy = 0; xy < dimensions[0]; xy++) {
				if (this.xCoords[xy] == 0.0 | this.yCoords[xy] == 0.0) {
					//distances[k] = -999.0;
					arrayDistances.setValueAsDouble(k,Double.NaN);
				}else{
					//distances[k] = Math.sqrt(Math.pow((x - this.xCoords[xy]),2) + Math.pow((y - this.yCoords[xy]),2) + Math.pow((z - this.zCoords[lay]),2));
					arrayDistances.setValueAsDouble(k,Math.sqrt((x - this.xCoords[xy])*(x - this.xCoords[xy]) + (y - this.yCoords[xy])*(y - this.yCoords[xy]) + (z - this.zCoords[lay])*(z - this.zCoords[lay])));
				}
				k++;
			}
		}
		return arrayDistances;
	}
}