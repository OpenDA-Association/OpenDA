package org.openda.model_delft3d;

/**
 * Created by Theo on 19.04.2016.
 */
public class D3dGrid2D {

	private int mmax;
	private int nmax;
	private double[][] coordinateXArray;
	private double[][] coordinateYArray;

	public D3dGrid2D(int mmax, int nmax) {
		this.mmax = mmax;
		this.nmax = nmax;
	}

	public D3dGrid2D() {
	}

	public int getMmax() {
		return mmax;
	}

	public int getNmax() {
		return nmax;
	}

	public void setMmax(int mmax) {
		this.mmax = mmax;
	}

	public void setNmax(int nmax) {
		this.nmax = nmax;
	}

	public void setCoordinateArrays(double[][] coordinateXArray,double[][] coordinateYArray) {
		this.coordinateXArray = coordinateXArray;
		this.coordinateYArray = coordinateYArray;
	}
}
