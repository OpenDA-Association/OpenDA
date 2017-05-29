/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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
package org.openda.noiseModels;
import org.openda.interfaces.IMatrix;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.IVector;
import org.openda.noiseModels.SpatialCorrelationStochVector.CoordinatesType;
import org.openda.utils.Matrix;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

/**
 * This StochVector is a faster implementation for the SpatialCorrelationStochVector for regular 2D-grids.
 * It uses separabilily of the directions to speed up the computations.
 * 
 * @author verlaanm
 *
 */
public class Spatial2DCorrelationStochVector implements IStochVector {
	
	private double[] x=null;
	private double[] y=null;
	private int xn;
	private int yn;
	private double standardDeviation = 1.0;
	private double lengthScale=1000000.0;
	private CoordinatesType type=CoordinatesType.XY;
	
	private Matrix meanMatrix=null;
	
	private SpatialCorrelationStochVector xStochVector=null;
	private SpatialCorrelationStochVector yStochVector=null;
	private IMatrix xSqrtCov = null;
	private IMatrix ySqrtCov = null;

	private StochVector whiteNoise=null;
	//ODA-248 Stored so it can be shared between other instances and it does not have to be recalculated everytime
	private final SpatialCorrelationCovariance[] sharableCovariance = new SpatialCorrelationCovariance[2];

	/**
	 * Constructor for a StochVector with Gaussian correlations in space on a regular grid. Both directions
	 * are treated as independent (separable) ie r(x1,y1,x2,y2)=r_x(x1,x2)*r_y(y1,y2)
	 * @param coordsType compute distances on a plane in xy or an approximation to part of a sphere. Note that the
	 * computed distances are not truly computed on a sphere, due to the separation of lat and lon directions.
	 * @param standardDeviation
	 * @param lengthscale spatial correlation length-scale
	 * @param x grid in x-direction
	 * @param y grid in y-direction
	 * @param preCalculatedCovariances
	 */
	public Spatial2DCorrelationStochVector(CoordinatesType coordsType, double standardDeviation,
										   double lengthscale, double[] x, double[] y, SpatialCorrelationCovariance[] preCalculatedCovariances) {
		this.x = new double[x.length];
		System.arraycopy(x, 0, this.x, 0, x.length);
		this.y = new double[y.length];
		System.arraycopy(y, 0, this.y, 0, y.length);
		this.type=coordsType;
		
		this.lengthScale = lengthscale;
		this.standardDeviation = standardDeviation;
		
		this.xn=x.length;
		this.yn=y.length;
		double midX=0.5*(x[0]+x[xn-1]); //assume regular grid
		double midY=0.5*(y[0]+y[yn-1]);
		this.meanMatrix = new Matrix(yn,xn); //row ordering m[y][x]

		// init for x-direction
		double[] midYCopies=new double[xn];
		for(int ix=0;ix<xn;ix++){midYCopies[ix]=midY;}

		SpatialCorrelationCovariance[] preCalculatedXCovariance = preCalculatedCovariances == null ? null : new SpatialCorrelationCovariance[]{preCalculatedCovariances[0]};
		this.xStochVector = new SpatialCorrelationStochVector(coordsType, 1.0, lengthscale, x, midYCopies, preCalculatedXCovariance);
		this.sharableCovariance[0] = this.xStochVector.getSharableCovariance()[0];

		this.xSqrtCov=xStochVector.getSqrtCovariance().asMatrix();
		// init for y-direction
		double[] midXCopies=new double[yn];
		for(int iy=0;iy<yn;iy++){midXCopies[iy]=midX;}

		SpatialCorrelationCovariance[] preCalculatedYCovariance = preCalculatedCovariances == null ? null : new SpatialCorrelationCovariance[]{preCalculatedCovariances[1]};
		this.yStochVector = new SpatialCorrelationStochVector(coordsType, 1.0, lengthscale, midXCopies, y, preCalculatedYCovariance);
		this.sharableCovariance[1] = this.yStochVector.getSharableCovariance()[0];

		this.ySqrtCov=yStochVector.getSqrtCovariance().asMatrix();

		this.whiteNoise=new StochVector(xn*yn, 0., 1.);
	}

	/*		this.xStochVector = new SpatialCorrelationStochVector(coordsType, 1.0, lengthscale, x, midYCopies, preCalculatedCovarianceMatrix == null ? null : new SpatialCorrelationCovariance[]{preCalculatedCovarianceMatrix[0]});

		this.sharableCovariance[0] = this.xStochVector.getSharableCovariance()[0];

		this.xSqrtCov = xStochVector.getSqrtCovariance().asMatrix();
		// init for y-direction
		double[] midXCopies = new double[yn];
		for (int iy = 0; iy < yn; iy++) {
			midXCopies[iy] = midX;
		}
		this.yStochVector = new SpatialCorrelationStochVector(coordsType, 1.0, lengthscale, midXCopies, y, preCalculatedCovarianceMatrix == null ? null : new SpatialCorrelationCovariance[]{preCalculatedCovarianceMatrix[1]});

		this.sharableCovariance[1] = this.yStochVector.getSharableCovariance()[0];*/
	
	public IVector createRealization() {
		Vector whiteSample = (Vector) this.whiteNoise.createRealization();
		Matrix sampleMatrix = new Matrix(whiteSample,yn,xn);
		Matrix xCorrelatedSample = new Matrix(yn,xn);
		xCorrelatedSample.multiply(1.0, sampleMatrix, xSqrtCov, 0., false, true);
		Matrix xyCorrelatedSample = new Matrix(yn,xn);
		xyCorrelatedSample.multiply(this.standardDeviation, ySqrtCov, xCorrelatedSample, 0., false, false);
		return xyCorrelatedSample.asVector();
	}

	
	public double evaluatePdf(IVector tv) {
		throw new UnsupportedOperationException("No evaluatePdf for Spatial2DCorrelationStochVector");
	}

	
	public IVector getExpectations() {
		Vector result = new Vector(this.xn*this.yn);
		return result;
	}

	
	public ISqrtCovariance getSqrtCovariance() {
		throw new UnsupportedOperationException("No getSsqrtCovariance for Spatial2DCorrelationStochVector");
	}

	public SpatialCorrelationCovariance[] getSharableCovariance() {
		return sharableCovariance;
	}
	
	public boolean hasCorrelatedElements() {
		return true;
	}

	
	public IVector getStandardDeviations() {
		Vector result = new Vector(this.xn*this.yn);
		result.setConstant(this.standardDeviation);
		return result;
	}
	
	public String toString(){
		String result = "Spatial2DCorrelationStochVector(";
		result+="lengthScale="+this.lengthScale+",";
		result+="standardDeviation="+this.standardDeviation;
		if(this.x.length<40){
			result+=",x="+new Vector(this.x);
			result+=",y="+new Vector(this.y);
		}else{
				result+=",x=["+x[0]+","+x[1]+", ... ,"+x[x.length-1]+"]";
				result+=",y=["+y[0]+","+y[1]+", ... ,"+y[y.length-1]+"]";
		}
		result+=")";
		return result;
	}
}
