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
package org.openda.noiseModels;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.IVector;
import org.openda.utils.Matrix;
import org.openda.utils.SqrtCovariance;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

public class SpatialCorrelationStochVector implements IStochVector {

	// distances are different on a globe
	public enum CoordinatesType {
	    WGS84, XY 
	}
	
	private double[] x=null;
	private double[] y=null;
	private double standardDeviation = 1.0;
	private double lengthScale=1000000.0;
	private CoordinatesType type=CoordinatesType.XY;
	
	private Matrix covariance = null;
	private Matrix sqrtCovariance = null;
	private double determinantCov = 1.0;
	private Vector mean=null;
	private Vector standardDeviations = null;
	private StochVector whiteNoise = null;
	
	private final double radiusEarth=6372800.0; //average radius in leastsquares sense in meters
	private final double degreesToRadians=Math.PI/180.0;
	
	public SpatialCorrelationStochVector(CoordinatesType coordsType, double standardDeviation, 
			double lengthscale, double[] x, double[] y) {
		this.x = new double[x.length];
		System.arraycopy(x, 0, this.x, 0, x.length);
		this.y = new double[y.length];
		System.arraycopy(y, 0, this.y, 0, y.length);
		if(x.length!=y.length){
			throw new RuntimeException("SpatialCorrelationStochVector: x and y should have equal length");
		}
		this.type=coordsType;
		
		this.lengthScale = lengthscale;
		this.standardDeviation = standardDeviation;
		
		int n=x.length;
		this.mean = new Vector(n);
		this.standardDeviations = new Vector(n);
		this.standardDeviations.setConstant(this.standardDeviation);
		this.covariance = new Matrix(n,n);
		computeCovariance();
		
		this.determinantCov = this.covariance.determinant(); //
		
		factorizeCorrelation();
		
		IVector zeroMean = new Vector(n);
		IVector stdOne   = new Vector(n); stdOne.setConstant(1.0);
		this.whiteNoise=new StochVector(zeroMean, stdOne);
	}

	private void computeCovariance(){
		double covij;
		double scale=this.standardDeviation*this.standardDeviation;
		double dist;
		for(int i=0;i<x.length;i++){
			for(int j=0;j<x.length;j++){
				dist = distance(x[i],y[i],x[j],y[j]); 
				covij = scale*Math.exp(-0.5*dist*dist/(this.lengthScale*this.lengthScale));
				this.covariance.setValue(i, j, covij);
			}
		}
	}
	
	private double distance(double x1, double y1, double x2, double y2){
		double result=0.0;
		if(this.type==CoordinatesType.WGS84){
			// simplified computation with sphere and chord length
			// we can consider both cheaper and more accurate alternatives
			double lon1=x1*this.degreesToRadians;
			double lat1=y1*this.degreesToRadians;
			double lon2=x2*this.degreesToRadians;
			double lat2=y2*this.degreesToRadians;
			double dX = Math.cos(lat2)*Math.cos(lon2) - Math.cos(lat1)*Math.cos(lon1); //3d on sphere with unit length
			double dY = Math.cos(lat2)*Math.sin(lon2) - Math.cos(lat1)*Math.sin(lon1);
			double dZ = Math.sin(lat2)-Math.sin(lat1);
			result = this.radiusEarth*2.0*Math.asin(0.5*Math.sqrt(dX*dX+dY*dY+dZ*dZ));
		}else{
			result = Math.sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
		}
		return result;
	}
	
	private void factorizeCorrelation(){
		this.sqrtCovariance=this.covariance.sqrt();
	}
	
	@Override
	public IVector createRealization() {
		IVector result = this.mean.clone();
		IVector whiteSample = this.whiteNoise.createRealization();
		this.sqrtCovariance.rightMultiply(1.0, whiteSample, 1.0, result); //x=alpha*x+beta*L*v
		return result;
	}

	@Override
	public double evaluatePdf(IVector tv) {
		IVector diff = tv.clone();
		diff.axpy(-1.0, this.mean); //this=alpha*x+this
		IVector decorrelated = this.whiteNoise.getExpectations(); // decorrelated=0 of right size
		this.sqrtCovariance.rightSolve(diff, decorrelated);
		return this.whiteNoise.evaluatePdf(decorrelated)/Math.sqrt(this.determinantCov);
	}

	@Override
	public IVector getExpectations() {
		return mean.clone();
	}

	@Override
	public ISqrtCovariance getSqrtCovariance() {
		return new SqrtCovariance(this.sqrtCovariance);
	}

	@Override
	public boolean hasCorrelatedElements() {
		return true;
	}

	@Override
	public IVector getStandardDeviations() {
		// TODO Auto-generated method stub
		return this.standardDeviations.clone();
	}
	
	public String toString(){
		String result = "SpatialCorrelationStochVector(";
		result+="lengthScale="+this.lengthScale+",";
		result+="standardDeviation="+this.standardDeviation;
		if(this.x.length<40){
			result+=",x="+new Vector(this.x);
			result+=",y="+new Vector(this.y);
		}
		result+=")";
		return result;
	}
}
