/* OpenDA v2.4 
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
package org.openda.utils;

import org.openda.interfaces.IMatrix;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.IVector;

import java.util.Arrays;
import java.util.Random;

/**
 * Stochastic tree vector.
 * This class provide a simple implementation of the StochVector interface.
 */
public class StochVector implements IStochVector {

	IVector mean = null;  //assumes Gaussian distribution
	IVector std = null;
	Matrix covariance = null;
	Matrix sqrtCovariance = null;
	boolean correlated = false; // assumes independent elements

	private static Random generator;

	static {
		if (generator == null) {
			setInitialSeedType(InitialSeedType.fixed);
		}
	}

	public static void setInitialSeedType(StochVector.InitialSeedType initialSeedType) {
		setInitialSeedType(initialSeedType, 20100816);
	}

	public static void setInitialSeedType(StochVector.InitialSeedType initialSeedType, int initialSeedValue) {
		if (initialSeedType == InitialSeedType.random) {
			initialSeedValue = (new Random()).nextInt();
		}
		// Use the DistributedCounter to make sure we have different seeds in a parallel run
		DistributedCounter seedWithOffset = new DistributedCounter(initialSeedValue);
		generator = new Random(seedWithOffset.val());
	}

	public static void setSeed(long seed) {
		generator.setSeed(seed);
	}

	public enum InitialSeedType {
		fixed,
		random,
		specify
	}
	/**
	 * Create a stochvector of a certain size, from two double, mean and standard-deviation
	 *
	 * @param size vector size
	 * @param mean average value as Vector
	 * @param std  standard-deviation as Vector
	 */
	public StochVector(int size, double mean, double std) {
		this.mean = new Vector(size);
		this.mean.setConstant(mean);
		this.std = new Vector(size);
		this.std.setConstant(std);
		this.correlated = false;
	}

	/**
	 * Create a stochvector from two vectors, mean and standard-deviation
	 *
	 * @param mean average value as Vector
	 * @param std  standard-deviation as Vector
	 */
	public StochVector(IVector mean, IVector std) {
		this.mean = new Vector(mean);
		this.std = new Vector(std);
		this.correlated = false;
	}

	/**
	 * Create a stochvector from two arrays of doubles, mean and standard-deviation
	 *
	 * @param mean average value as arrays of doubles
	 * @param std  standard-deviation as arrays of doubles
	 */
	public StochVector(double[] mean, double[] std) {
		this.mean = new Vector(mean);
		this.std = new Vector(std);
		this.correlated = false;
	}

	/**
	 * Create a stochvector from two strings, mean and standard-deviation
	 *
	 * @param meanstring average value as String, eg. "[1.0,0.0]"
	 * @param stdstring  standard-deviation as String
	 */
	public StochVector(String meanstring, String stdstring) {
		this.mean = new Vector(meanstring);
		this.std = new Vector(stdstring);
		this.correlated = false;
	}

	/**
	 * Create a stochvector from one string with mean and standard-deviation
	 *
	 * @param valueString mean and std value as String, eg. "{[1.0,0.0],[0.1,0.1]}"
	 */
	public StochVector(String valueString) {
		int i1first = valueString.indexOf("{") + 1;
		int i1last = valueString.indexOf("],") + 1;
		int i2first = valueString.indexOf(",[") + 1;
		int i2last = valueString.indexOf("}");
		String meanstring = valueString.substring(i1first, i1last);
		String stdstring = valueString.substring(i2first, i2last);
		this.mean = new Vector(meanstring);
		this.std = new Vector(stdstring);
		this.correlated = false;
	}

	/**
	 * Create a stochvector of a certain size, from a mean covariance
	 *
	 * @param mean average value as Vector
	 * @param covariance  covariance as Matrix
	 * @param isSquareRoot
	 */
	public StochVector(IVector mean, IMatrix covariance, boolean isSquareRoot) {
		this.mean = new Vector(mean);
		this.correlated = true;
		if(isSquareRoot){
			this.sqrtCovariance = new Matrix(covariance);
			this.covariance = Matrix.mult(this.sqrtCovariance, this.sqrtCovariance, false, true);
		}else{
			this.covariance = new Matrix(covariance);
			this.sqrtCovariance = this.covariance.sqrt();
		}
		this.std = this.covariance.diag();
		this.std.sqrt();
	}

	/**
	 * Draw a realization from the uncertainty internal to the StochVector
	 *
	 * @return generated vector created, most often from pseudo-random numbers
	 */
	public Vector createRealization() {
		Vector sample = new Vector(this.mean.getSize());
		for (int i = 0; i < this.mean.getSize(); i++) {
			double randomValue = generator.nextGaussian();
			sample.setValue(i, randomValue);
		}
		if (this.correlated) {
			Vector temp = new Vector(sample.getSize());
			this.sqrtCovariance.rightMultiply(1.0, sample, 1.0, temp); //x=alpha*x+beta*L*v
			sample=temp;
		}else{
			sample.pointwiseMultiply(this.std);
		}
		sample.axpy(1.0, this.mean);
		return sample;
	}

	/**
	 * Evaluate the probability density function.
	 *
	 * @param tv value for which to check pdf
	 * @return Pdf value
	 */
	public double evaluatePdf(IVector tv) {
		IVector diff = tv.clone();
		diff.axpy(-1.0, this.mean); //this=alpha*x+this
		double sqrtDetCov=1.0;
		if (this.correlated) {
			IVector decorrelated = new Vector(diff.getSize()); // decorrelated=0 of right size
			this.sqrtCovariance.rightSolve(diff, decorrelated);
			diff = decorrelated;
			sqrtDetCov = Math.sqrt(this.covariance.determinant());
		}else{
			diff.pointwiseDivide(this.std);
			for (int i = 0; i < this.mean.getSize(); i++) {
				sqrtDetCov/= this.std.getValue(i);
			}
		}
		double prob = 1.0;
		double t;		//temporary variable
		for (int i = 0; i < this.mean.getSize(); i++) {
			t = diff.getValue(i);
			prob *= Math.sqrt(0.5 / Math.PI) *  Math.exp(-0.5 * t * t);
		}
		return prob/sqrtDetCov;
	}

	/**
	 * Evaluate the marginal probability density functions for all elements in the vector.
	 *
	 * @param tv value for which to check pdf
	 * @return Pdf value
	 */
	public IVector evaluateMarginalPDFs(IVector tv) {
		IVector probVec = tv.clone();
		double t;		//temporary variable
		for (int i = 0; i < this.mean.getSize(); i++) {
			t = (tv.getValue(i) - this.mean.getValue(i)) / this.std.getValue(i);
			probVec.setValue(i, Math.sqrt(0.5 / Math.PI) / this.std.getValue(i) * Math.exp(-0.5 * t * t));
		}
		return probVec;
	}


	/**
	 * Get mean value of the uncertain Vector
	 *
	 * @return mean values
	 */
	public IVector getExpectations() {
		return new Vector(this.mean);
	}

	/**
	 * Get square-root of the covariance as an object. A square-root of the
	 * covariance P is a matrix L that satisfies P=L*transpose(L). Most often working with the
	 * square-root is more convenient than workin with the covariance itself.
	 *
	 * @return Square-root of the covariance.
	 */
	public ISqrtCovariance getSqrtCovariance() {
		if(this.correlated){
			return new SqrtCovariance(this.sqrtCovariance);
		}else{
			return new SqrtCovariance(this.std);
		}
	}

	/**
	 * True if the elements of the stochastic vector are uncorrelated. You can treat the values one
	 * by one in a linear context if this is so.
	 *
	 * @return true if independent, false if dependencies exist.
	 */
	public boolean hasCorrelatedElements() {
		return this.correlated;
	}

	/**
	 * Get the standard deviation for each element of the stochastic vector.
	 *
	 * @return Vector with standard deviations
	 */
	public IVector getStandardDeviations() {
		return new Vector(this.std);
	}

	/**
	 * Write StochVector to string
	 * <p/>
	 */
	public String toString() {
		if (this.correlated) {
			return "{" + this.mean + "," + this.covariance + "}";
		}else{
			return "{" + this.mean + "," + this.std + "}";
		}
	}

	public void add(IVector means, IVector stds) {
		if(this.correlated){
			throw new UnsupportedOperationException("StochVector.add not supported for correlated vectors yet.");
		}
		int orgSize = this.mean.getSize();
		int totLength = means.getSize() + orgSize;
		double[] mean;
		double[] std;
		if (orgSize > 0) {
			mean = Arrays.copyOf(this.mean.getValues(), totLength);
			std = Arrays.copyOf(this.std.getValues(), totLength);
		} else {
			mean = new double[totLength];
			std = new double[totLength];
		}

		System.arraycopy(means.getValues(), 0, mean, orgSize, means.getSize());
		System.arraycopy(stds.getValues(), 0, std, orgSize, stds.getSize());
		this.mean = new Vector(mean);
		this.std = new Vector(std);

	}
}
