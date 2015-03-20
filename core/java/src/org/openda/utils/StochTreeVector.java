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
package org.openda.utils;

import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.util.ArrayList;

/**
 * Stochastic tree vector.
 * This class provide a simple implementation of the StochVector interface.
 */
public class StochTreeVector implements IStochVector {

	ArrayList<IStochVector> children = new ArrayList<IStochVector>();

	boolean correlated = false; // assumes independent elements
	String id = "anonymousStochTreeVector";
	
	/**
	 * Create a stochvector
	 *
	 * @param id The identifier for this treevector
	 */
	public StochTreeVector(String id) {
		this.correlated = false;
		this.id = id;
	}

	/**
	 * Return Id of toplevel node
	 * @return
	 */
    public String getId() {
        return this.id;
    }

	/**
	 * Add a branch to the tree of a StochTreeVector
	 *
	 * @param stochVector StochVector to be added
	 */
	public void addChild(IStochVector stochVector) {
		children.add(stochVector);
	}

	/**
	 * Draw a realization from the uncertainty internal to the StochVector
	 *
	 * @return generated vector created, most often from pseudo-random numbers
	 */
	public IVector createRealization() {
		if (this.correlated) {
			throw new RuntimeException("StochVector.createRealization()not implemented for correlated noise");
		}
		TreeVector realization = new TreeVector(this.id);
		int i=0;
		for (IStochVector child : children) {
			if(child instanceof ITreeVector){
				realization.addChild((TreeVector)child.createRealization());				
			}else{
				realization.addChild(new TreeVector(id+"_sub"+i,child.createRealization()));
			}
			i++;
		}
		return realization;
	}

	/**
	 * Evaluate the probability density function.
	 *
	 * @param tv value for which to check pdf
	 * @return Pdf value
	 */
	public double evaluatePdf(IVector tv) {
		if (this.correlated) {
			throw new RuntimeException("StochVector.evaluatePdf()not implemented for correlated noise");
		}

		double prob = 1.0;
		double values[] = tv.getValues();
		int istart=0;
		for (IStochVector child : children){
			int length = child.getExpectations().getSize();
			double valuesPart[] = new double[length];
			System.arraycopy(values, istart, valuesPart, 0, valuesPart.length);
			prob *= child.evaluatePdf(new Vector(valuesPart));
			istart+=length;
		}

		return prob;
	}


	/**
	 * Get mean value of the uncertain Vector
	 *
	 * @return mean values
	 */
	public IVector getExpectations() {
		TreeVector expectation = new TreeVector("mean_"+this.id);
		int i=0;
		for (IStochVector child : children) {
			if(child instanceof StochTreeVector){
				expectation.addChild((ITreeVector)child.getExpectations());				
			}else{
				expectation.addChild(new TreeVector(id+"_sub"+i,child.getExpectations()));
			}
			i++;
		}
		return expectation;
	}

	/**
	 * Get square-root of the covariance as an object. A square-root of the
	 * covariance P is a matrix L that satisfies P=L*transpose(L). Most often working with the
	 * square-root is more convenient than workin with the covariance itself.
	 *
	 * @return Square-root of the covariance.
	 */
	public ISqrtCovariance getSqrtCovariance() {
		if (this.correlated) {
			throw new RuntimeException("StochVector.getSqrtCovariance()not implemented for correlations"
					+" between subvectors. Id="+this.id);
		}
		//TODO there is no block-diagonal square-root yet, so now just catch possible error.
		// This should befixed when needed.
		for (IStochVector child : children) {
			if(child.hasCorrelatedElements()){
				throw new RuntimeException("StochVector.getSqrtCovariance()not implemented for "
			                +"correlations within subvectors. Id="+this.id);
			}
		}

		return new SqrtCovariance(getStandardDeviations());
	}

	/**
	 * True if the elements of the stochastic vector are uncorrelated. You can treat the values one
	 * by one in a linear context if this is so.
	 *
	 * @return true if independent, false if dependencies exist.
	 */
	public boolean hasCorrelatedElements() {
		boolean result = this.correlated;
		for (IStochVector child : children) {
			if(child.hasCorrelatedElements()){
				result = true;
			}
		}
		return result;
	}

	/**
	 * Get the standard deviation for each element of the stochastic vector.
	 *
	 * @return Vector with standard deviations
	 */
	public IVector getStandardDeviations() {
		TreeVector std = new TreeVector("std_"+this.id);
		int i=0;
		for (IStochVector child : children) {
			if(child instanceof StochTreeVector){
				std.addChild((TreeVector)child.getStandardDeviations());				
			}else{
				std.addChild(new TreeVector(id+"_sub"+i,child.getStandardDeviations()));
			}
			i++;
		}
		return std;
	}

	/**
	 * Write StochVector to string
	 * <p/>
	 */
	public String toString() {
		String string = "StochTreeVector{ ";
		if (this.correlated) {
			string = " correlated ";			
		}
		for (int i = 0 ; i < children.size(); i++) {
			IStochVector child = children.get(i);
			if (i > 0) string += ",";
			string += child.toString();
		}
		string += " }";
		return string;
	}

	public ArrayList<IStochVector> getChildren() {
		return children;
	}
}
