/* OpenDA v2.4.3 
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
package org.openda.algorithms;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;

/**
 * Wrapper around an existing function that scales the parameters, ie a new function g is defined as:
 * g(...,q_i,...) := f([ ...,r_i + s_i * q_i,...)
 * where r_i is the i'th element of a vector with offsets and s is the scaling vector.
 */
	public class PreconditionedCostFunctionWithGradient extends SimulationKwadraticCostFunctionWithGradient {

	SimulationKwadraticCostFunctionWithGradient f = null;
    IVector initialVector = null;
    IVector scalingVector = null;

    public PreconditionedCostFunctionWithGradient(SimulationKwadraticCostFunctionWithGradient f, IStochModelFactory modFac, IStochObserver obs, IVector initialVector, IVector scalingVector){
    	super(modFac, obs);
    	this.f = f;
        this.initialVector = initialVector;
        this.scalingVector = scalingVector;
    }
    
    public void prepare(IVector p) {
        IVector pScaled = p.clone();
        pScaled.pointwiseMultiply(this.scalingVector);
        pScaled.axpy(1.0, this.initialVector);
        this.f.prepare(pScaled);
    }

//    public double evaluate(IVector p) {
//        /*
//         * g(q) = f(r + s.*q)
//         */
//    	IVector pScaled = p.clone();
//        pScaled.pointwiseMultiply(this.scalingVector);
//        pScaled.axpy(1.0, this.initialVector);
//        return this.f.evaluate(pScaled,"any");
//    }

    public double evaluate(IVector p, String context) {
        /*
         * g(q) = f(r + s.*q)
         */
    	IVector pScaled = p.clone();
        pScaled.pointwiseMultiply(this.scalingVector);
        pScaled.axpy(1.0, this.initialVector);
        return this.f.evaluate(pScaled,context);
    }

    public IVector evaluateGradient(IVector p) {
        /*
         * (d/dq)g = (d/dp)f .* s with p = r + s .* q
         */
        IVector pScaled = p.clone();
        pScaled.pointwiseMultiply(this.scalingVector);
        pScaled.axpy(1.0, this.initialVector);
        IVector gradientScaled = this.f.evaluateGradient(pScaled);
        gradientScaled.pointwiseMultiply(this.scalingVector);
        return gradientScaled;
    }

    public IVector getCosts() {
        return this.f.getCosts();
    }

    public IVector[] getParameters() {
    	IVector[] p = this.f.getParameters().clone();
    	for (int i=0;i<p.length;i++) {
        	p[i].axpy(-1.0, this.initialVector);
        	p[i].pointwiseDivide(this.scalingVector);    		
    	}
        return p;
    }

    public double getOptimalCost() {
        return f.getOptimalCost();
    }

    public IVector getOptimalParameters() {
        /*
         * f(q) = g((q - r) .* (s^-1))
         */
    	IVector p = this.f.getOptimalParameters().clone();
    	p.axpy(-1.0, this.initialVector);
    	p.pointwiseDivide(this.scalingVector);
        return p;
    }

    public double getMultiplicationFactor() {
        return this.f.getMultiplicationFactor();
    }

    public void writeResults() {
    	this.f.writeResults();
    }

    public boolean getTryParallel() {
        return this.f.getTryParallel();
    }

    public void setTryParallel(boolean tryParallel) {
    	this.f.setTryParallel(tryParallel);
    }
    
    public SimulationKwadraticCostFunctionWithGradient clone(){
        return new PreconditionedCostFunctionWithGradient(this.f,this.modFac,this.obs,
        												  this.initialVector,this.scalingVector);
    }
}
