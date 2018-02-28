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

public class SimulationKwadraticCostFunctionWithGradient extends SimulationKwadraticCostFunction
														 implements ICostFunctionWithGradient{
	
	public SimulationKwadraticCostFunctionWithGradient(IStochModelFactory modFac,IStochObserver obs){
		super(modFac, obs);
	}

	/**
	 * Evaluate the gradient of the cost function for some parameters
	 * @param p : Vector with parameters
	 * @return grad : Gradient as a Vector
	 */

	public IVector evaluateGradient(IVector p) {
        
		/* 
		 * numerical (forward) derivative [f(x+h_i)-f(x)] / h_i
		 * needs n+1 function evaluations
		 */
		IVector grad = p.clone();
		int n = p.getSize();

		IVector h = this.getParameterUncertainty().getStandardDeviations(); 
		h.scale(1.0e-4);	// (1.0e-16)^0.25	
		
		double fc = evaluate(p,"evaluate gradient");
		for (int i = 0; i < n; i++)	{
			double pi = p.getValue(i);
			p.setValue(i,pi + h.getValue(i));
			double fp = evaluate(p,"evaluate gradient");
			p.setValue(i,pi);
			grad.setValue(i, (fp-fc)/h.getValue(i));
		}
		return grad;
	}
	
	public IVector evaluateGradientCentered(IVector p) {
		
		/* 
		 * numerical (centered) derivative: [f(x+h)-f(x-h)] / 2h
		 * needs 2*n function evaluations
		 */
		IVector grad = p.clone();
		int n = p.getSize();

		IVector h = this.getParameterUncertainty().getStandardDeviations(); 
		h.scale(1.0e-4);	// (1.0e-16)^0.25	

		for (int i = 0; i < n; i++)	{
			double pi = p.getValue(i);
			p.setValue(i,pi + h.getValue(i));
			double fp = evaluate(p,"evaluate gradient");
			p.setValue(i,pi - h.getValue(i));
			double fm = evaluate(p,"evaluate gradient");
			p.setValue(i,pi);
			grad.setValue(i, (fp-fm)/(2.0*h.getValue(i)));
		}
		return grad;
	}
}
