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

package org.openda.uncertaintygui.tablemodels;

import due.utilities.matrix.DenseDoubleMatrix2D;
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.FunctionParameter;

import java.util.ArrayList;

/**
 * This class has an autoCorrelationFunction object which can be of
 * different types (subclasses) and some methods for
 * using the autoCorrelationFunction object. This class uses the
 * strategy pattern.
 */
public class AutoCorrelationFunctionContext {

	//to store an autoCorrelationFunction object.
    private AutoCorrelationFunction autoCorrelationFunction;

    /**
     * Constructor to make a new autoCorrelationFunctionContext object.
     * @param autoCorrelationFunction to store in this context object.
     */
    public AutoCorrelationFunctionContext(AutoCorrelationFunction autoCorrelationFunction) {
        this.autoCorrelationFunction = autoCorrelationFunction;
    }

    /**
     * This method creates a clone of this autoCorrelationFunctionContext object.
     * @return clone a clone of this context object.
     */
    public AutoCorrelationFunctionContext clone() {

		AutoCorrelationFunction autoCorrelationFunction = null;
    	if (this.autoCorrelationFunction != null) {
    		autoCorrelationFunction = this.autoCorrelationFunction.clone();
    	}

        return new AutoCorrelationFunctionContext(autoCorrelationFunction);
    }

    /**
	 * Gets the graph of this contexts autoCorrelationFunction object.
     * Not supported for "No Auto Correlation".
     *
	 * @param numberOfPoints the number of points in the returned DenseDoubleMatrix2D object
	 * @return DenseDoubleMatrix2D containing the points of the graph
	 * @throws UnsupportedOperationException if this context has no autocorrelationfunction object
     */
	public DenseDoubleMatrix2D getGraphPoints(int numberOfPoints) throws UnsupportedOperationException {
		if (this.autoCorrelationFunction != null) {
			return getCorrelationGraph(this.autoCorrelationFunction, numberOfPoints);
		}
		else {
			throw new UnsupportedOperationException("Graph not available for 'No Auto Correlation'.");
		}
	}

    /**
	 * Evaluates a correlation model function on numberOfPoints points within
	 * a certain range. These points are used to make a graph of the
	 * correlation model function.
	 *
	 * @param autoCorrelationFunction the function to get the graph for
	 * @param numberOfPoints the number of points in the returned DenseDoubleMatrix2D object
	 * @return DenseDoubleMatrix2D containing the points of the graph
	 * @throws IllegalArgumentException if numberOfPoints <= 0
	 */
	public DenseDoubleMatrix2D getCorrelationGraph(AutoCorrelationFunction autoCorrelationFunction,  int numberOfPoints) throws IllegalArgumentException {

		if (numberOfPoints > 0) {
			double xLowerLimit = autoCorrelationFunction.getGraphXLowerLimit();
			double xUpperLimit = autoCorrelationFunction.getGraphXUpperLimit();

			double step = (xUpperLimit - xLowerLimit) / numberOfPoints;

			DenseDoubleMatrix2D samples = new DenseDoubleMatrix2D(numberOfPoints, 2);
			for (int j = 0; j < numberOfPoints; j++) {
                samples.setElement(j,0,j*step);
                samples.setElement(j,1,autoCorrelationFunction.getCorrelationCoefficient(j*step));
            }

			return samples;
		}
		else {
			throw new IllegalArgumentException("Invalid number of points supplied.");
		}
    }

	/**
	 * Returns a string with the type of the autoCorrelationFunctionObject.
	 * @return type of auto correlation function or "No Auto Correlation"
	 */
	public String toString() {
		if (this.autoCorrelationFunction != null) {
    		return this.autoCorrelationFunction.toString();
		}
		else {
			return "No Auto Correlation";
		}
	}

	/**
	 * Get this contexts autoCorrelationFunction object.
	 * @return autoCorrelationFunction or null if this object
	 *         has no autoCorrelationFunction object.
	 */
	public AutoCorrelationFunction getAutoCorrelationFunctionObject() {
		return this.autoCorrelationFunction;
	}

	/**
	 * Returns an arraylist with parameters of the autoCorrelationFunctionObject.
	 * @return arraylist of parameters of auto correlation function or empty arraylist
	 *         if this object has no autoCorrelationFunction object.
	 */
	public ArrayList<FunctionParameter> getParams() {
		if (this.autoCorrelationFunction != null) {
    		return this.autoCorrelationFunction.getParams();
		}
		else {
			//return empty list.
	        return new ArrayList<FunctionParameter>();
		}
	}
}

