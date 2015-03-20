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

import java.util.ArrayList;

import org.openda.uncertainties.FunctionParameter;
import org.openda.uncertainties.variationfunctions.Variation;

/**
 * This class has a variation object which can be of
 * different types (subclasses) and some methods for
 * using the variation object. This class uses the
 * strategy pattern.
 */

public class VariationFunctionContext {

	//to store a variation object.
    private Variation variationFunction;

    /**
     * Constructor to make a new variationFunctionfContext object.
     * @param variationFunction to store in this context object.
     */
    public VariationFunctionContext(Variation variationFunction) {
        this.variationFunction = variationFunction;
    }

    /**
     * This method creates a clone of this VariationFunctionContext object.
     * @return clone a clone of this context object.
     */
    public VariationFunctionContext clone() {

    	Variation clonedVariationFunction = this.getVariationFunctionObject().clone();

    	VariationFunctionContext variationFunctionContext = new VariationFunctionContext(clonedVariationFunction);

    	return variationFunctionContext;
    }

	/**
	 * Returns a string with the type of the variation object.
	 * @return type of variation function
	 */
    public String toString() {
		return this.variationFunction.toString();
	}

	/**
	 * Get this contexts variation object.
	 * @return variation function
	 */
	public Variation getVariationFunctionObject() {
		return this.variationFunction;
	}

	/**
	 * Returns an arraylist with parameters of the variation object.
	 * @return arraylist of parameters of variation object.
	 */
	public ArrayList<FunctionParameter> getParams() {
		return this.variationFunction.getParams();
	}
}

