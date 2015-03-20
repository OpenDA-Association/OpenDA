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

package org.openda.uncertainties.autocorrelationfunctions;

import java.util.ArrayList;

import org.openda.uncertainties.FunctionParameter;
import org.openda.uncertainties.UncertainItem;


/**
 * Abstract Class for general autocorrelation model with
 * a sill parameter and a range parameter.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public abstract class AutoCorrelationFunction {

	//to store properties of the UncertainItem to which
	//this autoCorrlationFunction corresponds.
	protected UncertainItem uncertainItem = null;

    protected ArrayList<FunctionParameter> params = null;
    //TODO change arraylist to hashmap. AK
    //private HashMap<String, FunctionParameter> paramsMap;


	//x limits for graph plotting.
	protected final double graphXLowerLimit = 0.0;
	protected final double graphXUpperLimitFactor = 5.0;

	//constants for the names of the parameters of this correlation model.
    //The overall sill of the dependence model.
	protected static final String PARAM_SILL = "Sill";
    //The range of the shape function.
	protected static final String PARAM_RANGE = "Range";

	//constants for the indices of the parameters in the ArrayList params.
	protected static final int PARAM_INDEX_SILL = 0;
	protected static final int PARAM_INDEX_RANGE = 1;

	//default values for the parameters.
	protected static final double DEFAULT_SILL = 1.0;
	protected static final double DEFAULT_RANGE = 1.0;

	//constant int for isotropic correlation model.
	protected static final int ISOTROPIC = 0;

    /**
     * Records the order of anisotropy, where 0 denotes isotropy in the range,
     * 2 denotes two-dimensional anisotropy in the range and 3 denotes three-
     * dimensional anisotropy in the range.
     */
    protected int orderOfAnisotropy = ISOTROPIC;


    public AutoCorrelationFunction newInstance() {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    public AutoCorrelationFunction() {
        params =  new ArrayList<FunctionParameter>();
        //paramsMap =  new HashMap<String, FunctionParameter>();
      }

    public ArrayList<FunctionParameter> getParams() {
        return this.params;
    }

    protected void setParams(ArrayList<FunctionParameter> params) {
        this.params = params;
    }

    public AutoCorrelationFunction clone() {
    	AutoCorrelationFunction clonedAutoCorrelationFunction = this.newInstance();
        ArrayList<FunctionParameter> paramsList = this.getParams();
        ArrayList<FunctionParameter> newParamsList = new ArrayList<FunctionParameter>();
        for (FunctionParameter param : paramsList) {
            newParamsList.add(param.clone());
        }
        clonedAutoCorrelationFunction.setParams(newParamsList);

        //clone uncertainItem
        if (this.uncertainItem != null) {
            clonedAutoCorrelationFunction.setUncertainItem(this.uncertainItem.clone());
        }

        return clonedAutoCorrelationFunction;
    }


    /**
     * Evaluates an isotropic function for the given lag distance.
     *
     * @param lagDistance the lag distance between points
     * @return the value of the function
     * @throws ArithmeticException
     * @throws IllegalArgumentException if the correlation model is anisotropic.
     */
    public double evaluateFunction(double lagDistance) throws ArithmeticException, IllegalArgumentException {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    /**
     * Returns the correlation coefficient (rho) for the specified lag distance (h)
     * in an isotropic correlogram model.
     *
     * @param lagDistance the lag distance
     * @return the correlation coefficient (rho)
     * @throws ArithmeticException
     * @throws IllegalArgumentException if the correlation model is anisotropic.
     */
    public double getCorrelationCoefficient(double lagDistance) throws ArithmeticException, IllegalArgumentException {
        return this.getSill() - this.evaluateFunction(lagDistance);
    }


    /**
     * Method returns default lower limit on horizontal axis for correlation model graph plotting.
     * @return lower limit
     */
    public double getGraphXLowerLimit() {
        return this.graphXLowerLimit;
    }

    /**
     * Method returns default upper limit on horizontal axis for correlation model graph plotting.
     * @return upper limit
     */
    public double getGraphXUpperLimit() {
        return this.getRange()*this.graphXUpperLimitFactor;
    }

    /**
     * Set a specified parameter.
     *
     * @param String paramType the name of the parameter
     * @param double value of the parameter
     * @throws IllegalArgumentException if range <= 0
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_SILL)) {
			this.params.get(PARAM_INDEX_SILL).setValue(value);

		} else if (paramType.equalsIgnoreCase(PARAM_RANGE)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The range parameter must be positive.");
	        }
		    else {
    			this.params.get(PARAM_INDEX_RANGE).setValue(value);
		    }

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

	/**
	 * Get the value of parameter sill.
	 * @return double sill.
	 */
	public double getSill() {
		return this.params.get(PARAM_INDEX_SILL).getValue();
	}

	/**
	 * Get the value of parameter range.
	 * @return double range.
	 */
	public double getRange() {
		return this.params.get(PARAM_INDEX_RANGE).getValue();
	}

	public UncertainItem getUncertainItem() {
		return uncertainItem;
	}

	public void setUncertainItem(UncertainItem uncertainItem) {
		this.uncertainItem = uncertainItem;
	}
}
