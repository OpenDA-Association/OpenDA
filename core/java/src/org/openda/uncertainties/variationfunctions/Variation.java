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

package org.openda.uncertainties.variationfunctions;

import due.utilities.mathutil.UniformRandomNumber;
import org.openda.uncertainties.FunctionParameter;
import org.openda.uncertainties.UncertainItem;
import org.openda.uncertainties.Uncertainty;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: Juzer Dhondia
 * Date: 9-nov-2007
 * Time: 14:52:52
 * To change this template use File | Settings | File Templates.
 */
public class Variation implements Uncertainty {

	//to store properties of the UncertainItem to which
	//this variation function corresponds.
	protected UncertainItem uncertainItem = null;

    protected ArrayList<FunctionParameter> params = null;
    //TODO change arraylist to hashmap. AK
    //private HashMap<String, FunctionParameter> paramsMap;


    public Variation() {
        params =  new ArrayList<FunctionParameter>();
        //paramsMap =  new HashMap<String, FunctionParameter>();
    }

    public Variation newInstance() {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    public Variation clone() {
    	Variation clonedVariationFunction = this.newInstance();
        ArrayList<FunctionParameter> paramsList = this.getParams();
        ArrayList<FunctionParameter> newParamsList = new ArrayList<FunctionParameter>();
        for (FunctionParameter param : paramsList) {
            newParamsList.add(param.clone());
        }
        clonedVariationFunction.setParams(newParamsList);

        //clone uncertainItem
        if (this.uncertainItem != null) {
            clonedVariationFunction.setUncertainItem(this.uncertainItem.clone());
        }

        return clonedVariationFunction;
    }

    public ArrayList<FunctionParameter> getParams() {
        return this.params;
    }

    protected void setParams(ArrayList<FunctionParameter> params) {
        this.params = params;
    }

	public void setParam(String paramType, double value) throws IllegalArgumentException {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    public double getRealization(long seed) {

		double realization;

		if (this instanceof RangeVariation)
			realization = ((RangeVariation) this).getRealizationFromSeed(seed);
        else throw new RuntimeException("Value must be specified");

        return realization;
	}

    public double getRealization(double value, long seed) {

		double realization;

		if (this instanceof RangeVariation)
			realization = ((RangeVariation) this).getRealizationFromSeed(seed);
		else if (this instanceof PercentageVariation)
			realization = ((PercentageVariation) this).getRealizationFromSeed(
					value, seed);
        else throw new RuntimeException("Unexpected variation type");

        return realization;
	}

	public UncertainItem getUncertainItem() {
		return uncertainItem;
	}

	public void setUncertainItem(UncertainItem uncertainItem) {
		this.uncertainItem = uncertainItem;
	}
}
