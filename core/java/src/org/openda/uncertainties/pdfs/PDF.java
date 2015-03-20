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
package org.openda.uncertainties.pdfs;

import due.utilities.mathutil.UniformRandomNumber;
import org.openda.uncertainties.FunctionParameter;
import org.openda.uncertainties.UncertainItem;
import org.openda.uncertainties.Uncertainty;

import java.util.ArrayList;

public class PDF implements Uncertainty {

	//to store properties of the UncertainItem to which
	//this PDF corresponds.
	protected UncertainItem uncertainItem = null;

    protected ArrayList<FunctionParameter> params = null;
    //TODO change arraylist to hashmap. AK
    //private HashMap<String, FunctionParameter> paramsMap;

    private double cumulativeProbabilityUpperLimit;
    private double cumulativeProbabilityLowerLimit;
    private double probabilityUpperLimit;
    private double probabilityLowerLimit;

    public PDF() {
      params =  new ArrayList<FunctionParameter>();
      //paramsMap =  new HashMap<String, FunctionParameter>();
      probabilityLowerLimit = 0.0001;
      probabilityUpperLimit = 0.9999;
      cumulativeProbabilityLowerLimit = 0.0001;
      cumulativeProbabilityUpperLimit = 0.9999;
    }

    public PDF newInstance() {
        throw new RuntimeException(" to be implemented in extended class") ;
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

    //TODO not needed for the moment. AK
//    public void addParam(String paramType, double value) throws IllegalArgumentException {
//        // add to ;ist
//        FunctionParameter parameter = new FunctionParameter(paramType, value);
//        params.add(parameter) ;
//        paramsMap.put(paramType, parameter ) ;
//    }

//    public void setParam(String paramType, double value) throws IllegalArgumentException {
//        if (paramsMap.containsKey(paramType)) {
//            this.paramsMap.get(paramType).setValue(value);
//        } else {
//            throw new IllegalArgumentException("Invalid parameter type supplied.");
//        }
//    }

//    protected double getParamsValue(String parameterName) {
//        // todo check first if this parameter is available
//        // todo then return its value
//        FunctionParameter parameter = this.paramsMap.get(parameterName);
//        return parameter.getValue();
//    }


    public PDF clone() {
        PDF clonedPdf = this.newInstance();
        ArrayList<FunctionParameter> paramsList = this.getParams();
        ArrayList<FunctionParameter> newParamsList = new ArrayList<FunctionParameter>();
        for (FunctionParameter param : paramsList) {
            newParamsList.add(param.clone());
        }
        clonedPdf.setParams(newParamsList);

        //clone uncertainItem
        if (this.uncertainItem != null) {
            clonedPdf.setUncertainItem(this.uncertainItem.clone());
        }

        return clonedPdf;
    }

    public double getPDFPoint(double inputPoint) throws ArithmeticException, IllegalArgumentException {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    /**
     * Get the cumulative probability P(X <= x) for a specified x.  Relies on a
     * numerical approximation of the Error Function errorFunction().
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
    public double getCDFPoint(double x) throws ArithmeticException {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
        throw new RuntimeException(" to be implemented in extended class") ;
    }

    /**
     * Return a random number from the distribution.  The default implementation
     * calls getInverseCDF() (i.e. Inverse Transform Method).  Some classes override
     * this default.
     *
     * @param newRandom a new uniform random number generator
     * @return a random number from the distribution
     */
    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {
        return getInverseCDFPoint(newRandom.nextDouble()); //Sample a random number with the Inverse Tranform Method.
    }

    public double getRealization(long seed) {
        return this.nextDouble(new UniformRandomNumber(seed));
    }

    public double evaluatePDF(double measuredValue, double computedValue) {
        throw new RuntimeException(this.getClass() + ": evaluatePDF() Function not implemented");
    }

    public double getLowerLimit() {
        return this.getInverseCDFPoint(probabilityLowerLimit);
    }

    public double getUpperLimit() {
        return this.getInverseCDFPoint(probabilityUpperLimit);
    }

    public double getCumulativeLowerLimit() {
        return this.getInverseCDFPoint(cumulativeProbabilityLowerLimit);
    }

    public double getCumulativeUpperLimit() {
        return this.getInverseCDFPoint(cumulativeProbabilityUpperLimit);
    }

	public UncertainItem getUncertainItem() {
		return uncertainItem;
	}

	public void setUncertainItem(UncertainItem uncertainItem) {
		this.uncertainItem = uncertainItem;
	}

}
