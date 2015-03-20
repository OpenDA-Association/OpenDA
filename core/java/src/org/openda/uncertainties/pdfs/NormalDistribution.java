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

import due.utilities.mathutil.Mathematics;
import due.utilities.mathutil.UniformRandomNumber;
import org.openda.uncertainties.FunctionParameter;

import java.io.Serializable;

/**
 * The NormalDistribution class implements the Normal probability distribution function
 * with a mean and standard deviation as input parameters.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class NormalDistribution extends PDF implements Serializable{

	/**
     * Cache to store one random sample from the normal distribution
     * while the parameters of the distribution remain unchanged (i.e. benefit from
     * the two samples produced with the Polar Method in getRandom()).
     */
    boolean cacheRandomExists = false;

    /**
     * Cached realisation from the polar method.
     */
    double cacheRandom;

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Normal";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_MEAN = "Mean";
	private static final String PARAM_STD = "Std";
    private static final String PARAM_STDISFACTOR = "Std is factor";

    //constants for the indices of the parameters in the ArrayList params.
    private static final int PARAM_INDEX_MEAN = 0;
	private static final int PARAM_INDEX_STD = 1;
    private static final int PARAM_INDEX_STDISFACTOR = 2;

	//default values for the parameters.
	private static final double DEFAULT_MEAN = 0.0;
	private static final double DEFAULT_STD = 1.0;
    private static final int DEFAULT_STDISFACTOR = 0;

    public static final String PARAM_IS_FACTOR = "Is Factor";

	public NormalDistribution newInstance() {
        return new NormalDistribution() ;
    }

	/**
	 * Construct a normal distribution with default values for the parameters.
	 */
	public NormalDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_MEAN, DEFAULT_MEAN));
		params.add(new FunctionParameter(PARAM_STD, DEFAULT_STD));
        params.add(new FunctionParameter(PARAM_STDISFACTOR, DEFAULT_STDISFACTOR));
	}

    /**
     * Construct the PDF with a mean and std (standard deviation) parameter.
     *
     * @param mean parameter
     * @param std parameter (standard deviation)
     * @throws IllegalArgumentException if std <= 0
     */
	public NormalDistribution(double mean, double std, int stddvIsFactor)  throws IllegalArgumentException  {
        super();

		if (std <= 0) {
            throw new IllegalArgumentException("The standard deviation must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_MEAN, mean));
		params.add(new FunctionParameter(PARAM_STD, std));
        params.add(new FunctionParameter(PARAM_STDISFACTOR, stddvIsFactor));
    }

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit.
     * @return the probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double cdfValue = 0;

		double mean = getMean();
		double std = getStd();

        if (x > 0.0) {
            cdfValue =  ( 0.5 + 0.5 * Mathematics.errorFunction( (x-mean)
            		    / Math.sqrt(2.0 * ( Math.pow(std, 2.0 ) ) ) ) );
        }
        else {
            cdfValue = ( 0.5 - 0.5 * Mathematics.errorFunction( (-(x-mean) )
            		   / Math.sqrt(2.0 *  ( Math.pow(std, 2.0 ) ) ) ) );
        }

	    return cdfValue;
	}

    /**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required.
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double mean = getMean();
		double std = getStd();

		double pdfValue = ( 1/ (std * ( Math.sqrt(2 * Math.PI) ) ) )
                          * Math.exp( -0.5 * ( ((x-mean)/std) * ((x-mean)/std) ) );

        return pdfValue;
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
		double mean = getMean();
		double std = getStd();

        double x, y, z, y2, x0, x1;
        int code;

        double s2pi = Math.sqrt( 2.0* Math.PI );

        /* Constants for getInverseCDF() approximation for 0 <= |y - 0.5| <= 3/8 */

        final double P0[] = {
            -5.99633501014107895267E1,
            9.80010754185999661536E1,
            -5.66762857469070293439E1,
            1.39312609387279679503E1,
            -1.23916583867381258016E0,
        };

        final double Q0[] = {
            /* 1.00000000000000000000E0,*/
            1.95448858338141759834E0,
            4.67627912898881538453E0,
            8.63602421390890590575E1,
            -2.25462687854119370527E2,
            2.00260212380060660359E2,
            -8.20372256168333339912E1,
            1.59056225126211695515E1,
            -1.18331621121330003142E0,
        };

        /* Approximation for interval z = sqrt(-2 log y ) between 2 and 8
         * i.e., y between exp(-2) = .135 and exp(-32) = 1.27e-14.
         */

        final double P1[] = {
            4.05544892305962419923E0,
            3.15251094599893866154E1,
            5.71628192246421288162E1,
            4.40805073893200834700E1,
            1.46849561928858024014E1,
            2.18663306850790267539E0,
            -1.40256079171354495875E-1,
            -3.50424626827848203418E-2,
            -8.57456785154685413611E-4,
        };

        final double Q1[] = {
            /*  1.00000000000000000000E0,*/
            1.57799883256466749731E1,
            4.53907635128879210584E1,
            4.13172038254672030440E1,
            1.50425385692907503408E1,
            2.50464946208309415979E0,
            -1.42182922854787788574E-1,
            -3.80806407691578277194E-2,
            -9.33259480895457427372E-4,
        };

        /* Approximation for interval z = sqrt(-2 log y ) between 8 and 64
         * i.e., y between exp(-32) = 1.27e-14 and exp(-2048) = 3.67e-890.
         */

        final double  P2[] = {
            3.23774891776946035970E0,
            6.91522889068984211695E0,
            3.93881025292474443415E0,
            1.33303460815807542389E0,
            2.01485389549179081538E-1,
            1.23716634817820021358E-2,
            3.01581553508235416007E-4,
            2.65806974686737550832E-6,
            6.23974539184983293730E-9,
        };

        final double  Q2[] = {
            /*  1.00000000000000000000E0,*/
            6.02427039364742014255E0,
            3.67983563856160859403E0,
            1.37702099489081330271E0,
            2.16236993594496635890E-1,
            1.34204006088543189037E-2,
            3.28014464682127739104E-4,
            2.89247864745380683936E-6,
            6.79019408009981274425E-9,
        };

        if (probability < 0.0 || probability > 1.0) {
            throw new IllegalArgumentException("Incorrect input");
        }
        code = 1;
        y = probability;

        if( y > (1.0 - 0.13533528323661269189) ) {         //* 0.135... = exp(-2) */
            y = 1.0 - y;
            code = 0;
        }

        if( y > 0.13533528323661269189 ) {
            y = y - 0.5;
            y2 = y * y;
            x = y + y * (y2 * Mathematics.polevl( y2, P0, 4)/Mathematics.p1evl( y2, Q0, 8 ));
            x = x * s2pi;
            return((x*std)+mean);
        }

        x = Math.sqrt( -2.0 * Math.log(y) );
        x0 = x - Math.log(x)/x;

        z = 1.0/x;

        if( x < 8.0 ) /* y > exp(-32) = 1.2664165549e-14 */
            x1 = z * Mathematics.polevl( z, P1, 8 )/Mathematics.p1evl( z, Q1, 8 );
        else
            x1 = z * Mathematics.polevl( z, P2, 8 )/Mathematics.p1evl( z, Q2, 8 );
        x = x0 - x1;

        if( code != 0 )
            x = -x;

        return((x*std)+mean);
    }


    public double getLowerLimit() {
        return super.getLowerLimit();
    }

    public double getUpperLimit() {
        return super.getUpperLimit();
    }

    public double getCumulativeLowerLimit() {
        return super.getCumulativeLowerLimit();
    }

    public double getCumulativeUpperLimit() {
        return super.getCumulativeUpperLimit();
    }

    /**
     * Set a specified parameter.
     *
     * @param paramType String the name of the parameter
     * @param value (double) of the parameter
     * @throws IllegalArgumentException if std <= 0
     */
    public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_MEAN)) {
			this.params.get(PARAM_INDEX_MEAN).setValue(value);

		} else if (paramType.equalsIgnoreCase(PARAM_STD)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The standard deviation must be greater than zero.");
	        }
		    else {
    			this.params.get(PARAM_INDEX_STD).setValue(value);
		    }
        } else if (paramType.equalsIgnoreCase(PARAM_STDISFACTOR)) {
            if (value < 0.5) {
                this.params.get(PARAM_INDEX_STDISFACTOR).setValue(0.0);
            }
            else {
                this.params.get(PARAM_INDEX_STDISFACTOR).setValue(1.0);
            }
        } else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Return a random number from the probability distribution using the Polar Method.
     *
     * @param newRandom new Uniform Random Number
     * @return a random number from the distribution
     */
    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {

		double mean = getMean();
		double std = getStd();

	    if (std <= 0) {
	    	return mean;
	    }

        if (cacheRandomExists) {
            cacheRandomExists = false;
            return (cacheRandom);
        } else {
            //sample two random numbers from the Normal distribution using the Polar Method.
            double u1, u2, y, z;

            do {
                u1 = (2.0 * newRandom.nextDouble())-1.0;
                u2 = (2.0 * newRandom.nextDouble())-1.0;
                y = (u1*u1) + (u2*u2);
            }

            while (y >= 1.0);

            z = Math.sqrt( (-2.0 * Math.log(y) )/ y );

            cacheRandom = mean + (std * z * u1);  //add one sample to cache
            cacheRandomExists = true;

            return (mean + (std * z * u2)); //return the other sample
        }
    }

	/**
	 * Get the name of this distribution.
	 * @return String name of this distribution.
	 */
    public String toString() {
		return DISTRIBUTION_NAME;
	}

	/**
	 * Get the value of parameter mean.
	 * @return double mean.
	 */
    public double getMean() {
		return this.params.get(PARAM_INDEX_MEAN).getValue();
	}

	/**
	 * Get the value of parameter std.
	 * @return double std.
	 */
	public double getStd() {
		return this.params.get(PARAM_INDEX_STD).getValue();
	}

	public boolean isStdFactor() {
        return this.params.get(PARAM_INDEX_STDISFACTOR).getValue() >= 0.5 ;
	}
}
