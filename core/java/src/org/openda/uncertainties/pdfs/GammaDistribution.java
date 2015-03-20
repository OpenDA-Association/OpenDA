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

import org.openda.uncertainties.FunctionParameter;

import due.utilities.mathutil.Mathematics;
import due.utilities.mathutil.UniformRandomNumber;

/**
 * The GammaDistribution class implements the Gamma probability distribution function with
 * a shape parameter and a scale parameter as inputs.  The location
 * parameter is not included.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class GammaDistribution extends PDF {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Gamma";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_SHAPE = "Shape";
	private static final String PARAM_SCALE = "Scale";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_SHAPE = 0;
	private static final int PARAM_INDEX_SCALE = 1;

	//default values for the parameters.
	private static final double DEFAULT_SHAPE = 2.0;
	private static final double DEFAULT_SCALE = 2.0;

    public GammaDistribution newInstance() {
        return new GammaDistribution() ;
    }

	/**
	 * Construct a gamma distribution with default values for the parameters.
	 */
	public GammaDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_SHAPE, DEFAULT_SHAPE));
		params.add(new FunctionParameter(PARAM_SCALE, DEFAULT_SCALE));
	}

    /**
     * Construct pdf with a shape and a scale as input parameters assume
     * shape = mean*mean/variance and scale = 1/(variance/mean)
     *
     * @param shape the shape parameter
     * @param scale the scale parameter
     * @throws IllegalArgumentException if shape <= 0 or scale <= 0
     */
	public GammaDistribution(double shape, double scale) throws IllegalArgumentException {
        super();

        if (shape <= 0.0  || scale <= 0.0) {
            throw new IllegalArgumentException("The shape and scale parameters must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_SHAPE, shape));
		params.add(new FunctionParameter(PARAM_SCALE, scale));
	}

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double shape = this.getShape();
		double scale = this.getScale();

        if( x < 0.0 ) {
            return 0.0;
        }
        else {
            return Mathematics.incompleteGamma(scale, shape*x);
        }
	}

	/**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double shape = this.getShape();
		double scale = this.getScale();

        if(x < 0.0) {
            throw new IllegalArgumentException("The pdf is undefined for x < 0.0.");
        }
        return ( ( ( Math.pow(x,shape-1.0) * Math.exp(-x) ) / Mathematics.gamma(shape) ) * scale );
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).  Uses the
     * inverse CDF of the ChiSquare distribution.
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
		double shape = this.getShape();

        ChiSquareDistribution newChi = new ChiSquareDistribution(2.0*shape);
        return newChi.getInverseCDFPoint(probability)/(2.0*shape);
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
     * @param String paramType the name of the parameter
     * @param double value of the parameter
     * @throws IllegalArgumentException if shape <= 0 or scale <= 0
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_SHAPE)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The shape and scale parameters must be greater than zero.");
	        }
		    else {
    			this.params.get(PARAM_INDEX_SHAPE).setValue(value);
		    }

		} else if (paramType.equalsIgnoreCase(PARAM_SCALE)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The shape and scale parameters must be greater than zero.");
	        }
		    else {
    			this.params.get(PARAM_INDEX_SCALE).setValue(value);
		    }

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Returns a random number from the distribution using Acceptance Rejection combined
     * with Acceptance Complement.
     * <dt>
     * This is a port of <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandGamma.html">RandGamma</A>
     * used in <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++).
     * CLHEP's implementation, in turn, is based on <tt>gds.c</tt> from the
     * <A HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND / WIN-RAND</A> library.
     * C-RAND's implementation, in turn, is based upon
     * <p>
     * J.H. Ahrens, U. Dieter (1974): Computer methods for sampling from gamma, beta, Poisson and binomial distributions,
     * Computing 12, 223-246.
     * <p>
     * and
     * <p>
     * J.H. Ahrens, U. Dieter (1982): Generating gamma variates by a modified rejection technique,
     * Communications of the ACM 25, 47-54.
     *
     * Some superficial changes have been made in the method calls (names, parameters etc.) for the current
     * implementation (brown@science.uva.nl).
     *
     * @param newRandom new Uniform Random Number
     * @return a random number from the distribution
     * @author wolfgang.hoschek@cern.ch (original author)
     * @version 1.0, 09/24/99
     */

    /******************************************************************
     *                                                                *
     *    Gamma Distribution - Acceptance Rejection combined with     *
     *                         Acceptance Complement                  *
     *                                                                *
     ******************************************************************
     *                                                                *
     * FUNCTION:    - nextDouble() samples a random number from the   *
     *		  gamma distribution with parameters scale and    *
     *		  shape > 0.                                      *
     *                Acceptance Rejection  gs  for  a < 1 ,          *
     *                Acceptance Complement gd  for  a >= 1.          *
     * REFERENCES:  - J.H. Ahrens, U. Dieter (1974): Computer methods *
     *                for sampling from gamma, beta, Poisson and      *
     *                binomial distributions, Computing 12, 223-246.  *
     *              - J.H. Ahrens, U. Dieter (1982): Generating gamma *
     *                variates by a modified rejection technique,     *
     *                Communications of the ACM 25, 47-54.            *
     * SUBPROGRAMS: - newRandom ...(0,1)- Uniform random number       *
     *                                                                *
     ******************************************************************/

    /**
     * Returns a random number from the gamma distribution.
     *
     * @param newRandom a uniform random number generator
     * @return a random number from the distribution
     */
    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {
		double shape = this.getShape();
		double scale = this.getScale();


        double a = shape;
        double aa = -1.0, aaa = -1.0, b=0.0, c=0.0, d=0.0, e, r, s=0.0,
        si=0.0, ss=0.0, q0=0.0, q1 = 0.0416666664, q2 =  0.0208333723,
        q3 = 0.0079849875, q4 = 0.0015746717, q5 = -0.0003349403,
        q6 = 0.0003340332, q7 = 0.0006053049, q8 = -0.0004701849,
        q9 = 0.0001710320, a1 = 0.333333333,  a2 = -0.249999949,
        a3 = 0.199999867, a4 =-0.166677482,  a5 =  0.142873973,
        a6 =-0.124385581, a7 = 0.110368310,  a8 = -0.112750886,
        a9 = 0.104089866, e1 = 1.000000000,  e2 =  0.499999994,
        e3 = 0.166666848, e4 = 0.041664508,  e5 =  0.008345522,
        e6 = 0.001353826, e7 = 0.000247453;

        double gds,p,q,t,sign_u,u,v,w,x;
        double v1,v2,v12;

        if (a < 1.0) { // CASE A: Acceptance rejection algorithm gs
            b = 1.0 + 0.36788794412 * a;              // Step 1
            for(;;) {
                p = b * newRandom.nextDouble();

                if (p <= 1.0) {                       // Step 2. Case gds <= 1
                    gds = Math.exp(Math.log(p) / a);
                    if (Math.log(newRandom.nextDouble()) <= -gds) return(gds*scale);
                }

                else {                                // Step 3. Case gds > 1
                    gds = - Math.log((b - p) / a);
                    if (Math.log(newRandom.nextDouble()) <= ((a - 1.0) * Math.log(gds))) return(gds*scale);
                }
            }
        }

        else { // CASE B: Acceptance complement algorithm gd (gaussian distribution, box muller transformation)
            if (a != aa) {                            // Step 1. Preparations
                aa = a;
                ss = a - 0.5;
                s = Math.sqrt(ss);
                d = 5.656854249 - 12.0 * s;
            }
            // Step 2. Normal deviate
            do {
                v1 = 2.0 * newRandom.nextDouble() - 1.0;
                v2 = 2.0 * newRandom.nextDouble() - 1.0;
                v12 = v1*v1 + v2*v2;
            } while ( v12 > 1.0 );
            t = v1*Math.sqrt(-2.0*Math.log(v12)/v12);
            x = s + 0.5 * t;
            gds = x * x;
            if (t >= 0.0) return(gds*scale);         // Immediate acceptance

            u = newRandom.nextDouble();                // Step 3. Uniform random number
            if (d * u <= t * t * t) return(gds*scale); // Squeeze acceptance

            if (a != aaa) {                           // Step 4. Set-up for hat case
                aaa = a;
                r = 1.0 / a;
                q0 = ((((((((q9 * r + q8) * r + q7) * r + q6) * r + q5) * r + q4) *
                r + q3) * r + q2) * r + q1) * r;
                if (a > 3.686) {

                    if (a > 13.022) {
                        b = 1.77;
                        si = 0.75;
                        c = 0.1515 / s;
                    }

                    else {
                        b = 1.654 + 0.0076 * ss;
                        si = 1.68 / s + 0.275;
                        c = 0.062 / s + 0.024;
                    }
                }

                else {
                    b = 0.463 + s - 0.178 * ss;
                    si = 1.235;
                    c = 0.195 / s - 0.079 + 0.016 * s;
                }
            }

            if (x > 0.0) {                               // Step 5. Calculation of q
                v = t / (s + s);                         // Step 6.

                if (Math.abs(v) > 0.25) {
                    q = q0 - s * t + 0.25 * t * t + (ss + ss) * Math.log(1.0 + v);
                }

                else {
                    q = q0 + 0.5 * t * t * ((((((((a9 * v + a8) * v + a7) * v + a6) *
                    v + a5) * v + a4) * v + a3) * v + a2) * v + a1) * v;
                }                                        // Step 7. Quotient acceptance
                if (Math.log(1.0 - u) <= q) return(gds*scale);
            }

            for(;;) {              			 // Step 8. Double exponential deviate t
                do {
                    e = -Math.log(newRandom.nextDouble());
                    u = newRandom.nextDouble();
                    u = u + u - 1.0;
                    sign_u = (u > 0)? 1.0 : -1.0;
                    t = b + (e * si) * sign_u;
                } while (t <= -0.71874483771719);        // Step 9. Rejection of t
                v = t / (s + s);                         // Step 10. New q(t)

                if (Math.abs(v) > 0.25) {
                    q = q0 - s * t + 0.25 * t * t + (ss + ss) * Math.log(1.0 + v);
                }

                else {
                    q = q0 + 0.5 * t * t * ((((((((a9 * v + a8) * v + a7) * v + a6) *
                    v + a5) * v + a4) * v + a3) * v + a2) * v + a1) * v;
                }
                if (q <= 0.0) continue;                  // Step 11.
                if (q > 0.5) {
                    w = Math.exp(q) - 1.0;
                }
                else {
                    w = ((((((e7 * q + e6) * q + e5) * q + e4) * q + e3) * q + e2) * q + e1) * q;
                }                    			 // Step 12. Hat acceptance
                if ( c * u * sign_u <= w * Math.exp(e - 0.5 * t * t)) {
                    x = s + 0.5 * t;
                    return(x*x*scale);
                }
            }
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
	 * Get the value of parameter shape.
	 * @return double shape.
	 */
	public double getShape() {
		return this.params.get(PARAM_INDEX_SHAPE).getValue();
	}

	/**
	 * Get the value of parameter scale
	 * @return double scale.
	 */
	public double getScale() {
		return this.params.get(PARAM_INDEX_SCALE).getValue();
	}
}
