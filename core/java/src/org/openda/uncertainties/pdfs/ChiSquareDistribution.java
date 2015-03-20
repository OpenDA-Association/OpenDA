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

/**
 * The ChiSquareDistribution class implements the Chi-squared probability distribution
 * function with a degrees of freedom parameter as input.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class ChiSquareDistribution extends PDF {

    /**
     * Cached variables for method getRandom()
     */
    private double freedom_in = -1.0, b, vm, vp, vd;

	//distribution name.
	private static final String DISTRIBUTION_NAME = "ChiSquare";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_SHAPE = "Shape";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_SHAPE = 0;

	//default values for the parameters.
	private static final double DEFAULT_SHAPE = 10.0;

    public ChiSquareDistribution newInstance() {
        return new ChiSquareDistribution() ;
    }

	/**
	 * Construct a chi squared distribution with default values for the parameters.
	 */
	public ChiSquareDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_SHAPE, DEFAULT_SHAPE));
	}

    /**
     * Construct the PDF with a degrees of freedom parameter.
     *
     * @param shape the degrees of freedom parameter
     * @throws IllegalArgumentException if shape <= 0
     */
	public ChiSquareDistribution(double shape) throws IllegalArgumentException {
        super();

		if (shape <= 0) {
            throw new IllegalArgumentException("The shape parameter must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_SHAPE, shape));
	}

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double shape = this.getShape();

        if (x < 0 || shape < 1.0) {
            return 0.0;
        }
        return ( Mathematics.incompleteGamma(shape/2.0,x/2.0) );

	}

	/**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double shape = this.getShape();

		if (x < 0) {
            throw new IllegalArgumentException("The pdf is undefined for x < 0.");
        } else {
            return Math.exp( (shape/2.0 - 1.0) * Math.log(x/2.0) - x/2.0 - Mathematics.logGamma(shape/2.0) ) / 2.0;
        }
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).  Based on:
     *
     * Best DJ & Roberts DE (1975) The percentage points of the Chi2 distribution.
     * Applied Statistics 24: 385-388.  (AS91)
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
		double shape = this.getShape();

		NormalDistribution tempNormal = new NormalDistribution(0.0, 1.0, 0);

        double e = 0.5e-6, aa = 0.6931471805, g;
        double xx, c, ch, a = 0, q = 0, p1 = 0, p2 = 0, t = 0, x = 0, b = 0, s1, s2, s3, s4, s5, s6;
        if (probability < 0.000002 || probability > 0.999998) {
            throw new ArithmeticException("Error: probability too small for evaluation.");
        }
        g = Mathematics.logGamma(shape/2.0);
        xx = shape/2;
        c = xx-1;
        if (shape < -1.24*Math.log(probability)) {
            ch = Math.pow((probability*xx*Math.exp(g+xx*aa)), 1/xx);
            if (ch-e < 0) {
                return ch;
            }
        } else {
            if (shape > 0.32) {
                x = tempNormal.getInverseCDFPoint(probability);
                p1 = 0.222222/shape;
                ch = shape*Math.pow((x*Math.sqrt(p1)+1-p1), 3.0);
                if(ch>2.2*shape+6) {
                    ch=-2*(Math.log(1-probability)-c*Math.log(.5*ch)+g);
                }
            } else {
                ch = 0.4;
                a = Math.log(1-probability);

                do {
                    q = ch;
                    p1 = 1+ch*(4.67+ch);
                    p2 = ch*(6.73+ch*(6.66+ch));
                    t = -0.5+(4.67+2*ch)/p1 - (6.73+ch*(13.32+3*ch))/p2;
                    ch -= (1-Math.exp(a+g+.5*ch+c*aa)*p2/p1)/t;
                }
                while (Math.abs(q/ch-1)-.01 > 0);
            }
        }
        do {
            q = ch;
            p1 = 0.5*ch;
            if ((t = Mathematics.incompleteGamma(xx,p1)) < 0) {
                throw new ArithmeticException();
            }
            p2 = probability-t;
            t = p2*Math.exp(xx*aa+g+p1-c*Math.log(ch));
            b = t/ch;
            a = 0.5*t-b*c;

            s1 = (210+a*(140+a*(105+a*(84+a*(70+60*a)))))/420;
            s2 = (420+a*(735+a*(966+a*(1141+1278*a))))/2520;
            s3 = (210+a*(462+a*(707+932*a)))/2520;
            s4 = (252+a*(672+1182*a)+c*(294+a*(889+1740*a)))/5040;
            s5 = (84+264*a+c*(175+606*a))/2520;
            s6 = (120+c*(346+127*c))/5040;
            ch += t*(1+0.5*t*s1-b*c*(s1-b*(s2-b*(s3-b*(s4-b*(s5-b*s6))))));
        }
        while (Math.abs(q/ch-1) > e);
        return (ch);
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
     * @param paramType paramType the name of the parameter
     * @param value of the parameter
     * @throws IllegalArgumentException if shape <= 0
     */
    public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_SHAPE)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The shape parameter must be greater than zero.");
	        }
		    else {
				this.params.get(PARAM_INDEX_SHAPE).setValue(value);
		    }

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Return a random number from the probability distribution using the ratio of
     * uniforms with a shift.
     * <dt>
     * This is a port of <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandChiSquare.html">RandChiSquare</A>
     * used in <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++).
     * CLHEP's implementation, in turn, is based on <tt>chru.c</tt> from the
     * <A HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND / WIN-RAND</A> library.
     * C-RAND's implementation, in turn, is based upon
     * <p>J.F. Monahan (1987): An algorithm for generating chi random variables, ACM Trans.
     * Math. Software 13, 168-172.
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
     *        Chi Distribution - Ratio of Uniforms with shift         *
     *                                                                *
     ******************************************************************
     *                                                                *
     * FUNCTION :   - getRandom() samples a random number from the    *
     *                Chi distribution with parameter  a > 1.        *
     * REFERENCE :  - J.F. Monahan (1987): An algorithm for           *
     *                generating chi random variables, ACM Trans.     *
     *                Math. Software 13, 168-172.                     *
     * SUBPROGRAM : - anEngine  ... pointer to a (0,1)-Uniform        *
     *                engine                                          *
     *                                                                *
     * Implemented by R. Kremer, 1990                                 *
     ******************************************************************/

    /**
     * Returns a random number from the chiSquare distribution.
     *
     * @param newRandom a uniform random number generator
     * @return a random number from the distribution
     */
    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {
		double shape = this.getShape();


        double u,v,z,zz,r;

        //if( a < 1 )  return (-1.0); // Check for invalid input value

        if (shape == 1.0) {

            for(;;) {
                u = newRandom.nextDouble();
                v = newRandom.nextDouble() * 0.857763884960707;
                z = v / u;
                if (z < 0) continue;
                zz = z * z;
                r = 2.5 - zz;
                if (z < 0.0) r = r + zz * z / (3.0 * z);
                if (u < r * 0.3894003915) return(z*z);
                if (zz > (1.036961043 / u + 1.4)) continue;
                if (2.0 * Math.log(u) < (- zz * 0.5 )) return(z*z);
            }
        }

        else {

            if (shape != freedom_in) {
                b = Math.sqrt(shape - 1.0);
                vm = - 0.6065306597 * (1.0 - 0.25 / (b * b + 1.0));
                vm = (-b > vm) ? -b : vm;
                vp = 0.6065306597 * (0.7071067812 + b) / (0.5 + b);
                vd = vp - vm;
                freedom_in = shape;
            }

            for(;;) {
                u = newRandom.nextDouble();
                v = newRandom.nextDouble() * vd + vm;
                z = v / u;
                if (z < -b) continue;
                zz = z * z;
                r = 2.5 - zz;
                if (z < 0.0) r = r + zz * z / (3.0 * (z + b));
                if (u < r * 0.3894003915) return((z + b)*(z + b));
                if (zz > (1.036961043 / u + 1.4)) continue;
                if (2.0 * Math.log(u) < (Math.log(1.0 + z / b) * b * b - zz * 0.5 - z * b)) return((z + b)*(z + b));
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
}
