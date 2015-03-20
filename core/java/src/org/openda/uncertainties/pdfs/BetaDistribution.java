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
 * Class implementing the beta distribution.
 * The BetaDistribution class implements the Beta probability distribution function with alpha
 * and beta as input parameters, together with the lower and upper limits of the
 * distribution (0 and 1, respectively, for the standard distribution). The location
 * parameter of the beta distribution is alpha and the scale parameter is beta - alpha.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class BetaDistribution extends PDF {

    // Cache variables shared by b* (beta random) methods
    private double a_last = 0.0, b_last = 0.0;
    private double a_, b_, t, fa, fb, p1, p2;

    // Cache variables for b00
    private double c;

    // Cache variables for b01
    private double ml, mu;

    // Cache variables for b1prs
    private double p_last = 0.0, q_last = 0.0;
    private double a, b, s, m, D, Dl, x1, x2, x4, x5, f1, f2, f4, f5;
    private double ll, lr, z2, z4, p3, p4;


	//distribution name.
	private static final String DISTRIBUTION_NAME = "Beta";

	//constants for the names of the
    // meters of this distribution.
	private static final String PARAM_ALPHA = "Alpha";
	private static final String PARAM_BETA = "Beta";
	private static final String PARAM_MIN = "Min";
	private static final String PARAM_MAX = "Max";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_ALPHA = 0;
	private static final int PARAM_INDEX_BETA = 1;
	private static final int PARAM_INDEX_MIN = 2;
	private static final int PARAM_INDEX_MAX = 3;

	//default values for the parameters.
	private static final double DEFAULT_ALPHA = 2.0;
	private static final double DEFAULT_BETA = 2.0;
	private static final double DEFAULT_MIN = 0.0;
	private static final double DEFAULT_MAX = 1.0;

    public BetaDistribution newInstance() {
        return new BetaDistribution() ;
    }

    /**
	 * Make a beta distribution with default values for the parameters.
	 */
	public BetaDistribution() {
        super();
		params.add(new FunctionParameter(PARAM_ALPHA, DEFAULT_ALPHA));
		params.add(new FunctionParameter(PARAM_BETA, DEFAULT_BETA));
		params.add(new FunctionParameter(PARAM_MIN, DEFAULT_MIN));
		params.add(new FunctionParameter(PARAM_MAX, DEFAULT_MAX));
	}

    /**
     * Constructs the PDF with alpha and beta parameters, together with the lower
     * and upper limits of the distribution.
     *
     * @param alpha the alpha parameter
     * @param beta the beta parameter
     * @param min the minimum value
     * @param max the maximum value
     * @throws IllegalArgumentException if min >= max or alpha <= 0 or beta <= 0
     */
	public BetaDistribution(double alpha, double beta, double min, double max) throws IllegalArgumentException {
        super();

		if (min >= max) {//check if supplied xMin value < supplied xMax value.
            throw new IllegalArgumentException("The minimum value must be less than the maximum value.");

		} else if (alpha <= 0 || beta <= 0) {//check if alpha > 0 and beta > 0.
            throw new IllegalArgumentException("The alpha (location) and beta (scale) parameters must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_ALPHA, alpha));
		params.add(new FunctionParameter(PARAM_BETA, beta));
		params.add(new FunctionParameter(PARAM_MIN, min));
		params.add(new FunctionParameter(PARAM_MAX, max));
	}

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit.
     * @return the probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double alpha = getAlpha();
		double beta = getBeta();
		double min = getMin();
		double max = getMax();

		if(x < min) {
            return 0.0;
        }
        else if(x > max) {
            return 1.0;
        }
        x = (x-min)/(max-min);
        return Mathematics.incompleteBeta(alpha, beta, x);
	}

    /**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required.
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double alpha = getAlpha();
		double beta = getBeta();
		double min = getMin();
		double max = getMax();

		double top = Math.pow(x - min,alpha-1.0) * Math.pow(max-x,beta-1.0);
        return top / (Mathematics.beta(alpha,beta) * Math.pow(max-min, alpha+beta-1.0));
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     * This method is not implemented yet.
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
        throw new IllegalArgumentException("Method not yet implemented.");
    }


    public double getLowerLimit() {
        return this.params.get(PARAM_INDEX_MIN).getValue();
    }

    public double getUpperLimit() {
        return this.params.get(PARAM_INDEX_MAX).getValue();
    }

    public double getCumulativeLowerLimit() {
        return this.params.get(PARAM_INDEX_MIN).getValue();
    }

    public double getCumulativeUpperLimit() {
        return this.params.get(PARAM_INDEX_MAX).getValue();
    }

    /**
     * Set a specified paramater (alpha, beta, min or max).
     *
     * @param paramType
     * @param value
     *
     * @throws IllegalArgumentException if min >= max or alpha <= 0 or beta <= 0
     */
    public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_ALPHA)) {
    		if (value <= 0) {
                throw new IllegalArgumentException("The alpha (location) and beta (scale) parameters must be greater than zero.");
    		}
    		else {
    			this.params.get(PARAM_INDEX_ALPHA).setValue(value);
    		}

		} else if (paramType.equalsIgnoreCase(PARAM_BETA)) {
    		if (value <= 0) {
                throw new IllegalArgumentException("The alpha (location) and beta (scale) parameters must be greater than zero.");
    		}
    		else {
	    		this.params.get(PARAM_INDEX_BETA).setValue(value);
    		}

		} else if (paramType.equalsIgnoreCase(PARAM_MIN)) {
			if (value >= this.params.get(PARAM_INDEX_MAX).getValue()) {
	            throw new IllegalArgumentException("The minimum value must be less than the maximum value.");
			}
			else {
                this.params.get(PARAM_INDEX_MIN).setValue(value);
			}

		} else if (paramType.equalsIgnoreCase(PARAM_MAX)) {
			if (value <= this.params.get(PARAM_INDEX_MIN).getValue()) {
	            throw new IllegalArgumentException("The minimum value must be less than the maximum value.");
			}
			else {
				this.params.get(PARAM_INDEX_MAX).setValue(value);
			}

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Returns a random sample from the beta distribution using Stratified Rejection/Patchwork
     * Rejection.
     *
     * <dt>This is a port of <tt>bsprc.c</tt> from the
     * <A HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND / WIN-RAND</A> library.
     * C-RAND's implementation, in turn, is based upon <p> H. Sakasegawa (1983): Stratified rejection
     * and squeeze method for generating beta random numbers, Ann. Inst. Statist. Math. 35 B, 291-302.
     * <p>
     * and
     * <p>
     * Stadlober E., H. Zechner (1993), <A HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">
     * Generating beta variates via patchwork rejection,</A>, Computing 50, 1-18.
     *
     * Some superficial changes have been made in the method calls (names, parameters etc.) for the current
     * implementation (brown@science.uva.nl).
     *
     * @author wolfgang.hoschek@cern.ch (original author)
     * @version 1.0, 09/24/99
     */

    /******************************************************************
     *                                                                *
     * Beta Distribution - Stratified Rejection/Patchwork Rejection   *
     *                                                                *
     ******************************************************************
     * For parameters a < 1 , b < 1  and  a < 1 < b   or  b < 1 < a   *
     * the stratified rejection methods b00 and b01 of Sakasegawa are *
     * used. Both procedures employ suitable two-part power functions *
     * from which samples can be obtained by inversion.               *
     * If a > 1 , b > 1 (unimodal case) the patchwork rejection       *
     * method b1prs of Zechner/Stadlober is utilized:                 *
     * The area below the density function f(x) in its body is        *
     * rearranged by certain point reflections. Within a large center *
     * interval variates are sampled efficiently by rejection from    *
     * uniform hats. Rectangular immediate acceptance regions speed   *
     * up the generation. The remaining tails are covered by          *
     * exponential functions.                                         *
     * If (a-1)(b-1) = 0  sampling is done by inversion if either a   *
     * or b are not equal to one. If  a = b = 1  a uniform random     *
     * variate is delivered.                                          *
     *                                                                *
     ******************************************************************
     *                                                                *
     * FUNCTION :   - br3 samples a random variate from the beta    *
     *                distribution with parameters  a > 0, b > 0.     *
     * REFERENCES : - H. Sakasegawa (1983): Stratified rejection and  *
     *                squeeze method for generating beta random       *
     *                numbers, Ann. Inst. Statist. Math. 35 B,        *
     *                291-302.                                        *
     *              - H. Zechner, E. Stadlober (1993): Generating     *
     *                beta variates via patchwork rejection,          *
     *                Computing 50, 1-18.                             *
     *                                                                *
     * SUBPROGRAMS: - newRandom() ... (0,1) Uniform random number.    *
     *              - b00(seed,a,b) ... Beta generator for a<1, b<1   *
     *              - b01(seed,a,b) ... Beta generator for a<1<b or   *
     *                                  b<1<a                         *
     *              - b1prs(seed,a,b) ... Beta generator for a>1, b>1 *
     *                with unsigned long integer *seed, double a, b.  *
     *                                                                *
     ******************************************************************/

    /**
     * Returns a random number from the beta distribution.
     *
     * @param newRandom a uniform random number generator
     * @return a random number from the distribution
     */
    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {
		double alpha = getAlpha();
		double beta = getBeta();
		double min = getMin();
		double max = getMax();


        double a = alpha;
        double b = beta;

        double fin = 0.0;

        if (a  > 1.0) {
            if (b  > 1.0) {
                fin = b1prs(a, b, newRandom);
            }
            if (b  < 1.0) {
                fin = 1.0 - b01(b, a, newRandom);
            }
            if (b == 1.0) {
                fin = Math.exp(Math.log(newRandom.nextDouble()) / a);
            }
            return min + ((max-min) * fin);
        }

        if (a  < 1.0) {
            if (b  > 1.0) {
                fin = b01(a, b, newRandom);
            }
            if (b  < 1.0) {
                fin = b00(a, b, newRandom);
            }
            if (b == 1.0) {
                fin = Math.exp(Math.log(newRandom.nextDouble()) / a);
            }
            return min + ((max-min) * fin);
        }

        if (a == 1.0) {
            if (b != 1.0) {
                fin = 1.0 - Math.exp(Math.log(newRandom.nextDouble()) / b);
            }
            if (b == 1.0) {
                fin = newRandom.nextDouble();
            }
            return min + ((max-min) * fin);
        }
        return 0.0;
    }

    /**
     * Internal method for random number generation.
     *
     * @param a alpha
     * @param b beta
     * @param newRandom random number U[0,1]
     * @return a random number
     */

    private double b00(double a, double b, UniformRandomNumber newRandom) {
        double U, V, X, Z;

        if (a != a_last || b != b_last) {
            a_last = a;
            b_last = b;

            a_ = a - 1.0;
            b_ = b - 1.0;
            c = (b * b_) / (a * a_);                            // b(1-b) / a(1-a)
            t = (c == 1.0) ? 0.5 : (1.0 - Math.sqrt(c))/(1.0 - c);  // t = t_opt
            fa = Math.exp(a_ * Math.log(t));
            fb = Math.exp(b_ * Math.log(1.0 - t));              // f(t) = fa * fb

            p1 = t/a;                                           // 0 < X < t
            p2 = (1.0 - t)/b + p1;                              // t < X < 1
        }

        for (;;) {
            if ((U = newRandom.nextDouble() * p2) <= p1) {       //  X < t
                Z = Math.exp(Math.log(U/p1) / a);  X = t*Z;
                // squeeze accept:   L(x) = 1 + (1 - b)x
                if ((V = newRandom.nextDouble() * fb) <= 1.0 - b_*X)  break;
                // squeeze reject:   U(x) = 1 + ((1 - t)^(b-1) - 1)/t * x
                if (V <= 1.0 + (fb - 1.0)*Z) {
                    // quotient accept:  q(x) = (1 - x)^(b-1) / fb
                    if (Math.log(V) <= b_ * Math.log(1.0 - X))  break;
                }
            } else {                                                      //  X > t
                Z = Math.exp(Math.log((U-p1)/(p2-p1)) / b);  X = 1.0 - (1.0 - t)*Z;
                // squeeze accept:   L(x) = 1 + (1 - a)(1 - x)
                if ((V = newRandom.nextDouble() * fa) <= 1.0 - a_*(1.0 - X))  break;
                // squeeze reject:   U(x) = 1 + (t^(a-1) - 1)/(1 - t) * (1 - x)
                if (V <= 1.0 + (fa - 1.0)*Z) {
                    // quotient accept:  q(x) = x^(a-1) / fa
                    if (Math.log(V) <= a_ * Math.log(X))  break;
                }
            }
        }
        return(X);
    }

    /**
     * Internal method for random number generation.
     *
     * @param a alpha
     * @param b beta
     * @param newRandom random number U[0,1]
     * @return a random number
     */

    private double b01(double a, double b, UniformRandomNumber newRandom) {
        double U, V, X, Z;

        if (a != a_last || b != b_last) {
            a_last = a;
            b_last = b;

            a_ = a - 1.0;
            b_ = b - 1.0;
            t = a_/(a - b);                   // one step Newton * start value t
            fb = Math.exp((b_ - 1.0) * Math.log(1.0 - t));  fa = a - (a + b_)*t;
            t -= (t - (1.0 - fa) * (1.0 - t)*fb / b) / (1.0 - fa*fb);
            fa = Math.exp(a_ * Math.log(t));
            fb = Math.exp(b_ * Math.log(1.0 - t));             // f(t) = fa * fb
            if (b_ <= 1.0) {
                ml = (1.0 - fb) / t;                           //   ml = -m1
                mu = b_ * t;                                   //   mu = -m2 * t
            } else {
                ml = b_;
                mu = 1.0 - fb;
            }
            p1 = t/a;                                           //  0 < X < t
            p2 = fb * (1.0 - t)/b + p1;                         //  t < X < 1
        }

        for (;;) {
            if ((U = newRandom.nextDouble() * p2) <= p1) {       //  X < t
                Z = Math.exp(Math.log(U/p1) / a);  X = t*Z;
                // squeeze accept:   L(x) = 1 + m1*x,  ml = -m1
                if ((V = newRandom.nextDouble() ) <= 1.0 - ml*X)  break;
                // squeeze reject:   U(x) = 1 + m2*x,  mu = -m2 * t
                if (V <= 1.0 - mu*Z) {
                    // quotient accept:  q(x) = (1 - x)^(b-1)
                    if (Math.log(V) <= b_ * Math.log(1.0 - X))  break;
                }
            } else {                                                      //  X > t
                Z = Math.exp(Math.log((U-p1)/(p2-p1)) / b);  X = 1.0 - (1.0 - t)*Z;
                // squeeze accept:   L(x) = 1 + (1 - a)(1 - x)
                if ((V = newRandom.nextDouble()  * fa) <= 1.0 - a_*(1.0 - X))  break;
                // squeeze reject:   U(x) = 1 + (t^(a-1) - 1)/(1 - t) * (1 - x)
                if (V <= 1.0 + (fa - 1.0)*Z) {
                    // quotient accept:  q(x) = (x)^(a-1) / fa
                    if (Math.log(V) <= a_ * Math.log(X))  break;
                }
            }
        }
        return(X);
    }

    /**
     * Internal method for random number generation.
     *
     * @param p alpha
     * @param q beta
     * @param newRandom random number U[0,1]
     * @return a random number
     */

    private double b1prs(double p, double q, UniformRandomNumber newRandom) {
        double U, V, W, X, Y;

        if (p != p_last || q != q_last) {
            p_last = p;
            q_last = q;

            a = p - 1.0;
            b = q - 1.0;
            s = a + b;   m = a / s;
            if (a > 1.0 || b > 1.0)  D = Math.sqrt(m * (1.0 - m) / (s - 1.0));

            if (a <= 1.0) {
                x2 = (Dl = m * 0.5);  x1 = z2 = 0.0;  f1 = ll = 0.0;
            } else {
                x2 = m - D;  x1 = x2 - D;
                z2 = x2 * (1.0 - (1.0 - x2)/(s * D));
                if (x1 <= 0.0 || (s - 6.0) * x2 - a + 3.0 > 0.0) {
                    x1 = z2;  x2 = (x1 + m) * 0.5;
                    Dl = m - x2;
                } else {
                    Dl = D;
                }
                f1 = f(x1, a, b, m);
                ll = x1 * (1.0 - x1) / (s * (m - x1));          // z1 = x1 - ll
            }
            f2 = f(x2, a, b, m);

            if (b <= 1.0) {
                x4 = 1.0 - (D = (1.0 - m) * 0.5);  x5 = z4 = 1.0;  f5 = lr = 0.0;
            } else {
                x4 = m + D;  x5 = x4 + D;
                z4 = x4 * (1.0 + (1.0 - x4)/(s * D));
                if (x5 >= 1.0 || (s - 6.0) * x4 - a + 3.0 < 0.0) {
                    x5 = z4;  x4 = (m + x5) * 0.5;
                    D = x4 - m;
                }
                f5 = f(x5, a, b, m);
                lr = x5 * (1.0 - x5) / (s * (x5 - m));          // z5 = x5 + lr
            }
            f4 = f(x4, a, b, m);

            p1 = f2 * (Dl + Dl);                                //  x1 < X < m
            p2 = f4 * (D  + D) + p1;                            //  m  < X < x5
            p3 = f1 * ll       + p2;                            //       X < x1
            p4 = f5 * lr       + p3;                            //  x5 < X
        }

        for (;;) {
            if ((U = newRandom.nextDouble() * p4) <= p1) {
                // immediate accept:  x2 < X < m, - f(x2) < W < 0
                if ((W = U/Dl - f2) <= 0.0)  return(m - U/f2);
                // immediate accept:  x1 < X < x2, 0 < W < f(x1)
                if (W <= f1)  return(x2 - W/f1 * Dl);
                // candidates for acceptance-rejection-test
                V = Dl * (U = newRandom.nextDouble());
                X = x2 - V;  Y = x2 + V;
                // squeeze accept:    L(x) = f(x2) (x - z2) / (x2 - z2)
                if (W * (x2 - z2) <= f2 * (X - z2))  return(X);
                if ((V = f2 + f2 - W) < 1.0) {
                    // squeeze accept:    L(x) = f(x2) + (1 - f(x2))(x - x2)/(m - x2)
                    if (V <= f2 + (1.0 - f2) * U)  return(Y);
                    // quotient accept:   x2 < Y < m,   W >= 2f2 - f(Y)
                    if (V <= f(Y, a, b, m))  return(Y);
                }
            } else if (U <= p2) {
                U -= p1;
                // immediate accept:  m < X < x4, - f(x4) < W < 0
                if ((W = U/D - f4) <= 0.0)  return(m + U/f4);
                // immediate accept:  x4 < X < x5, 0 < W < f(x5)
                if (W <= f5)  return(x4 + W/f5 * D);
                // candidates for acceptance-rejection-test
                V = D * (U = newRandom.nextDouble());
                X = x4 + V;  Y = x4 - V;
                // squeeze accept:    L(x) = f(x4) (z4 - x) / (z4 - x4)
                if (W * (z4 - x4) <= f4 * (z4 - X))  return(X);
                if ((V = f4 + f4 - W) < 1.0) {
                    // squeeze accept:    L(x) = f(x4) + (1 - f(x4))(x4 - x)/(x4 - m)
                    if (V <= f4 + (1.0 - f4) * U)  return(Y);
                    // quotient accept:   m < Y < x4,   W >= 2f4 - f(Y)
                    if (V <= f(Y, a, b, m))  return(Y);
                }
            } else if (U <= p3) {                                     // X < x1
                Y = Math.log(U = (U - p2)/(p3 - p2));
                if ((X = x1 + ll * Y) <= 0.0)  continue;            // X > 0!!
                W = newRandom.nextDouble() * U;
                // squeeze accept:    L(x) = f(x1) (x - z1) / (x1 - z1)
                //                    z1 = x1 - ll,   W <= 1 + (X - x1)/ll
                if (W <= 1.0 + Y)  return(X);
                W *= f1;
            } else {                                                  // x5 < X
                Y = Math.log(U = (U - p3)/(p4 - p3));
                if ((X = x5 - lr * Y) >= 1.0)  continue;            // X < 1!!
                W = newRandom.nextDouble() * U;
                // squeeze accept:    L(x) = f(x5) (z5 - x) / (z5 - x5)
                //                    z5 = x5 + lr,   W <= 1 + (x5 - X)/lr
                if (W <= 1.0 + Y)  return(X);
                W *= f5;
            }
            // density accept:  f(x) = (x/m)^a ((1 - x)/(1 - m))^b
            if (Math.log(W) <= a*Math.log(X/m) + b*Math.log((1.0 - X)/(1.0 - m)))  return(X);
        }
    }

    /**
     * Internal method call for nextDouble().
     *
     * @param x
     * @param a
     * @param b
     * @param m
     * @return double
     */
    private double f(double x, double a, double b, double m) {
        return Math.exp(a*Math.log(x/m) + b*Math.log((1.0 - x)/(1.0 - m)));
    }

    /**
	 * Get the name of this distribution.
	 * @return String name of this ditribution.
	 */
	public String toString() {
		return DISTRIBUTION_NAME;
	}

	/**
	 * Get the value of param alpha.
	 * @return double alpha.
	 */
	public double getAlpha() {
		return this.params.get(PARAM_INDEX_ALPHA).getValue();
	}

	/**
	 * Get the value of param beta.
	 * @return double beta.
	 */
	public double getBeta() {
		return this.params.get(PARAM_INDEX_BETA).getValue();
	}

	/**
	 * Get the value of param minimum.
	 * @return double min.
	 */
	public double getMin() {
		return this.params.get(PARAM_INDEX_MIN).getValue();
	}

	/**
	 * Get the value o param max.
	 * @return double max.
	 */
	public double getMax() {
		return this.params.get(PARAM_INDEX_MAX).getValue();
	}
}
