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
import due.utilities.mathutil.FunctionLibrary;
import due.utilities.matrix.*;
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;

/**
 * Created by IntelliJ IDEA.
 * User: Juzer Dhondia
 * Date: 28-nov-2007
 * Time: 9:36:24
 */
public class JointNormalDistribution extends PDF {
    private AutoCorrelationFunction autoCorrelationFunction;
    private NormalDistribution normalDistribution;
    private DenseDoubleMatrix1D std;
    private boolean isFactorisedMatrixAvailable;
    private DenseDoubleMatrix2D covarianceMatrix;

    public JointNormalDistribution(NormalDistribution normalDistribution, AutoCorrelationFunction autoCorrelationFunction) {
        this();
        if (normalDistribution == null) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ": normalDistribution ==null");
        }
        this.normalDistribution = normalDistribution;
        setUncertainItem(this.normalDistribution.getUncertainItem());
        if (autoCorrelationFunction == null) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ": autoCorrelationFunction ==null");
        }
        this.autoCorrelationFunction = autoCorrelationFunction;
        isFactorisedMatrixAvailable = false;
    }

    public JointNormalDistribution newInstance() {
        return new JointNormalDistribution();
    }

    /**
     * Construct a normal distribution with default values for the parameters.
     */
    public JointNormalDistribution() {
        super();
        normalDistribution = null;
        autoCorrelationFunction = null;
        std = null;
    }

    /**
     * Return a random number from the probability distribution using the Polar Method.
     *
     * @param newRandom new Uniform Random Number
     * @return a random number from the distribution
     */
    public double nextDouble(UniformRandomNumber newRandom) throws ArithmeticException {
        // Joint Normal always has autocorrelation
        if (autoCorrelationFunction != null && normalDistribution != null) {
            return normalDistribution.nextDouble(newRandom);
        }
        return 0; //todo
    }

    public double getStd() {
        if (normalDistribution.isStdFactor()) {
            throw new IllegalStateException(this.getClass().getSimpleName() + ": no value provide for Std that is a factor of the actual value");
        } else {
            return normalDistribution.getStd();
        }
    }

    public double getStd(double value) {
        if (!normalDistribution.isStdFactor()) {
            throw new IllegalStateException(this.getClass().getSimpleName() + ": no value provide for Std that is a factor of the actual value");
        } else {
            return Math.abs(value * normalDistribution.getStd());
        }
    }

    public double[] getRealization(double[] pdfNoise, double[] valuesToBeUsed) {
        int size = valuesToBeUsed.length;
        // generate covariance matrix
        if (normalDistribution.isStdFactor()) {
            // generate std again
            std = new DenseDoubleMatrix1D(size);
            for (int i = 0; i < size; i++) {
                std.internalSetElement(i, valuesToBeUsed[i] * normalDistribution.getStd());
            }
        }  else if (std==null) {
            std = new DenseDoubleMatrix1D(size);
            for (int i = 0; i < size; i++) {
                std.internalSetElement(i, normalDistribution.getStd());
            }
        }

        if (!isFactorisedMatrixAvailable) {
            covarianceMatrix = new DenseDoubleMatrix2D(size,size);
            //Populate the matrix with covariances using the correlation model
            for(int i = 0; i < size; i++) {
                for(int j = 0; j < size ; j++) {
                    //Process variance
                    if(i == j) {
                        double st = std.internalGetElement(i);
                        covarianceMatrix.internalSetElement(i,j, st*st);
                    }
                    //Covariance
                    else {
                        double stdI = std.internalGetElement(i);  //Assumes the std vector has been populated row-wise from a matrix
                        double stdJ = std.internalGetElement(j);
                        covarianceMatrix.internalSetElement(i,j,
                                (autoCorrelationFunction.getSill() - autoCorrelationFunction.evaluateFunction(Math.abs(j-i)))*stdI*stdJ);

                           // j - i  gives the distance (distance is equivalent to seperation within an array) // todo with coordinates
                    }
                }
            }
            // factorised and validate the Matrix
            covarianceMatrix = (DenseDoubleMatrix2D) factoriseAndValidate(covarianceMatrix);
        }

        // realised values
        DoubleMatrix1D noise = new DenseDoubleMatrix1D(pdfNoise);
        // mean values
        DoubleMatrix1D meanValues = new DenseDoubleMatrix1D(valuesToBeUsed);


        // todo change Matrix operation to standard matrix Jama or RealMatrix or Colt jd
        MatrixAlgebra algebra = new MatrixAlgebra();
        //Compute x = mu + (L * Z(0,I))* (1 /std)
        DoubleMatrix1D doubleMatrix1D = algebra.multiply(covarianceMatrix, noise);
        double[] doubles = doubleMatrix1D.toArray(); //todo perform matrix operatiion instead of this double array multiplications jd
        for (int i = 0; i < doubles.length; i++) {
            doubles[i]/=normalDistribution.getStd();
        }
        doubleMatrix1D.setMatrixValues(doubles);

        Matrix matrix = (Matrix) meanValues.assign(doubleMatrix1D, FunctionLibrary.plus, false);
        return (double[]) matrix.getMatrixValues();
    }


    public boolean isStdFactor() {
        return normalDistribution.isStdFactor();
    }

    /**
     * The factoriseAndValidate() method attempts to validate a covariance matrix by
     * first establishing the existance of a factorised matrix and, if a factorised
     * matrix does not exist, by factorising the current matrix.  Matrix factorisation
     * is used to establish the determinant of a covariance matrix (i.e. whether it
     * is positive definite), as well as to solve the matrix for sampling.  If a
     * factorised matrix exists, the validity of the matrix depends on the status of
     * the existing flags.  By default, Cholesky factorisation is called.
     *
     * @return diagnostic Information on the validation status of the matrix.
     * @param covarianceMatrix Covariance matrix
     */

    public Matrix factoriseAndValidate(DenseDoubleMatrix2D covarianceMatrix) {
        if (isFactorisedMatrixAvailable) {
            return covarianceMatrix;
        }
        //The matrix is not valid or has not been factorised: call the default Cholesky method.
        else {
            Cholesky chol = new Cholesky(covarianceMatrix);
            Matrix factorisedMatrix = chol.getL();
            isFactorisedMatrixAvailable = chol.isSymPosDef();
            return factorisedMatrix;
        }
    }
}
