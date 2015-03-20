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

package org.openda.interfaces;


import java.io.Serializable;

/**
 * General interface to a mathematical vector.
 */
public interface IVector extends Cloneable, Serializable {

    /**
     * Set whole vector equal to a constant value.
     * <p/>
     * Note:  This method can only be used if all elements of the vector
     * have the same data type.
     *
     * @param value value to set
     */
    void setConstant(double value);

    /**
     * Scale vector.
     *
     * @param alpha scale factor
     */
    void scale(double alpha);

    /**
     * Set all values of the vector.
     * <p/>
     * Note:  This method can only be used if all elements of the vector
     * are of the same data type.
     *
     * @param values values to be set
     */
    void setValues(double[] values);

    /**
     * Get all values of the vector.
     * <p/>
     * Note:  This method can only be used if all elements of the vector
     * are of the same data type.
     *
     * @return vector with values
     */
    double[] getValues();

    /**
     * Set single value of the vector.
     *
     * @param index index in vector
     * @param value value to be set
     */
    void setValue(int index, double value);

    /**
     * Get single value at specific index of the vector.
     *
     * @param index index in vector
     * @return value at index
     */
    double getValue(int index);

    /**
     * Get number of elements in vector.
     *
     * @return size of vector
     */
    int getSize();

    /**
     * Axpy operation between two vectors.
     * <p/>
     * Note:  Axpy: this=alpha*x+this. Add alpha times vector x to
     * this vector.
     *
     * @param alpha scalar
     * @param x handle of vector x
     */
    void axpy(double alpha, IVector x);

    /**
     * Compute dotProduct product of two vectors.
     * <p/>
     * Note:  dotprod = sum[all i]  (this.hVector_i * hVector2_i)
     *
     * @param otherVector the other vector
     * @return the dotProduct
     */
    double dotProduct(IVector otherVector);

    /**
     * Compute the 2-norm of a vector.
     *
     * @return the 2-norm of a vector
     */
    double norm2();

    /**
     * Divide each value in the vector by the value at the same index of another vector of the same size.
     *
     * @param otherVector a vector of the same size containing the values to divide by.
     */
    void pointwiseDivide(IVector otherVector);

    /**
     * Multiply each value in the vector with the value at the same index of another vector of the same size.
     *
     * @param otherVector a vector of the same size containing the values to multiply with.
     */
    void pointwiseMultiply(IVector otherVector);

     /**
     * Compute square root of all elements in the vector.
     *
     */
    void sqrt();

    /**
     * Clone (that is duplicate) a vector.
     * <p/>
     * Note: Duplication means that a new vector is created that is identical to
     * the current vector. All data in the current vector is also copied.
     *
     * @return A copy of the present vector.
     */
    IVector clone();

    /**
     * Free a vector Instance.
     */
    void free();

    /**
     * Print the contents of a vector with given indentation.
     *
     * @param indent indentation used
     * @return String that can be printed
     */
    String printString(String indent);
}
