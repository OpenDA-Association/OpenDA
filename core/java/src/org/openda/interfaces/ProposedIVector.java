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
public interface ProposedIVector extends Cloneable, Serializable, IArray {

    /**
     * already in IArray
     */
    void setConstant(double value);

    /**
     * already in IArray as setValuesAsDoubles
     */
    //void setValues(double[] values);

    /**
     * already in IArray as getValuesAsDoubles
     */
    //double[] getValues();

    /**
     * already in IArray as setValueAsDouble
     */
    //void setValue(int index, double value);

    /**
     * already in IArray as getValueAsDouble
     */
    //double getValue(int index);

    /**
     * replaced by length() from IArray
     */
    //int getSize();

    /**
     * Scale vector.
     *
     * @param alpha scale factor
     */
    void scale(double alpha);

    /**
     * Axpy operation between two vectors.
     * <p/>
     * Note:  Axpy: this=alpha*x+this. Add alpha times vector x to
     * this vector.
     *
     * @param alpha scalar
     * @param x handle of vector x
     */
    void axpy(double alpha, ProposedIVector x);

    /**
     * Compute dotProduct product of two vectors.
     * <p/>
     * Note:  dotprod = sum[all i]  (this.hVector_i * hVector2_i)
     *
     * @param otherVector the other vector
     * @return the dotProduct
     */
    double dotProduct(ProposedIVector otherVector);

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
    void pointwiseDivide(ProposedIVector otherVector);

    /**
     * Multiply each value in the vector with the value at the same index of another vector of the same size.
     *
     * @param otherVector a vector of the same size containing the values to multiply with.
     */
    void pointwiseMultiply(ProposedIVector otherVector);

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
    ProposedIVector clone();

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
    
    /**
     * get underlying Array when possible.
     * @return
     */
    IArray getArray();
    
}
