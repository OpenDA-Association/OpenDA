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
package org.openda.utils;

/**
 * Mask class that can be used for selections
 */
public class Mask {
   /**
    * The actual values in this mask
    */
   private boolean[] values;


   /**
    * Oft-recurring error message.
    */
   private final String sizeMismatch = "Mask: the number of elements of the mask and the vector do not match.";

   //=========================================================================
   // CONSTRUCTORS
   //=========================================================================

   /**
    * Create a new mask of the specfied size and initialized with the specified value.
    *
    * @param size          Number of elements in this mask.
    * @param initialValue  Initial value of the elements in this mask.
    */
   public Mask(int size, boolean initialValue) {
      values = new boolean[size];
      for (int i = 0; i < size; ++i) values[i] = initialValue;
   }

   /**
    * Create an uninitialized new mask of the specfied size
    *
    * @param size          Number of elements in this mask.
    */
   private Mask(int size) {
      values = new boolean[size];
   }

   /**
    * Copy constructor
    *
    * @param other The mask to use for initialization.
    */
   public Mask(Mask other) {
      this.values = other.values.clone();
   }

 //=========================================================================
 // BASIC ARRAY OPERATIONS
 //=========================================================================

   /**
    * The number of elements in this mask.
    *
    * @return The number of elements in this mask.
    */
   public int getSize() {
      return this.values.length;
   }

   /**
    * Set a specific value
    *
    * @param index Index to set.
    * @param value Value to set.
    */
   public void setValue(int index, boolean value) {
      this.values[index] = value;
   }

   /**
    * Get a specific value
    *
    * @param index The index of the value to return.
    *
    * @return The value at the specified index.
    */
   public boolean getValue(int index) {
      return this.values[index];
   }

   /**
    * Get a copy of the value array
    *
    * @return A copy of the value array.
    */
   public boolean[] toArray() {
      return this.values.clone();
   }

   //=========================================================================
   // LOGICAL OPERATIONS
   //=========================================================================

   /**
    * Reverse the boolean values in this mask
    *
    * @return This mask.
    */
   public Mask not() {
      for (int i = 0; i < this.values.length; ++i) this.values[i] = !this.values[i];
      return this;
   }

   /**
    * Perform an "and" operation with another mask
    *
    * @param  mask The mask to use.
    *
    * @return This mask.
    */
   public Mask and(Mask mask) {
      if (mask == null || mask.getSize() != this.values.length) throw new RuntimeException(this.sizeMismatch);
      for (int i = 0; i < this.values.length; ++i) this.values[i] &= mask.values[i];
      return this;
   }

   /**
    * Perform an "or" operation with another mask
    *
    * @param  mask The mask to use.
    *
    * @return This mask.
    */
   public Mask or(Mask mask) {
      if (mask == null || mask.getSize() != this.values.length) throw new RuntimeException(this.sizeMismatch);
      for (int i = 0; i < this.values.length; ++i) this.values[i] |= mask.values[i];
      return this;
   }

   //=========================================================================
   // CREATE MASKS BY SELECTIONS ON VECTORS OF DOUBLES
   //=========================================================================

   /**
    * Create a new mask with true where elements of the given vector
    * are equal to the specified value.
    *
    * @param vector The vector to base the mask on.
    * @param value  The value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementEQ(double[] vector, double value) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] >= value - Double.MIN_VALUE
                                                        && vector[i] <= value + Double.MIN_VALUE);
      return result;
   }

   /**
    * Create a new mask with true where elements of the given vector
    * are not equal to the specified value.
    *
    * @param vector The vector to base the mask on.
    * @param value  The value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementNE(double[] vector, double value) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] < value - Double.MIN_VALUE
                                                        || vector[i] > value + Double.MIN_VALUE);
      return result;
   }

   /**
    * Create a new mask with true where elements of the given vector
    * are greater than the specified value.
    *
    * @param vector The vector to base the mask on.
    * @param value  The value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementGT(double[] vector, double value) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] > value);
      return result;
   }

   /**
    * Create a new mask with true where elements of the given vector
    * are less than the specified value.
    *
    * @param vector The vector to base the mask on.
    * @param value  The value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementLT(double[] vector, double value) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] < value);
      return result;
   }

   /**
    * Create a new mask with true where elements of the given vector
    * are greater than or equal to the specified value.
    *
    * @param vector The vector to base the mask on.
    * @param value  The value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementGE(double[] vector, double value) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] >= value - Double.MIN_VALUE);
      return result;
   }

   /**
    * Create a new mask with true where elements of the given vector
    * are less than or equal to the specified value.
    *
    * @param vector The vector to base the mask on.
    * @param value  The value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementLE(double[] vector, double value) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] <= value + Double.MIN_VALUE);
      return result;
   }

   /**
    * Create a new mask with true where elements of the given vector
    * between (and including) the specified values.
    *
    * @param vector The vector to base the mask on.
    * @param lbound The lower bound value to compare against.
    * @param ubound The upper bound value to compare against.
    *
    * @return A mask based on the given vector
    */
   public static Mask elementBetween(double[] vector, double lbound, double ubound) {
      final int len = (vector == null ? 0 : vector.length);
      Mask result = new Mask(len);
      for (int i = 0; i < len; ++i) result.values[i] = (vector[i] >= lbound - Double.MIN_VALUE &&
                                                        vector[i] <= ubound + Double.MIN_VALUE);
      return result;
   }
}
