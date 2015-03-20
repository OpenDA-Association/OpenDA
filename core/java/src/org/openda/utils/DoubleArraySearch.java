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

import java.io.Serializable;

public class DoubleArraySearch implements Serializable {
	private double[] vector;								// The array to search
	private double lastValueSearched = Double.NaN;			// The last value searched for
	private int result = -1;								// The index of the last result found
	private int lbound = -1;								// In case it isn't found, the lower bound of the
	// interval the index of the result is in
	private int ubound = -1;								// In case it isn't found, the upper bound of the
	// interval the index of the result is in
	private double position = 0.;							// Position, relative to ubound, of the index
	private double epsilonForComparison = 1e-6;	// Required accuracy for comparison of doubles

	// of the value searched. It's a value in
	// [0,1).

	/**
	 * Constructor
	 *
	 * @param sortedArray - the (ascending) sorted array of doubles to search
	 */
	public DoubleArraySearch(double[] sortedArray) {
		this.vector = sortedArray;
	}

	/**
	 * Constructor that specifies the accuracy for comparison of doubles (mostly time stamps)
	 *
	 * @param sortedArray			The (ascending) sorted array of doubles to search
	 * @param epsilonForComparison	Required accuracy for comparison of doubles 
	 */
	public DoubleArraySearch(double[] sortedArray, double epsilonForComparison) {
		this(sortedArray);
		this.epsilonForComparison = epsilonForComparison;
	}

	/**
	 * Search for a value. This will set the values of result, lbound and
	 * ubound.
	 *
	 * @param value - value to find
	 * @return Index of the value, or -1 in case it isn't found.
	 */
	public int search(double value) {
		// If the last value searched for is this value, the results are still valid
		if (value == this.lastValueSearched) return this.result;
		this.lastValueSearched = value;

		// Quick check on array bounds
		if (this.vector.length < 1) {
			this.lbound = -1;
			this.ubound = -1;
			this.position = 0.;
			this.result = -1;
			return -1;
		}
		if (value < this.vector[0]) {
			this.lbound = -1;
			this.ubound = 0;
			this.position = 0.;
			this.result = -1;
			return -1;
		}
		if (value > this.vector[this.vector.length - 1]) {
			this.lbound = this.vector.length - 1;
			this.ubound = -1;
			this.position = 0.;
			this.result = -1;
			return -1;
		}

		// If we had a previous result, check whether it is what we search
		// or check its neighbors (we might be iterating, after all...)
		if (checkResult(value)) return this.result;

		// Finally, search the vector using bisection
		return bisect(value, 0, this.vector.length - 1);
	}

	/*
		* Find a value in the given vector by examining the previous result
		*
		* @param value - value to find
		*
		* @return true if we have a result, false if we haven't
		*/

	private boolean checkResult(double value) {
		if (this.result != -1) {
			double currentValue = this.vector[this.result];

			if (value == currentValue) {
				this.lbound = this.result;
				this.ubound = this.result;
				this.position = 0.;
				return true;
			}

			if (value > currentValue) {
				// Searching forward...
				if (this.result < this.vector.length - 1) {
					double nextValue = this.vector[this.result + 1];
					if (Math.abs(nextValue - value) < this.epsilonForComparison) {
						// It's the next value!
						++this.result;
						this.lbound = this.result;
						this.ubound = this.result;
						this.position = 0.;
						return true;
					} else if (nextValue > value) {
						// It's between the last result and the next value
						this.lbound = this.result;
						this.ubound = this.result + 1;
						this.position = (value - currentValue) / (nextValue - currentValue);
						this.result = -1;
						return true;
					}
				} else {
					// The value searched is not in the vector
					this.lbound = this.result;
					this.ubound = -1;
					this.position = 0.;
					this.result = -1;
					return true;
				}
			} else {
				// Searching backward...
				if (this.result > 0) {
					double prevValue = this.vector[this.result - 1];
					if (Math.abs(value - prevValue) < this.epsilonForComparison) {
						// It's the previous value!
						--this.result;
						this.lbound = this.result;
						this.ubound = this.result;
						this.position = 0.;
						return true;
					} else if (prevValue < value) {
						// It's between the previous value and the last result
						this.ubound = this.result;
						this.lbound = this.result - 1;
						this.position = (value - prevValue) / (currentValue - prevValue);
						this.result = -1;
						return true;
					}
				} else {
					// The value searched is not in the vector
					this.lbound = -1;
					this.ubound = this.result;
					this.position = 0.;
					this.result = -1;
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Find a value in the given double vector using bisection
	 *
	 * @param value - value to find
	 * @param low   Lower bound index.
	 * @param high  Upper bound index.
	 * @return Index of the value, or -1 in case it isn't found.
	 */
	private int bisect(double value, int low, int high) {
		if (high < low) {
			this.result = -1;
			this.lbound = high;
			this.ubound = (low < this.vector.length ? low : -1);
			if (this.lbound == -1 || this.ubound == -1) {
				this.position = 0.;
			} else {
				this.position = (value - this.vector[this.lbound]) / (this.vector[this.ubound] - this.vector[this.lbound]);
			}
			return -1;
		}

		int mid = (high + low) / 2;
		double midval = this.vector[mid];

		if (Math.abs(value - midval) < epsilonForComparison) {
			this.result = mid;
			this.lbound = mid;
			this.ubound = mid;
			this.position = 0.;
			return mid;
		} else if (value < midval) {
			return bisect(value, low, mid - 1);
		} else {
			return bisect(value, mid + 1, high);
		}
	}

	/**
	 * @return The index of the value searched (or -1)
	 */
	public int getResult() {
		return this.result;
	}

	/**
	 * @return Lower bound of the interval the index of the value searched is in
	 *         (or -1)
	 */
	public int getLowerBound() {
		return this.lbound;
	}

	/**
	 * @return Upper bound of the interval the index of the value searched is in
	 *         (or -1)
	 */
	public int getUpperBound() {
		return this.ubound;
	}

	/**
	 * Get the position of the value searched between the upper and the lower
	 * bound indices. This is a value in the [0,1) interval.
	 *
	 * @return Position between the lower and the upper bound.
	 */
	public double getPosition() {
		return this.position;
	}

	/**
	 * Indicate whether the search result is out of bounds
	 *
	 * @return Whether the search result is out of bounds.
	 */
	public boolean isOutOfBounds() {
		return (this.result == -1) && (this.lbound == -1 || this.ubound == -1);
	}
}
