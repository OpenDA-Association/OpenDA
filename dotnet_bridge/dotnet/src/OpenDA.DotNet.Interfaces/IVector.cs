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


using System;

namespace OpenDA.DotNet.Interfaces
{
	/// <summary>
	/// Vector
	/// </summary>
	public interface IVector
	{
		/// <summary>
		/// Set whole vector equal to a constant value.
		/// <p/>
		/// Note:  This method can only be used if all elements of the vector
		/// have the same data type.
		/// 
		/// <param name"value">value to set</param>
		/// </summary>
		void SetConstant(double value);

		/// <summary>
		/// Scale vector.
		/// 
		/// @param alpha       scalar
		/// </summary>
		void Scale(double alpha);

		/// <summary>
		/// Set / Get all values of the vector.
		/// <p/>
		/// </summary>
		double[] Values { get; set; }

		/// <summary>
		/// Set single value of the vector.
		/// 
		/// @param index    index of value in vector
		/// @param value    value to be set
		/// </summary>
		void SetValue(int index, double value);

		/// <summary>
		/// Get single value of the vector.
		/// 
		/// @param index       index in value in vector
		/// @return todo describe
		/// </summary>
		double GetValue(int index);

		/// <summary>
		/// Get size of vector.
		/// 
		/// @return todo describe
		/// </summary>
		int Size { get; }

		/// <summary>
		/// Axpy operation between two Vectors.
		/// <p/>
		/// Note:  Axpy: this=alpha*x+this. Add alpha times Vector x to
		/// this Vector.
		/// 
		/// @param alpha scalar
		/// @param x     handle of Vector x
		/// </summary>
		void Axpy(double alpha, IVector x);

		/// <summary>
		/// Compute DotProduct product of two Vectors.
		/// <p/>
		/// Note:  dotprod = sum[all i]  (this.hVector_i * hVector2_i)
		/// 
		/// @param otherVector the other Vector
		/// @return todo describe
		/// </summary>
		double DotProduct(IVector otherVector);

		/// <summary>
		/// Compute the 2-norm of a Vector.
		/// 
		/// @return todo describe
		/// </summary>
		double Norm2();

		/// <summary>
		/// Devide each value in the vector by the related value in an other vector.
		/// 
		/// @param otherVector The values to devide the vector by.
		/// </summary>
		void PointwiseDivide(IVector otherVector);

		/// <summary>
		/// Multiply each value in the vector with the related value in an other vector.
		/// 
		/// @param otherVector The values to devide the vector by.
		/// </summary>
		void PointwiseMultiply(IVector otherVector);

		/// <summary>
		/// Compute square root of all elements in the vector.
		/// 
		/// </summary>
		void Sqrt();

		/// <summary>
		/// Clone (i.e. duplicate) a vector.
		/// <p/>
		/// Note: Duplication means that a new vector is created that is identical to
		/// the current vector. All data in the current vector is also copied.
		/// 
		/// @return A copy of the present vector.
		/// </summary>
		IVector Clone();

		/// <summary>
		/// Free a Vector Instance.
		/// </summary>
		void Finish();

		/// <summary>
		/// Print the contents in a nice way
		/// </summary>
		String PrintString(String indent);
	}
}