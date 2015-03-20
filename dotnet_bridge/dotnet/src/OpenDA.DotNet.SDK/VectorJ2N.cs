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
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.SDK
{
	public class VectorJ2N : IVector
	{
		private org.openda.utils.Vector _javaVector;

		public VectorJ2N(string timeSpanString)
		{
			_javaVector = new org.openda.utils.Vector(timeSpanString);
		}

		public VectorJ2N(double[] doubles)
		{
			_javaVector = new org.openda.utils.Vector(doubles);
		}

		public VectorJ2N(int size)
		{
			_javaVector = new org.openda.utils.Vector(size);
		}

		public void SetConstant(double value)
		{
			_javaVector.setConstant(value);
		}

		public void Scale(double alpha)
		{
			_javaVector.scale(alpha);
		}

		public double[] Values
		{
			get { return _javaVector.getValues(); }
			set { _javaVector.setValues(value); }
		}

		public void SetValue(int index, double value)
		{
			_javaVector.setValue(index, value);
		}

		public double GetValue(int index)
		{
			return _javaVector.getValue(index);
		}

		public int Size
		{
			get { return _javaVector.getSize(); }
		}

		public int MaxFullExpandLength
		{
			set { _javaVector.maxFullExpandLength = value; }
		}

		public void Axpy(double alpha, IVector x)
		{
			_javaVector.axpy(alpha, new org.openda.utils.Vector(x.Values));
		}

		public double DotProduct(IVector otherVector)
		{
			return _javaVector.dotProduct(new org.openda.utils.Vector(otherVector.Values));
		}

		public double Norm2()
		{
			return _javaVector.norm2();
		}

		public void PointwiseDivide(IVector otherVector)
		{
			_javaVector.pointwiseDivide(new org.openda.utils.Vector(otherVector.Values));
		}

		public void PointwiseMultiply(IVector otherVector)
		{
			_javaVector.pointwiseMultiply(new org.openda.utils.Vector(otherVector.Values));
		}

		public void Sqrt()
		{
			_javaVector.sqrt();
		}

		public IVector Clone()
		{
			VectorJ2N clone = new VectorJ2N();
			clone._javaVector = _javaVector.clone();
			return clone;
		}

		public void Finish()
		{
			_javaVector.free();
		}

		public string PrintString(string indent)
		{
			return _javaVector.printString(indent);
		}

		private VectorJ2N()
		{
			// no action
		}
	}
}