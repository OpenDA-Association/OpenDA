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

namespace OpenDA.DotNet.Bridge
{
	public class Vector : IVector
	{
		private double[] _content = new double[0];
		private const int _printStringSize = 5;

		public Vector(double[] values)
		{
			_content = new double[values.Length];
			for (int i = 0; i < _content.Length; i++)
			{
				_content[i] = values[i];
			}
		}

		public void SetConstant(double value)
		{
			for (int i = 0; i < _content.Length; i++)
			{
				_content[i] = value;
			}
		}

		public void Scale(double alpha)
		{
			for (int i = 0; i < _content.Length; i++)
			{
				_content[i] = alpha;
			}
		}

		public double[] Values
		{
			get { return _content; }
			set { _content = value; }
		}

		public void SetValue(int index, double value)
		{
			CheckIndex("SetValue", index);
			_content[index] = value;
		}

		public double GetValue(int index)
		{
			CheckIndex("GetValue", index);
			return _content[index];
		}

		public int Size
		{
			get { return _content.Length; }
		}

		public void Axpy(double alpha, IVector x)
		{
			throw new NotImplementedException();
		}

		public double DotProduct(IVector otherVector)
		{
			throw new NotImplementedException();
		}

		public double Norm2()
		{
			throw new NotImplementedException();
		}

		public void PointwiseDivide(IVector otherVector)
		{
			throw new NotImplementedException();
		}

		public void PointwiseMultiply(IVector otherVector)
		{
			throw new NotImplementedException();
		}

		public void Sqrt()
		{
			throw new NotImplementedException();
		}

		public IVector Clone()
		{
			return new Vector(Values);
		}

		public void Finish()
		{
			_content = null;
		}

		public string PrintString(string indent)
		{
			string printString = indent + "[" + _content;
			for (int i = 0; i < _printStringSize/2; i++)
			{
				printString += String.Format("{0:%8.4f}", _content[i]);
			}
			return printString;
		}

		private void CheckIndex(string routineName, int index)
		{
			if (index < 0 || index >= _content.Length)
			{
				throw new Exception("Index (=" + index + ") out of range (0-"
				                    + _content.Length + ") in routine " + routineName);
			}
		}
	}
}