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
	public class DoublesExchangeItem : IExchangeItem
	{
		private string _id;
		private string _description;
		private Role _role;
		private double[] _values;
		private readonly bool _isTimesSeries;

		private void Initialize(string id, string description, Role role)
		{
			_id = id;
			_description = description;
			_role = role;
		}

		// constructor for java side of bridge
		public DoublesExchangeItem(string id, string description, int role, double value)
		{
			Initialize(id, description, Utils.RoleMapJ2N(role));
			_values = new[] {value};
		}

		// constructor for java side of bridge
		public DoublesExchangeItem(string id, string description, int role, double[] times, double[] values)
		{
			Initialize(id, description, Utils.RoleMapJ2N(role));
			Times = times;
			_values = values;
			_isTimesSeries = true;
		}

		// c# constructor
		public DoublesExchangeItem(string id, string description, Role role, double value)
		{
			Initialize(id, description, role);
			_values = new[] { value };
		}

		// c# constructor
		public DoublesExchangeItem(string id, string description, Role role, double[] values)
		{
			Initialize(id, description, role);
			_values = values;
			_isTimesSeries = true;
		}

		// c# constructor
		public DoublesExchangeItem(string id, string description, Role role, double[] times, double[] values)
		{
			Initialize(id, description, role);
			Times = times;
			_values = values;
			_isTimesSeries = true;
		}

		public DoublesExchangeItem(string id, string description, Role role, IVector vector)
		{
			Initialize(id, description, role);
			_values = (double[]) vector.Values.Clone();
		}

		public string Id
		{
			get { return _id; }
		}

		public string Description
		{
			get { return _description; }
		}

		public Type ValueType
		{
			get { return typeof(double); }
		}

		public Role Role
		{
			get { return _role; }
		}

		public object Values
		{
			get
			{
				if (_isTimesSeries)
				{
					return _values;
				}
				return _values[0];
			}
			set
			{
				if (value is double)
				{
					_values = new[] { (double)value };
				}
				else if (value is double[])
				{
					_values = (double[])value;
				} else
				{
					throw new Exception("Unexpected value type " + value.GetType() +
						" for exchange item " + Id);
				}
			}
		}

		public double[] ValuesAsDoubles
		{
			get { return _values; }
			set { _values = value; }
		}

		public void AxpyOnValues(double alpha, double[] axpyValues)
		{
			double[] values = ValuesAsDoubles;
			for (int i = 0; i < values.Length; i++)
			{
				values[i] += alpha * axpyValues[i];
			}
			ValuesAsDoubles = values;
		}

		public void MultiplyValues(double[] multiplicationFactors)
		{
			double[] values = ValuesAsDoubles;
			for (int i = 0; i < values.Length; i++)
			{
				values[i] *= multiplicationFactors[i];
			}
			ValuesAsDoubles = values;
		}

		public double[] Times { get; set; }
	}
}
