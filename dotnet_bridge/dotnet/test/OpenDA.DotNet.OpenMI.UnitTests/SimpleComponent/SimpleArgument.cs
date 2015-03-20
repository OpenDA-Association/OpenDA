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
using System.Collections.Generic;
using OpenMI.Standard2;

namespace OpenDA.DotNet.OpenMI.UnitTests
{
	internal class SimpleArgument : IArgument
	{
		private readonly string _id;
		private string _value;

		public SimpleArgument(string id, string value)
		{
			_id = id;
			_value = value;
		}

		public string Id
		{
			get { return _id; }
		}

		public string Caption { get; set; }

		public string Description { get; set; }

		public Type ValueType
		{
			get { return typeof(string); }
		}

		public bool IsOptional
		{
			get { return false; }
		}

		public bool IsReadOnly
		{
			get { return false; }
		}

		public object Value
		{
			get { return _value; }
			set
			{
				if ( value != null && !(value is string))
				{
					throw new Exception("Unknown object type: " + value);
				}
				_value = (string) value;
			}
		}

		public object DefaultValue
		{
			get { return ""; }
		}

		public IList<object> PossibleValues
		{
			get { return null; }
		}

		public string ValueAsString
		{
			get { return _value; }
			set { Value = value; }
		}
	}
}