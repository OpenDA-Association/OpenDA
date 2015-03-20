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
using OpenMI.Standard2;

namespace OpenDA.DotNet.OpenMI.UnitTests
{
	internal class SimpleQuantity : IQuantity
	{
		private readonly IUnit _unit = new SimpleUnit("-");

		public SimpleQuantity(string caption)
		{
			Caption = caption;
		}

		public string Caption { get; set; }

		public string Description { get; set; }

		public Type ValueType
		{
			get { return typeof(double); }
		}

		public object MissingDataValue
		{
			get { return -99.0d; }
		}

		public IUnit Unit
		{
			get { return _unit; }
		}
	}
}