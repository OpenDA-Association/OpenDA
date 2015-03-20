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


using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.UnitTests
{
	internal class SimpleElementSet : ISpatialDefinition
	{
		public string ElementSetName { get; set; }

		public SimpleElementSet(string elementSetName)
		{
			Caption = elementSetName;
		}

		public string Caption { get; set; }

		public string Description { get; set; }

		public string SpatialReferenceSystemWkt
		{
			get { return "<not relevant>"; }
		}

		public int ElementCount
		{
			get { return 1; }
		}

		public int Version
		{
			get { return 0; }
		}
	}
}