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


namespace OpenDA.DotNet.Interfaces
{
	/// <summary>
	/// Enumeration for the role of an exchange item.
	/// </summary>
	public enum Role
	{
		/// <summary>
		/// The exchange item can be used as input.
		/// </summary>
		Input,
		/// <summary>
		/// The exchange item can be used as output.
		/// </summary>
		Output,
		/// <summary>
		/// The exchange item can be used both as input and output.
		/// </summary>
		InOut
	}
}