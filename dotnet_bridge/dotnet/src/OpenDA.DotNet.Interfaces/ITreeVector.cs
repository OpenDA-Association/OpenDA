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

namespace OpenDA.DotNet.Interfaces
{
	/// <summary>
	/// TreeVector
	/// </summary>
	public interface ITreeVector : IVector
	{
		/// <summary>
		/// Get a TreeVector's identifier.
		/// <return>The TreeVector's identifier.</return>
		/// </summary>
		String Id { get; }

		/// <summary>
		/// Get a TreeVector's caption.
		/// <return>The TreeVector's caption (i.e. descriptive name)</return>
		/// </summary>
		String Caption { get; }

		/// <summary>
		/// Get a TreeVector's description.
		/// <return>The TreeVector's extensive description. </return>
		/// </summary>
		String Description { get; }

		/// <summary>
		/// Get the identifiers of the children
		/// <return>the sub-treevector ids</return>
		/// </summary>
		IList<String> SubTreeVectorIds { get; }

		/// <summary>
		/// Get a child in the tree vector, the child is known to be a SubTreeVector
		/// (otherwise a run time exception will be thrown).
		/// <param name"id">identifier of the child, may contain "/"'s to specify full path in tree.</param>
		/// <return>Child SubTreeVector with identifier <code>id</code>.</return>
		/// </summary>
		ITreeVector GetSubTreeVector(String id);

		/// <summary>
		/// Get the number of dimensions in the TreeVector.
		/// <return>Returns <code>null</code> if there are no dimensions defined, returns the array with dimensions
		///         otherwise. The length of the returned array is equal to the number of dimensions </return>
		/// </summary>
		IDimensionIndex[] DimensionIndices { get; }

		/// <summary>
		/// Should this part of the tree vector be excluded from the vector of is parent?
		/// <return>Returns <code>true</code> if this part should be excluded.</return>
		/// </summary>
		bool ExcludeFromVector();

		/// <summary>
		/// Clone (i.e. duplicate) a TreeVector.
		/// <p/>
		/// Note: Duplication means that a new vector is created that is identical to
		/// the current vector. All data in the current vector is also copied.
		/// 
		/// <return>A deep copy of the present vector.</return>
		/// </summary>
		new ITreeVector Clone();
	}
}