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
	public interface IExchangeItem
	{
		/// <summary>
		/// The identifier for the exchangeItem (must be unique within the context of a model instance).
		/// </summary>
		string Id { get; }

		/// <summary>
		/// Optional additional description for the exchange item.
		/// </summary>
		string Description { get; }

		/// <summary>
		/// Ask which object type will be returned by the Value property.
		/// </summary>
		Type ValueType { get; }

		/// <summary>
		/// The role of the exchange item (input, output, or both) 
		/// </summary>
		Role Role { get; }

		/// <summary>
		/// Get/set the values of the exchange item, returning/providing the type as defined in the
		/// ValueType property.
		/// </summary>
		Object Values { get; set; }

		/// <summary>
		/// Get/set the values of the exchange item as an array of doubles.
		/// </summary>
		double[] ValuesAsDoubles { get; set; }

		/// <summary>
		/// Perform a values += alpha * axpyValues</c> operation on each value in the exchange item.
		/// </summary>
		/// <param name="alpha">The <c>alpha</c> in <c>values += alpha * vector</c>.</param>
		/// <param name="axpyValues">The values for the Axpy-operation on all values in the exchange item.</param>
		void AxpyOnValues(double alpha, double[] axpyValues);

		/// <summary>
		/// Multiply each value in the exchange item's value with the related multiplication factor.
		/// </summary>
		/// <param name="multiplicationFactors">The multiplication factors for all exchange time values.</param>
		void MultiplyValues(double[] multiplicationFactors);

		/// <summary>
		// Get/Set the times for the exchangeItem as Modified Julian Days
		// Setting the times property is only allowed if Times != null
		// and if the exchangeItem has role Input or InOut 
		/// </summary>
		double[] Times { get; set; }
	}
}