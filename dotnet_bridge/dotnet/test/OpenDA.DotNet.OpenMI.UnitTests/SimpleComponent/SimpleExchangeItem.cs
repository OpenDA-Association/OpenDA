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

namespace OpenDA.DotNet.OpenMI.UnitTests.SimpleComponent
{
	internal class SimpleExchangeItem : IBaseExchangeItem
	{
		private readonly string _id;
		private string _caption;
		private readonly SimpleLinkableComponent _linkableComponent;
		private readonly IValueDefinition _valueDefinition;
		protected IBaseValueSet _values;

		protected SimpleExchangeItem(string locationID, string quantityID, SimpleLinkableComponent linkableComponent)
		{
			_id = locationID + "." + quantityID;
			_linkableComponent = linkableComponent;
			_values = new ValueSet<Double>(Double.NaN);
			_valueDefinition = new SimpleQuantity(quantityID);
		}

		public string Caption
		{
			get { return _caption ?? _id; }
			set { _caption = value; }
		}

		public string Description { get; set; }

		public string Id
		{
			get { return _id; }
		}

		public IValueDefinition ValueDefinition
		{
			get { return _valueDefinition; }
		}

		public IBaseLinkableComponent Component
		{
			get { return _linkableComponent; }
		}

		public event EventHandler<ExchangeItemChangeEventArgs> ItemChanged;
	}
}