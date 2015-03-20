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
package org.openda.dotnet;
import cli.OpenDA.DotNet.Bridge.DoublesExchangeItem;
import cli.OpenDA.DotNet.Interfaces.IExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

import java.util.List;

/**
 * Java wrapper around .net class for observation descripitons
 */
public class ObservationDescriptionsN2J implements IObservationDescriptions {

	cli.OpenDA.DotNet.Interfaces.IObservationDescriptions _dotNetObservationDescriptions;

	public ObservationDescriptionsN2J(IObservationDescriptions javaObservationDescriptions ) {

		List<IPrevExchangeItem> javaExchangeItems = javaObservationDescriptions.getExchangeItems();
		cli.OpenDA.DotNet.Interfaces.IExchangeItem[] dotNetExchangeItems = new
				cli.OpenDA.DotNet.Interfaces.IExchangeItem[javaExchangeItems.size()];
		for (int i = 0; i < javaExchangeItems.size(); i++) {
			IPrevExchangeItem javaExchangeItem = javaExchangeItems.get(i);
			int role = UtilsJ2NAndN2J.ExchangeItemRoleMapJ2N(javaExchangeItem.getRole());
			IExchangeItem dotNetExchangeItem =
					new DoublesExchangeItem(
							javaExchangeItem.getId(), javaExchangeItem.getDescription(), role,
							javaExchangeItem.getTimes(), javaExchangeItem.getValuesAsDoubles());
			dotNetExchangeItems[i] = dotNetExchangeItem;
		}

		_dotNetObservationDescriptions = new cli.OpenDA.DotNet.Bridge.ObservationDescriptions(dotNetExchangeItems);
	}

	public cli.OpenDA.DotNet.Interfaces.IObservationDescriptions getDotNetObservationDescriptions() {
		return _dotNetObservationDescriptions;
	}

	@Override
	public List<IPrevExchangeItem> getExchangeItems() {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getExchangeItems(): Not implemented yet.");
	}

	@Override
	public IVector getValueProperties(String Key) {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getValueProperties(): Not implemented yet.");
	}

	@Override
	public String[] getStringProperties(String Key) {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getStringProperties(): Not implemented yet.");
	}

	@Override
	public String[] getPropertyKeys() {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getPropertyKeys(): Not implemented yet.");
	}

	@Override
	public int getPropertyCount() {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getPropertyCount(): Not implemented yet.");
	}

	@Override
	public int getObservationCount() {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getObservationCount(): Not implemented yet.");
	}

	@Override
	public ITime[] getTimes() {
		throw new UnsupportedOperationException("org.openda.dotnet.ObservationDescriptionsN2J.getTimes(): Not implemented yet.");
	}
}
