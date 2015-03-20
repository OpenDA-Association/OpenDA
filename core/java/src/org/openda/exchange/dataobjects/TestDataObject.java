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

package org.openda.exchange.dataobjects;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.iotools.IoUtils;
import org.openda.interfaces.IComposableDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

/**
 * DataObject that writes data to a .txt file. This is only used for unit tests.
 *
 * @author Arno Kockx
 */
public class TestDataObject implements IComposableDataObject {
	private File file = null;
	private List<IExchangeItem> exchangeItems = new ArrayList<IExchangeItem>();

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		this.file = new File(workingDir, fileName);

		if (this.file.exists()) {
			//reading not implemented.

		} else {//if file does not exist.
			//no data to read. This dataObject can still be used for writing data to file.
		}
	}

	@Override
	public String[] getExchangeItemIDs() {
		String[] exchangeItemIDs = new String[this.exchangeItems.size()];

		for (int n = 0; n < this.exchangeItems.size(); n++) {
			exchangeItemIDs[n] = this.exchangeItems.get(n).getId();
		}

		return exchangeItemIDs;
	}

	@Override
	public String[] getExchangeItemIDs(Role role) {
		ArrayList<String> exchangeItemIDs = new ArrayList<String>();

		for (int n = 0; n < this.exchangeItems.size(); n++) {
			IExchangeItem exchangeItem = this.exchangeItems.get(n);
			if (exchangeItem.getRole().equals(role)) {
				exchangeItemIDs.add(exchangeItem.getId());
			}
		}

		return exchangeItemIDs.toArray(new String[exchangeItemIDs.size()]);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		for (int n = 0; n < this.exchangeItems.size(); n++) {
			IExchangeItem exchangeItem = this.exchangeItems.get(n);
			if (exchangeItem.getId().equals(exchangeItemID)) {
				return exchangeItem;
			}
		}

		return null;
	}

	public void addExchangeItem(IExchangeItem item) {
		//copy exchange item.
		ArrayExchangeItem itemCopy = new ArrayExchangeItem(item.getId(), item.getRole());
		itemCopy.copyValuesFromItem((IExchangeItem) item);

		//add copy of exchange item.
		this.exchangeItems.add(itemCopy);
	}

	@Override
	public void finish() {
		List<IExchangeItem> outputExchangeItems = new ArrayList<IExchangeItem>();
		for (IExchangeItem exchangeItem : this.exchangeItems) {
			if (exchangeItem.getRole() == IPrevExchangeItem.Role.Output || exchangeItem.getRole() == IPrevExchangeItem.Role.InOut) {
				outputExchangeItems.add(exchangeItem);
			}
		}

		Results.putMessage(this.getClass().getSimpleName() + ": writing data to file " + this.file.getAbsolutePath());

		try {
			Writer writer = new FileWriter(this.file);
			try {
				for (IExchangeItem exchangeItem : outputExchangeItems) {
					IoUtils.writeExchangeItem(writer, exchangeItem);
				}
			} finally {
				writer.close();
			}
		} catch (IOException e){
			throw new RuntimeException("Problem while writing file '" + this.file.getAbsolutePath() + "'.", e);
		}
	}
}
