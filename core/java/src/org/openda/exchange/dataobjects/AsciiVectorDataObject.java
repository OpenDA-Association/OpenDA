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

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * General ExchangeItem for storing array based data.
 *
 * This implementation can handle ASCII files containing a single vector
 * Each line of the file contains a single variable
 * No comment/empty lines etc are allowed
 * The vector will always have exchangeID="ascii_vec"
 *
 *
 * @author Nils van Velzen
 */

public class AsciiVectorDataObject implements IDataObject {

	// Either read the values in the ASCII file as:
	//  - an array of numbers (ARRAY_EXCHANGE_ITEM), or as
	//  - an array of exchange items (SEPARATE_EXCHANGE_ITEMS)
    private enum Mode {ARRAY_EXCHANGE_ITEM, SEPARATE_EXCHANGE_ITEMS};

    private Mode mode;
	private File myFile;

	private ArrayExchangeItem myExchangeItem;
	private List<DoubleExchangeItem> myExchangeItems;

	/**
	 * Initialize the IDataObject
	 *
	 * @param workingDir Working directory
	 * @param arguments  Additional arguments (first one being the file name)
	 */
	public void initialize(File workingDir, String[] arguments) {
		// Check whether the fileName is null or contains * or ?
		// If so, use the multi-file version
		// It it doesn't, call readNoosTimeSeries directly

		if (arguments.length == 0) {
			throw new RuntimeException("The arguments array is empty, at least one argument is expected, namely the filename");
		} else if (arguments.length > 2) {
			throw new RuntimeException("No more than two arguments are supported.");
		}

		for (String arg : arguments) {
			System.out.printf("  %s%n", arg);
		}

		String fileName = arguments[0];

		// The (optional) second argument determines how the data in the ASCII file is translated to exchange items.
		mode = Mode.ARRAY_EXCHANGE_ITEM;
		if (arguments.length == 2) {
			if (arguments[1].equalsIgnoreCase("as_separate_exchange_items")) {
				mode = Mode.SEPARATE_EXCHANGE_ITEMS;
			}
		}
		if (fileName == null || "".equals(fileName)) {
			throw new RuntimeException("The given filename is empty. Did you forget to fill it in in you XML configuration");
		}

		// Open File
		myFile = new File(workingDir,fileName);
		if (!myFile.exists()) {
			throw new RuntimeException("I cannot open the file "+myFile+" Hence I cannot read the vector. Did you make a configuration error or did the model not produce any output?");
		}

		// Read File.
		List<Double> allValues = new ArrayList<>();
		try (
			FileReader reader = new FileReader(myFile);
			BufferedReader buff_reader = new BufferedReader(reader)
		) {
			String line;
			NumberFormat format = NumberFormat.getInstance(Locale.US);
			while((line = buff_reader.readLine()) != null) {

				Number number = format.parse(line);
				Double d = number.doubleValue();
				allValues.add(d);
			}
		} catch (FileNotFoundException|ParseException e) {
			e.printStackTrace();
		} catch (IOException e){
			e.printStackTrace();
		}

		// Create exchange items.
		String file_basename = getFileBaseName(myFile.getName());
		if (mode == Mode.ARRAY_EXCHANGE_ITEM) {
			double[] values = new double[allValues.size()];
			for (int i = 0; i < allValues.size(); i++) {
				values[i] = allValues.get(i);
			}
			myExchangeItem = new ArrayExchangeItem(file_basename, IExchangeItem.Role.InOut);
			myExchangeItem.setValuesAsDoubles(values);
		} else {
			myExchangeItems = new ArrayList<>();
			for (int i = 0; i < allValues.size(); i++) {
				String current_exchange_item_id = String.format("%s@%d", file_basename, i+1);
				myExchangeItems.add(new DoubleExchangeItem(current_exchange_item_id, IExchangeItem.Role.InOut, allValues.get(i)));
			}
		}
	}

	/**
	 * Ask which elements can be accessed
	 *
	 * @return The list of element identifiers that can be accessed
	 */
	public IExchangeItem[] getExchangeItems() {

		assert(myExchangeItem == null || myExchangeItems == null);

		IExchangeItem[] items;
		if (myExchangeItem != null) {
			items = new IExchangeItem[1];
			items[0]=this.myExchangeItem;
		} else {
			items = myExchangeItems.toArray(new IExchangeItem[myExchangeItems.size()]);
		}
		return items;
	}

	public String[] getExchangeItemIDs() {

		assert(myExchangeItem == null || myExchangeItems == null);

		String[] ids;
		if (myExchangeItem != null) {
			ids = new String[1];
			ids[0] = this.myExchangeItem.getId();
		} else {
			ids = new String[myExchangeItems.size()];
			for (int i = 0; i < myExchangeItems.size(); i++) {
				ids[i] = myExchangeItems.get(i).getId();
			}
		}
		return ids;
	}

	public String[] getExchangeItemIDs(IExchangeItem.Role role) {

		assert(myExchangeItem == null || myExchangeItems == null);
		String[] ids;

		if (myExchangeItem != null) {
			if (role == this.myExchangeItem.getRole()) {
				ids = getExchangeItemIDs();
			} else {
				ids = new String[0];
			}
		} else {
			// At this moment, the role cannot be set manually and is always InOut AFAIK,
            // therefore the code below may be simplified.
			List<DoubleExchangeItem> exchange_items_with_requested_role = new ArrayList<>();
			for (DoubleExchangeItem exchange_item : myExchangeItems) {
				if (role == exchange_item.getRole()) {
					exchange_items_with_requested_role.add(exchange_item);
				}
			}
			ids = new String[exchange_items_with_requested_role.size()];
			for (int i = 0; i < exchange_items_with_requested_role.size(); i++) {
				ids[i] = exchange_items_with_requested_role.get(i).getId();
			}
		}

		return ids;
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {

		assert(myExchangeItem == null || myExchangeItems == null);

		if (myExchangeItem != null) {
			if (this.myExchangeItem.getId().equals(exchangeItemID)) {
				return this.myExchangeItem;
			}
		} else {
			for (DoubleExchangeItem item : myExchangeItems) {
				if (item.getId().equals(exchangeItemID)) {
					return item;
				}
			}
		}
		return null;
	}

	private String getFileBaseName(String fileName) {
		String fileBaseName = fileName;
		if (fileName.indexOf('.') > 0)
			fileBaseName = fileName.substring(0, fileName.lastIndexOf('.'));
		return fileBaseName;
	}

	@Override
	public void finish() {
		//write values to file
		assert(myExchangeItem == null || myExchangeItems == null);

		PrintWriter fo = null;
		try {
			fo = new PrintWriter(new FileOutputStream(myFile));
			double[] values;

			if (myExchangeItem != null) {
				values = this.myExchangeItem.getValuesAsDoubles();
			} else {
				values = new double[myExchangeItems.size()];
				for (int i = 0; i < myExchangeItems.size(); i++) {
					values[i] = myExchangeItems.get(i).getValue();
				}
			}

			for (double value : values) {
				String s = String.format(Locale.US, "%f", value);
				fo.println(s);
			}
			fo.close();

		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
}
