/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.model_delwaq;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openda.utils.io.AsciiFileUtils.readLines;
import static org.openda.utils.io.AsciiFileUtils.writeLines;

/**
 * Created with IntelliJ IDEA.
 * User: bos_en
 * Date: 8/26/13
 * Baseclass for Delwaq *.cfg files, which belong to the *.inp file.
 */
public abstract class delwaqInpFile implements IDataObject {
	// File handles to the inp-file's configuration component files (*.cfg).
	private File names_cfg = null;
	private File values_cfg = null;
	// Content of the the inp-file's configuration component files.
	private List<String> names_lines = null;
	private List<String> values_lines = null;
	// Regular expression matcher.
	private Matcher regexMatcherA = null;
	private Matcher regexMatcherB = null;
	// Regular expression patterns.
	protected Pattern name_line_pattern = null;
	protected Pattern name_word_pattern = null;
	//    Range of regex group to interpret as name.
	protected int name_word_start = 0;
	protected int name_word_stop = 0;
	protected Pattern value_line_pattern = null;
	protected Pattern value_word_pattern = null;
	// ExchangeItem array.
	private ArrayList<IExchangeItem> items = null;
	protected String itemDescription = null;
	// Format string for a DecimalFormat formatted.
	protected String format = null;

	/**
	 * Get the identifiers of the exchange items that can be retrieved from and set to the model.
	 * Should return String[0] if there are no items.
	 *
	 * @return The array of exchange item identifiers.
	 */
	public String[] getExchangeItemIDs() {
		String[] identifiers = new String[items.size()];
		for (int i=0; i<items.size(); i++) {
			identifiers[i] = items.get(i).getId();
		}
		return identifiers;
	}

	/**
	 * Get the identifiers of the exchange items that can be retrieved from and set to the model,
	 * according to the specified role (input, output, both)
	 * Should return String[0] if there are no matching items.
	 *
	 * @param role Input, Output, or InOut (i.e. both)
	 * @return The array of exchange item identifiers.
	 */
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		ArrayList<String> identifiers = null;
		for (IExchangeItem item : items) {
			if (item.getRole().equals(role)) {
				identifiers.add(item.getId());
			}
		}
		return identifiers.toArray(new String[identifiers.size()]);
	}

	/**
	 * Get the exchange item specified by <c>exchangeItemID</c>.
	 * Returns null if no exchangeItem with the given exchangeItemID is found.
	 *
	 * @param exchangeItemID The exchange item identifier.
	 * @return The required exchange item.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		for (IExchangeItem item : items) {
			if (exchangeItemID.equals(item.getId())) {
				return item;
			}
		}
		return null; // No matching ExchangeItem found.
	}

	/**
	 * Shut down this dataObject. Typically this implies, flushing output, closing files and network connections.
	 * It is also possible that work is done directly after modification of the exchangeItems, so then no work
	 * may be left.
	 */
	public void finish() {
		// Assumes the order of items is unchanged since initialize(), therefor identical to the order in values_lines.
		int line_number = 0;
		String line = null;
		for (IExchangeItem item : items) {
			for (int i=line_number; i<values_lines.size(); i++) {
				line = values_lines.get(i);
				regexMatcherA = value_line_pattern.matcher(line);
				if (regexMatcherA.matches()) {
					regexMatcherB = value_word_pattern.matcher(line);
					if (line.contains(" ; "+((ScientificNotationExchangeItem) item).getId())) {
						line = regexMatcherB.replaceFirst((((ScientificNotationExchangeItem) item).getValueAsString()));
						values_lines.set(i, line);
					}
				}
			}
		}
		writeLines(values_cfg, values_lines);
	}

	/**
	 * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
	 * where its configuration file is), and provide its arguments.
	 *
	 * @param workingDir The directory indicating the where the configurable is started (not as 'current
	 *                   working directory', but as the root path for its configuration files etc).
	 * @param arguments  The two file name strings of the names and values *.cfg files.
	 */
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 2) throw new RuntimeException("Incorrect arguments: 2 file name strings are required."); // Sanity check.
		// Construct the file handles.
		names_cfg = new File(workingDir, arguments[0]);
		values_cfg = new File(workingDir, arguments[1]);
		// Read the configuration files into ArrayLists.
		names_lines = readLines(names_cfg);
		values_lines = readLines(values_cfg);
		// Process the ArrayLists into ExchangeItems.
		ArrayList<String> names = new ArrayList<String>();
		ArrayList<String> values = new ArrayList<String>();
		items = new ArrayList<IExchangeItem>();
		if (names_lines != null && values_lines != null) {
			for (String line : names_lines) {
				regexMatcherA = name_line_pattern.matcher(line);
				if (regexMatcherA.matches()) {
					regexMatcherB = name_word_pattern.matcher(line);
					if (regexMatcherB.find()) {
						names.add(regexMatcherB.group().substring(name_word_start, regexMatcherB.group().length() + name_word_stop));
					}
				}
			}
			for (String line : values_lines) {
				regexMatcherA = value_line_pattern.matcher(line);
				if (regexMatcherA.matches()) {
					regexMatcherB = value_word_pattern.matcher(line);
					if (regexMatcherB.find()) values.add(regexMatcherB.group());
				}
			}
			if (names.size() != values.size()) throw new RuntimeException("Unequal number of names and values."); // Sanity check.
			for (int i=0; i<names.size(); i++) {
				items.add(new ScientificNotationExchangeItem(names.get(i).toString(),
						                                     itemDescription,
						                                     IPrevExchangeItem.Role.InOut,
						                                     Double.parseDouble(values.get(i).toString()),
						                                     format));
			}
		}
	}
}
