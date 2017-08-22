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
import org.openda.utils.Time;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created with IntelliJ IDEA.
 * User: bos_en
 * Date: 9/2/13
 * Reads binary his and map files.
 */
public class delwaqBinaryFile implements IDataObject {
	private File ioFile;
	private RandomAccessFile raf;
	private static final int nr_title_lines = 4;
	private static final int title_line_length = 40;
	private int nr_substances;
	private int nr_segments;
	private String[] substances_names;
	private String[] segment_names;
	private boolean has_segment_names;
	private static final int names_line_length = 20;
	private double[] times; // Mjd
	private int header_size;
	private int record_size;
	private long t0; //
	private ArrayList<IExchangeItem> items; // List of created ExchangeItems.
	private String itemDescription;
	private String[] ids; // ExchangeItems with these IDs can be made on-demand.
	private int[] ids_substance_index; // Synchronous with ids.
	private int[] ids_segment_index; // Synchronous with ids.

	/**
	 * Get the identifiers of the exchange items that can be retrieved from and set to the model.
	 * Should return String[0] if there are no items.
	 *
	 * @return The array of exchange item identifiers.
	 */
	public String[] getExchangeItemIDs() {
		return ids;
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
		return ids; // For now return all IDs.
	}

	/**
	 * Get the exchange item specified by <c>exchangeItemID</c>.
	 * Returns null if no exchangeItem with the given exchangeItemID is found.
	 *
	 * @param exchangeItemID The exchange item identifier.
	 * @return The required exchange item.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		for (int i=0; i<ids.length; i++) {
			if (ids[i].equals(exchangeItemID)) {
				return findOrCreateExchangeItem(i);
			}
		}
		return null; // No matching ExchangeItem found.
	}

	/**
	 * Find or create the requested ExchangeItem.
	 *
	 * @param index The array index of the requested ExchangeItem.
	 * @return The requested ExchangeItem.
	 */
	private IExchangeItem findOrCreateExchangeItem(int index) {
		// find
		for (IExchangeItem item : items) {
			if (item.getId().equals(ids[index])) {
				return item;
			}
		}
		// create
		double[] values = new double[times.length];
		try {
			for (int i=0; i<values.length; i++) {
				raf.seek(header_size + i * record_size + 4 + 4 * ids_segment_index[index] * (nr_segments-1) + 4 * ids_substance_index[index]);
				values[i] = readFloat();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		items.add(new delwaqSubstanceExchangeItem(ids[index], itemDescription, IPrevExchangeItem.Role.Input, values, this));
		return items.get(items.size()-1);
	}

	/**
	 * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
	 * where its configuration file is), and provide its arguments.
	 *
	 * @param workingDir The directory indicating the where the configurable is started (not as 'current
	 *                   working directory', but as the root path for its configuration files etc).
	 * @param arguments  The file name string. (The substance id. The segment id.)
	 */
	public void initialize(File workingDir, String[] arguments) {
		int substanceId = -1;
		int segmentId = -1;
		if (arguments.length == 0) throw new RuntimeException("Incorrect arguments: a file name string is required."); // Sanity check.
		if (arguments.length == 3) {
			substanceId = Integer.parseInt(arguments[1]);
			segmentId = Integer.parseInt(arguments[2]);
		}
		if (arguments[0].endsWith(".his")) {
			has_segment_names = true;
			itemDescription = "Delwaq substance from his file.";
		} else if (arguments[0].endsWith(".map")) {
			has_segment_names = false;
			itemDescription = "Delwaq substance from map file.";
		}
		ioFile = new File(workingDir, arguments[0]);
		try {
			raf = new RandomAccessFile(ioFile, "r");
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Could not open file "+ioFile.getPath());
		}
		processHeader();
		readTimes();
		items = new ArrayList<IExchangeItem>();
		// Initialise ids.
		if (substances_names.length == 0) {
			ids = new String[0];
		} else if (substanceId == -1 || segmentId == -1) {
			ids = new String[nr_substances*nr_segments];
			ids_substance_index = new int[nr_substances*nr_segments];
			ids_segment_index = new int[nr_substances*nr_segments];
			for (int i=0; i<substances_names.length; i++) {
				for (int j=0; j<nr_segments; j++) {
					if (has_segment_names) {
						ids[i * nr_segments + j] = substances_names[i]+"_"+segment_names[j];
					} else {
						ids[i * nr_segments + j] = substances_names[i]+"_Segment"+Integer.toString(j);
					}
					ids_substance_index[i * nr_segments + j] = i;
					ids_segment_index[i * nr_segments + j] = j;
				}
			}
		} else {
			ids = new String[1];
			ids_substance_index = new int[]{1};
			ids_segment_index = new int[]{1};
			if (has_segment_names) {
				ids[0] = substances_names[substanceId]+"_"+segment_names[segmentId];
			} else {
				ids[0] = substances_names[substanceId]+"_Segment"+Integer.toString(segmentId);
			}
		}
	}

	/**
	 * Shut down this dataObject. Typically this implies flushing output, closing files and network connections.
	 * It is also possible that work is done directly after modification of the exchangeItems, so then no work
	 * may be left.
	 */
	public void finish() {
		try {
			raf.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Read and interpret the binary file's header.
	 */
	private void processHeader() {
		String[] title = new String[nr_title_lines];
		for (int i=0; i<nr_title_lines; i++) {
			title[i] = readText(title_line_length).trim();
		}
		readT0(title);
		nr_substances = readInt();
		nr_segments = readInt();
		substances_names = new String[nr_substances];
		for (int i=0; i<nr_substances; i++) {
			substances_names[i] = readText(names_line_length).trim();
		}
		if (has_segment_names) {
			segment_names = new String[nr_segments];
			for (int i=0; i<nr_segments; i++) {
				readInt(); // skip filler
				segment_names[i] = readText(names_line_length).trim();
			}
			// Sanity checks: existence and uniqueness
			for (String name : segment_names) {
				if (name.length() == 0) throw new RuntimeException("Segment name of zero characters encountered.");
			}
			Set<String> segment_names_set = new HashSet<String>(Arrays.asList(segment_names));
			if (segment_names.length != segment_names_set.size()) throw new RuntimeException("Segment names are not unique.");
		}
		// Fill helper parameters.
		header_size = nr_title_lines * title_line_length + 4 + 4 + nr_substances * 20; // bytes
		if (has_segment_names) {
			header_size += nr_segments * 24; // bytes
		}
		record_size = 4 * (1 + nr_substances * nr_segments); // bytes
	}

	/**
	 * Process title and fill t0.
	 */
	private void readT0(String[] title) {
		Pattern t0_pattern = Pattern.compile("\\d+?[.]\\d+?[.]\\d+?\\s+?\\d+?[:]\\d+?[:]\\d+?"); // yyyy.mm.dd hh:mm::ss
		Matcher regexMatcher = t0_pattern.matcher(title[3]);
		if (regexMatcher.find()) {
			SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss");
			SimpleTimeZone timeZone = new SimpleTimeZone(0, TimeZone.getDefault().toString()); // TODO ? : Replace 0 by milliseconds equivalent of his file's stated timezone.
			dateFormat.setTimeZone(timeZone);
			try {
				t0 = dateFormat.parse(regexMatcher.group()).getTime(); // In milliseconds since January 1, 1970, 00:00:00 GMT until T0(line 4 of title)
			} catch (ParseException e) {
				throw new RuntimeException("Unable to parse T0 from the fourth title string.");
			}
		} else {
			throw new RuntimeException("Unable to read T0 from the fourth title string.");
		}
	}

	/**
	 * Read records from body and fill times.
	 */
	private void readTimes() {
		try {
			times = new double[(int) (raf.length() - header_size) / record_size];
			for (int i=0; i<times.length; i++) {
				raf.seek(header_size + i * record_size);
				times[i] = Time.milliesToMjd(t0 + (long) readInt() * 1000);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private String readText(int aLength) {
		byte[] bytes = new byte[aLength];
		try {
			raf.read(bytes);
		} catch (IOException e) {
			throw new RuntimeException("Failed to read an array of bytes.");
		}
		return new String(bytes).trim();
	}

	private int readInt() {
		try {
			int ch1 = raf.read();
			int ch2 = raf.read();
			int ch3 = raf.read();
			int ch4 = raf.read();
			int result = ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0));
			return result;
		} catch (IOException e) {
			throw new RuntimeException("Failed to read an array of bytes.");
		}
	}

	private float readFloat() {
		return Float.intBitsToFloat(readInt());
	}

	/**
	 * All ExchangeItems will have the same times array.
	 *
	 * @return Array of timestamps.
	 */
	public double[] getTimes() {
		return times;
	}
}
