/* OpenDA v2.4
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

package org.openda.exchange.ioobjects;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.ArrayExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.DoubleArraySearch;

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

public class ASCIIVectorIoObject implements IoObjectInterface {

	private File  myFile;
	private ArrayExchangeItem myExchangeItem;


	/**
	 * Initialize the IoObject
	 *
	 * @param workingDir Working directory
	 * @param fileName   The name of the file containing the data (relative to the working dir.)
	 * @param arguments  Additional arguments (may be null zero-length)
	 */
	public void initialize(File workingDir, String fileName, String[] arguments) {
		// Check whether the fileName is null or contains * or ?
		// If so, use the multi-file version
		// It it doesn't, call readNoosTimeSeries directly
		if (fileName == null || "".equals(fileName)) {
			throw new RuntimeException("The given filename is empty. Did you forget to fill it in in you XML configuration");
		}
		// Open File
		myFile = new File(workingDir,fileName);
		if (!myFile.exists()) {
			throw new RuntimeException("I cannot open the file "+myFile+" Hence I cannot read the vector. Did you make a configuration error or did the model not produce any output?");
		}
		// Read File
		List<Double> allValues = new ArrayList<Double>();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(myFile));

			String line = null;
			NumberFormat format = NumberFormat.getInstance(Locale.US);
			while((line = reader.readLine()) != null) {

				Number number = format.parse(line);
				Double d = number.doubleValue();
				allValues.add(d);
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ParseException e) {
			e.printStackTrace();
		}

		//Copy all values in array
		int n=allValues.size();
		double[] values = new double[n];
		for (int i=0;i<n;i++){
			values[i]=allValues.get(i);
		}
		//Create exchangeItem and set values
		myExchangeItem  = new ArrayExchangeItem("ascii_vec", IPrevExchangeItem.Role.InOut);
		myExchangeItem.setValuesAsDoubles(values);
	}

	/**
	 * Ask which elements can be accessed
	 *
	 * @return The list of element identifiers that can be accessed
	 */
	@Override
	public IPrevExchangeItem[] getExchangeItems() {
		IPrevExchangeItem[]  items =  new IPrevExchangeItem[1];
		items[0]=this.myExchangeItem;
		return items;
	}

	@Override
	public void finish() {
		//write values to file

		PrintWriter fo = null;
		NumberFormat format = NumberFormat.getInstance(Locale.US);
		try {
			fo = new PrintWriter(new FileOutputStream(myFile));
			double[] values = this.myExchangeItem.getValuesAsDoubles();
			for (int i=0; i<values.length; i++) {
				String s = String.format(Locale.US, "%f", values[i]);
				fo.println(s);
			}
			fo.close();

		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
}
