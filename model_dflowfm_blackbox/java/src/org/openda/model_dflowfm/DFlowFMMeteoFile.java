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

package org.openda.model_dflowfm;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Vector;

import java.io.*;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * IDataobject implementation for DFlow FM meteo files
 * <p/>
 * <p/>
 * ### START OF HEADER
 * ### This file is created by Deltares
 * ### Additional comments
 * FileVersion = 1.03
 * filetype = meteo_on_equidistant_grid
 * NODATA_value = -9999.0
 * n_cols = 5
 * n_rows = 4
 * grid_unit = m
 * x_llcenter = 60
 * y_llcenter = 60
 * dx = 110
 * dy = 110
 * n_quantity = 1
 * quantity1 = x_wind
 * unit1 = m s-1
 * ### END OF HEADER
 * TIME = 0 hours since 2006-01-01 00:00:00 +00:00
 * 10 10 10 10 10
 * 10 10 10 10 10
 * 10 10 10 10 10
 * 10 10 10 10 10
 * TIME = 1 hours since 2006-01-01 00:00:00 +00:00
 * -10 -10 -10 -10 -10
 * -10 -10 -10 -10 -10
 * -10 -10 -10 -10 -10
 * -10 -10 -10 -10 -10
 */

public class DFlowFMMeteoFile implements IDataObject {

	private static String[] knownFileTypes = {"meteo_on_equidistant_grid"};
	private Integer fileType = null;
	private ArrayExchangeItem exchangeItem = null;
	private File file = null;
	private int endOfHeader = 0;
	private int nCols, nRows;
	private HashMap<String, Double> unitFactorToMjd = new HashMap<String, Double>() {{
		put("hours", 1.0 / 24.0);
		put("minutes", 1.0 / 1440.0);
	}};
	private String timeUnits;
	private List<String> headerContent = new ArrayList<>();
	private double referenceDateInMjd;
	private String referenceDateString;


	// date related
	private static String dateFormat = "yyyy-MM-dd HH:mm:ss XXX"; // TIME minutes/hours since YYYY-MM-DD HH:MM:SS TIME ZONE
	private TimeZone timeZone;

	private Pattern datePattern = Pattern.compile("TIME\\s+=\\s+(\\d+)\\s+(minutes|hours)\\s+since(.+)$"); // TIME =  minutes/hours since YYYY-MM-DD HH:MM:SS TIME ZONE
	private static String dateOutFormat = "TIME = %d %s since %s"; // TIME minutes/hours since YYYY-MM-DD HH:MM:SS TIME ZONE
	private DateFormat dateFormatter;
	private Date referenceDate;


//    private Pattern datePattern = Pattern.compile("TIME\\s+=\\s+(minutes|hours)\\s+since(.+)$"); // TIME =  minutes/hours since YYYY-MM-DD HH:MM:SS TIME ZONE

	public void initialize(File workingDir, String[] arguments) {

		this.file = new File(workingDir, arguments[0]);

		HashMap<String, String> headerInfo = new HashMap<String, String>();

		try {
			Scanner input = new Scanner(file);
			input.useLocale(Locale.US);
			try {
				String line;

				// read header
				while ((line = input.nextLine()) != null) {
					this.endOfHeader++;
					headerContent.add(line);
					if (line.startsWith("### END OF HEADER")) break;
					if (line.startsWith("###")) continue;
					String[] lineParts = line.split("=", 2);
					headerInfo.put(lineParts[0].trim(), lineParts[1].trim());
				}

				if ( headerInfo.containsKey("filetype") ) {
					if ( !headerInfo.get("filetype").contains("meteo_on_equidistant_grid") ) {
						throw new RuntimeException("DFlowFMMeteoFile only supports filetype 'meteo_on_equidistant_grid'");
					}
				} else {
					throw new RuntimeException("File " + this.file + " does not contain 'filetype' in header." );
				}

				// handle relevant header information
				if ( headerInfo.containsKey("n_cols") ) {
					nCols = Integer.parseInt(headerInfo.get("n_cols"));
				} else {
					throw new RuntimeException("File " + this.file + " does not contain 'n_cols' in header." );
				}

				if ( headerInfo.containsKey("n_rows") ) {
					nRows = Integer.parseInt(headerInfo.get("n_rows"));
				} else {
					throw new RuntimeException("File " + this.file + " does not contain 'n_rows' in header." );
				}

				String exchangeItemId;
				if ( headerInfo.containsKey("quantity1") ) {
					exchangeItemId = headerInfo.get("quantity1");
				} else {
					throw new RuntimeException("File " + this.file + " does not contain 'quantity1' in header." );
				}


				// read data

				Vector timesVector = new Vector(0);
				Vector valuesVector = new Vector(0);

				while (input.hasNextLine()) {
					line = input.nextLine();
					if (line.isEmpty()) continue;

					Matcher m = datePattern.matcher(line);
					if (m.find()) {
						double[] timeInMjd = new double[1];
						timeInMjd[0] = Double.parseDouble(m.group(1));
						this.timeUnits = m.group(2);
						timeInMjd[0] *= this.unitFactorToMjd.get(this.timeUnits);
						try {
							this.referenceDateString = m.group(3).trim();
							this.referenceDateInMjd = TimeUtils.date2Mjd(this.referenceDateString, dateFormat);
							this.dateFormatter = new SimpleDateFormat(dateFormat);
							timeInMjd[0] += this.referenceDateInMjd;
							timesVector = Vector.concatenate(timesVector, new Vector(timeInMjd));
						} catch (ParseException e) {
							throw new RuntimeException("Cannot parse date from " + line + " for format " + dateFormat + " in file " + file);
						}
					} else {
						throw new RuntimeException("Cannot parse date from " + line + " using regex in file " + file);
					}

					double[] values = new double[nCols * nRows];
					for (int i = 0; i < nCols * nRows; i++) {
						values[i] = input.nextDouble();
					}
					valuesVector = Vector.concatenate(valuesVector, new Vector(values));
				}

				exchangeItem = new ArrayExchangeItem(exchangeItemId, IExchangeItem.Role.Input);
				exchangeItem.setValuesAsDoubles(valuesVector.getValues());
				exchangeItem.setTimeInfo(new TimeInfo(timesVector.getValues()));
			} finally {
				input.close();
			}
//              catch (NumberFormatException ex) {
//                 throw new RuntimeException("Cannot read file");
//              }
		} catch (IOException ex) {
			throw new RuntimeException("Cannot read file '" +
					file.getAbsolutePath() + "'");
		}

	}

	public String[] getExchangeItemIDs() {
		String[] result = new String[1];
		result[0] = exchangeItem.getId();
		return result;
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		if (exchangeItem.getRole() == role) {
			String[] result = new String[1];
			result[0] = exchangeItem.getId();
			return result;
		} else {
			return null;
		}
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (exchangeItem.getId() == exchangeItemID) {
			return exchangeItem;
		} else {
			throw new RuntimeException("bla");
		}
	}

	public void finish() {

		try {
			BufferedWriter output = new BufferedWriter(new FileWriter(this.file));

			// write header
			for (int i = 0; i < headerContent.size(); i++) {
				try {
					//System.out.println("WRITE " + sContent[i]);
					output.write(headerContent.get(i));
					output.newLine();
				} catch (IOException e) {
					throw new RuntimeException("Cannot write line in file: " + file);
				}
			}

			double[] times = exchangeItem.getTimes();
			double[] values = exchangeItem.getValuesAsDoubles();

			int index = 0;
			for (int timeIndex = 0; timeIndex < times.length; timeIndex++) {
				double timeDeltaInUnits = times[timeIndex] - referenceDateInMjd;
				timeDeltaInUnits /= unitFactorToMjd.get(timeUnits);
				output.write(String.format(Locale.US,  this.dateOutFormat, Math.round(timeDeltaInUnits), this.timeUnits, this.referenceDateString));
				output.newLine();
				for (int m = 0; m < nRows; m++) {
					String line = "";
					String formatString = "%g ";
					for (int n = 0; n < nCols; n++) {
						line += String.format(Locale.US, formatString, values[timeIndex * nRows * nCols + m * nCols + n]);
					}
					output.write(line.trim());
					output.newLine();
				}
			}
			output.close();
		} catch (IOException ex) {
			throw new RuntimeException("Cannot write file " + file);
		}

	}


}
