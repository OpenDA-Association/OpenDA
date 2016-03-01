/* MOD_V2.0
 * Copyright (c) 2016 OpenDA Association
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

package org.openda.wrapper_utils.io;

import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Reader and writer for Delft3D space varying wind and pressure files of FileVersion 1.03.
 *
 * Customary file extensions are *.amu for wind x-component, *.amv for wind y-component and *.amp for pressure or all combined in *.apwxwy or spiderweb *.spw.
 */
public class SpaceVaryingWindAndPressureFile {

	private File svwpFile = null;
	LinkedHashMap<String, String> svwpFileHeader = null;

	private List<String> supportedGridFileTypeStrings = new ArrayList<>(Arrays.asList("meteo_on_equidistant_grid", "meteo_on_curvilinear_grid", "meteo_on_spider_web_grid"));
	private List<String> supportedFirstDataValueStrings = new ArrayList<>(Arrays.asList("grid_llcorner", "grid_ulcorner", "grid_lrcorner", "grid_urcorner"));
	private List<String> supportedDataRowStrings = new ArrayList<>(Arrays.asList("grid_row", "grid_col"));

	private double[] times;
	private List<String> originalTimeStrings = new ArrayList<String>();
	private Pattern datePattern = Pattern.compile("time\\s+=\\s+(-?\\d+(\\.\\d+)?)\\s+(minutes|hours)\\s+since(.+)$"); // TIME = x.y minutes/hours since YYYY-MM-DD HH:MM:SS TIME ZONE
	private HashMap<String, Double> unitFactorToMjd = new HashMap<String, Double>() {
		{
			put("hours", 1.0 / 24.0);
			put("minutes", 1.0 / 1440.0);
		}
	};
	private static String dateFormat = "yyyy-MM-dd HH:mm:ss XXX"; // TIME minutes/hours since YYYY-MM-DD HH:MM:SS TIME ZONE
	private static final double millisToDays      = 1.0 / (1000 * 60 * 60 * 24);
	private static final double mjdAtJanFirst1970 = 40587.0;

	private double[] values;


	// Getters

	private String gridType = null;
	private String nodata_value = null;
	private String n_quantity = null;
	private String[] quantities = {null, null, null};
	private String[] units = {null, null, null};
	private int n_cols = 0;
	private int n_rows = 0;
	private String grid_unit = null;
	private String dx = null;
	private String dy = null;
	private String x_llcorner = null;
	private String y_llcorner = null;
	private String x_llcenter = null;
	private String y_llcenter = null;
	private String grid_file = null;
	private String first_data_value = null;
	private String data_row = null;
	private String spw_radius = null;
	private String spw_rad_unit = null;
	private String spw_merge_frac = null;

	/**
	 * Grid type on which the data is specified.
	 *
	 * Supported types:
	 *  - meteo_on_spider_web_grid
	 *  - meteo_on_equidistant_grid
	 *  - meteo_on_curvilinear_grid
	 *
	 * @return Value for header keyword "FileType".
	 */
	public String getFileType() { return gridType; }

	/**
	 * Value used to indicate missing data.
	 *
	 * @return Value for header keyword "NODATA_value".
	 */
	public String getNodataValue() { return nodata_value; }

	/**
	 * Number of quantities in this file.
	 *
	 * @return Value for header keyword "n_quantity".
	 */
	public String getNQuantity() { return n_quantity; }

	/**
	 * Name of the first quantity.
	 *
	 * @return Value for the header keyword "quantity1".
	 */
	public String getQuantity1() {return this.quantities[0]; }

	/**
	 * Name of the second quantity.
	 *
	 * @return Value for the header keyword "quantity2", null if not present.
	 */
	public String getQuantity2() {return this.quantities[1]; }

	/**
	 * Name of the third quantity.
	 *
	 * @return Value for the header keyword "quantity3", null if not present.
	 */
	public String getQuantity3() {return this.quantities[2]; }

	/**
	 * Name of the first unit.
	 *
	 * @return Value for the header keyword "unit1".
	 */
	public String getUnit1() {return this.units[0]; }

	/**
	 * Name of the second unit.
	 *
	 * @return Value for the header keyword "unit2", null if not present.
	 */
	public String getUnit2() {return this.units[1]; }

	/**
	 * Name of the third unit.
	 *
	 * @return Value for the header keyword "unit3", null if not present.
	 */
	public String getUnit3() {return this.units[2]; }

	/**
	 * Number of data columns.
	 * In case "Filetype = meteo_on_spider_web_grid": Number of radial cells.
	 *
	 * @return Value for the header keyword "n_cols", null if not present.
	 */
	public int getN_cols() { return n_cols; }

	/**
	 * Number of data rows.
	 * In case "Filetype = meteo_on_spider_web_grid": Number of angular cells.
	 *
	 * @return Value for the header keyword "n_rows", null if not present.
	 */
	public int getN_rows() { return n_rows; 	}

	/**
	 * Unit of the data, m or degree.
	 *
	 * @return Value for the header keyword "grid_unit", null if not present.
	 */
	public String getGrid_unit() { return grid_unit; }

	/**
	 * Cellsize in x-direction.
	 *
	 * @return Value for the header keyword "dx", null if not present.
	 */
	public String getDx() { return dx; }

	/**
	 * Cellsize in y-direction.
	 *
	 * @return Value for the header keyword "dy", null if not present.
	 */
	public String getDy() { return dy; }

	/**
	 * Name of the curvilinear grid file on which the data is specified.
	 *
	 * @return Value for the header keyword "grid_file", null if not present.
	 */
	public String getGrid_file() { return grid_file; }

	/**
	 * Radius of the spiderweb.
	 *
	 * @return Value for the header keyword "spw_radius", null if not present.
	 */
	public String getSpw_radius() { return spw_radius; }

	/**
	 * Unit of the spiderweb radius, only m is supported.
	 *
	 * @return Value for the header keyword "spw_rad_unit", null if not present.
	 */
	public String getSpw_rad_unit() { return spw_rad_unit; }

	/**
	 * Fraction of the spiderweb radius where merging starts of the background wind with the spiderweb wind.
	 *
	 * @return Value for the header keyword "spw_merge_frac", null if not present.
	 */
	public String getSpw_merge_frac() { return spw_merge_frac; }

	public double[] getTimes() { return times; }

	public double[] getValues() { return values; }

	public SpaceVaryingWindAndPressureFile(File inputFile) {
		this.svwpFile = inputFile;

		try {
			processHeader();
			if (n_cols == 0) getMnFromGridFile();
			readBody();
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Input file does not exist: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
		}
	}

	/**
	 * Read, validate and interpret the file header.
	 *
	 * @return Hashmap of header key-value pairs.
	 * @throws FileNotFoundException
	 */
	private void processHeader() throws FileNotFoundException {

		// --- 1 --- Parse the header.

		Scanner svwpFileScanner = new Scanner(this.svwpFile);
		svwpFileScanner.useLocale(Locale.US);
		String line;

		this.svwpFileHeader = new LinkedHashMap<String, String>();

		while (svwpFileScanner.hasNextLine()) {
			line = svwpFileScanner.nextLine().trim().toLowerCase();

			// Stop at start of body, skipping comment lines and skipping empty lines.
			if (line.startsWith("time")) break;
			if (line.startsWith("#")) continue;
			if (line.isEmpty()) continue;

			// Interpret the header line as a key-value pair.
			String[] lineParts = line.split("=|#");
			this.svwpFileHeader.put(lineParts[0].trim(), lineParts[1].trim());
		}

		svwpFileScanner.close();

		// --- 2 --- Validate and interpret the Filetype-independent part of the header.

		if (!(this.svwpFileHeader.containsKey("fileversion")) || !(this.svwpFileHeader.get("fileversion").equals("1.03"))) {
			throw new RuntimeException("Input file header is not of FileVersion 1.03: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
		}

		if (!(this.svwpFileHeader.containsKey("filetype"))) {
			throw new RuntimeException("Input file header does not specify Filetype in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
		}

		this.gridType = this.svwpFileHeader.get("filetype");

		if (!(supportedGridFileTypeStrings.contains(this.gridType))) {
			throw new RuntimeException("Input file header specifies a unsupported Filetype " + this.gridType + ", choose from: " + supportedGridFileTypeStrings.toString() + "   (" + this.getClass().getName() + ")");
		}

		if (!(this.svwpFileHeader.containsKey("nodata_value"))) {
			throw new RuntimeException("Input file header does not specify NODATA_value in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
		}

		this.nodata_value = this.svwpFileHeader.get("nodata_value");

		if (!(this.svwpFileHeader.containsKey("n_quantity"))) {
			throw new RuntimeException("Input file header does not specify n_quantity in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
		}

		this.n_quantity = this.svwpFileHeader.get("n_quantity");

		int numberOfQuantities;
		try {
			numberOfQuantities = Integer.parseInt(this.n_quantity);
		} catch (NumberFormatException e) {
			throw new RuntimeException("Input file header specifies an invalid value for n_quantity: " + this.n_quantity + " in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
		}

		for (int i=1; i<=numberOfQuantities; i++) {
			String keyword = "quantity" + i;
			if (!(this.svwpFileHeader.containsKey(keyword))) {
				throw new RuntimeException("Input file header does not specify " + keyword + " in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
			}
			this.quantities[i-1] = this.svwpFileHeader.get("keyword");
		}

		for (int i=1; i<=numberOfQuantities; i++) {
			String keyword = "unit" + i;
			if (!(this.svwpFileHeader.containsKey(keyword))) {
				throw new RuntimeException("Input file header does not specify " + keyword + " in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
			}
			this.units[i-1] = this.svwpFileHeader.get("keyword");
		}

		// --- 3 --- Validate and interpret the Filetype-dependent part of the header.

		switch (this.gridType) {
			case "meteo_on_equidistant_grid":

				if (!(this.svwpFileHeader.containsKey("n_cols"))) {
					throw new RuntimeException("Input file header does not specify n_cols in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.n_cols = Integer.parseInt(this.svwpFileHeader.get("n_cols"));

				if (!(this.svwpFileHeader.containsKey("n_rows"))) {
					throw new RuntimeException("Input file header does not specify n_rows in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.n_rows = Integer.parseInt(this.svwpFileHeader.get("n_rows"));

				if (!(this.svwpFileHeader.containsKey("grid_unit"))) {
					throw new RuntimeException("Input file header does not specify grid_unit in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.grid_unit = this.svwpFileHeader.get("grid_unit");

				if (!(this.svwpFileHeader.containsKey("dx"))) {
					throw new RuntimeException("Input file header does not specify dx in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.dx = this.svwpFileHeader.get("dx");

				if (!(this.svwpFileHeader.containsKey("dy"))) {
					throw new RuntimeException("Input file header does not specify dy in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.dy = this.svwpFileHeader.get("dy");

				if (this.svwpFileHeader.containsKey("x_llcorner")) {
					this.x_llcorner = this.svwpFileHeader.get("x_llcorner");
				}

				if (this.svwpFileHeader.containsKey("y_llcorner")) {
					this.y_llcorner = this.svwpFileHeader.get("y_llcorner");
				}

				if (this.svwpFileHeader.containsKey("x_llcenter")) {
					this.x_llcenter = this.svwpFileHeader.get("x_llcenter");
				}

				if (this.svwpFileHeader.containsKey("y_llcenter")) {
					this.y_llcenter = this.svwpFileHeader.get("y_llcenter");
				}

				if (this.x_llcorner == null && this.x_llcenter == null) {
					throw new RuntimeException("Input file header specifies neither x_llcorner nor x_llcenter in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				if (this.x_llcorner != null && this.x_llcenter != null) {
					throw new RuntimeException("Input file header specifies both x_llcorner and x_llcenter in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				if (this.y_llcorner == null && this.y_llcenter == null) {
					throw new RuntimeException("Input file header specifies neither y_llcorner nor y_llcenter in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				if (this.y_llcorner != null && this.y_llcenter != null) {
					throw new RuntimeException("Input file header specifies both y_llcorner and y_llcenter in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				break;

			case "meteo_on_curvilinear_grid":

				if (!(this.svwpFileHeader.containsKey("grid_file"))) {
					throw new RuntimeException("Input file header does not specify grid_file in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.grid_file = this.svwpFileHeader.get("grid_file");

				if (!(this.svwpFileHeader.containsKey("first_data_value"))) {
					throw new RuntimeException("Input file header does not specify first_data_value in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.first_data_value = this.svwpFileHeader.get("first_data_value");

				if (!(supportedFirstDataValueStrings.contains(this.first_data_value))) {
					throw new RuntimeException("Input file header specifies a unsupported first_data_value " + this.first_data_value + ", choose from: " + supportedFirstDataValueStrings.toString() + "   (" + this.getClass().getName() + ")");
				}

				if (!(this.svwpFileHeader.containsKey("data_row"))) {
					throw new RuntimeException("Input file header does not specify data_row in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.data_row = this.svwpFileHeader.get("data_row");

				if (!(supportedDataRowStrings.contains(this.data_row))) {
					throw new RuntimeException("Input file header specifies a unsupported data_row " + this.data_row + ", choose from: " + supportedDataRowStrings.toString() + "   (" + this.getClass().getName() + ")");
				}

				break;

			case "meteo_on_spider_web_grid":

				if (!(this.svwpFileHeader.containsKey("n_cols"))) {
					throw new RuntimeException("Input file header does not specify n_cols in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.n_cols = Integer.parseInt(this.svwpFileHeader.get("n_cols")); // count in angular direction

				if (!(this.svwpFileHeader.containsKey("n_rows"))) {
					throw new RuntimeException("Input file header does not specify n_rows in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.n_rows = Integer.parseInt(this.svwpFileHeader.get("n_rows")); // count in radial direction

				if (!(this.svwpFileHeader.containsKey("grid_unit"))) {
					throw new RuntimeException("Input file header does not specify grid_unit in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.grid_unit = this.svwpFileHeader.get("grid_unit");

				if (!(this.svwpFileHeader.containsKey("spw_radius"))) {
					throw new RuntimeException("Input file header does not specify spw_radius in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.spw_radius = this.svwpFileHeader.get("spw_radius");

				if (!(this.svwpFileHeader.containsKey("spw_rad_unit"))) {
					throw new RuntimeException("Input file header does not specify spw_rad_unit in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.spw_rad_unit = this.svwpFileHeader.get("spw_rad_unit");

				if (!(this.svwpFileHeader.containsKey("spw_merge_frac"))) {
					throw new RuntimeException("Input file header does not specify spw_merge_frac in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}

				this.spw_merge_frac = this.svwpFileHeader.get("spw_merge_frac");

				// air_pressure_reference keyword is not used in practice

				break;
		}
	}

	private void getMnFromGridFile() throws FileNotFoundException {
		if (this.grid_file == null) return;

		File svwpFileFile = new File(String.valueOf(this.svwpFile));
		File gridFileFile = new File(svwpFileFile.getParent(), this.grid_file);
		Scanner gridFileScanner = new Scanner(new File(gridFileFile.getAbsolutePath()));
		gridFileScanner.useLocale(Locale.UK);
		String line;

		while (gridFileScanner.hasNextLine()) {
			line = gridFileScanner.nextLine().trim().toLowerCase();

			// Skip comment and empty lines.
			if (line.startsWith("*")) continue;
			if (line.isEmpty()) continue;
			if (line.startsWith("co")) continue;

			// Read m (n_cols) and n (n_rows).
			String[] mnValues = line.split("[\t ]+");
			this.n_cols = Integer.parseInt(mnValues[0]);
			this.n_rows = Integer.parseInt(mnValues[1]);
			break;
		}
		gridFileScanner.close();
	}

	private void readBody() throws FileNotFoundException {

		Scanner svwpFileScanner = new Scanner(this.svwpFile);
		svwpFileScanner.useLocale(Locale.US);
		String line;
		boolean insideHeader = true;

		List<Double> timesList = new ArrayList<Double>();
		List<Double> valuesList = new ArrayList<Double>();

		while (svwpFileScanner.hasNextLine()) {
			line = svwpFileScanner.nextLine().trim().toLowerCase();

			// Read past the header, skipping comment lines and skipping empty lines.
			if (line.startsWith("#")) continue;
			if (line.isEmpty()) continue;
			if (insideHeader) {
				if (line.startsWith("time")) {
					insideHeader = false;
				} else {
					continue;
				}
			}
			if (line.indexOf('#') > -1) {
				line = line.substring(0, line.indexOf('#'));
			}

			// Process the time string.
			Matcher m = datePattern.matcher(line);
			if (m.find()) {
				this.originalTimeStrings.add(line);
				double timeInMjd = Double.parseDouble(m.group(1));
				timeInMjd *= this.unitFactorToMjd.get(m.group(3));
				try {
					TimeZone tz = TimeZone.getTimeZone("GMT");
					SimpleDateFormat formatter = new SimpleDateFormat(dateFormat, Locale.UK);
					formatter.setTimeZone(tz);
					Date t = formatter.parse(m.group(4).trim());
					timeInMjd += (t.getTime()) * millisToDays + mjdAtJanFirst1970;
					timesList.add(timeInMjd);
				} catch (ParseException e) {
					throw new RuntimeException("Cannot parse date from " + line + " for format " + dateFormat + " in file " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
				}
			} else {
				throw new RuntimeException("Unable to find time data in line " + line + " in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
			}

			// Process the values strings.
			// Data for meteo_on_equidistant_grid is stored (east-west and south-north).
			// Data for meteo_on_curvilinear_grid is stored (east-west and south-north) for cell_corners.
			// Data for meteo_on_spider_web_grid is stored (east-west and south-north).
			try {
				for (int i = 0; i < (this.n_cols * this.n_rows); i++) {
					valuesList.add(svwpFileScanner.nextDouble());
				}
			} catch (InputMismatchException e) {
				throw new RuntimeException("Unable to find " + Integer.toString(this.n_cols*this.n_rows) + " data values after time string " + line + " in file: " + this.svwpFile.getAbsolutePath() + "   (" + this.getClass().getName() + ")");
			}
		}
		svwpFileScanner.close();

		this.times = new double[timesList.size()];
		for (int i=0; i<timesList.size(); i++) {
			this.times[i] = timesList.get(i);
		}

		this.values = new double[valuesList.size()];
		for (int i=0; i<valuesList.size(); i++) {
			this.values[i] = valuesList.get(i);
		}
	}

	public void writeFile(double[] updatedValues) {
		try {
			BufferedWriter output = new BufferedWriter(new FileWriter(this.svwpFile));

			// write header
			for (Map.Entry<String,String> entry : this.svwpFileHeader.entrySet()) {
				try {
					output.write(entry.getKey() + " = " + entry.getValue());
					output.newLine();
				} catch (IOException e) {
					throw new RuntimeException("Cannot write line in file: " + this.svwpFile + "   (" + this.getClass().getName() + ")");
				}
			}

			for (int timeIndex = 0; timeIndex < this.originalTimeStrings.size(); timeIndex++) {
				output.write(this.originalTimeStrings.get(timeIndex));
				output.newLine();
				for (int m = 0; m < n_rows; m++) {
					String line = "";
					String formatString = "%g ";
					for (int n = 0; n < n_cols; n++) {
						line += String.format(Locale.US, formatString, updatedValues[timeIndex * n_rows * n_cols + m * n_cols + n]);
					}
					output.write(line.trim());
					output.newLine();
				}
			}
			output.close();
		} catch (IOException e) {
			throw new RuntimeException("Cannot write file " + this.svwpFile + "   (" + this.getClass().getName() + ")");
		}
	}
}
