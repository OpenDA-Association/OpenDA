package org.openda.exchange.dataobjects;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Array;
import org.openda.utils.generalJavaUtils.StringUtilities;
import org.openda.utils.io.CsvReader;

import java.io.*;
import java.text.ParseException;

/**
 * DataObject for an ESRI ASCII Grids
 */
public class EsriAsciiGridDataObject implements IDataObject{

	private int ncols=-1;
	private int nrows=-1;
	private double xllcorner=Double.NEGATIVE_INFINITY;
	private double yllcorner=Double.NEGATIVE_INFINITY;
	private double cellsize=-1;
	boolean isCenter=false;

	private CsvReader csvReader;
	private String noDataText = "-999999";
	private boolean useScientificNotation = false;
	private int maxNumberOfDecimals = 6;

	String exchangeItemId = "AsciiGrid-item";
	String timeStampString = null;
	String timeStampFormat = null;
	private String outputFileName = null;
	private ArrayExchangeItem exchangeItem = null;
	private File inputFile;
	private File outputFile;

	public void initialize(File workingDir, String[] arguments) {

		// check argument

		String usageMessage = "EsriAsciiGridDataObject usage: inputFile [outputFile=...] [timeStamp=... timeStampFormat=...]," +
			" Files relative to working dir. If outputFile is ommitted, writing will be done to inputFile." +
			" If timeStampFormat is not specified, timeStamp contains the time stamp for which the " +
			" exchange item in the data object is valid." +
			" If timeStampFormat is specified, a file <inputFile>_<timeStamp>.asc is selected" +
			" (and <outputFile>_<timeStamp>.asc for output), with timestamp specified in" +
			" yyyyMMddHHmmss or yyyyMMdd format.";

		if (arguments == null || arguments.length == 0 || arguments.length > 4) {
			throw new RuntimeException(usageMessage);
		}

		for (int i = 1; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case "outputFile":
					outputFileName = value;
					continue;
				case "timeStampFormat":
					timeStampFormat = value;
					continue;
				case "timeStamp":
					timeStampString = checkValidTimeString(value, null);
					continue;
				default:
					throw new RuntimeException(usageMessage);
			}
		}

		String inputFileName = arguments[0];
		if (timeStampFormat == null) {
			inputFile = new File(workingDir, inputFileName);
			if (outputFileName == null) {
				outputFile = inputFile;
			} else {
				outputFile = new File(workingDir, outputFileName);
			}
		} else {
			inputFile = new File(workingDir, inputFileName + "_" + timeStampString + ".asc");
			if (outputFileName == null) {
				outputFile = inputFile;
			} else {
				outputFile = new File(workingDir, outputFileName + "_" + timeStampString + ".asc");
			}
		}
		if (!inputFile.exists()) throw new RuntimeException("Input file does not exist: " + inputFile);

		try {
			csvReader = new CsvReader(inputFile);
			csvReader.setColumnSeparatorChar(' ');
			csvReader.setDoTrimLine(true);

			// use file name's prefix as exchange_item name
			String fileName=arguments[0];
			String baseName = getBaseName(inputFile.getName());
			exchangeItemId = baseName;
			int underscorePos = baseName.indexOf('_');
			if (underscorePos > 1) {
				exchangeItemId = baseName.substring(0, underscorePos);
				if (timeStampString == null) {
					timeStampString = checkValidTimeString(baseName.substring(underscorePos + 1), inputFile);
				}
			}

			String[] rowItems = readHeader();
			readValues(rowItems);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("File not found: " + inputFile);
		} catch (IOException e) {
			throw new RuntimeException("Could not read header/values from " + inputFile + ", " + e.getMessage());
		} finally {
			try {
				csvReader.close();
			} catch (IOException e) {
				// no action
			}
		}
	}

	public String[] getExchangeItemIDs() {
		return new String[] {exchangeItemId};
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItemID.equals(exchangeItem.getId())) {
			throw new RuntimeException("Unknown exchange item ID \"" + exchangeItemID + "\", expected " + exchangeItem.getId());
		}
		return exchangeItem;
	}

	public void finish() {
		Writer writer = null;
		try {
			writer = new FileWriter(outputFile);
			writeHeader(writer);
			writeValues(writer);
		} catch (IOException e) {
			throw new RuntimeException("EsriAsciiGridDataObject: " + e.getMessage());
		} finally {
			if (writer!= null) {
				try {
					writer.close();
				} catch (IOException e) {
					// no action
				}
			}
		}
	}

	/**
	 * Reads header from ascii file
	 * @throws IOException
	 */
	public String[] readHeader() throws IOException {

		String[] rowItems;

		while ((rowItems = csvReader.readCSVLineTrimElements()) != null) {

			if (rowItems.length!=2) {
				// first value line, header processed
				break;
			}

			String key = rowItems[0].toLowerCase();
			String value = rowItems[1];

			if(value != null && !value.isEmpty()) {
				if (key.equals("ncols")){
					ncols=Integer.parseInt(value);
				} else if (key.equals("nrows")){
					nrows = Integer.parseInt(value);
				} else if (key.equals("xllcenter")){
					isCenter = true;
					xllcorner = Double.parseDouble(value);
				} else if (key.equals("xllcorner")){
					isCenter=false;
					xllcorner = Double.parseDouble(value);
				} else if (key.equals("yllcenter")){
					yllcorner = Double.parseDouble(value);
				} else if (key.equals("yllcorner")){
					yllcorner = Double.parseDouble(value);
				} else if (key.equals("cellsize")){
					cellsize = Double.parseDouble(value);
				} else if (key.equals("nodata_value")){
					noDataText = value.trim();
				} else {
					boolean nextLineStartsWithNumber = true;
					try {
						double aDouble = Double.parseDouble(key);
					} catch (NumberFormatException e) {
						nextLineStartsWithNumber = false;
					}
					if (nextLineStartsWithNumber)
						// first value line, header processed
						break;
				}
			}
		}

		if (ncols == -1 || nrows == -1 || xllcorner == Double.NEGATIVE_INFINITY || yllcorner == Double.NEGATIVE_INFINITY ||
			cellsize == -1 || noDataText == null)                       {

			String message = new String ("Missing header items in ASCII grid:");
			if (ncols==-1) message += " ncols";
			if (nrows==-1) message += " nrows";
			if (xllcorner==Double.NEGATIVE_INFINITY) message += " xllcorner/xllcenter";
			if (yllcorner == Double.NEGATIVE_INFINITY) message += " yllcorner/yllcenter";
			if (cellsize == -1) message += " cellsize";
			if (noDataText == null) message += " nodata_value";
			throw new IOException(message);

		}
		return rowItems;

	}

	/**
	 * Read cell values of the ascii grid
	 * @throws IOException
	 */
	private void readValues(String[] rowItems) throws IOException {

		double[] values = new double[ncols * nrows];

		int cellindex=0;
		while (rowItems != null) {

			// start: if the first column is 'space'
			// (JHS)
			int iStart = 0;
			if (rowItems[0].isEmpty()){
				iStart = 1;
			}
			// end: if the first column is 'space'

			for (int i = 0+iStart; i < rowItems.length; i++) {

				if (cellindex>=values.length)
					throw new IOException("Grid size does not match size of grid geometry:" + values.length);

				String rowItem = rowItems[i].toLowerCase();
				double value = Double.parseDouble(rowItem);
				if (rowItems[0].equals(noDataText)) {
					values[cellindex] = Double.NaN;
				} else {
					values[cellindex] = value;
					if (rowItem.contains("e")) {
						useScientificNotation = true;
					} else {
						int numberOfDecimals = rowItem.lastIndexOf('.');
						if (numberOfDecimals > maxNumberOfDecimals) {
							maxNumberOfDecimals = numberOfDecimals;
						}
					}
				}

				cellindex++;

			}
			rowItems = csvReader.readCSVLineTrimElements();
		}
		IArray array = new Array(values, new int[]{nrows, ncols}, false);

		// x-coordinates
		IQuantityInfo xCoordsQuantityInfo = new QuantityInfo("x", "m");
		int[] xCoordValueIndices = new int[] {1};
		double[] xCoords = new double[ncols];
		for (int n = 0; n < ncols; n++) {
			xCoords[n] = xllcorner + n*cellsize;
		}
		IArray xCoordArray = new Array(xCoords);

		// y-coordinates (highest y is on the last line of the file)
		IQuantityInfo yCoordsQuantityInfo = new QuantityInfo("y", "m");
		int[] yCoordValueIndices = new int[] {0}; // rows are major
		double[] yCoords = new double[nrows];
		for (int n = 0; n < nrows; n++) {
			yCoords[n] = yllcorner + n;
		}
		IArray yCoordArray = new Array(yCoords);

		ArrayGeometryInfo geometryInfo = new ArrayGeometryInfo(yCoordArray, yCoordValueIndices,
			yCoordsQuantityInfo, xCoordArray, xCoordValueIndices, xCoordsQuantityInfo,null,null,null);

		exchangeItem = new ArrayExchangeItem(exchangeItemId, IPrevExchangeItem.Role.InOut);
		exchangeItem.setGeometryInfo(geometryInfo);
		exchangeItem.setArray(array);

		if (timeStampString != null) {
			double timeAsMJD;
			try {
				timeAsMJD = TimeUtils.date2Mjd(timeStampString, "yyyyMMddHHmmss");
			} catch (ParseException e) {
				throw new RuntimeException("Unrecognized date/time string");
			}
			exchangeItem.setTimeInfo(new TimeInfo(new double[] {timeAsMJD}));
		}
	}

	private void writeHeader(Writer writer) throws IOException {

		writer.write("ncols         " + ncols + "\n");
		writer.write("nrows         " + nrows + "\n");

		String llText = isCenter ? "center" : "corner";
		writer.write("xll" + llText + "     " + xllcorner + "\n");
		writer.write("yll" + llText + "     " + yllcorner + "\n");

		writer.write("cellsize      " + cellsize + "\n");
		writer.write("NODATA_value  " + noDataText  + "\n");
	}

	private void writeValues(Writer writer) throws IOException {

		char[] lineChars = new char[15 * ncols];
		int noDataTextLength = noDataText.length();

		double[] values = exchangeItem.getArray().getValuesAsDoubles();

		for (int i = 0; i < nrows; i++) {
			int k = 0;
			for (int j = 0; j < ncols; j++) {
				if (j > 0) lineChars[k++] = ' ';

				double value = values[i * ncols + j];
				if (Double.isNaN(value)) {
					byte[] noDataTextBytes = noDataText.getBytes();
					System.arraycopy(noDataTextBytes, 0,lineChars, k, noDataTextLength);
					k += noDataTextLength;
					continue;
				}

				if (value == 0f) {
					lineChars[k++] = '0';
					continue;
				}

				String valueText;
				int valueTextLength;
				if (useScientificNotation) {
					valueText = String.format("%f", (float)value);
					valueTextLength = valueText.length();
				} else {
					valueText = Float.toString((float)value);
					valueTextLength = valueText.length();
					int numberOfDecimals = valueText.lastIndexOf('.');
					if (numberOfDecimals > maxNumberOfDecimals) {
						valueText = valueText.substring(0, valueTextLength + maxNumberOfDecimals - numberOfDecimals);
					}
				}
				System.arraycopy(valueText.toCharArray(), 0,lineChars, k, valueTextLength);
				k += valueTextLength;
			}
			writer.write(lineChars, 0, k);
			writer.write('\n');
		}
		writer.close();
	}

	private String getBaseName(String fileName) {

		String baseName = fileName;
		int dotPos = fileName.lastIndexOf('.');
		if (dotPos > 1) {
			baseName = fileName.substring(0, dotPos);
		}
		return baseName;
	}

	private String checkValidTimeString(String timeString, File file) {
		if (timeString.length() == 14) {
			return timeString;
		} else if (timeString.length() == 8){
			return timeString + "000000";
		} else {
			throw new RuntimeException("Invalid time string" + timeString + " in " + (file!=null ? " file " + file : " initialization arugment"));
		}
	}
}
