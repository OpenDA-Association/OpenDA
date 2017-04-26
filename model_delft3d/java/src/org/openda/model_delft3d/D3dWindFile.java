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

package org.openda.model_delft3d;

import org.apache.commons.lang3.StringUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

/**
 * Delft3D 2d-field wind files reader/writer.
 *
 * Supported capabilities:
 *    Space varying wind on a separate curvilinear grid. (*.amu + *.amv + *.grd)
 */
public class D3dWindFile implements IDataObject {

	private static List<String> supportedFieldTypes = Arrays.asList(ModelDefinitionFile.WINDGU, ModelDefinitionFile.WINDGV, ModelDefinitionFile.WINDWU, ModelDefinitionFile.WINDWV);
	private ModelDefinitionFile modelDefinitionFile = null;
	private String fieldType = null;
	private String gridFileName = null;
	private File gridFile = null;
	private int mGrid;
	private int nGrid;
	private int endOfHeader;
	private D3dWindExchangeItem windExchangeItem = null;

	public void initialize(File workingDirectory, String[] arguments) {

		if (arguments.length != 2) {
			// Change to String.join upon migrating to Java 1.8.
			throw new RuntimeException("Please specify a mdf-filename and a wind field type as arguments. Supported types are: " + StringUtils.join(supportedFieldTypes, ", "));
		}

		String mdfFileName = arguments[0];

		fieldType = arguments[1];
		if (!(supportedFieldTypes.contains(fieldType))) {
			// Change to String.join upon migrating to Java 1.8.
			throw new RuntimeException("Unrecognised wind field type specified as argument, choose from: " + StringUtils.join(supportedFieldTypes, ", "));
		}

		modelDefinitionFile = ModelDefinitionFile.getModelDefinitionFile(workingDirectory, mdfFileName);

		File windFile = modelDefinitionFile.getFieldFile(fieldType, true);

		try {
			FileReader fileReader = new FileReader(windFile);
			BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

			if (fieldType.equals(ModelDefinitionFile.WINDGU)) {
				windExchangeItem = readExchangeItem2D(inputFileBufferedReader, "windgu");
			} else if (fieldType.equals(ModelDefinitionFile.WINDGV)) {
				windExchangeItem = readExchangeItem2D(inputFileBufferedReader, "windgv");
			}else if (fieldType.equals(ModelDefinitionFile.WINDWU)) {
				windExchangeItem = readExchangeItem2D(inputFileBufferedReader, "windu");
			}else if (fieldType.equals(ModelDefinitionFile.WINDWV)) {
				windExchangeItem = readExchangeItem2D(inputFileBufferedReader, "windv");
			}
			inputFileBufferedReader.close();
			fileReader.close();

		} catch (IOException e) {
			throw new RuntimeException("Could not read from " + windFile.getAbsolutePath() + "\n" + e.getMessage());
		}
	}

	public String[] getExchangeItemIDs() {
		return new String[] {windExchangeItem.getId()};
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		if (windExchangeItem.getRole().equals(role)) {
			return new String[] {windExchangeItem.getId()};
		}
		return new String[0];
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (exchangeItemID.equals(windExchangeItem.getId())) {
			return (IExchangeItem)windExchangeItem;
		}
		return null;
	}

	public void finish() {
		if (windExchangeItem.getDataChanged()) {
			try {
				FileWriter fileWriter = new FileWriter(this.modelDefinitionFile.getFieldFile(fieldType, true));
				BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
				if (fieldType.equals(ModelDefinitionFile.WINDGU)) {
					writeExchangeItem2D(outputFileBufferedWriter, windExchangeItem);
				} else if (fieldType.equals(ModelDefinitionFile.WINDGV)) {
					writeExchangeItem2D(outputFileBufferedWriter, windExchangeItem);
				}else if (fieldType.equals(ModelDefinitionFile.WINDWU)) {
					writeExchangeItem2D(outputFileBufferedWriter, windExchangeItem);
				}else if (fieldType.equals(ModelDefinitionFile.WINDWV)) {
					writeExchangeItem2D(outputFileBufferedWriter, windExchangeItem);
				}
				outputFileBufferedWriter.close();
				fileWriter.close();
			} catch (IOException e) {
				throw new RuntimeException("Error writing file " + modelDefinitionFile.getFieldFile(fieldType, false));
			}
		}
	}

	private D3dWindExchangeItem readExchangeItem2D(BufferedReader inputFileBufferedReader, String exchangeItemId) throws IOException {
        ArrayList<String> content = new ArrayList();
		endOfHeader = 0;

		// First: read the header. We need to know the grid type.
		boolean isCurvilinear = false;
		boolean isEquidistant = false;
		boolean gridTypeFound = false;

		String line = inputFileBufferedReader.readLine();
        content.add(line);
		while (line != null && !gridTypeFound) {
			String[] fields = line.split("=");
			if (fields.length == 2) {
				if (fields[0].trim().equalsIgnoreCase("filetype")) {
					if (fields[1].contains("meteo_on_curvilinear_grid")) {
						isCurvilinear = true;
						gridTypeFound = true;
					}else if (fields[1].contains("meteo_on_equidistant_grid")){
						isEquidistant = true;
						gridTypeFound = true;
					}
				}
			}
			line = inputFileBufferedReader.readLine();
			content.add(line);
		}

		if (!gridTypeFound) {
			throw new RuntimeException("Could not determine grid type (curvilinear or equidistant).");
		}

		D3dGrid2D grid2D;
		grid2D = new D3dGrid2D();

		if (isCurvilinear) {

			// Getting grid filename from header
			while (line != null && gridFileName == null) {
				String[] fields = line.split("[\t ]+");
				if (fields[0].equalsIgnoreCase("grid_file")) {
					gridFileName = fields[2].trim();
					gridFile = new File(modelDefinitionFile.getMdFile().getParentFile(), gridFileName);
				}
				line = inputFileBufferedReader.readLine();
				content.add(line);
			}
			// intermezzo: open the grid file to read the dimensions
			FileReader fileReader = new FileReader(gridFile);
			BufferedReader gridFileBufferedReader = new BufferedReader(fileReader);
			String gridFileLine = gridFileBufferedReader.readLine();
			ArrayList < Double > fieldsAsDouble = new ArrayList < Double >();
			double[][] fullCoordinateArray;
			while (gridFileLine != null) {
				String[] fields = gridFileLine.split("= *");
				if (fields[0].contains("Coordinate")) {
					String sizeLine = gridFileBufferedReader.readLine();
					String[] mnValues = sizeLine.trim().split("[\t ]+");
					if (mnValues.length != 2) {
						throw new RuntimeException("Invalid MN line\n\t" + sizeLine + "\nin gridfile" + gridFile.getAbsolutePath());
					}
					mGrid = Integer.parseInt(mnValues[0]);
					nGrid = Integer.parseInt(mnValues[1]);
					grid2D.setMmax(mGrid);
					grid2D.setNmax(nGrid);
				}else if (fields[0].contains("ETA")){
					String[] fieldsString =  fields[1].split(" +");
					for (int i=0; i<(fieldsString.length-1); i++){
						fieldsAsDouble.add(Double.parseDouble(fieldsString[1+i]));
					}
					// This is in case the grid line is split into several lines in the grid file
					int counter = fieldsString.length-1;
					while (counter != mGrid){
						gridFileLine = gridFileBufferedReader.readLine();
						fieldsString =  gridFileLine.split(" +");
						for (int i=1; i<(fieldsString.length); i++){
							fieldsAsDouble.add(Double.parseDouble(fieldsString[i]));
						}
						counter += (fieldsString.length-1);
					}
				}
				gridFileLine = gridFileBufferedReader.readLine();
			}
			double[][] coordinateXArray = new double[nGrid][mGrid];
			double[][] coordinateYArray = new double[nGrid][mGrid];

			for (int i=0; i<nGrid; i++){
				for (int j=0; j<mGrid; j++){
					coordinateXArray[i][j] = fieldsAsDouble.get(i*mGrid + j);
					coordinateYArray[(nGrid-1)-i][j] = fieldsAsDouble.get(i*mGrid + j + mGrid*nGrid);
				}
			}
			grid2D.setCoordinateArrays(coordinateXArray,coordinateYArray);
		}

		// Retrieving information in the header (now only grid dimensions)
		else if (isEquidistant){
			grid2D = new D3dGrid2D();
			double dx = Double.NaN;
			double dy = Double.NaN;
			double xll = Double.NaN;
			double yll = Double.NaN;
			while (line != null && !line.toLowerCase().contains("TIME".toLowerCase())) {
				String[] fields = line.split("= *");
				if (fields[0].contains("n_rows")) {
					nGrid = Integer.parseInt(fields[1]);
					grid2D.setNmax(nGrid);
				}else if (fields[0].contains("n_cols")){
					mGrid = Integer.parseInt(fields[1]);
					grid2D.setMmax(mGrid);
				}else if (fields[0].contains("x_llcenter")){
					xll = Double.parseDouble(fields[1]);
				}else if (fields[0].contains("y_llcenter")){
					yll = Double.parseDouble(fields[1]);
				}else if (fields[0].contains("dx")){
					dx = Double.parseDouble(fields[1]);
				}else if (fields[0].contains("dy")){
					dy = Double.parseDouble(fields[1]);
				}
				line = inputFileBufferedReader.readLine();
				content.add(line);
			}
			if (Double.isNaN(dx) | Double.isNaN(dy) | Double.isNaN(xll) | Double.isNaN(yll)) {
				throw new RuntimeException("Could not retrieve necessary equidistant grid info from header.");
			}
			double[][] coordinateXArray = new double[nGrid][mGrid];
			double[][] coordinateYArray = new double[nGrid][mGrid];
			for (int i=0; i<nGrid; i++){
				for (int j=0; j<mGrid; j++){
					coordinateXArray[i][j] = xll+j*dx;
					coordinateYArray[(nGrid-1)-i][j] = yll+i*dy;
				}
			}
			grid2D.setCoordinateArrays(coordinateXArray,coordinateYArray);
		}

		// continue with wind file, until first/next timestep
		List<D3dValuesOnGrid2D> Fvalues = new ArrayList<D3dValuesOnGrid2D>();
		List<Double> timeOffsets = new ArrayList<Double>();
		String lineTime = null;

		while (line != null) {
			String[] fields = line.split("= *");
			if (fields[0].contains("TIME")) {

				String[] tempTime = fields[1].split(" +");
				double timeOffset = Double.parseDouble(tempTime[0]);
				timeOffsets.add(timeOffset);
				lineTime = fields[1].replace(tempTime[0],""); // One of those lines needs to be stored to get reference date

				if (endOfHeader == 0) {endOfHeader = content.size()-1;}
				else {
			    	content.add(line);
				}
				double[] timeValues = new double[nGrid * mGrid];
				int index = 0;
				line = inputFileBufferedReader.readLine();
				while (line != null && index < nGrid * mGrid) {
					fields = line.trim().split("[\t ]+");
					for (String field : fields) {
						if (field.length() > 0) {
							timeValues[index++] = Double.parseDouble(field);
						}
					}
					if (index < nGrid * mGrid) {
						line = inputFileBufferedReader.readLine();
					}
				}

				D3dValuesOnGrid2D valuesOnGrid2D = new D3dValuesOnGrid2D(grid2D, timeValues);
				Fvalues.add(valuesOnGrid2D);
			}
			line = inputFileBufferedReader.readLine();
			if (endOfHeader == 0) {content.add(line);}
		}

		double[] timesAsMJD = TimeUtils.ConvertBcTimesToModifiedJulianDays(lineTime,timeOffsets);
//		List<Double> timesAsMJD = TimeUtils.ConvertBcTimesToModifiedJulianDays(line,timeOffsets);
//		double[] times = new double[timesAsMJD.size()];
//		for (int i=0;i<timesAsMJD.size();i++){
//			times[i] = timesAsMJD.get(i);
//		}
//		return new D3dWindExchangeItem(exchangeItemId, Fvalues,content, endOfHeader, times);
		return new D3dWindExchangeItem(exchangeItemId, Fvalues,content, endOfHeader, timesAsMJD);
	}

	private void writeExchangeItem2D(BufferedWriter outputFileBufferedWriter, D3dWindExchangeItem EI) throws IOException {

		Locale locale = new Locale("EN");
		String floatValueFormat = "%10.3f";

		// first write the header:
		int eoh = EI.getEndOfHeader();
		List<String> content = EI.getTextContent();
		for (int i=0; i < eoh; i++){
			outputFileBufferedWriter.write(content.get(i));
			outputFileBufferedWriter.newLine();
		}
		int times = EI.getDims()[0];
		double[] values = EI.getValuesAsDoubles();
		// now write for each time level the timestring and the values.

		int index = 0;
        for (int timeIndex=0; timeIndex < times; timeIndex ++){
			outputFileBufferedWriter.write(content.get(eoh+timeIndex));
			outputFileBufferedWriter.newLine();
			for (int n = 0; n < EI.getDims()[2]; n++) {
				for (int m = 0; m < EI.getDims()[1]; m++) {
					outputFileBufferedWriter.write(String.format(locale, floatValueFormat, values[index++]));
				}
				outputFileBufferedWriter.newLine();
			}
		}
	}
}
