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

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Delft3D 2d-field files reader/writer
 */
public class D3dWindFile implements IoObjectInterface {

	private static String[] knownFileTypes = {ModelDefinitionFile.WINDGU, ModelDefinitionFile.WINDGV}; //TODO: windu, windv
	private ModelDefinitionFile modelDefinitionFile = null;
	private D3dWindExchangeItem[] exchangeItems = null;
	private String fileKey = null;
	private String gridFileName = null;
	private File gridFile = null;
	private int lineLength = 12;
	private int mGrid;
	private int nGrid;
	private int endOfHeader;

	public void initialize(File workingDir, String mdFileName, String[] arguments) {

		if (arguments.length != 1) {
			throw new RuntimeException("Expected one argument, wind field type: " +
					ModelDefinitionFile.getKnownFileTypesString(knownFileTypes));
		}

		fileKey = arguments[0];
		ModelDefinitionFile.checkD3dFileArguments(fileKey, knownFileTypes);
		modelDefinitionFile = ModelDefinitionFile.getModelDefinitionFile(workingDir, mdFileName);
		File fieldFile = modelDefinitionFile.getFieldFile(fileKey, true);

		try {
			FileReader fileReader = new FileReader(fieldFile);
			BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

			if (fileKey.equals(ModelDefinitionFile.WINDGU)) {
				exchangeItems = new D3dWindExchangeItem[1];
				exchangeItems[0] = readExchangeItem2D(inputFileBufferedReader, "windgu");
			} else if (fileKey.equals(ModelDefinitionFile.WINDGV)) {
				exchangeItems = new D3dWindExchangeItem[1];
				exchangeItems[0] = readExchangeItem2D(inputFileBufferedReader, "windgv");
			}
			inputFileBufferedReader.close();
			fileReader.close();

		} catch (IOException e) {
			throw new RuntimeException("Could not read from " + fieldFile.getAbsolutePath());
		}
	}

	public IPrevExchangeItem[] getExchangeItems() {
		return exchangeItems;
	}

	public void finish() {

		boolean dataChanged = false;
		for (int i = 0; !dataChanged && i < exchangeItems.length; i++) {
			dataChanged = exchangeItems[i].getDataChanged();
		}

		if (dataChanged) {
			try {
				FileWriter fileWriter = new FileWriter(this.modelDefinitionFile.getFieldFile(fileKey, true));
				BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
				if (fileKey.equals(ModelDefinitionFile.WINDGU)) {
					writeExchangeItem2D(outputFileBufferedWriter, exchangeItems[0]);
				} else if (fileKey.equals(ModelDefinitionFile.WINDGV)) {
					writeExchangeItem2D(outputFileBufferedWriter, exchangeItems[0]);
				}
				outputFileBufferedWriter.close();
				fileWriter.close();
			} catch (IOException e) {
				throw new RuntimeException("Error writing file " + modelDefinitionFile.getFieldFile(fileKey, false));
			}
		}
		modelDefinitionFile = null;
		exchangeItems = null;
		fileKey = null;
	}

	private D3dWindExchangeItem readExchangeItem2D(BufferedReader inputFileBufferedReader, String exchangeItemId) throws IOException {
        ArrayList<String> content = new ArrayList();
		endOfHeader = 0;
		// First: read the header. We need to know the grid file to obtain the field size.
		String line = inputFileBufferedReader.readLine();
        content.add(line);
		while (line != null && gridFileName == null) {
			String[] fields = line.split("[\t ]+");
			if (fields[0].equalsIgnoreCase("grid_file")) {
				gridFileName = fields[2].trim();
				gridFile = new File(modelDefinitionFile.getMdFile().getParentFile(), gridFileName);
			}
			line = inputFileBufferedReader.readLine();
			content.add(line);

		}

		// intermezzo: open the grid file to read the dimensions!
		FileReader fileReader = new FileReader(gridFile);
		BufferedReader gridFileBufferedReader = new BufferedReader(fileReader);
		String gridFileLine = gridFileBufferedReader.readLine();
		boolean gridSizeFound = false;
		while (gridFileLine != null && !gridSizeFound) {
			String[] fields = gridFileLine.split("[\t ]+");
			if (fields[0].equalsIgnoreCase("Coordinate")) {
				String sizeLine = gridFileBufferedReader.readLine();
				String[] mnValues = sizeLine.trim().split("[\t ]+");
				if (mnValues.length != 2) {
					throw new RuntimeException("Invalid MN line\n\t" + sizeLine + "\nin gridfile" + gridFile.getAbsolutePath());
				}
				mGrid = Integer.parseInt(mnValues[0]);
				nGrid = Integer.parseInt(mnValues[1]);
				gridSizeFound = true;
			}
	        gridFileLine = gridFileBufferedReader.readLine();
		}

		// continue with wind file, until first/next timestep
		List<D3dField2D> Fvalues = new ArrayList<D3dField2D>();

		while (line != null) {
			String[] fields = line.split("[\t ]+");
			if (fields[0].equalsIgnoreCase("TIME")) {

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

				D3dField2D d3dField2D = new D3dField2D(mGrid, nGrid, timeValues);
				Fvalues.add(d3dField2D)   ;

			}
			line = inputFileBufferedReader.readLine();
			if (endOfHeader == 0) {content.add(line);}
		}

		return new D3dWindExchangeItem(exchangeItemId, Fvalues,content, endOfHeader);
	}

	private void writeExchangeItem2D(BufferedWriter outputFileBufferedWriter, D3dWindExchangeItem EI) throws IOException {

		Locale locale = new Locale("EN");
		String floatValueFormat = "%10.2e";

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
