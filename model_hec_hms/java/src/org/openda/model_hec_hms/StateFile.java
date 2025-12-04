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
package org.openda.model_hec_hms;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.util.*;

public class StateFile implements IDataObject {
	private static final String[] EMPTY_STRING_ARRAY = new String[0];
	private File workingDirectory;
	private String filename = null;

	private final Map<String, StateFileExchangeItem> items = new HashMap<>();
	private final Map<String, GridBounds> gridToBounds = new HashMap<>();
	private final Map<String, Double[]> gridToValues = new HashMap<>();
	private final Map<String, Map<GridCell, Double>> gridToCellToValues = new HashMap<>();

	private final Map<Integer, String> lines = new LinkedHashMap<>();
	private final Map<Integer, GridIdToGridCell> valuesToUpdate = new HashMap<>();

	@Override
	public void initialize(File workingDirectory, String[] arguments) {
		this.workingDirectory = workingDirectory;

		if (null == arguments || 1 != arguments.length) {
			throw new RuntimeException("Expected filename as only argument.");
		}

		filename = arguments[0];

		File inputFile = new File(workingDirectory, filename);

		try {
			if (!inputFile.exists()) {
				throw new RuntimeException("File: " + inputFile.getCanonicalFile() + " does not exist.");
			}

			if (!inputFile.isFile()) {
				throw new RuntimeException("File: " + inputFile.getCanonicalFile() + " is not a file.");
			}
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}

		try (FileInputStream fileInputStream = new FileInputStream(inputFile);
			 InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream);
			 BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
			String line = bufferedReader.readLine();
			int lineNumber = 0;

			String subBasin = null;
			int x = 0;
			int y = 0;
			String loss = null;
			int layerNumber = 0;

			while (null != line) {
				lines.put(lineNumber, line);

				if (line.contains(":")) {
					String[] labelAndValue = line.split(":");
					String label = labelAndValue[0].trim();

					switch (label) {
						case "Subbasin":
							subBasin = labelAndValue[1].trim();
							break;
						case "End":
							subBasin = null;
							break;
						case "GridCell":
							String gridCellString = labelAndValue[1].trim();
							String[] gridCellIndices = gridCellString.split(",");
							x = Integer.parseInt(gridCellIndices[0]);
							y = Integer.parseInt(gridCellIndices[1]);
							break;
						case "End Grid Cell":
							break;
						case "Loss":
							loss = labelAndValue[1].trim();
							break;
						case "End Loss":
							loss = null;
							break;
						case "Number Groundwater Layers":
							layerNumber = 0;
							break;
						case "Groundwater Storage":
							double value = Double.parseDouble(labelAndValue[1].trim());
							String gridId = subBasin + "_" + loss + "_" + layerNumber;
							layerNumber++;

							GridBounds gridBounds = gridToBounds.computeIfAbsent(gridId, bounds -> new GridBounds());
							gridBounds.addCell(x, y);

							Map<GridCell, Double> cellToValue = gridToCellToValues.computeIfAbsent(gridId, m -> new HashMap<>());
							GridCell gridCell = new GridCell(x, y);
							cellToValue.put(gridCell, value);

							GridIdToGridCell gridIdToGridCell = valuesToUpdate.put(lineNumber, new GridIdToGridCell(gridId, gridCell));
							break;
						default:
							// Do nothing with other labels
					}
				}

				line = bufferedReader.readLine();
				lineNumber++;
			}
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}

		for (Map.Entry<String, GridBounds> gridToBound : gridToBounds.entrySet()) {
			int valueArraySize = gridToBound.getValue().getSize();
			gridToValues.put(gridToBound.getKey(), new Double[valueArraySize]);
		}

		for (Map.Entry<String, Map<GridCell, Double>> gridToCellToValue : gridToCellToValues.entrySet()) {
			for (Map.Entry<GridCell, Double> cellToValue : gridToCellToValue.getValue().entrySet()) {
				Double[] values = gridToValues.get(gridToCellToValue.getKey());
				GridBounds gridBounds = gridToBounds.get(gridToCellToValue.getKey());
				int valueArrayIndex = gridBounds.getIndex(cellToValue.getKey());
				values[valueArrayIndex] = cellToValue.getValue();
			}
		}

		for (Map.Entry<String, Double[]> gridToValue : gridToValues.entrySet()) {
			String gridId = gridToValue.getKey();
			GridBounds gridBounds = gridToBounds.get(gridId);
			int x = gridBounds.getX();
			int y = gridBounds.getY();
			int width = gridBounds.getWidth();
			int height = gridBounds.getHeight();
			double[] values = toPrimitiveDoubleArray(gridToValue.getValue());
			StateFileExchangeItem stateFileExchangeItem = new StateFileExchangeItem(gridId, x, y, width, height, values);
			items.put(gridId, stateFileExchangeItem);
		}
	}

	@Override
	public void finish() {
		File outputFile = new File(workingDirectory, filename);

		try (FileOutputStream fileOutputStream = new FileOutputStream(outputFile);
			 OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream);
			 BufferedWriter bufferedWriter = new BufferedWriter(outputStreamWriter)) {
			int lineNumber = 0;
			for (Map.Entry<Integer, String> line : lines.entrySet()) {
				if (valuesToUpdate.containsKey(lineNumber)) {
					String label = line.getValue().split(":")[0];
					bufferedWriter.write(label);
					GridIdToGridCell gridIdToGridCell = valuesToUpdate.get(lineNumber);
					StateFileExchangeItem exchangeItem = items.get(gridIdToGridCell.getGridId());
					GridBounds gridBounds = gridToBounds.get(gridIdToGridCell.getGridId());
					int cellIndex = gridBounds.getIndex(gridIdToGridCell.getGridCell());
					double[] values = exchangeItem.getValuesAsDoubles();
					bufferedWriter.write(": " + values[cellIndex]);
				} else {
					bufferedWriter.write(line.getValue());
				}
				bufferedWriter.newLine();

				lineNumber++;
			}
		} catch (IOException ioException) {
			throw new RuntimeException(ioException);
		}
	}

	@Override
	public String[] getExchangeItemIDs() {
		return items.keySet().toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return items.keySet().toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return items.get(exchangeItemID);
	}

	private double[] toPrimitiveDoubleArray(Double[] values) {
		double[] primitiveArray = new double[values.length];

		for (int valueIndex = 0; valueIndex < values.length; valueIndex++) {
			if (null == values[valueIndex]) {
				primitiveArray[valueIndex] = Double.NaN;
			} else {
				primitiveArray[valueIndex] = values[valueIndex];
			}
		}

		return primitiveArray;
	}
}
