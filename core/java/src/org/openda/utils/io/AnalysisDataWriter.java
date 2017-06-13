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

package org.openda.utils.io;

import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.geometry.GeometryUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Class for writing data from model state exchange items before and after analysis (state update) to netcdf files.
 * This can be easily used e.g. in any ModelInstance implementation.
 *
 * @author Arno Kockx
 */
//TODO move AnalysisOutputXML to core xsd
public class AnalysisDataWriter {
	private final ScalarExchangeItemNetcdfWriter beforeAnalysisScalarWriter;
	private final GridExchangeItemNetcdfWriter beforeAnalysisGridWriter;

	private final ScalarExchangeItemNetcdfWriter afterAnalysisScalarWriter;
	private final GridExchangeItemNetcdfWriter afterAnalysisGridWriter;

	/**
	 * @param exchangeItems with data to write.
	 * @param outputFolder in which output files will be written.
	 */
	public AnalysisDataWriter(Collection<IExchangeItem> exchangeItems, File outputFolder) {
		if (exchangeItems == null) throw new IllegalArgumentException("exchangeItems == null");
		if (exchangeItems.isEmpty()) throw new IllegalArgumentException("exchangeItems.isEmpty()");

		List<IExchangeItem> scalarItems = new ArrayList<>();
		List<IExchangeItem> gridItems = new ArrayList<>();
		for (IExchangeItem item : exchangeItems) {
			if (GeometryUtils.isScalar(item.getGeometryInfo())) {//if scalar.
				scalarItems.add(item);
			} else {//if grid.
				gridItems.add(item);
			}
		}

		if (scalarItems.isEmpty()) {
			beforeAnalysisScalarWriter = null;
			afterAnalysisScalarWriter = null;
		} else {
			beforeAnalysisScalarWriter = createScalarWriter(scalarItems, outputFolder, "model_scalar_data_before_analysis.nc");
			afterAnalysisScalarWriter = createScalarWriter(scalarItems, outputFolder, "model_scalar_data_after_analysis.nc");
		}

		if (gridItems.isEmpty()) {
			beforeAnalysisGridWriter = null;
			afterAnalysisGridWriter = null;
		} else {
			beforeAnalysisGridWriter = createGridWriter(gridItems, outputFolder, "model_grid_data_before_analysis.nc");
			afterAnalysisGridWriter = createGridWriter(gridItems, outputFolder, "model_grid_data_after_analysis.nc");
		}
	}

	private ScalarExchangeItemNetcdfWriter createScalarWriter(List<IExchangeItem> scalarItems, File outputFolder, String outputFileName) {
		File outputFile = new File(outputFolder, outputFileName);
		return new ScalarExchangeItemNetcdfWriter(scalarItems.toArray(new IExchangeItem[scalarItems.size()]), outputFile,
				NetcdfUtils.STATION_ID_VARIABLE_NAME, NetcdfUtils.STATION_DIMENSION_VARIABLE_NAME);
	}

	private GridExchangeItemNetcdfWriter createGridWriter(List<IExchangeItem> gridItems, File outputFolder, String outputFileName) {
		File outputFile = new File(outputFolder, outputFileName);
		return new GridExchangeItemNetcdfWriter(gridItems.toArray(new IExchangeItem[gridItems.size()]), outputFile);
	}

	public void writeDataBeforeAnalysis() {
		if (beforeAnalysisScalarWriter != null) beforeAnalysisScalarWriter.writeDataForCurrentTimeStep();
		if (beforeAnalysisGridWriter != null) beforeAnalysisGridWriter.writeDataForCurrentTimeStep();
	}

	public void writeDataAfterAnalysis() {
		if (afterAnalysisScalarWriter != null) afterAnalysisScalarWriter.writeDataForCurrentTimeStep();
		if (afterAnalysisGridWriter != null) afterAnalysisGridWriter.writeDataForCurrentTimeStep();
	}

	public void close() {
		if (beforeAnalysisScalarWriter != null) beforeAnalysisScalarWriter.close();
		if (beforeAnalysisGridWriter != null) beforeAnalysisGridWriter.close();

		if (afterAnalysisScalarWriter != null) afterAnalysisScalarWriter.close();
		if (afterAnalysisGridWriter != null) afterAnalysisGridWriter.close();
	}
}
