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
package org.openda.exchange.dataobjects;
import java.io.File;

/**
 * Netcdf Data object for reading time series from e.g. Sobek3 time series file
 */
public class NetcdfDataScalarTimeSeriesDataObject extends NetcdfDataObject {

	public NetcdfDataScalarTimeSeriesDataObject() {
	}

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 2 && arguments.length != 3) {
			throw new RuntimeException("NetcdfDataScalarTimeSeriesDataObject.initialize expects workingDir and arguments: " +
					"ncfile timeseriesIdVarName [timeseriesSizeVarname]");
		}
		String ncFileName = arguments[0];
		this.stationIdVarName = arguments[1];
		this.stationDimensionVarName = arguments.length == 3 ? arguments[2] : "id";
		super.initialize(workingDir, new String[] {ncFileName, "true"} );
	}
}
