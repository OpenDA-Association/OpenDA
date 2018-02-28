/* OpenDA v2.4.3 
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
package org.openda.model_delft3d;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;

/**
 * Netcdf Data object for reading time series from Delft3D trih-* files
 */
public class D3dNetcdfHisDataObject extends NetcdfDataObject {

	public D3dNetcdfHisDataObject() {
		stationIdVarName = "NAMST";
		stationDimensionVarName = "NOSTAT";
	}

	public void initialize(File workingDir, String[] arguments) {
		super.initialize(workingDir, arguments);
		// For now, remove the variable that are not scalar (but a vector along the vertical axis)
		// TODO: split in EI per layer?
		// Also remove MNSTAT (time dependent monitoring station location)
		String[] nonScalarVariables = new String[] { "ZCURU", "ZCURV","ZQXK", "ZQYK","MNSTAT"};
		for (String nonScalarVariable : nonScalarVariables) {
			for (IExchangeItem exchangeItem: exchangeItems) {
				if (exchangeItem.getId().equalsIgnoreCase(nonScalarVariable)) {
					exchangeItems.remove(exchangeItem);
					break;
				}
			}
		}
	}
}
