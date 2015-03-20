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

package org.openda.model_swan;

import java.io.File;

/**
 * Swan parameters that can be calibrated, but are not available in the Swivt XML file
 * Note: This is not used yet, it is a remainder of the DATools Swan Uncertainty Analysis
 *       implementation.
 *       If one needs to calibrate other parameters then available in the swivt xml file,
 *       the keywords for those parameters need to be added in a 'swivt-like way' to the
 *       swan input file template. The present SwanNonSwivtParameters can be used to
 *       store the values to be used before they are actually written to the swan input file.
 */
public class SwanNonSwivtParameters {

    private String[] ids = new String[0];

    public SwanNonSwivtParameters(File nonSwivtParametersFile) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanNonSwivtParameters.SwanNonSwivtParameters(): Not implemented yet.");
    }

	public String[] getIds() {
		return ids;
	}

    public double getValue(String parameterId) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanNonSwivtParameters.getValue(): Not implemented yet.");
    }

	public void setValue(String parameterId, double value) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanNonSwivtParameters.setValue(): Not implemented yet.");
	}
}
