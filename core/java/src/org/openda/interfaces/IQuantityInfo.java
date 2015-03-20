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
package org.openda.interfaces;

/**
 * Interface to retrieve quantities and units for the values.
 * It is recommended to use standardized names and SI units.
 */
public interface IQuantityInfo {

	/**
	 * Quantity of the values. Where possible, it is recommended to used standardized names,
	 * such as the netcdf CF standard_names:
     * <p>
     * <a href="http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/18/cf-standard-name-table.html">http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/18/cf-standard-name-table.html</a>
     * </p>
	 * Use <code>null</code> for an unknown value.
	 * @return Name of quantity as a String
	 */
	public String getQuantity();

	/**
	 * Units of the values, preferably in SI units. More specifically we recommend the notation
	 * as recognized by udunits:
     * <p>
     * <a href="http://www.unidata.ucar.edu/packages/udunits">http://www.unidata.ucar.edu/packages/udunits</a>
     * </p>
	 * Use <code>null</code> for an unkown value.
	 * @return Unit of quantity as a String, for instance m/s^2
	 */
	public String getUnit();
}
