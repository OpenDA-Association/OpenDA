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

package org.openda.blackbox.interfaces;

import java.io.File;

/**
 * Interface for a selector that selects data in a generic io-object.
 */
public interface SelectorInterface {

    /**
     * Initalize the selector
     * @param workingDir Working directory
     * @param arguments Configuration arguments
     */
    void initialize(File workingDir, String[] arguments);

	/**
	 * Apply this selector
     * @param inputObject The input object for the selection
     * @return The resulting selection
     */
	public Object select(Object inputObject);

    /**
     * Apply this selector in the inversion way
     * @param selection Result of a previous select() call
     * @return The adjusted original input for the selection
     */
    public Object deselect(Object selection);

}
