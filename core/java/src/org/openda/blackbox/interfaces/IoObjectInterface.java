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

import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;

/**
 * Interface for the generic io-object. This interface is needed to give a single interface to
 * various sources of io: ascii-files, model specific binary files, databases, etc..
 */
public interface IoObjectInterface {

    /**
     * Initialize the IoObject
     * @param workingDir   Working directory
     * @param fileName The name of the file containing the data (relative to the working dir.)
     * @param arguments Additional arguments (may be null zero-length)
     */
    public void initialize(File workingDir, String fileName, String[] arguments);

    /**
     * Ask which elements can be accessed
     * @return The list of element identifiers that can be accessed
     */
    public IPrevExchangeItem[] getExchangeItems(); //

    public void finish();
}
