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

import java.io.File;

/**
 * 'Restart' state of a model instance.
 */
public interface IModelState {

	/**
	 * Write the algorithm state to file.
	 *
	 * Ask the model to save the state identified by this ModelState object to the given file.
	 * If the state consists of multiple files, these can be zipped to collect them in a single file.
	 *
	 * @param savedStateFile the file to which this state has to be saved.
	 */
	void savePersistentState(File savedStateFile);

}
