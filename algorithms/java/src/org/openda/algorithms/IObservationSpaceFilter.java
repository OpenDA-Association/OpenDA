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
package org.openda.algorithms;
import org.openda.interfaces.IConfigurable;

import java.io.File;

/**
 * This interface supports the plug-in structure for ObservationSpaceFiltering in openda.
 * 
 * @author verlaanm
 *
 */
public interface IObservationSpaceFilter extends IConfigurable{

	
	/**
	 * Apply the filter to the input data. Eg selection of 'valid' observations and predictions in some sense.
	 * @param input
	 * @return
	 */
	public ObservationSpace applyFilter(ObservationSpace input);

    public void initialize(File workingDir, String[] arguments);
}
