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

package org.openda.blackbox.interfaces;

import org.openda.interfaces.IConfigurable;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;

/**
 * Factory for creating Model instances
 * @see IModelInstance
 */
public interface IModelFactory extends IConfigurable {

    /**
     * Create an instance of the Model
     * @param arguments Arguments for this instance. (arguments == null) or (arguments.length == 0) means: no arguments.
     * @param outputLevel The level of output to be produced by the new instance (default, suppressed, etc.)
	 * @return The Model instance
     */
    IModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel);

	/**
	 * Shut down the model factory.
	 */
	void finish();
}
