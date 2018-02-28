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


package org.openda.interfaces;

import java.io.File;


/**
 * Factory for creating Stochastic Model instances.
 */
public interface IStochModelFactory extends IConfigurable {

	/**
	 * Possible levels of output to be produced by a model instance.
	 */
	public enum OutputLevel {
        /** as little output as possible */
		Suppress,
        /** as specified in the model's schematization/configuration files */
		ModelDefault,
        /** produce additional output */
		Debug
	}

    /**
     * Create an instance of the stochastic Model
     * @return The stochastic Model instance
	 * @param outputLevel Amount of output that the new instance should produce.
     */
    IStochModelInstance getInstance(OutputLevel outputLevel);

    /**
     * Create an instance of the stochastic Model's Postprocessor
     * @param instanceDir The working directory for the postprocessor
     * @return The stochastic Postprocessor instance
     */
    IStochModelPostProcessor getPostprocessorInstance(File instanceDir);

	/**
	 * Shut down the stochastic model factory.
	 */
	void finish();
}

