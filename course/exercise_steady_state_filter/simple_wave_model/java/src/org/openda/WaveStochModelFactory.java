/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association 
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
package org.openda;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.models.simpleModel.SimpleStochModelFactory;

import java.io.File;

/* ================================================================
 * OpenDa interfaces
 * ================================================================
 *
 * Project Info:  http://www.openda.org
 *
 * ----------------------------------------------------------------
 */

/**
 * Factory for creating Stochastic Model instances. Here it creates a
 * AdvectionModelInstance
 */
public class WaveStochModelFactory extends SimpleStochModelFactory {

    public IStochModelInstance getInstance(OutputLevel outputLevel) {
    	WaveStochModelInstance result = new WaveStochModelInstance(this.workingDir, this.arguments[0]);
    	result.outputLevel = outputLevel;
        return new WaveStochModelInstance(this.workingDir, this.arguments[0]);
    }

    public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
        throw new UnsupportedOperationException("org.openda.WaveStochModelFactory.getPostprocessorInstance(): Not implemented yet.");
    }

    public void finish() {
       // no action needed (yet)
    }
}
