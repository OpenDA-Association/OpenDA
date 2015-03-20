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
package org.openda.matlab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;

/**
 * This class contains tools for using OpenDA directly from Matlab
 *
 * @author nils van Velzen
 *
 */

public class Utilities
{
	/** Matlab cannot handle the enumeration argument (outputlevel) of the getInstance method from the
	 *  ModelFactory interface. This static method takes an integer argument instead and is therefore callable
	 *  from Matlab.
	 *
	 * @param factory      Model factory of which an instance needs to be created
	 * @param outputLevel  Requested output level 0:Suppress, 1:ModelDefault, 2:Debug
	 * @return             Model instance
	 */
	static public IStochModelInstance getInstance(IStochModelFactory factory, int outputLevel){
		IStochModelFactory.OutputLevel outputLevelEnum;
		if (outputLevel == 0){
		   	outputLevelEnum = IStochModelFactory.OutputLevel.Suppress;
		}
		else if (outputLevel == 1){
			outputLevelEnum = IStochModelFactory.OutputLevel.ModelDefault;
		}
		else if (outputLevel == 2){
			outputLevelEnum = IStochModelFactory.OutputLevel.Debug;
		}
		else {
			throw new RuntimeException("Illegal outputLevel. Allowed values are 0:Suppress, 1:ModelDefault and 2:Debug");
		}
		return factory.getInstance(outputLevelEnum);
	}
}
