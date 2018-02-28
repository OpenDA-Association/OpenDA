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


package org.openda.dotnet;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

/**
 * Java wrapper around .net class for a Stoch Model Factory
 */
public class StochModelFactoryN2J implements IStochModelFactory
	{
		private cli.OpenDA.DotNet.Bridge.StochModelFactory _dotNetStochModelFactory;

		public void initialize(File workingDir, String[] arguments)
		{
			if (arguments.length != 1)
			{
				throw new RuntimeException(this.getClass().getName() + " expects one argument: \"<className>;<configFile>");
			}
			_dotNetStochModelFactory = new cli.OpenDA.DotNet.Bridge.StochModelFactory();
			_dotNetStochModelFactory.Initialize(workingDir.getAbsolutePath(), arguments);
		}

		public org.openda.interfaces.IStochModelInstance getInstance(IStochModelFactory.OutputLevel outputLevel)
		{
			cli.OpenDA.DotNet.Interfaces.IStochModelInstance dotNetModelInstance =
					_dotNetStochModelFactory.GetInstance(UtilsJ2NAndN2J.OutputLevelMapJ2N(outputLevel));
			return new StochModelInstanceN2J(dotNetModelInstance);
		}

		public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
			throw new UnsupportedOperationException("org.openda.dotnet.StochModelFactoryN2J.getPostprocessorInstance(): Not implemented yet.");
		}

		public void finish() {
//          todo: Activate when Finish() is also available at .net side
//			if (_dotNetStochModelFactory  != null) {
//				_dotNetStochModelFactory.Finish();
//			}
		}
	}
