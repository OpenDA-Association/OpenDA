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


using System;
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.Bridge
{
	public class StochModelFactory
	{
		private static IStochModelFactory _insertedStochModelFactory = null;
		private IStochModelFactory _actualStochModelFactory = null;

		public StochModelFactory()
		{
		}

		public void Initialize(String workingDirPath, string[] arguments)
		{
			if (arguments.Length != 1)
			{
				throw new Exception(GetType() + " expects one argument: \"<className>;<configFile>");
			}
			string[] splitArguments = arguments[0].Split(new[] { ';' });
			string dotNetFactoryClassName = splitArguments[0];
			string[] dotNetFactoryArguments = new string[splitArguments.Length - 1];
			for (int i = 0; i < dotNetFactoryArguments.Length; i++)
			{
				dotNetFactoryArguments[i] = splitArguments[i + 1];
			}
			if (_insertedStochModelFactory != null)
			{
				// Model factory already set. Check class name
				if (!_insertedStochModelFactory.GetType().ToString().Equals(dotNetFactoryClassName))
				{
					//throw new Exception("Inserted ModelFactory is not of the type specified in the OpenDA config file");
				}
				_actualStochModelFactory = _insertedStochModelFactory;
			}
			else
			{
				// Note: this dynamic object creation 
				IConfigurable modelFactoryObject = Utils.CreateConfigurableInstance(dotNetFactoryClassName, typeof(IStochModelFactory));
				if (!(modelFactoryObject is IStochModelFactory))
				{
					throw new Exception("Unexpected object type " + modelFactoryObject.GetType());
				}
				_actualStochModelFactory = (IStochModelFactory)modelFactoryObject;
			}
			_actualStochModelFactory.Initialize(workingDirPath, dotNetFactoryArguments);
		}

		public IStochModelInstance GetInstance(int outputLevel)
		{
			return _actualStochModelFactory.GetInstance(outputLevel);
		}

		public static void InsertModelFactory(IStochModelFactory modelFactory)
		{
			_insertedStochModelFactory = modelFactory;
		}
	}
}