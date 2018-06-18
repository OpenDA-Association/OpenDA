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
using System.IO;
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.Bridge
{
	public class ModelFactory
	{
        private static IModelFactory _insertedModelFactory;
        private IModelFactory _actualModelFactory;

		private static string _logFile = "openda-dotnet-exception-log.txt";

		public ModelFactory()
		{
			_actualModelFactory = null;
		}

		public static void InsertLogFileName(String logFile)
		{
			_logFile = logFile;
		}

		public static void AppendLogMessage(string message)
		{
			FileStream fileStream = File.Open(_logFile, FileMode.Append);
			StreamWriter streamWriter = new StreamWriter(fileStream);
			streamWriter.WriteLine(message);
			streamWriter.Close();
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
            if (_insertedModelFactory != null)
            {
                // Model factory already set. Check class name
                if (!_insertedModelFactory.GetType().ToString().Equals(dotNetFactoryClassName))
                {
                    throw new Exception("Inserted ModelFactory is not of the type specified in the OpenDA config file");
                }
                _actualModelFactory = _insertedModelFactory;
            }
            else
            {
                // Note: this dynamic object creation 
                IConfigurable modelFactoryObject = Utils.CreateConfigurableInstance(dotNetFactoryClassName, typeof(IModelFactory));
                if (!(modelFactoryObject is IModelFactory))
                {
                    throw new Exception("Unexpected object type " + modelFactoryObject.GetType());
                }
                _actualModelFactory = (IModelFactory)modelFactoryObject;
            }
            _actualModelFactory.Initialize(workingDirPath, dotNetFactoryArguments);
		}

		public IModelInstance GetInstance(string[] arguments, int outputLevel)
		{
			return _actualModelFactory.GetInstance(arguments, outputLevel);
		}

	    public static void InsertModelFactory(IModelFactory modelFactory)
	    {
	        _insertedModelFactory = modelFactory;
	    }
	}
}