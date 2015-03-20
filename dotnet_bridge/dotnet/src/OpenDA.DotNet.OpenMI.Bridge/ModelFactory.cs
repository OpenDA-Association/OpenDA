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
using OpenDA.DotNet.AdditionalInterfaces;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.Bridge
{
	public class ModelFactory : IModelFactory
	{
        private static IOpenDaModelProvider _insertedOpenDAModelProvider;
        private IOpenDaModelProvider _openDAModelProvider;
        private static string _logFile = "DeltaShellExceptions.txt";

	    public void Initialize(string workingDirPath, string[] arguments)
		{
            if (_insertedOpenDAModelProvider == null)
		    {
		        _openDAModelProvider = Utils.CreateOpenDaModelProviderInstance(arguments[0], typeof(IOpenDaModelProvider));
		    } else
            {
                // Model factory already set. Check class name
                if (!_insertedOpenDAModelProvider.GetType().ToString().Equals(arguments[0]))
                {
                    throw new Exception(String.Format("Inserted OpenDAModelProvider {0} is not of the type specified in the OpenDA config file {1}",
						_insertedOpenDAModelProvider.GetType(), arguments[0]));
                }
                _openDAModelProvider = _insertedOpenDAModelProvider;
            }
		}

        public static void InsertModelFactory(IOpenDaModelProvider openDAModelProvider)
        {
            _insertedOpenDAModelProvider = openDAModelProvider;
        }

        public IModelInstance GetInstance(string[] arguments, int outputLevel)
        {
            ITimeSpaceComponent timeSpaceComponent = _openDAModelProvider.CreateInstance();
            timeSpaceComponent.Initialize();
            timeSpaceComponent.Validate();
            timeSpaceComponent.Prepare();
            return new ModelInstance(this, timeSpaceComponent);
        }

	    public IInstance GetParent()
		{
			return null;
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
	}
}