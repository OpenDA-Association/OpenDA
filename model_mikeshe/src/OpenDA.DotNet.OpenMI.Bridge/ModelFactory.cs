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
        private static IOpenDaModelProvider _insertedOpenMIModelFactory;
        private IOpenDaModelProvider _openMIModelFactory;
        private static string _logFile = "DeltaShellExceptions.txt";

        public static void InsertModelFactory(IOpenDaModelProvider openMIModelFactory)
        {
            _insertedOpenMIModelFactory = openMIModelFactory;
        }

        public void Initialize(string workingDirPath, string[] arguments)
		{
            if (_insertedOpenMIModelFactory != null)
		    {
                _openMIModelFactory = _insertedOpenMIModelFactory;
            }
            else
            {
                _openMIModelFactory = Utils.CreateOpenDaModelProviderInstance(arguments[0], typeof(IOpenDaModelProvider));
            }
		}

        public IModelInstance GetInstance(string[] arguments, int outputLevel)
        {
            ITimeSpaceComponent timeSpaceComponent = _openMIModelFactory.CreateInstance();
            //timeSpaceComponent.Initialize();
            //timeSpaceComponent.Validate();
            //timeSpaceComponent.Prepare();
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