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
using System.Linq;
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.Bridge
{
	public class Utils
	{
        public static IConfigurable CreateConfigurableInstance(string className, Type objectType)
        {
            Type classType = AppDomain.CurrentDomain.GetAssemblies().Select(a => a.GetType(className)).FirstOrDefault(t => t != null);
            if (classType == null)
            {
                throw new Exception(className + " not found ");
            }
            object instance = Activator.CreateInstance(classType);
		    IConfigurable configurable = instance as IConfigurable;
            if (configurable == null)
            {
                throw new Exception(className + " is not an IConfigurable but a " + instance.GetType());
            }
            return configurable;
		}

	    public static OutputLevel OutputLevelMapJ2N(int outputLevel)
		{
			switch (outputLevel)
			{
				case 0:
					return OutputLevel.Suppress;
				case 1:
					return OutputLevel.ModelDefault;
				case 2:
					return OutputLevel.Debug;
			}
			throw new Exception("Unknown IStochModelFactory.OutputLevel value: " + outputLevel);
		}

		public static Role RoleMapJ2N(int role)
		{
			switch (role)
			{
				case 0:
					return Role.Input;
				case 1:
					return Role.Output;
				case 2:
					return Role.InOut;
			}
			throw new Exception("Unknown IExchangeItem.Role  value: " + role);
		}

		public static long GetTimeStep(Time timeHorizon, Time toReach)
		{
			double stepCount = Math.Ceiling((toReach.MJD - timeHorizon.BeginTimeAsMJD) / timeHorizon.StepMJD);
			return (long)stepCount;
		}
	}
}