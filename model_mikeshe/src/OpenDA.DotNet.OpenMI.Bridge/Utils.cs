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
using OpenDA.DotNet.AdditionalInterfaces;

namespace OpenDA.DotNet.OpenMI.Bridge
{
	public class Utils
	{
        public static IOpenDaModelProvider CreateOpenDaModelProviderInstance(string className, Type objectType)
		{
            Type classType = AppDomain.CurrentDomain.GetAssemblies().Select(a => a.GetType(className)).FirstOrDefault(t => t != null);
            if (classType == null)
            {
                throw new Exception(className + " not found ");
            }
            object instance = Activator.CreateInstance(classType);
            IOpenDaModelProvider configurable = instance as IOpenDaModelProvider;
            if (configurable == null)
            {
                throw new Exception(className + " is not an IOpenDaModelProvider but a " + instance.GetType());
            }
            return configurable;
        }
	}
}