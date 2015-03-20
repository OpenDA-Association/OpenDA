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

/* ================================================================
 * OpenDa interfaces
 * ================================================================
 *
 * Project Info:  http://www.openda.org
 *
 * ----------------------------------------------------------------
 */

using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.Models
{
	public abstract class AbstractSimpleModelFactory : IModelFactory
	{
		protected string workingDir;
		protected string[] arguments;


		/**
     * Initialize the factory. Here we only store the workingDir and configstring and 
     * no work is done.
     * @param workingDir The working directory for the  Model Factory
     * @param arguments One string, either with xml or with the filename of an xml-file
     * For a description of the format, see the simpleModelInstance
     */

		public void Initialize(string workingDir, string[] arguments)
		{
			this.workingDir = workingDir;
			this.arguments = arguments;
		}

		/**
     * Abstract method to create a new instance. Each model that uses this class should override
     * with something like
     * return new MymodelModelInstance(this.workingDir, this.configstring);
     * here.
	 * @param outputLevel
	 */
		public abstract IModelInstance GetInstance(string[] arguments, int outputLevel);
		// override with something like:
		//return new MymodelModelInstance(string[] arguments, this.workingDir, this.configstring);

		public override string ToString()
		{
			string result = "ModelFactory(" + GetType() + "){";
			if (workingDir != null)
			{
				result += "\n   " + workingDir + "," + arguments[0] + "}";
			}
			result += "}";
			return result;
		}

		public IInstance GetParent()
		{
			return null;
		}
	}
}