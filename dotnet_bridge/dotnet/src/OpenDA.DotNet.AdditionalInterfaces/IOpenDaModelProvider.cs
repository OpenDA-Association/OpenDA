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


using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.AdditionalInterfaces
{
	public interface IOpenDaModelProvider
	{

        /// <summary>
        /// Initialize the factory that provides the model instances
        /// </summary>
        /// <param name="workingDirPath">Directory where the model factory can find its config files</param>
        /// <param name="args">Initialization arguments, usually being one argument, the config file</param>
		void Initialize(string workingDirPath, params string[] args);

		/// <summary>
		/// <para>Returns a model instance (an OpenMI time/space linkable component).</para>
		/// </summary>
        ITimeSpaceComponent CreateInstance();

		/// <summary>
		/// <para>Stores an instance that was adjust by the outer world.</para>
		/// </summary>
        void SaveInstance(ITimeSpaceComponent timeSpaceComponent);
	}
}