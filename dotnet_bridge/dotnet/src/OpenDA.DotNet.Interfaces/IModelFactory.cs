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


namespace OpenDA.DotNet.Interfaces
{
	public interface IModelFactory : IConfigurable, IInstance
	{
		/**
		 * Create an instance of the Model
		 * @param arguments Arguments for this instance. (arguments == null) or (arguments.length == 0) means: no arguments.
		 * @param outputLevel The level of output to be produced by the new instance (default, suppressed, etc.)
		 * @return The Model instance
		 */
		IModelInstance GetInstance(string[] arguments, int outputLevel);
	}
}