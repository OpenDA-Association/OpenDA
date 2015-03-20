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


package org.openda.dotnet;

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IStochModelFactory;

public class UtilsJ2NAndN2J
	{
		public static int OutputLevelMapJ2N(IStochModelFactory.OutputLevel outputLevel)
		{
			if (outputLevel == org.openda.interfaces.IStochModelFactory.OutputLevel.ModelDefault)
			{
				return 0; // TODO cli.OpenDA.DotNet.Interfaces.OutputLevel.ModelDefault;
			}
			if (outputLevel == org.openda.interfaces.IStochModelFactory.OutputLevel.Suppress)
			{
				return 1; // TODO  cli.OpenDA.DotNet.Interfaces.OutputLevel.Suppress;
			}
			throw new RuntimeException("Unknown IStochModelFactory.OutputLevel type: " + outputLevel);
		}

		public static int ExchangeItemRoleMapJ2N(IPrevExchangeItem.Role role) {
			if (role == IPrevExchangeItem.Role.Input) {
				return 0; // TODO cli.OpenDA.DotNet.Interfaces.Role.Input;
			}
			if (role == IPrevExchangeItem.Role.Output) {
				return 1; // TODO cli.OpenDA.DotNet.Interfaces.Role.Input;
			}
			if (role == IPrevExchangeItem.Role.InOut) {
				return 2; // TODO cli.OpenDA.DotNet.Interfaces.Role.Input;
			}
			throw new RuntimeException("Unknown IPrevExchangeItem.Role type: " + role);
		}
	}
