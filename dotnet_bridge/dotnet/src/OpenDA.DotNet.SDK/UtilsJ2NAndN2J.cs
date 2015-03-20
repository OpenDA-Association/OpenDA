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
using java.lang;
using OpenDA.DotNet.Interfaces;
using Exception=System.Exception;

namespace OpenDA.DotNet.SDK
{
	public class UtilsJ2NAndN2J
	{
		public static int CompareTimeStampAndSpan(double timeStamp, double timeStampBegin, double timeSpanEnd)
		{
			throw new NotImplementedException();
		}

		public static int CompareTimeSpanAndStamp(double timeStampBegin, double timeSpanEnd, double timeStamp)
		{
			return -1 * CompareTimeSpanAndStamp(timeStamp, timeStampBegin, timeSpanEnd);
		}

		public static int CompareTimeSpans(double timeStamp1Begin, double timeSpan1End,
										   double timeStamp2Begin, double timeSpan2End)
		{
			throw new NotImplementedException();
		}

		public static org.openda.interfaces.IPrevExchangeItem.Role RoleMapN2J(int role)
		{
			switch (role)
			{
				case 0:
					return org.openda.interfaces.IPrevExchangeItem.Role.Input;
				case 1:
					return org.openda.interfaces.IPrevExchangeItem.Role.Output;
				case 2:
					return org.openda.interfaces.IPrevExchangeItem.Role.InOut;
			}
			throw new Exception("Unknown IExchangeItem.Role  value: " + role);
		}

		public static org.openda.interfaces.IStochModelFactory.OutputLevel OutputLevelMapN2J(int outputLevel)
		{
			switch (outputLevel)
			{
				case 0:
					return org.openda.interfaces.IStochModelFactory.OutputLevel.Suppress;
				case 1:
					return org.openda.interfaces.IStochModelFactory.OutputLevel.ModelDefault;
				case 2:
					return org.openda.interfaces.IStochModelFactory.OutputLevel.Debug;
			}
			throw new Exception("Unknown IStochModelFactory.OutputLevel value: " + outputLevel);
		}

		public static Type ValueTypeMapJ2N(Class valueType)
		{
			string valueTypeString = valueType.GetType().Name;
			if (valueTypeString.ToLower().Equals("double"))
			{
				return typeof (double);
			}
			if (valueTypeString.Equals("double[]"))
			{
				return typeof(double[]);
			}
			throw new Exception("Unknown value type " + valueTypeString);
		}

		public static Role RoleMapJ2N(org.openda.interfaces.IPrevExchangeItem.Role role)
		{
			string roleString = role.toString();
			if (roleString.ToLower().Equals("input"))
			{
				return Role.Input;
			}
			if (roleString.ToLower().Equals("output"))
			{
				return Role.Output;
			}
			if (roleString.ToLower().Equals("inout"))
			{
				return Role.InOut;
			}
			throw new Exception("Unknown IExchangeItem.Role  value: " + roleString);
		}
	}
}
