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
using System.Collections.Generic;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.UnitTests
{
	internal class SimpleTimeSet : ITimeSet
	{
		private readonly IList<ITime> _times;
		private readonly ITime _timeHorizon;
		private readonly bool _hasDurations;

		public SimpleTimeSet(double stampAsModifiedJulianDay)
		{
			_times = new List<ITime> { new SimpleTime(stampAsModifiedJulianDay) };
		}

		public SimpleTimeSet(double stampAsModifiedJulianDay, double durationInDays)
		{
			_times = new List<ITime>();
			_timeHorizon = new SimpleTime(stampAsModifiedJulianDay, durationInDays);
			_hasDurations = true;
		}

		public IList<ITime> Times
		{
			get { return _times; }
		}

		public bool HasDurations
		{
			get { return _hasDurations; }
		}

		public double OffsetFromUtcInHours
		{
			get { return 0.0d; }
		}

		public ITime TimeHorizon
		{
			get { return _timeHorizon; }
		}
	}
}