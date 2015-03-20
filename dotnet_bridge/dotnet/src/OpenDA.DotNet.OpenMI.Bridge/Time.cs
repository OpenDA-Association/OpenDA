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
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.OpenMI.Bridge
{
	class Time : ITime
	{
		private const double _epsilon = 1.0e-9;
		private readonly global::OpenMI.Standard2.TimeSpace.ITime _openMITime;

		public Time(global::OpenMI.Standard2.TimeSpace.ITime openMITime)
		{
			_openMITime = openMITime;
		}

		public bool IsStamp()
		{
			return _openMITime.DurationInDays > 0;
		}

		public bool IsSpan()
		{
			return !IsStamp();
		}

		public double MJD
		{
			get { return _openMITime.StampAsModifiedJulianDay; }
		}

		public double StepMJD
		{
			get { return Double.NaN; }
		}

		public ITime BeginTime
		{
			get { return new DotNet.Bridge.Time(_openMITime.StampAsModifiedJulianDay); }
		}

		public ITime EndTime
		{
			get { return new DotNet.Bridge.Time(_openMITime.StampAsModifiedJulianDay + _openMITime.DurationInDays); }
		}

		public bool InSpan(double otherTimeBeginAsMJD, double otherTimeEndAsMJD)
		{
			if (!(otherTimeEndAsMJD > otherTimeBeginAsMJD + _epsilon))
			{
				throw new Exception("Argument is timeSTAMP where a timeSPAN was expected.");
			}
			return (_openMITime.StampAsModifiedJulianDay > otherTimeBeginAsMJD)
			       & (_openMITime.StampAsModifiedJulianDay + _openMITime.DurationInDays <= otherTimeEndAsMJD);
		}

		public bool After(double otherTimeAsMJD)
		{
			throw new NotImplementedException();
		}

		public bool BeforeEquals(double otherTimeAsMJD)
		{
			throw new NotImplementedException();
		}

		public void Finish()
		{
			// no action
		}
	}
}