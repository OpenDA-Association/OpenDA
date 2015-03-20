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

import org.openda.interfaces.ITime;

class TimeN2J implements org.openda.interfaces.ITime {

	private cli.OpenDA.DotNet.Interfaces.ITime _dotNetTime;

	public TimeN2J(cli.OpenDA.DotNet.Interfaces.ITime dotNetTime) {
		if (dotNetTime == null) {
			throw new IllegalArgumentException("Argumen dotNetTime can not be null");
		}
		_dotNetTime = dotNetTime;
	}

	public double getMJD() {
		return _dotNetTime.get_MJD();
	}

	public boolean isSpan() {
		return _dotNetTime.IsSpan();
	}

	public ITime getBeginTime() {
		return new TimeN2J(_dotNetTime.get_BeginTime());
	}

	public ITime getEndTime() {
		return new TimeN2J(_dotNetTime.get_EndTime());
	}

	public boolean isStamp() {
		return _dotNetTime.IsStamp();
	}

	public double getStepMJD() {
		return _dotNetTime.get_StepMJD();
	}

	public boolean inSpan(ITime timeSpan) {
		return _dotNetTime.InSpan(timeSpan.getBeginTime().getMJD(), timeSpan.getEndTime().getMJD());
	}

	public boolean after(ITime otherTime) {
		return _dotNetTime.After(otherTime.getMJD());
	}

	public boolean beforeEquals(ITime otherTime) {
		return _dotNetTime.BeforeEquals(otherTime.getMJD());
	}

	public void free() {
		_dotNetTime.Finish();
	}

	public String toString() {
		return _dotNetTime.ToString();
	}

	public int compareTo(ITime otherTime) {
		int result = 0;
		if (otherTime.after(this)) {
			result = -1;
		} else if (this.after(otherTime)) {
			result = 1;
		}
		return result;
	}
}
