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
	public interface ITime
	{
		//
		// First all timeStamp methods
		//

		/// <summary>
		/// Check of the time instance is a time stamp.
		/// </summary>
		/// <returns>True if the time instance is a time stamp.</returns>
		bool IsStamp();

		/// <summary>
		/// Check of the time instance is a time span.
		/// </summary>
		/// <returns>True if the time instance is a time span.</returns>
		bool IsSpan();

		/// <summary>
		/// Get the time stamp as Modified Julian Day.
		/// </summary>
		double MJD { get; }

		/// <summary>
		/// Get the time step interval as Modified Julian Day.
		/// </summary>
		double StepMJD { get; }

		/// <summary>
		/// Get the time span's begin time stamp as openDA time.
		/// </summary>
		ITime BeginTime { get; }

		/// <summary>
		/// Get the time span's end time stamp as openDA time.
		/// </summary>
		ITime EndTime { get; }

		/// <summary>
		/// Check if a time stamp lies within a time span. t1.InSpan(t2) is true if 
		/// t1 is lower than or equal to t2.endTime, and t1 is greater then t2.startTime.
		/// Method can only be invoked if <code>IsSpan()</code> is true
		/// for toCheck and <code>IsStamp()</code> is true for <code>this</code>.
		/// </summary>
		/// <param name="otherTimeBeginAsMJD">Start time of time span to check.</param>
		/// <param name="otherTimeEndAsMJD">End time of time span to check.</param>
		/// <returns>True if the time lies in span.</returns>
		bool InSpan(double otherTimeBeginAsMJD, double otherTimeEndAsMJD);

		/// <summary>
		/// Check if this time stamp is After another. t2.After(t1) means that t2 is greater than t1.
		/// </summary>
		/// <param name="otherTimeAsMJD">The time stamp to check. Method can only be invoked if
		/// <code>IsStamp()</code> is true for this and <code>IsStamp()</code> is true for <code>toCheck</code>.</param>
		/// <returns>True if the time lies afther the time stamp to check.</returns>
		bool After(double otherTimeAsMJD);

		/// <summary>
		/// Check if this time stamp is before or equal to another. t1.BeforeEquals(t2) means that
		/// t1 is lower than or equal to t2.
		/// </summary>
		/// <param name="otherTimeAsMJD">The time stamp to check. Method can only be invoked if
		/// <code>IsStamp()</code> is true for this and <code>IsStamp()</code> is true for <code>toCheck</code>.</param>
		/// <returns>True if the time lies before or is equal to the time stamp to check.</returns>
		bool BeforeEquals(double otherTimeAsMJD);

		/// <summary>
		/// Finish a time instance (i.e. free memory, needed for native implementations).
		/// </summary>
		void Finish();

		/// <summary>
		/// Return a string representation of the ITime object.
		/// </summary>
		/// <returns>The ITime object's string representation.</returns>
		string ToString();
	}
}