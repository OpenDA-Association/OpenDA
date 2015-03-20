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

package org.openda.interfaces;


import java.io.Serializable;

/**
 * Interface that may describe a time span or a time stamp.
 * <ul compact>
 * <li>Time Span, indicated by begin, end and step
 * <li>Time Stamp, indicated by just one time
 * </ul>
 */
public interface ITime extends Comparable<ITime>, Serializable {

    /* First all timeStamp methods */

    /**
     * Check if the time instance is a time stamp.
     *
     * @return <code>True</code> if the time instance is a time stamp.
     */
    boolean isStamp();

    /**
     * Check if the time instance is a time span.
     *
     * @return <code>True</code> if the time instance is a time span.
     */
    boolean isSpan();

    /**
     * Get the time stamp as Modified Julian Day.
     * If this is a timeSpan then (getBeginTime().getMJD+getEndTime().getMJD)/2 is returned.
     *
     * @return The time stamp. 
     */
    double getMJD();

    /**
     * Get the time step interval as Modified Julian Day.
     *
     * @return The time step interval. Throw an exception if is is not available.
     */
    double getStepMJD();

    /**
     * Get the time span's begin time stamp as openDA time.\
     *
     * @return The begin time stamp of the time span. Method can only be invoked if <code>isSpan()</code> is true.
     */
    ITime getBeginTime();

    /**
     * Get the time span's end time stamp as openDA time.
     *
     * @return The end time stamp of the time span. Method can only be invoked if <code>isSpan()</code> is true.
     */
    ITime getEndTime();

    /**
     * Check if a timestamp lies within a timespan. t1.inSpan(t2) is true if
     * t1<=t2.endTime and t1>t2.startTime
     *
     * @param toCheck The timespan to check. Method can only be invoked if <code>isSpan()</code> is true for toCheck and <code>isStamp()</code> is true for <code>this</code>.
     * @return <code>True</code> if the time lies in span.
     */
    boolean inSpan(ITime toCheck);

    /**
     * Check if this timestamp is after another.
     * t2.after(t1) means t2>t1.
     *
     * @param toCheck The timestamp to check. Method can only be invoked if <code>isStamp()</code> is true for this and <code>isStamp()</code> is true for <code>toCheck</code>.
     * @return <code>True</code> if the time is equal.
     */
    boolean after(ITime toCheck);

    /**
     * Check if this timestamp is before or equal to another.
     * t1.beforeEquals(t2) means t1<=t2.
     *
     * @param toCheck The timestamp to check. Method can only be invoked if <code>isStamp()</code> is true for this and <code>isStamp()</code> is true for <code>toCheck</code>.
     * @return <code>True</code> if the time is equal.
     */
    boolean beforeEquals(ITime toCheck);

    /**
     * Free a time instance.
     */
    void free();

    /**
     * Return a string representation of the Time object.
     *
     * @return string
     */
    String toString();

}
