/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.utils;

import org.openda.interfaces.ITime;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import static org.openda.utils.performance.OdaGlobSettings.getTimePrecision;


public class Time implements ITime {

	// Internal storage
	private double startTimeAsMJD = Double.NEGATIVE_INFINITY;
	private double endTimeAsMJD = Double.POSITIVE_INFINITY;
	private boolean isSpan = false;
	private double stepAsMJD = Double.NaN;

	/**
	 * Create a time stamp object
	 *
	 * @param timeAsMDJ timestamp as Modified Julian Day
	 */
	public Time(double timeAsMDJ) {
		this.startTimeAsMJD = this.endTimeAsMJD = timeAsMDJ;
		this.isSpan = false;
	}

	/**
	 * Create a time stamp from another time object, while adding a delta.
	 *
	 * @param sourceTime The other time object
	 * @param delta      Interval to be added
	 */
	public Time(ITime sourceTime, double delta) {
		if (sourceTime.isSpan()) {
			throw new RuntimeException("Can not create new time stamp from time span " + sourceTime.toString());
		}
		this.startTimeAsMJD = this.endTimeAsMJD = sourceTime.getMJD() + delta;
		this.isSpan = false;
	}

	/**
	 * Create a time stamp object from a java Date
	 *
	 * @param date start of span
	 */
	public Time(Date date) {
		this.startTimeAsMJD = this.endTimeAsMJD = milliesToMjd(date.getTime());
	}

	/**
	 * Create a time span object
	 *
	 * @param startTimeAsMDJ start of span
	 * @param endTimeAsMDJ   end of span
	 */
	public Time(double startTimeAsMDJ, double endTimeAsMDJ) {
		this.startTimeAsMJD = startTimeAsMDJ;
		this.endTimeAsMJD = endTimeAsMDJ;
		determineDeltaT();
		this.isSpan = true;
	}

	/**
	 * Create a time span object form two time stamps
	 *
	 * @param startTime start of span
	 * @param endTime   end of span
	 */
	public Time(ITime startTime, ITime endTime) {
		this.startTimeAsMJD = startTime != null ? startTime.getMJD() : Double.NEGATIVE_INFINITY;
		this.endTimeAsMJD = endTime != null ? endTime.getEndTime().getMJD() : Double.POSITIVE_INFINITY;
		determineDeltaT();
		this.isSpan = true;
	}

	/**
	 * Create a copy of an existing Time object
	 * @param time source of copy
	 */
	public Time(ITime time) {
		if (time==null) {
			this.startTimeAsMJD = Double.NEGATIVE_INFINITY;
			this.endTimeAsMJD = this.startTimeAsMJD;
			this.isSpan = false;
		} else if(time.isSpan()){
			this.startTimeAsMJD = time.getBeginTime().getMJD();
			this.endTimeAsMJD = time.getEndTime().getMJD();
			this.stepAsMJD = time.getStepMJD();
			this.isSpan = true;
		}else{
			this.startTimeAsMJD = time.getMJD();
			this.endTimeAsMJD = this.startTimeAsMJD;
			this.isSpan = false;    		
		}
	}


	/**
	 * Create a time span object form two time java Dates
	 *
	 * @param startDate start of span
	 * @param endDate   end of span
	 */
	public Time(Date startDate, Date endDate) {
		this(startDate != null ? milliesToMjd(startDate.getTime()) : Double.NEGATIVE_INFINITY,
				endDate != null ? milliesToMjd(endDate.getTime()) : Double.POSITIVE_INFINITY);
		determineDeltaT();
	}

	public Time(Date startTime, Date endTime, double deltaTasMJD) {
		this(startTime, endTime);
		this.stepAsMJD = deltaTasMJD;
	}

    public Time(ITime startTime, ITime endTime, double timeStepMJD) {
        this(startTime, endTime);
        if (!Double.isNaN(timeStepMJD)) {
            this.setStep(timeStepMJD);
        }
    }

    private void determineDeltaT() {
		if (startTimeAsMJD > Double.NEGATIVE_INFINITY && endTimeAsMJD < Double.POSITIVE_INFINITY) {
			this.stepAsMJD = this.endTimeAsMJD - this.startTimeAsMJD;
		}
	}

	public Time(double startTimeAsMJD, double endTimeAsMJD, double deltaTasMJD) {
		this(startTimeAsMJD, endTimeAsMJD);
		this.stepAsMJD = deltaTasMJD;
	}

	/**
	 * {@inheritDoc}
	 */
	 public boolean isSpan() {
		 return this.isSpan;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public boolean isStamp() {
		 return (!this.isSpan);
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public double getMJD() {
		 double result=0.0;
		 if(this.isSpan){
			 result=0.5d*(this.startTimeAsMJD+this.endTimeAsMJD);
		 }else{
			 result=this.startTimeAsMJD;
		 }
		 return result;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public double getBeginMJD() {
		 return this.startTimeAsMJD;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public double getEndMJD() {
		 return this.endTimeAsMJD;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public ITime getBeginTime() {
		 return new Time(this.startTimeAsMJD);
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public ITime getEndTime() {
		 return new Time(this.endTimeAsMJD);
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public long getStepCount() {
		 double stepCount = Math.ceil((this.endTimeAsMJD-getTimePrecision() - this.startTimeAsMJD) / this.stepAsMJD);
		 return (long) stepCount;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public double getStepMJD() {
		 // not implemented for Time
		 return this.stepAsMJD;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public long getTimeStep(ITime toReach) {
		 double stepCount = Math.ceil((toReach.getMJD()-getTimePrecision() - this.startTimeAsMJD) / this.stepAsMJD);
		 return (long) stepCount;
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public boolean inSpan(ITime toCheck) {
		 if (!toCheck.isSpan()) {
			 throw new RuntimeException("Argument is timeSTAMP where a timeSPAN was expected.");
		 }
		 return (this.startTimeAsMJD > toCheck.getBeginTime().getMJD())
		 & (this.endTimeAsMJD <= toCheck.getEndTime().getMJD());
	 }

	 /**
	  * Check if this time t  t>=lower and t<=upper
	  */
	 public static boolean inSpanIncludeLowerBound(ITime thisTime, ITime toCheck) {
		 if (!toCheck.isSpan()) {
			 throw new RuntimeException("Argument is timeSTAMP where a timeSPAN was expected.");
		}
		return (thisTime.getBeginTime().compareTo(toCheck.getBeginTime())>=0)
		&       (thisTime.getEndTime().compareTo(toCheck.getEndTime())  <=0);
	 }
	 
	 /**
	  * {@inheritDoc}
	  */
	 public boolean after(ITime toCheck) {
		 return (this.getBeginMJD() > toCheck.getEndTime().getMJD());
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public boolean beforeEquals(ITime toCheck) {
		 return (this.getEndMJD() <= toCheck.getBeginTime().getMJD());
	 }

	 /**
	  * {@inheritDoc}
	  */
	 public void free() {
		 // nothing to do
	 }

	/**
	 * Represent the time object as string
	 *
	 * @return string representation of time
	 * @param time Time to be presented as string
	 */
	public static String asDateString(ITime time) {
		String result;
		if (time.isStamp()) {
			result = timeStampToDate(time.getMJD()).toString();
		} else {
			if ((time.getEndTime().getMJD() - time.getBeginTime().getMJD()) < getTimePrecision()) {
				result = timeStampToDate(0.5 * (time.getBeginTime().getMJD()
						+ time.getEndTime().getMJD())).toString();
			} else {
				Locale locale = new Locale("EN");
				String stepFormat = "%6.6f";
				result = timeStampToDate(time.getBeginTime().getMJD()).toString() + " - step=" +
						String.format(locale, stepFormat, time.getStepMJD()) + " - " +
						timeStampToDate(time.getEndTime().getMJD());
			}
		}
		return result;
	}

	/**
	  * Represent the time object as string
	  *
	  * @return string representation of time
	  */
	 public String toString() {
		 String result;
		 if (this.isStamp()) {
			 result = "" + this.startTimeAsMJD;
		 } else {
			 if ((this.endTimeAsMJD - this.startTimeAsMJD) < getTimePrecision()) {
				 result = "" + 0.5 * (this.startTimeAsMJD + this.endTimeAsMJD);
			 } else {
				 result = "" + this.startTimeAsMJD + ":" + this.stepAsMJD + ":" + this.endTimeAsMJD;
			 }
		 }
		 return result;
	 }

	 /**
	  * Compares two times ; is needed for sorting in java.util
	  *
	  * @param otherTime time
	  * @return int with 1,0 or -1 for this>other, this==other and this<other
	  */
	 public int compareTo(ITime otherTime) {
		 int result = 0;
		 if (otherTime.after(this)) {
			 result = -1;
		 } else if (this.after(otherTime)) {
			 result = 1;
		 }
		 return result;
	 }

	 public void setStep(double stepAsMJD) {
		 this.stepAsMJD = stepAsMJD;
	 }

	 // TIME Conversion
	 public Date getDate() {
		 gmtCalendar.setTimeInMillis(mjdToMillies(this.startTimeAsMJD));
		 return gmtCalendar.getTime();
	 }

	 // conversion constants:
	 // -  #milliseconds in a day
	 // -  Modified Julian Day value for "epoch" (1970-01-01 00:00:00.0)
	 private static final long MILLIS_PER_DAY = 24*60*60*1000;
	 private static final double EPOCH_AS_MJD = 40587;     //

	 private static final GregorianCalendar gmtCalendar = new GregorianCalendar( TimeZone.getTimeZone("GMT"));

	 public static long mjdToMillies(double timeAsMJD) {
		 long millis = (long)((timeAsMJD - EPOCH_AS_MJD) * (double) MILLIS_PER_DAY);
		 return roundTimeToWholeSeconds(millis);
	 }

	 public static double milliesToMjd(long millies) {
		 long daysSinceEpoch = millies / MILLIS_PER_DAY;
		 long millisInDay = millies - daysSinceEpoch * MILLIS_PER_DAY;
		 return EPOCH_AS_MJD + daysSinceEpoch + (double) millisInDay / (double) MILLIS_PER_DAY;
	 }

	 public static Date timeStampToDate(ITime timeStamp) {
		 if (timeStamp.isSpan()) {
			 throw new RuntimeException("Argument is timeSPAN where a timeSTAMP was expected.");
		 }
		 return new Date(mjdToMillies(timeStamp.getMJD()));
	 }

	public static Date timeStampToDate(double timeStampAsMJD) {
		return new Date(mjdToMillies(timeStampAsMJD));
	}

	private static long roundTimeToWholeSeconds(long time) {
		 long mod = time % 1000;
		 if (mod == 0) return time;
		 if (mod >= 500) return time - mod + 1000;
		 if (mod <= -500) return time - mod - 1000;
		 return time-mod;
	 }

	 public static Time[] Mjds2Times(double[] mjds){
		 Time result[] = new Time[mjds.length];
		 for(int i=0;i<mjds.length;i++){
			 result[i]=new Time(mjds[i]);
		 }
		 return result;
	 }

	 public static Time[] createSelection(ITime[] times, ITime selectionSpan){
		 Time result[] = null;
		 // find first index that should be included
		 // iStart will be set to length if forall times : startTime > times
		 int n = 0;
		 if (times != null) {
			 n = times.length;
		 }

		 int iStart = 0;
		 while ((iStart < n) && (!inSpanIncludeLowerBound(times[iStart],selectionSpan))) {
			 iStart++;
		 }
		 // find first index that should NOT be included anymore
		 int iEnd = iStart;
		 if ((n > 0) && (inSpanIncludeLowerBound(times[n - 1],selectionSpan))) {
			 iEnd = times.length;
		 }else {
			 while ((iEnd < n) && (inSpanIncludeLowerBound(times[iEnd],selectionSpan))) {
				 iEnd++;
			 }
		 }
		 if (iEnd > iStart) {
			 result = new Time[iEnd - iStart];
			 System.arraycopy(times, iStart, result, 0, iEnd - iStart);
		 }
		 return result;
	 }
	 
	 public double durationInDays(){
		 if (this.isStamp()){
			 return 0.0;
		 }else{
			return this.endTimeAsMJD-this.startTimeAsMJD; 
		 }
	 }
	 
	 
	 public Time extendInterval(double delta){
		 return new Time(this.getBeginMJD()-delta,this.getEndMJD()+delta, this.getStepMJD());
	 }
}
