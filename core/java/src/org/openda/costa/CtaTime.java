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

package org.openda.costa;

import org.openda.interfaces.ITime;

/**
 * Costa implementation for Time interface
 */
public class CtaTime extends CtaObject implements ITime {

    /**
     * Constructor for Time.
     */
    public CtaTime() {
        ctaHandle = this.create();
    }

    public CtaTime(ITime time) {
    	ctaHandle = this.create();
    	if(time.isStamp()){
    		this.setMJD(time.getMJD());
    	}else{
    		this.setSpanMJD(time.getBeginTime().getMJD(), time.getEndTime().getMJD(), time.getStepMJD());
    	}
    }

    protected native int create();

    public boolean isStamp() {
        return (this.getEndMJD() == this.getBeginMJD());
    }

    public boolean isSpan() {
        return !isStamp();
    }

    public ITime getBeginTime() {
        CtaTime time_ret = new CtaTime();
        double step=this.getStepMJD();
        double start=this.getBeginMJD();
        time_ret.setSpanMJD(start, start, step);
        return time_ret;
    }

    public ITime getEndTime() {
        CtaTime time_ret = new CtaTime();
        double step=this.getStepMJD();
        double end=this.getEndMJD();
        time_ret.setSpanMJD(end, end, step);
        return time_ret;
    }

    public native double getMJD();

    public native void setMJD(double mjd);

    public native double getBeginMJD();

    public native double getEndMJD();

    public native double getStepMJD();

    public native double getBeginUserTime();

    public native double getEndUserTime();

    public native double getUserStep();

    public native void setSpanMJD(double startTime, double endTime, double timeStep);

    public native void setUserSpan(double startTime, double endTime, double timeStep);

    public native long getStepCount();

    public boolean inSpan(ITime toCheck) {
        boolean result = false;
        if (toCheck.isStamp()) {
            result = false; //Nothing can exist in empty interval
        } else {
            if (toCheck.isSpan()) {
                result = (this.after(toCheck))
                        & (this.beforeEquals(toCheck));
            }
        }
        return result;
    }

    public boolean after(ITime toCheck) {
        return this.getBeginMJD() > toCheck.getEndTime().getMJD();
    }

    public boolean beforeEquals(ITime toCheck) {
        return (!(this.getEndMJD() > toCheck.getBeginTime().getMJD()));
    }

    @SuppressWarnings({"CloneDoesntDeclareCloneNotSupportedException"})
    public native ITime clone();

    /**
     * Compares two times ; is needed for sorting in java.util
     *
     * @param toCheck other time
     * @return int with 1,0 or -1 for this>other, this==other and this<other
     */
    public int compareTo(ITime toCheck) {
        int result = 0;
        if (toCheck.after(this)) {
            result = -1;
        } else if (this.after(toCheck)) {
            result = 1;
        }
        return result;
    }

    public String toString() {
        return "t" + this.getBeginMJD() + "-t" + this.getEndMJD();
    }
}
