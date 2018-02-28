/* OpenDA v2.4.3 
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
package org.openda.exchange;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.ITime;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.Array;

import java.io.Serializable;
import java.util.Arrays;

/**
 * Simple timeInfo implementation
 */
public class TimeInfo implements IArrayTimeInfo, Serializable {

	private IArray times = null;
	private final int[] timeIndex = {0};

	public TimeInfo(){} //TODO This is bad why create empty timeInfo. This leads to null pointers.

	public TimeInfo(double[] times) {
		this.times = new Array(times);
	}
 
	public TimeInfo(IArray times){
		this.times=new Array(times);
	}
	
	/**
	 * Create TimeInfo from a string. Formats are:
	 * dateTimeString=true  "201008241130,201008241140,...,201008242350"
	 * dateTimeString=true  "201008240000,201008240300,201008240600"
	 * dateTimeString=false "48259.0,48259.125,48259.25"
	 * @param timesString formatted sequence as above
	 * @param dateTimeString Do we interpret as YYYYMMDDhhmm (true) or and Mjd (false)
	 */
	public TimeInfo(String timesString,boolean dateTimeString){
		if(dateTimeString){
			this.times = new Array(TimeUtils.dateTimeSequenceString2Mjd(timesString));
		}else{
			this.times = new Array(TimeUtils.MjdSequenceString2Mjd(timesString));
		}
	}

	/**
	 * Creates a new TimeInfo object that is a copy of the given timeInfo.
	 *
	 * @param timeInfo
	 */
	public TimeInfo(ITimeInfo timeInfo) {
        if (timeInfo != null && timeInfo.getTimes() != null) {
			double[] times = timeInfo.getTimes();
		    this.times = new Array(times);
        }
	}

	public double[] getTimes() {
		if(times==null){
			return null;
		}else{
			return times.getValuesAsDoubles();
		}
	}

	public void setTimes(double[] times) {
		this.times = new Array(times);
	}

	public void setTimes(ITime[] times) {
		double[] content = new double[times.length];
		for (int i = 0; i < times.length; i++) {
			content[i] = times[i].getMJD();
		}
		this.times = new Array(content);
	}

	public IArray getTimeArray() {
		return this.times;
	}

	public int[] getTimeValueIndices() {
		return this.timeIndex;
	}
	
	public String toString(){
		String result="{TimeInfo ";
		if(this.times.length()<8){
			result+=this.times.toString();
			result+=" {";
			for(int i=0;i<times.length();i++){
				if(i>0) result+=",";
				result+=TimeUtils.mjdToString(times.getValueAsDouble(i));
			}
			result+="}";
		}else{
			result+=" {";
			for(int i=0;i<3;i++){
				if(i>0) result+=",";
				result+=times.getValueAsDouble(i);
			}
			result+=",...";
			for(int i=times.length()-3;i<times.length();i++){
				if(i>0) result+=",";
				result+=times.getValueAsDouble(i);
			}
			result+="}";
			result+=" {";
			for(int i=0;i<3;i++){
				if(i>0) result+=",";
				result+=TimeUtils.mjdToString(times.getValueAsDouble(i));
			}
			result+=",...";
			for(int i=times.length()-3;i<times.length();i++){
				if(i>0) result+=",";
				result+=TimeUtils.mjdToString(times.getValueAsDouble(i));
			}
			result+="}";
		}
		result+="}}";
		return result;
	}

	
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass() || o.hashCode() != hashCode()) {
			return false;
		}

		TimeInfo that = (TimeInfo) o;
		if (!Arrays.equals(this.timeIndex, that.timeIndex)) {
			return false;
		}
		if (this.times == null) {
			if (that.times != null) {
				return false;
			}
		} else {//if this.times != null.
			if (!this.times.equals(that.times)) {
				return false;
			}
		}

		return true;
	}

	
	public int hashCode() {
		int h = Arrays.hashCode(this.timeIndex);
		h = 31 * h + (this.times != null ? this.times.hashCode() : 0);
		return h;
	}
}
