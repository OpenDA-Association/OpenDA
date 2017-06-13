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


package org.openda.utils.performance;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 6/22/11
 * Time: 9:08 AM
 * To change this template use File | Settings | File Templates.
 */
public class OdaTiming {

	private static boolean doTiming = false;

	/**
	 * Activate
	 * @param doTiming do timing indeed
	 */
	public static void setDoTiming(boolean doTiming) {
		OdaTiming.doTiming = doTiming;
	}

    /* We want to keep track of all times ever created. This will allow us to make nice prints */
	static ArrayList<OdaTiming> allTimers = new ArrayList<OdaTiming>();

	String className;
	String methodName;
	String nameOfTimer;
	long wallStart;
	long wallStop;
	long cpuStart;
	long cpuStop;
	long cpuTotal;
	boolean timerIsRunning;
	boolean isSubTimer;
	long wallTotal;
	int nTimings;
	ArrayList<OdaTiming> subTimers;

	public OdaTiming(String nameOfTimer){

		if (!doTiming) { return; }

		int len=Thread.currentThread().getStackTrace().length;
		this.className  = Thread.currentThread().getStackTrace()[2].getClassName();
		this.methodName = Thread.currentThread().getStackTrace()[2].getMethodName();

		this.nameOfTimer=nameOfTimer;
		this.wallStart         = 0;
		this.wallStop          = 0;
		this.wallTotal         = 0;
	    this.cpuStart          = 0;
	    this.cpuStop           = 0;
	    this.cpuTotal          = 0;

		this.timerIsRunning    = false;
		this.nTimings          = 0;
		this.isSubTimer        = false;
		this.subTimers         = new ArrayList<OdaTiming>();

		/* Add this timer to the list of all timers */
		synchronized (allTimers) {
		   allTimers.add(this);
		}
	}

	public void AddSubTimer(OdaTiming subTimer){

		if (!doTiming) { return; }

		subTimer.setAsSubTimer();
		this.subTimers.add(subTimer);
	}

	protected void setAsSubTimer(){
		this.isSubTimer=true;
	}
	protected boolean isSubTimer(){
		return this.isSubTimer;
	}

	public void start(){

		if (!doTiming) { return; }

		if (this.timerIsRunning){
			/* Error the timer was already started */
			String message;
			message="Timer is started for the second time:\n"+
					"className="+this.className+"\n"+
					"methodName="+this.methodName+"\n"+
					"nameOfTimer="+this.nameOfTimer+"\n"+
					"nTimings="+nTimings;
			throw(new java.lang.RuntimeException(message));
		}
		else {
			this.wallStart      = System.nanoTime();
			this.cpuStart       = getCpuTime();
			this.timerIsRunning = true;
		}
	}

	public void stop(){

		if (!doTiming) { return; }

		this.wallStop = System.nanoTime();
		this.cpuStop  = getCpuTime();
		if (this.timerIsRunning){
		   long wTime=this.wallStop-this.wallStart;
		   this.wallTotal += wTime;
		   long cTime=this.cpuStop-this.cpuStart;
		   this.cpuTotal += cTime;
		   this.nTimings++;
			this.timerIsRunning = false;
		}
		else {
			/* Error the timer was not started */
			String message;
			message="Timer was stopped but it has not been started:\n"+
					"className="+this.className+"\n"+
					"methodName="+this.methodName+"\n"+
					"nameOfTimer="+this.nameOfTimer+"\n"+
					"nTimings="+nTimings;
			throw(new java.lang.RuntimeException(message));
		}
	}

	public void print(BufferedWriter outStream, int level) throws IOException {

		if (!doTiming) { return; }

		String message;
		double wallTimeSec = (double) this.wallTotal * 1.0e-9;
		double cpuTimeSec  = (double) this.cpuTotal  * 1.0e-9;
		String ident="     ";
		if (level>0){
		   ident=ident.substring(0,level-1)+"-"+ident.substring(level+1,5);
		}
		//message=this.nameOfTimer+" "+this.methodName+" "+this.nTimings+" "+wallTimeSec+ " "+cpuTimeSec+"\n";
		message = String.format("%5s %30s %30s %5d %12.4g %12.4g\n",ident, this.nameOfTimer, this.methodName, this.nTimings, wallTimeSec, cpuTimeSec);
		outStream.write(message);

        // Print all subtimers as well
        for (int iSub=0; iSub<this.subTimers.size(); iSub++){
			this.subTimers.get(iSub).print(outStream, level+1);
		}
	}

	/* Print all timers that have been created */
    public void	printAll(File workingDir){

		if (!doTiming) { return; }

		try{
             // Create file:

			 String procID=ManagementFactory.getRuntimeMXBean().getName();
			 File timingsFile = new File(workingDir,"OpenDATimings_"+procID+".txt");
             FileWriter fstream = new FileWriter(timingsFile);
             BufferedWriter outStream = new BufferedWriter(fstream);

		   	for (int iTimer=0; iTimer<allTimers.size(); iTimer++){
				OdaTiming aTimer = allTimers.get(iTimer);
				if (! aTimer.isSubTimer){
				      allTimers.get(iTimer).print(outStream,0);
				}
	   		}

			//Close the output stream
            outStream.close();
		}catch (Exception e){//Catch exception if any
			throw(new java.lang.RuntimeException("Cannot write OpenDATimings file:" +e.getMessage()));
		}
	}



/** Get CPU time in nanoseconds. */

private long getCpuTime( ) {
	ThreadMXBean bean = null;
	try {
		bean = ManagementFactory.getThreadMXBean( );
	} catch (Error e) {
		// not supported (happens in the by means of IKVM converted version (.net)
		return 0L;
	}
	return bean.isCurrentThreadCpuTimeSupported( ) ?
        bean.getCurrentThreadCpuTime( ) : 0L;
}

/** Get user time in nanoseconds. */
/*
public long getUserTime( ) {
    ThreadMXBean bean = ManagementFactory.getThreadMXBean( );
    return bean.isCurrentThreadCpuTimeSupported( ) ?
        bean.getCurrentThreadUserTime( ) : 0L;
}
*/
/** Get system time in nanoseconds. */
/*
public long getSystemTime( ) {
    ThreadMXBean bean = ManagementFactory.getThreadMXBean( );
    return bean.isCurrentThreadCpuTimeSupported( ) ?
        (bean.getCurrentCpuTime( ) - bean.getCurrentThreadUserTime( )) : 0L;
}
*/

}

