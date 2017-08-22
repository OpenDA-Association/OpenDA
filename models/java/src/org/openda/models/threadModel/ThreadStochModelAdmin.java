/* OpenDA v2.4.1 
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
package org.openda.models.threadModel;

/**
 * Thread that implements the Compute method of a stochastic Model
 * This class contains an administration for all compute threads that
 * are spawned by a single model factory. This administration takes care
 * of limiting the total number of threads to a pre defined maximum
 *
 * @author Nils van Velzen (VORtech)
 *
 */
public class ThreadStochModelAdmin {

    public Thread allThreads[]       = null; //All threads that are currently running
    public boolean allocatedThread[] = null; //Flag list indicating whether element in allThreads is taken or not
    public int maxThreads            = 0;    //Max number of simultaniusly running threads
    public int numActiveThreads      = 0;    //Number of threads that might still be running
    private Object lock1 = new Object();
	public long sleepTime;


    /**
     * Create a new thread model admin that maximizes the ammount of
     * simulaniously runnung threads.
     *
     * @param maxThreads The max number of simulaniously runnung threads
     */
    public ThreadStochModelAdmin(int maxThreads, long sleepTime){
        if (maxThreads<1){
			throw new RuntimeException("Max number of Threads should be >0. maxThreads ="+maxThreads);
		}
		else if (maxThreads>10000){
			throw new RuntimeException("Unrealistic max number of Threads. sould be <10000 maxThreads ="+maxThreads);
		}

		this.maxThreads  = maxThreads;
        this.allThreads       = new Thread[maxThreads];
        this.allocatedThread  = new boolean[maxThreads];
		this.sleepTime        = sleepTime;
        for (int iThread=0; iThread<maxThreads; iThread++){
            allThreads[iThread]      = null;
            allocatedThread[iThread] = false;
        }
        numActiveThreads = 0;
   }


    /**
     *  Wait for a particular thread to complete
     *
     *  @param threadID  Id of this thread in the admin
     *  @param thread    The thread itself
     *  @return          The handle to the thread (=null)
     *
    * */
    public Thread waitForThread(int threadID, Thread thread) {

        /* Simple return, there is no thread to wait for */
        if (thread == null) return null;

		try {
			thread.join();
		} catch (InterruptedException e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}


        /* Check whether thread is still in the administration and remove it when necessary */
       synchronized (lock1){
           if (thread == allThreads[threadID]){
             allThreads[threadID]=null;
             allocatedThread[threadID]=false;
			 numActiveThreads--;
           }
       }
       return null;
   }

     /**
     *  Wait until a free Thread is available. When available this thread will be allocated
     *
     *  @return    threadID of the thread
     *
     * */
    public int waitUntilFreeThread(){

        int threadID=-1;
        boolean waiting = true;

        while (waiting){
            synchronized (lock1){
                if (numActiveThreads<maxThreads){
                    threadID=allocateNewID();
					waiting=false;
                }
            	cleanAdmin();
			}
			if (this.sleepTime>0)
			try {
				Thread.sleep(this.sleepTime);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

            /*try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } */
        }
    return threadID;
   }

   public void setThread(int threadID, Thread thread){
       allThreads[threadID]=thread;
   }


    /**
     * return an allocate a free entry (threadID) in the administration.
     * Note: you should check in advance whether a free place exists.
     *
     * @return theadID that is allocated
     */
    private int allocateNewID(){
       int threadID=-1;

       for (int iThread=0; iThread<maxThreads; iThread++){
          if (! allocatedThread[iThread]){
             allocatedThread[iThread]=true;
             numActiveThreads++;
			 return iThread;
          }
       }
       throw new RuntimeException("Cannot find an non-allocated thread. This is a programming error");
   }

    /**
     * clean the administration.
     * Look for threads that have finished and remove them to make place for new threads
     */
    private void cleanAdmin(){
    //   	synchronized (lock1){
          	for (int iThread=0; iThread<maxThreads; iThread++){
            	if (allocatedThread[iThread]){
              		if (allThreads[iThread]!=null){
                  		if (!allThreads[iThread].isAlive()){
                    		allocatedThread[iThread] =false;
                    		allThreads[iThread]      =null;
					  		numActiveThreads--;
                  		}
              		}
            	}
        	}
       	}
   	//}
}
