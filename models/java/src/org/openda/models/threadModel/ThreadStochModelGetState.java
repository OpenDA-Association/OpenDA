/*
 * Copyright (c) 2019 OpenDA Association
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
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;
import org.openda.utils.DistributedCounter;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 8/24/12
 * Time: 4:07 PM
 * To change this template use File | Settings | File Templates.
 */
public class ThreadStochModelGetState  extends Thread {
   static DistributedCounter id = new DistributedCounter();



	    /* We need the model instance and the end time of compute */
    IStochModelInstance threadModel=null;
    ThreadStochModelCompute threadCompute=null;
	IVector state=null;

    public ThreadStochModelGetState(IStochModelInstance threadModel, ThreadStochModelCompute threadModelCompute){
		threadCompute = threadModelCompute;
		this.threadModel=threadModel;
    }


	/* This thread will wait for the compute method to complete and then call the getState
	   Method such that we have a parallel collection of states

	   -Note this method runs on a separate thread since it is likely to use other
	    resources as the compute method
	  */
    public void run() {
		int myId= id.val();
		id.inc();
		long itime=System.currentTimeMillis();
        // First wait for model to complete
		try {
			System.out.println(myId+": Parallel get State started ");
			this.threadCompute.join();
			System.out.println(myId+": Parallel get State model is done computing "+((System.currentTimeMillis()-itime)/1000.0));
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		// Get the state of the model
		this.state = this.threadModel.getState();
		System.out.println(myId+": Parallel get State model is done "+((System.currentTimeMillis()-itime)/1000.0));

    }

	/* Method to get the state. Should only be called after the run method has completed. */
	public IVector getModelState(){
		return this.state;
	}
}
