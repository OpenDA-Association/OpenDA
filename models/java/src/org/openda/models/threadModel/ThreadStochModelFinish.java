/*
* Copyright (c) 2021 OpenDA Association 
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

public class ThreadStochModelFinish extends Thread {

	/* We need the model instance and the end time of compute */
	private IStochModelInstance threadModel;
	private int threadId;

	public ThreadStochModelFinish(IStochModelInstance threadModel, int threadId) {
		this.threadModel = threadModel;
		this.threadId = threadId;
	}

	public void run() {
		System.out.println("Starting run of ThreadStochModelFinish with thread id: " + threadId);
		threadModel.finish();
		System.out.println("Run done of ThreadStochModelFinish with thread id: " + threadId);
	}
}
