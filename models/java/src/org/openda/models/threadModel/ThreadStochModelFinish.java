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
