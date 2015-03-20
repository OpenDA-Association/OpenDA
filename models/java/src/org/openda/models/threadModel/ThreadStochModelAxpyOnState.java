package org.openda.models.threadModel;

import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 11/2/12
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */
public class ThreadStochModelAxpyOnState extends Thread {

	IStochModelInstance threadModel=null;
	IVector x=null;
	double alpha;

	    public ThreadStochModelAxpyOnState(IStochModelInstance threadModel, double alpha, IVector x){
			this.threadModel=threadModel;
			this.alpha=alpha;
			this.x=x.clone();
	    }

	    public void run() {
			this.threadModel.axpyOnState(this.alpha, this.x);
            x.free();
			x=null;
	    }

}
