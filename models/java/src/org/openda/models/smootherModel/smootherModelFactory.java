package org.openda.models.smootherModel;

import org.openda.blackbox.wrapper.BBStochModelFactory;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 8/20/12
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class smootherModelFactory implements IStochModelFactory {

    IStochModelFactory bbfac=null;
	
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		return new smootherModelInstance(bbfac.getInstance(outputLevel));
	}

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void finish() {
		// no action needed (yet)
	}

	
	public void initialize(File workingDir, String[] arguments) {
       bbfac= new BBStochModelFactory();
	   bbfac.initialize(workingDir,arguments);
	}
}
