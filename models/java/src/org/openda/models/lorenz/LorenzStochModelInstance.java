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
package org.openda.models.lorenz;

/**
*  The lorenz model :
*  This simple model is often used as an example for chaotic behaviour. With the
*  default settings this model will be extremely sensitive to changes of the initial
*  conditions.
*  The background of this highly simplified model is the flow induced by heating a
*  fluid from below.
*
*   dx = sigma*(y-x)  *dt
*   dy = (rho*x-y-x*z)*dt
*   dz = (x*y-beta*z) *dt
*
* Author: M.Verlaan
**/



import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.File;



/**
 * Realisation of a Stochastic model. For the time being, this interface is the same
 * as the OpenDaModel interface. This means that both and StochModelInstance and ModelInstance
 * for now are the same, until the OpenDaModel interface is split up in separate parts.
 */
public class LorenzStochModelInstance extends org.openda.models.simpleModel.SimpleStochModelInstance {

	/**
	 * Most data structures are contained in SimpleStochModelInstance
	 * The abstract SimpleModel is based on the following items. Make
	 * sure you put your data in the proper items
	 *
	 * -	parameters of the model that can be used as control variables with their statistics
	 * 		java.util.Hashtable<String,Double> pars=null;
	 * -	Internal state of the model
	 * 		Vector state=null;
	 * -	Current time
	 * 		double t;
	 * 		int timeStep; //integer value is used for testing equality and counting
	 * -	Parameters used for calibration
	 * 		java.util.Vector<String> stochParNames=null;
	 * 		StochVector stochPars=null; //present values as a vector
	 * -	System noise for Kalman filtering, as standarddeviation per unit time
	 * 		StochVector sysNoiseIntensity=null;
    boolean autoNoise = false;
    StochVector initialStateUncertainty=null;

    // Counter for keeping track of instances
    private static int NextinstanceNumber = 1;
    private int thisInstanceNumber=0;

    // Output storage
    boolean storeObs=false;
    java.util.Vector<Double> tStore = new java.util.Vector<Double>();
    java.util.Vector<Integer> iStore = new java.util.Vector<Integer>();
    java.util.Vector<Vector> xStore = new java.util.Vector<Vector>();

    // Configuration
    File workingDir=null;
    String configString =null;

	 */

	public LorenzStochModelInstance(File workingDir, String configString){
		//
		//  DEFAULTS
		//

		// parameters of the model (deterministic part)
		pars.put("sigma",10.0);                // default for parameter
		pars.put("rho",28.0);                  // default for parameter
		pars.put("beta",8.0/3.0);              // default for parameter
		pars.put("t_start",0.0);               //start time of simulation []
		pars.put("t_stop",30.0);               //end time of simulation []
		pars.put("t_step",0.025);              //timestep of simulation []

		// Internal state of the model
		state = new Vector("[1.508870, -1.531271, 25.46091]");

		// Parameter used for calibration
		this.stochParNames.add("sigma");
		this.stochParNames.add("rho");
		this.stochParNames.add("beta");
		double meanArr[] = {pars.get("sigma"),pars.get("rho"),pars.get("beta")};
		double stdArr[] = {1.0,2.0,0.3};
	    stochPars = new StochVector(meanArr,stdArr);

	    // System noise for Kalman filtering
	    sysNoiseIntensity = new StochVector("[0.0,0.0,0.0]","[0.01,0.01,0.01]");
	    initialStateUncertainty = new StochVector("[0.0,0.0,0.0]","[0.5,0.5,0.5]");

	    /*
	     * now parse configuration
	     */
		Initialize(workingDir, configString);

	}



    /**
     * Compute time derivative of state at current time.
     * This is used by the time-integration "mod.compute(t)" as the core of the model.
     * @return vector dt
     */
    protected IVector dx(IVector xt,double t){
        IVector result = new Vector(3);
        // The lorenz model
        //
        //   dx = sigma*(y-x)  *dt
        //   dy = (rho*x-y-x*z)*dt
        //   dz = (x*y-beta*z) *dt
        double x[] = xt.getValues();
        double rho  = pars.get("rho");
        double sigma= pars.get("sigma");
        double beta = pars.get("beta");
        result.setValue(0, sigma*(x[1]-x[0]));
        result.setValue(1, rho*x[0]-x[1]-x[0]*x[2]);
        result.setValue(2, x[0]*x[1]-beta*x[2]);

    	return result;
    }

	public File getModelRunDir() {
		return new File(".");
	}

    public void finish() {
		// no action needed (yet)
	}
}

