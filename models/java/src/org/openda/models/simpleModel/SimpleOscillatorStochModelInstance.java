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
package org.openda.models.simpleModel;

/**
* The oscillator model
*
* simple linear oscilator (e.g. mass-spring system with friction)
* d(x)/d(t) = u
* d(u)/d(t) = - omega^2 * x - (2/t_damp) u
*/

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.File;



/**
 * Realisation of a Stochastic model. For the time being, this interface is the same
 * as the OpenDaModel interface. This means that both and StochModelInstance and ModelInstance
 * for now are the same, until the OpenDaModel interface is split up in separate parts.
 */
public class SimpleOscillatorStochModelInstance extends SimpleStochModelInstance {

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

	public SimpleOscillatorStochModelInstance(File workingDir, String configString){
		//
		//  DEFAULTS
		//

		// parameters of the model (deterministic part)
		pars.put("t_damp",8.0); //characteristic time-scale for friction [seconds]
		pars.put("omega",0.25 * 2.0 * Math.PI); //oscilation frequency [rad/s]
		// timespan for the simulation
		pars.put("t_start",0.0);               //start time of simulation [s]
		pars.put("t_stop",10.0);               //start time of simulation [s]
		pars.put("t_step",0.05);               //start time of simulation [s]

		// Internal state of the model
		state = new Vector("[0.8,0.0]");

		// Parameter used for calibration
		this.stochParNames.add("t_damp");
		this.stochParNames.add("omega");
		double meanArr[] = {pars.get("t_damp"),pars.get("omega")};
		double stdArr[] = {1.0,0.02*2*Math.PI};
	    stochPars = new StochVector(meanArr,stdArr);

	    // System noise for Kalman filtering
	    sysNoiseIntensity = new StochVector("[0.0,0.0]","[0.3,0.3]");
	    initialStateUncertainty = new StochVector("[0.0,0.0]","[0.8,0.8]");

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
        IVector result = new Vector(2);
        // The oscillator model
        //
        // simple linear oscilator (e.g. mass-spring system with friction)
        //% d(x)/d(t) = u
        // d(u)/d(t) = - omega^2 * x - (2/t_damp) u
        //
        double x[] = xt.getValues();
        double omega = Math.max(0.0, pars.get("omega"));
        double t_damp= Math.max(0.0,pars.get("t_damp"));
        result.setValue(0, x[1]);
        result.setValue(1,-(omega*omega)*x[0] - (2.0/t_damp*x[1]) );

    	return result;
    }

	public File getModelRunDir() {
		return new File(".");
	}

    public void finish() {
		// no action needed (yet)
	}
}

