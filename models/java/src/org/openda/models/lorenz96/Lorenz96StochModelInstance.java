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
package org.openda.models.lorenz96;

/**
*  The lorenz96 model :
*  This simple model is often used as an example for chaotic behaviour. With the
*  default settings this model will be extremely sensitive to changes of the initial
*  conditions.
*  The background of this model is an extremely simplified meteorological parameter
*  at a latitude circle
*
* (Lorenz and Emanuel 1998, "optimal sites for supplementary weather observations: Simulation with a small model"
*  J. Atmos. SCi. 127 2803-28020)
*
* dx(i)/dt = ( x(i+1) - x(i-2) ) x(i-1) - x(i) + F
* i=1:1:N N=40 cyclic and F=8.0
*
* Author: M.Verlaan
**/



import org.openda.interfaces.*;
import org.openda.noiseModels.SpatialCorrelationStochVector;
import org.openda.utils.Results;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.*;


/**
* Realisation of a Stochastic model. For the time being, this interface is the same
* as the OpenDaModel interface. This means that both and StochModelInstance and ModelInstance
* for now are the same, until the OpenDaModel interface is split up in separate parts.
*/
public class Lorenz96StochModelInstance extends org.openda.models.simpleModel.SimpleStochModelInstance {

	double stateCorrelationLength = 10.0;

	double[] xGrid = null; //spatial grid to determine state covariance
	double[] yGrid = null;

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

	public Lorenz96StochModelInstance(File workingDir, String configString){
		//
		//  DEFAULTS
		//

		// parameters of the model (deterministic part)
		pars.put("F",8.0);                     // default for parameter
		pars.put("t_start",0.0);               //start time of simulation []
		pars.put("t_stop",30.0);               //end time of simulation []
		pars.put("t_step",0.05);               //timestep of simulation []

		// Internal state of the model
		// Obtained with interation of model with N(0,?small) for t=0...1000
		state = new Vector(
				"[2.9432, 8.2790, 5.6142, 0.9046,-3.8377,-1.9521,-2.3153, 7.8081, 3.1460, "
				+"-8.4118, 0.9981,-1.9022,-0.1401, 6.3834, 7.1045, 3.8540, 3.4667, 2.5425, "
				+"5.1582, 3.6627,-2.9382, 1.3679, 8.8593, 1.6066,-3.9853, 1.8962,12.8660, "
				+"3.2394, 0.2166, 2.9771, 4.5963, 3.9998,-4.5373,-2.5524,-1.8132, 3.6624, "
				+"6.6582, 3.4951, 0.4849,-1.6945]");

		// Parameter used for calibration
		this.stochParNames.add("F");
		double meanArr[] = {pars.get("F")};
		double stdArr[] = {1.0};
	    stochPars = new StochVector(meanArr,stdArr);

	    // Defaults for System noise for Kalman filtering
	    sysNoiseIntensity = new StochVector(
	    		"[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]"
	    		,"[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]");
	    initialStateUncertainty = new StochVector(
	    		"[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]"
	    		,"[6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0]");


	    /*
	     * now parse configuration
	     */
		Initialize(workingDir, configString);

		// additional configuration
		// <initialStateUncertainty correlationLength="10.0">[0.8,0.8]</initialStateUncertainty>
		this.stateCorrelationLength = this.conf.getAsDouble("initialStateUncertainty@correlationLength", this.stateCorrelationLength);
		Results.putMessage("initialStateUncertainty@correlationLength ="+this.stateCorrelationLength);

		//compute grid = circle with distance 1 between grid points
		int n=this.state.getSize();
		this.xGrid = new double[n];
		this.yGrid = new double[n];
		double angleStep = 2*Math.PI/n;
		double radius = 0.5 / Math.sin(0.5*angleStep);
		for(int i=0;i<n;i++){
			this.xGrid[i] = radius*Math.cos(angleStep*i);
			this.yGrid[i] = radius*Math.sin(angleStep*i);
		}

		double standardDeviation = this.initialStateUncertainty.getStandardDeviations().getValue(0);
		this.initialStateUncertainty = new SpatialCorrelationStochVector(
				SpatialCorrelationStochVector.CoordinatesType.XY,
				standardDeviation, this.stateCorrelationLength, xGrid, yGrid);
	}



    /**
     * Compute time derivative of state at current time.
     * This is used by the time-integration "mod.compute(t)" as the core of the model.
     * @return vector dt
     */
    protected IVector dx(IVector xt,double t){
        /**
         * The lorenz model
         * (Lorenz and Emanuel 1998, "optimal sites for supplementary weather observations: Simulation with a small model"
         *  J. Atmos. SCi. 127 2803-28020)
         *
         * dx(i)/dt = ( x(i+1) - x(i-2) ) x(i-1) - x(i) + F
         * i=1:1:N N=40 cyclic and F=8.0
         */
        double x[] = xt.getValues();
        double F  = pars.get("F");
        int n = x.length;
        IVector result = new Vector(n);
        int iPlusOne, iMinusOne, iMinusTwo;
        for(int i=0;i<n;i++){
        	iPlusOne = i+1; if(iPlusOne>(n-1)) iPlusOne-=n;
        	iMinusOne = i-1; if(iMinusOne<0) iMinusOne+=n;
        	iMinusTwo = i-2; if(iMinusTwo<0) iMinusTwo+=n;
        	result.setValue(i, ( x[iPlusOne] - x[iMinusTwo] ) * x[iMinusOne] - x[i] + F );
        }
    	return result;
    }

	public File getModelRunDir() {
        return new File(".");
    }

    public void finish() {
		// no action needed (yet)
	}
}

