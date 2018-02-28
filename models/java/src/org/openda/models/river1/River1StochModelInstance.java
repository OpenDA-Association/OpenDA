/* OpenDA v2.4.3 
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
package org.openda.models.river1;

import org.openda.interfaces.IExchangeItem;
import org.openda.models.simpleModel.SimpleStochModelInstance;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;
import org.openda.interfaces.IVector;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;


/**
 * Simplified linear response of a river with two sections. This model does not perform
 * any real hydrodynamics computation, but only mimics the behaviour with a linear regression
 * formula. To add some time behaviour the model is started from the equilibrium for alpha_a=0
 * and alpha_b=0 and it performs an exponential relaxation towards the final equilibrium:
 * x0_equilibrium = xa0+ 0.1 * alpha_a
 * x1_equilibrium = xb0+ 0.15 * alpha_b + 0.03 * alpha_a
 * 
 * section a is the downstream section of the two and x[0] models the waterlevel at the upstream end
 * of section a. alpha_a is the change in friction in section a. Dito for the upstream section b.
 */
public class River1StochModelInstance extends SimpleStochModelInstance {

    private double xa0=1.0; 
    private double xb0=2.0;
    private double timeScale = 3.0; // e-folding timescale for the relaxation



	public River1StochModelInstance(File workingDir, String configString){
		//
		//  DEFAULTS
		//

		// parameters of the model (deterministic part)
		pars.put("alpha_a",1.0);
		pars.put("alpha_b",2.0);
		// timespan for the simulation
		pars.put("t_start",0.0);               //start time of simulation [s]
		pars.put("t_stop",10.0);               //start time of simulation [s]
		pars.put("t_step",0.05);               //stepsize in simulation [s]

		// Internal state of the model
		state = new Vector("[0.0,0.0]");

		// Parameter used for calibration
		this.stochParNames.add("alpha_a");
		this.stochParNames.add("alpha_b");
		double meanArr[] = {pars.get("alpha_a"),pars.get("alpha_b")};
		double stdArr[] = {1.0,1.0};
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
	public IVector dx(IVector xt,double t){
        IVector result = new Vector(2);

        // actually we have a stationary model. So If we make sure
        // that the model converges fast to the stationary solution, we can use
        // the Simple Model class for our model as well

        // dx(1)= scale * ((xao+ 0.1 * alpha_a) - x(1))
        // dx(2)= scale * ((xb0+ 0.15 * alpha_b + 0.03 * alpha_a) - x(2))
        result.setValue(0, xa0+0.1*pars.get("alpha_a"));
        result.setValue(1, xb0+0.15*pars.get("alpha_b")+0.03*pars.get("alpha_a"));
        result.axpy(-1.0, xt);

        // We can increase performance by making the equation a bit more stiff
        // but not too much since we are using an explicit RK4\
        result.scale(1./timeScale);

        return result;
    }

	public File getModelRunDir() {
        return null;
    }

    public void finish() {
		// no action needed (yet)
	}
}
