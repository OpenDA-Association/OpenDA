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
package org.openda;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;
import java.io.File;

/**
* The one dimensional advection model :
*
* Some concentration of eg a pollutant is measured along a one dimensional transport. This can
* arise in a river or when considering a streamline of the flow in more dimensions. Advection or
* transport models often use upwind schemes for numerical simulation. Many different schemes have
* been proposed, but here we use a first-order upwind scheme. This scheme is known to show quite
* large numerical diffusion.
*
* Author: M.Verlaan/ N. van Velzen
**/

public class AdvectionStochModelInstance extends org.openda.models.simpleModel.SimpleStochModelInstance {

        boolean secondOrder=false;

	public AdvectionStochModelInstance(File workingDir, String configString){
		//
		//  DEFAULTS
		//

		// parameters of the model (deterministic part)
		pars.put("v",1.0);                     // default for flow velocity
		pars.put("dx",0.02);                   // default for size of grid cell
		pars.put("T",0.2);                     // default for time correlation for noise on boundary
		pars.put("t_start",0.0);               //start time of simulation []
		pars.put("t_stop",3.0);                //end time of simulation []
                if (secondOrder){
		   pars.put("t_step",0.01);            //timestep of simulation []
                }
                else {
		   pars.put("t_step",0.02);
                }
		// Internal state of the model
		state = new Vector(52); // state[0] contains the ar(1) state
		                         // state[1:51] contain concentrations at each grid cell

		// Parameter used for calibration
		this.stochParNames.add("v");
		double meanArr[] = {pars.get("v")};
		Vector meanPars = new Vector(meanArr);
		double stdArr[] = {0.1};
		Vector stdPars  = new Vector(stdArr);
	    stochPars = new StochVector(meanPars,stdPars);

	    // System noise for Kalman filtering
	    // assign default for mean and standard deviation
	    Vector stdSystemNoise = (new Vector(52));
	    stdSystemNoise.setValue(0, 0.1);
	    sysNoiseIntensity = new StochVector(new Vector(52),stdSystemNoise);
	    initialStateUncertainty = new StochVector(new Vector(52),new Vector(52));

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
        int n = xt.getSize();
        IVector result = new Vector(n);
        // The advection model model
        //
        //   dx[0] = -1/T     continuous time decorrelation for AR(1)
        //   dx[1] = v ( x[i]- b )/dx  with boundary condition b=b0+b1*sin(omega*time)+x[0]
        //   for i=2...n
        //   dx[i] = v ( x[i]-x[i-1] )/dx
        double Pi = 3.14159265358979;
        double x[] = xt.getValues();
        double v  = pars.get("v");
        double dx= pars.get("dx");
        double T= pars.get("T");

        //AR noise model
        double dx0 = (-1.0/T)*x[0];
        result.setValue(0, dx0);
        //first computational point
        //double bound = 1.0 +  java.lang.Math.sin(t*Pi/5.0)+x[0];
        double bound = 1.0 +  0.5*java.lang.Math.sin(t*5.0*Pi)+x[0];
        double dx1 = -v * (x[1] - bound)/dx;
        result.setValue(1, dx1);
        //other computational points
        if (secondOrder){
           double dx2 = -v * (x[2] - x[1])/dx;
           result.setValue(2, dx2);

           for(int i=3;i<n;i++){
      	      double dxi = -v * (3.0*x[i]-4.0*x[i-1]+x[i-2])/(2.0*dx);
              result.setValue(i, dxi);
           }
        }
        else {
           for(int i=2;i<n;i++){
              double dxi = -(x[i]-x[i-1])/dx;
              result.setValue(i, dxi);
           }
        }

    	return result;
    }

    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
        if (role == IExchangeItem.Role.InOut) {
            return getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("org.openda.models.lorenz.LorenzStochModelInstance.getExchangeItemIDs(): Role selection not implemented yet.");
    }

	public File getModelRunDir() {
		return new File(".");
	}

	public void finish() {
		// no action needed (yet)
	}
}

