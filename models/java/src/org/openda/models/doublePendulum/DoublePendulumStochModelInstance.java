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
package org.openda.models.doublePendulum;

/**
*  The double pendulum model :
*  This simple model can used as an example for chaotic behavior.
*  See https://en.wikipedia.org/wiki/Double_pendulum for more details.
*
* dth1=(6m/l^2)*(2*pth1-3*cos(th1-th2)*pth2)/(16-9*cos^2(th1-th2));
* dth2=(6m/l^2)*(8*pth2-3*cos(th1-th2)*pth1)/(16-9*cos^2(th1-th2));
* dpth1=-0.5*m*l*l*(dth1*dth2*sin(th1-th2)+3*g/l*sin(th1));
* dpth2=-0.5*m*l*l*(-dth1*dth2*sin(th1-th2)+g/l*sin(th2));
* 
* x1=l/2 sin(th1)
* y1=-l/2 cos(th1)
* x2=l( sin(th1) + 1/2 sin(th2) )
* y2=-l( cos(th1) + 1/2 cos(th2) )
*
* Author: M.Verlaan
**/



import org.openda.interfaces.IVector;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;
import org.openda.localization.LocalizationDomainsSimpleModel;

import java.io.File;



/**
 * Realization of a Stochastic model. For the time being, this interface is the same
 * as the OpenDaModel interface. This means that both and StochModelInstance and ModelInstance
 * for now are the same, until the OpenDaModel interface is split up in separate parts.
 */
public class DoublePendulumStochModelInstance extends org.openda.models.simpleModel.SimpleStochModelInstance {


	public DoublePendulumStochModelInstance(File workingDir, String configString){
		//
		//  DEFAULTS
		//

		// parameters of the model (deterministic part)
		pars.put("l",0.3);                     //default for parameter l - length of one segment
		pars.put("g",9.81);                    //default for parameter g - acceleration of gravity
		pars.put("m",1.0);                     //default for parameter m - mass of one segment
		pars.put("t_start",0.0);               //start time of simulation []
		pars.put("t_stop",3.0);               //end time of simulation []
		pars.put("t_step",0.01);              //timestep of simulation []

		// Internal state of the model
		state = new Vector("[3.1415, 1.5707, 0.0000, 0.0000]"); //pi and pi/2

		// Create a localization here
		int[][] statedomains = {{0,1}, {2,3}};

		localizationDomains = new LocalizationDomainsSimpleModel(statedomains);


		// Parameter used for calibration
		this.stochParNames.add("l");
		this.stochParNames.add("g");
		this.stochParNames.add("m");
		double meanArr[] = {pars.get("l"),pars.get("g"),pars.get("m")};
		double stdArr[] = {1.0,2.0,0.3};
	    stochPars = new StochVector(meanArr,stdArr);

	    // System noise for Kalman filtering
	    sysNoiseIntensity = new StochVector("[0.0,0.0,0.0,0.0]","[0.0,0.0,0.0,0.0]");
	    initialStateUncertainty = new StochVector("[0.0,0.0,0.0,0.0]","[0.01,0.0,0.0,0.0]");

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
        IVector result = new Vector(4);
        // The double pendulum model
        // dth1=(6m/l^2)*(2*pth1-3*cos(th1-th2)*pth2)/(16-9*cos^2(th1-th2));
        // dth2=(6m/l^2)*(8*pth2-3*cos(th1-th2)*pth1)/(16-9*cos^2(th1-th2));
        // dpth1=-0.5*m*l*l*(dth1*dth2*sin(th1-th2)+3*g/l*sin(th1));
        // dpth2=-0.5*m*l*l*(-dth1*dth2*sin(th1-th2)+g/l*sin(th2));
        // 
        // x1=l/2 sin(th1)
        // y1=-l/2 cos(th1)
        // x2=l( sin(th1) + 1/2 sin(th2) )
        // y2=-l( cos(th1) + 1/2 cos(th2) )
        double x[] = xt.getValues(); // [th1 th2 pth1 pth2]
        double l= pars.get("l");
        double g= pars.get("g");
        double m= pars.get("m");
        double cosdth = Math.cos(x[0]-x[1]);
        double sindth = Math.sin(x[0]-x[1]);
        double temp1=6*m/l/l;
        double temp2=-0.5*m*l*l;
        // dth1=(6m/l^2)*(2*pth1-3*cos(th1-th2)*pth2)/(16-9*cos^2(th1-th2));
        double dth1= temp1*(2.0*x[2]-3.0*cosdth*x[3])/(16.0-9.0*cosdth*cosdth);
        result.setValue(0,  dth1);
        // dth2=(6m/l^2)*(8*pth2-3*cos(th1-th2)*pth1)/(16-9*cos^2(th1-th2));
        double dth2= temp1*(8.0*x[3]-3.0*cosdth*x[2])/(16.0-9.0*cosdth*cosdth);
        result.setValue(1, dth2);
        // dpth1=-0.5*m*l*l*(dth1*dth2*sin(th1-th2)+3*g/l*sin(th1));
        result.setValue(2, temp2*( dth1*dth2*sindth + 3.0*(g/l)*Math.sin(x[0])) );
        // dpth2=-0.5*m*l*l*(-dth1*dth2*sin(th1-th2)+g/l*sin(th2));
        result.setValue(3, temp2*(-dth1*dth2*sindth + (g/l)*Math.sin(x[1])));

    	return result;
    }
    

	public File getModelRunDir() {
		return new File(".");
	}

    public void finish() {
		// no action needed (yet)
	}
}

