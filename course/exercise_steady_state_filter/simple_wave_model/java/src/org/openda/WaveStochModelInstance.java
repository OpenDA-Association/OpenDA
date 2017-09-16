/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association
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

import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;
import java.io.File;

/**
* The one dimensional wave model :
*
* Some concentration of eg a pollutant is measured along a one dimensional transport. This can
* arise in a river or when considering a streamline of the flow in more dimensions. Wave or
* transport models often use upwind schemes for numerical simulation. Many different schemes have
* been proposed, but here we use a first-order upwind scheme. This scheme is known to show quite
* large numerical diffusion.
*
* Author: Nils van Velzen
**/

public class WaveStochModelInstance extends org.openda.models.simpleModel.SimpleStochModelInstance {

   static int NumMembers=0;
   int myMember=0;



   double depth[];
   int    nGrid  = 100;
   double L      = 1.0;
   double dx     = L/(double) nGrid;

   int AR1 =0;
   int WL  =1;
   int VEL =1+nGrid+1;
   int iCall=0;

   double Pi = Math.PI;


   double dBegin = 1.0;
   double dEnd   = 1.0;
   int    nState = 1+2*(nGrid+1);

   public WaveStochModelInstance(File workingDir, String configString){
      //
      //  DEFAULTS
      //


      NumMembers++; //Global counter of the number of members
      this.myMember=NumMembers;


      // parameters of the model (deterministic part)
      pars.put("T",  0.8);   // default for time correlation for noise on boundary
      pars.put("cf", 0.9);   // default friction coefficient
      pars.put("g",  9.81);  // gravitation constant

      // Depth (constant)
      depth = new double[nGrid+1];
      for (int i=0;i<nGrid+1;i++){
         depth[i]=1.0;
      }

      pars.put("t_start",0.0);          //start time of simulation []
      pars.put("t_stop",10.0);          //end time of simulation []
      pars.put("t_step",0.001);         //timestep of simulation []
      pars.put("t_step_out",0.1);      //timestep to write state to file []

      // Internal state of the model
      // state = new Vector(nState);
      state = new Vector(nState);
      state.setValue(AR1, 0.0);
      for (int i=0; i<nGrid; i++){
         state.setValue(VEL+i, 0.0);
      }
      for (int i=0; i<nGrid; i++){
         state.setValue(WL+i, 0.0);
      }

      // Parameter used for calibration
      this.stochParNames.add("cf");    // Friction
      double meanArr[] = {pars.get("cf")};
      Vector meanPars = new Vector(meanArr);
      double stdArr[] = {0.1};
      Vector stdPars  = new Vector(stdArr);

      stochPars = new StochVector(meanPars,stdPars);

      // System noise for Kalman filtering
      // assign default for mean and standard deviation
      Vector stdSystemNoise = (new Vector(nState));
      stdSystemNoise.setValue(0, 0.1);
      sysNoiseIntensity = new StochVector(new Vector(nState),stdSystemNoise);
      initialStateUncertainty = new StochVector(new Vector(nState),new Vector(nState));

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
    protected IVector dx(IVector xt, double t){
        int n = xt.getSize();
        IVector result = new Vector(n);

        double x[] = xt.getValues();         // Model state
        double dxi[] = new double[nState];   // Time derivative to model state

        double T   = pars.get("T");
        double cf  = pars.get("cf");
        double g   = pars.get("g");

        this.iCall++;
        if (NumMembers<3 && (iCall) % (4*10)<4) {
           this.outputLevel = OutputLevel.ModelDefault;
        }
        else {
           this.outputLevel = OutputLevel.Suppress;
        }


        // Noise model AR(1) on forcing
        dxi[0]= -x[0]/T;

        //first computational point
        double bound =  0.2*java.lang.Math.sin(t*2.0*Pi)+x[0];


        // h(x=0, t) = bound;
        x[WL]=bound;
        dxi[WL] = 0.0;
        dxi[VEL] = -g         * (x[WL+1] -x[WL])/dx - cf* x[VEL];
        for (int i=1; i<nGrid; i++){
           dxi[VEL+i] = -g         * (x[WL+i+1] -x[WL+i])/dx - cf* x[VEL+i];
           dxi[WL +i]  = -1.0 *       (x[VEL+i] -x[VEL+i-1])/dx;
        }
        // v(x=L, t) =0.0;
        dxi[VEL+nGrid] = 0.0;
        dxi[WL +nGrid]  = -1.0 *       (x[VEL+nGrid]  -x[VEL+nGrid-1])/dx;


        result.setValues(dxi);

    	return result;
    }

    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
        if (role == IExchangeItem.Role.InOut) {
            return getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("org.openda.WaveStochModelInstance.getExchangeItemIDs(): Role selection not implemented yet.");
    }

	public File getModelRunDir() {
		return new File(".");
	}

	public void finish() {
		// no action needed (yet)
	}
}

