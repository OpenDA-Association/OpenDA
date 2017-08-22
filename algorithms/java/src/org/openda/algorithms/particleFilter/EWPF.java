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
package org.openda.algorithms.particleFilter;

import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import org.openda.algorithms.kalmanFilter.AbstractSequentialEnsembleAlgorithm;
import org.openda.interfaces.*;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.io.File;
import java.util.Arrays;

/**
 * @author Nils van Velzen
 *
 * Equivalent weights particle filter
 *
 */

public class EWPF extends AbstractSequentialEnsembleAlgorithm {


	// Flag indicating whether the native code has been initialized or not
	private static boolean dllYetToBeInitialized = true;

	// Flag indicating whether we are searching for dll or so
	public static final boolean RUNNING_ON_WINDOWS = System.getProperty("os.name").startsWith("Windows");

	// Flag indicating whether we are searching for dll or so
	public static final boolean RUNNING_ON_MAC = System.getProperty("os.name").startsWith("Mac") || System.getProperty("os.name").startsWith("Darwin");


	// handle to native library
	private static ISangomaEWPFNativeDLL nativeDLL;



	int timeStepNumber =0;
	int interval       =10;
	int nextWeightStep = timeStepNumber+interval;
	int nE;
    boolean doMoreInit =true;

	double[] weight;


	static {
		if (RUNNING_ON_WINDOWS) {
			// TODO: sources are available, windows build environment to be added.
			throw new RuntimeException("Native Windows DLL does not exist (yet): sangoma.dll");
		} else {
			nativeDLL = (ISangomaEWPFNativeDLL) Native.loadLibrary("sangoma", ISangomaEWPFNativeDLL.class);
		}
	}



	public void initialize(File workingDir, String[] arguments){
		super.initialize(workingDir, arguments);
		doMoreInit=true;



	}

	public void analysis(IStochObserver obs, IVector obsValues, IVector predictions, IStochModelInstance mod, ITime analysisTime) {

		if (doMoreInit) {
			this.nE = this.ensemble.length;
			this.weight = new double[nE];
			Arrays.fill(this.weight, 1.0/this.nE);   //Don't know what initial value is best
			doMoreInit=false;
		}


		timeStepNumber++;

		int nX=this.setVariablesDLL(obs);
		int nY=obs.getCount();
		int nXAug = nX+nY;
		int nE=this.ensemble.length;
		IntByReference p_Ne = new IntByReference(nE);
		IntByReference p_nY = new IntByReference(nY);
		IntByReference p_nXAug = new IntByReference(nXAug);
		IntByReference p_count = new IntByReference(timeStepNumber);
		IntByReference p_interval = new IntByReference(this.interval);
		IntByReference p_nextWeightStep = new IntByReference(this.nextWeightStep);

		double[] y=obs.getValues().getValues();


		//Do we need to do an nudging step or a re-weighting step
		if (timeStepNumber %interval==0){
			/* re-weighting/re-sampling */
			this.nextWeightStep=this.timeStepNumber+this.interval;

            nativeDLL.oda_equal_weight_step(p_Ne, p_nXAug, p_nY, weight, y);
		}
		else {
			/* Nudging */
			nativeDLL.oda_proposal_step(p_Ne, p_nXAug, p_nY, weight, y, p_count, p_nextWeightStep,p_interval);
		}

		UpdateEnsembleStates(nX,nY,nXAug);

		Vector v_weight  = new Vector(weight);
		Results.putValue("weight", v_weight, weight.length , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		v_weight.free();


	}

    private void UpdateEnsembleStates(int nX, int nY, int nxAug) {

		int nE=this.ensemble.length;
		double[] newL   = new double[nX];
		double[] newLaug   = new double[nxAug];
		double[] x_a    = new double[nX];
		double[] pred_a = new double[nY];

		for (int i=0; i<nX; i++){x_a[i]=0.0;}
		for (int i=0; i<nY; i++){pred_a[i]=0.0;}

		for (int iEns=0; iEns<nE; iEns++) {
			IVector x=ensemble[iEns].getState();
			double[] orgL = x.getValues();
			IntByReference p_iEns=new IntByReference(iEns+1);  //Note 1-indexing in fortran
			IntByReference p_nX=new IntByReference(nX);
			IntByReference p_nE=new IntByReference(nE);

            // Get State from DLL (augmented with Hx)
			nativeDLL.oda_ewfp_get_l(newLaug, p_iEns, p_nX, p_nE);

			double[] xi_a = new double[nX];
			double[] predi_a = new double[nY];

			for (int i=0; i<nX; i++){
				xi_a[i] = newLaug[i];
				x_a[i] += newLaug[i];
				newL[i] = newLaug[i]-orgL[i];

			}
			int j=0;
			for (int i=nX; i<nxAug; i++){
				predi_a[j] = newLaug[i];
				pred_a[j] += newLaug[i];
			}

			x.setValues(newL);
			ensemble[iEns].axpyOnState(1.0,x);
			x.free();

			Vector v_xi_a   = new Vector(xi_a);
			Vector v_pred_a = new Vector(predi_a);

			Results.putValue("pred_a_"+iEns, v_xi_a, xi_a.length , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			Results.putValue("xi_a_"+iEns  , v_pred_a, predi_a.length,  "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);

			v_xi_a.free();
			v_pred_a.free();

		}

		double one_over_ne = 1.0/nE;
		for (int i=0; i<nX; i++){x_a[i]*= one_over_ne;}
		for (int i=0; i<nY; i++){pred_a[i]*= one_over_ne;}

		Vector v_x_a   = new Vector(x_a);
		Vector v_pred_a = new Vector(pred_a);

		Results.putValue("pred_a", v_x_a, x_a.length , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		Results.putValue("xi_a"  , v_pred_a, pred_a.length,  "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		v_x_a.free();
		v_pred_a.free();
	}

    //Transfer variables to DLL before calling the real computational routines
    private int setVariablesDLL(IStochObserver obs) {
		IntByReference p_nE = new IntByReference(this.ensemble.length);

		//Note: we augment the state with Hx (predictions)
		int nXAug = 0;

		IObservationDescriptions observationDescriptions=obs.getObservationDescriptions();

		IVector x_f= this.ensemble[0].getState();
		IVector pred_f = this.ensemble[0].getObservedValues(observationDescriptions);

		int nX = x_f.getSize();
		int nY = pred_f.getSize();

		// Copy augmented ensemble to DLL [x Hx] and Hx (needed for computation of HQH^T)
		for (int iEns = 0; iEns < this.ensemble.length; iEns++) {
			IVector state = this.ensemble[iEns].getState();
			double[] x = state.getValues();

			IVector HxVec = this.ensemble[iEns].getObservedValues(observationDescriptions);
			double[] Hx =HxVec.getValues();

			//Note for iEns=0 values are already set
            if (iEns>0){
				x_f.axpy(1.0,state);
				pred_f.axpy(1.0,HxVec);
			}

			Results.putValue("xi_f_" +iEns, state, state.getSize() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
			Results.putValue("pred_f_" + iEns, HxVec, HxVec.getSize(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);

			nXAug = nX+nY;

			double [] xAug=new double[nX+nY];
			for (int i=0;i<nX;i++){
				xAug[i]=x[i];
			}
			for (int i=0;i<nY;i++){
				xAug[nX+i]=Hx[i];
			}

			IntByReference p_nXAug = new IntByReference(nXAug);
			IntByReference p_nY = new IntByReference(nY);
			IntByReference p_iEns = new IntByReference(iEns+1); //Note 1-indexing in Fortran CODE

            //Set augmented state-vector
			nativeDLL.oda_ewfp_set_l(xAug, p_iEns, p_nXAug, p_nE);


			state.free();
			HxVec.free();

		}
		double one_over_ne = 1.0/nE;
		x_f.scale(one_over_ne);
		pred_f.scale(one_over_ne);
		Results.putValue("x_f", x_f, x_f.getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		Results.putValue("pred_f", pred_f, pred_f.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);

        //Compute standard deviations
		IVector x_std=x_f.clone();
		x_std.scale(0.0);
		for (int iEns=0; iEns<nE; iEns++){
			IVector x= this.ensemble[iEns].getState();
			x.axpy(-1.0,x_f);
			x.pointwiseMultiply(x);
			x_std.axpy(1.0, x);
            x.free();
		}
		double alpha=Math.sqrt(1.0 / (nE - 1.0));
        x_std.scale(alpha);
		Results.putValue("x_std", x_std, x_std.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);


        x_f.free();
        pred_f.free();
		x_std.free();


		// Compute root model error covariance
		nativeDLL.oda_ewfp_setuprooterror(p_nE);

		// Set variance of observation error to DLL
		double[] obsVar = obs.getStandardDeviations().getValues();
		for (int iObs = 0; iObs < obsVar.length; iObs++) {
			obsVar[iObs] = obsVar[iObs] * obsVar[iObs];
		}

		IntByReference p_nY = new IntByReference(obsVar.length);
		nativeDLL.oda_ewfp_set_diagr(obsVar, p_nY);

		return nX;
	}
}

