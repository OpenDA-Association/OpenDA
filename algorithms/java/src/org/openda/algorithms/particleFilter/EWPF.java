/* MOD_V2.0
 * Copyright (c) 2015 OpenDA Association
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
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.*;
import org.openda.utils.Results;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

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
		nativeDLL=(ISangomaEWPFNativeDLL) Native.loadLibrary("sangoma",ISangomaEWPFNativeDLL.class);
	}



	public void initialize(File workingDir, String[] arguments){
		super.initialize(workingDir, arguments);


		// Initialize the DLL, if not done yet
/*		if (dllYetToBeInitialized) {
			Results.putMessage(EWPF.class.getSimpleName() + ": initializing wdm dll " + EWPF.getAbsolutePath());

		//	String nativeDllPath = wdmDllPath.getAbsolutePath();
		//	File nativeDll = new File(nativeDllPath);
		//	if (!nativeDll.exists()) {
		//		throw new RuntimeException("WdmDll: Native DLL/SO does not exist: " + wdmDllPath.getAbsolutePath());
		//	}

			if(BBUtils.RUNNING_ON_WINDOWS) {
				nativeDLL = (ISangomaEWPFNativeDLL) Native.loadLibrary("libsangoma.dll");
			} else if {
				// For now assumes that gfortran is used for linux and ifort for windows
				//GfortranWdmFunctionMapper gfortranMapper = new GfortranWdmFunctionMapper();
				//HashMap<String, String> gfortranMap = gfortranMapper.getMap();
				nativeDLL = (ISangomaEWPFNativeDLL) Native.loadLibrary("libsangoma.so");
			}

			dllYetToBeInitialized = false;

			Results.putMessage(WdmDll.class.getSimpleName() + ": wdm dll initialized.");
		}
	}

*/




		doMoreInit=true;



	}

	public void analysis(IStochObserver obs, IVector obsValues, IVector predictions, IStochModelInstance mod, ITime analysisTime) {

		if (doMoreInit) {
			this.nE = this.ensemble.length;
			this.weight = new double[nE];
			Arrays.fill(this.weight, 1.0);   //Don't know what initial value is best
			doMoreInit=false;
		}




		timeStepNumber++;

		int nXAug=this.setVariablesDLL(obs);
		int nY=obs.getCount();
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
			nativeDLL.oda_proposal_step(p_Ne, p_nY, p_nXAug, weight, y, p_count, p_nextWeightStep,p_interval);
		}

		UpdateEnsembleStates();
	}

    private void UpdateEnsembleStates() {

		int nE=this.ensemble.length;
		for (int iEns=0; iEns<nE; iEns++) {
			IVector x=ensemble[iEns].getState();
			int nX=x.getSize();
			double[] newL = new double[nX];
			double[] orgL = x.getValues();
			IntByReference p_iEns=new IntByReference(iEns+1);  //Note 1-indexing in fortran
			IntByReference p_nX=new IntByReference(nX);
			IntByReference p_nE=new IntByReference(nE);

			nativeDLL.oda_ewfp_get_l(newL, p_iEns, p_nX, p_nE);
			for (int i=0; i<nX; i++){newL[i]=newL[i]-orgL[i];}
			x.setValues(newL);
			ensemble[iEns].axpyOnState(1.0,x);
		}

	}

    //Transfer variables to DLL before calling the real computational routines
    private int setVariablesDLL(IStochObserver obs) {
		IntByReference p_nE = new IntByReference(this.ensemble.length);

		//Note: we augment the state with Hx (predictions)
		int nXAug = 0;

		IObservationDescriptions observationDescriptions=obs.getObservationDescriptions();

		// Copy augmented ensemble to DLL [x Hx] and Hx (needed for computation of HQH^T)
		for (int iEns = 0; iEns < this.ensemble.length; iEns++) {
			IVector state = this.ensemble[iEns].getState();
			double[] x = state.getValues();
			state.free();

			IVector HxVec = this.ensemble[iEns].getObservedValues(observationDescriptions);
			double[] Hx =HxVec.getValues();

			int nY    = Hx.length;
			int nX    = x.length;
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

			//Set Hx
			nativeDLL.oda_ewfp_sethx(Hx, p_iEns, p_nY, p_nE);
		}

		// Compute root model error covariance
		nativeDLL.oda_ewfp_setuprooterror(p_nE);

		// Set variance of observation error to DLL
		double[] obsVar = this.stochObserver.getStandardDeviations().getValues();
		for (int iObs = 0; iObs < obsVar.length; iObs++) {
			obsVar[iObs] = obsVar[iObs] * obsVar[iObs];
		}

		IntByReference p_nY = new IntByReference(obsVar.length);
		nativeDLL.oda_ewfp_set_diagr(obsVar, p_nY);

		return nXAug;

	}









}
