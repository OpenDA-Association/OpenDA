package org.openda.model_nemo;

import org.openda.blackbox.config.BBModelConfig;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.*;

/**
 * Created by nils on 16/05/14.
 */
public class NemoModelInstance extends BBModelInstance implements IModelInstance, IModelExtensions {

	private boolean debug=false;
	int offset_sshb = 656668-1;
	int offset_tb   = 225424-1;
	int nz=11, ny=121, nx=81;



	boolean only_ssh=true;

	/**
	 * Create new blackbox model instance. You should not need to call this routine manually, but use the
	 * getInstance method from the factory instead.
	 *
	 * @param bbModelConfig  configuration, typically parsed from an input file.
	 * @param instanceNumber number for this copy of the model. Starts at 0 and does not reuse numbers
	 *                       even when a model has been cleaned from memory.
	 * @param timeHorizon    feed this timeHorizon from outside to the model. If it is null then use the
	 */
	public NemoModelInstance(BBModelConfig bbModelConfig, int instanceNumber, ITime timeHorizon) {
		super(bbModelConfig, instanceNumber, timeHorizon);
	}

	/**
	 * Get the localization vector
	 *
	 * @param exchangeItemID
	 * @param observationDescriptions observation description
	 * @param distance                characteristic distance for Cohn's formula
	 * @return weight vector for each observation location.
	 */
	public IVector[] getObservedLocalization(String exchangeItemID, IObservationDescriptions observationDescriptions, double distance) {
		System.out.println("Debug:Welcome to getObservedLocalization of the NEMO model");

		if (!exchangeItemID.equals("sshb") && !exchangeItemID.equals("sshn")) {
			throw new RuntimeException("No, we only support ssh in state!! not"+exchangeItemID);
		}


		IVector xVector = observationDescriptions.getValueProperties("x");
		IVector yVector = observationDescriptions.getValueProperties("y");

        if (xVector.getSize()!=1) {
			throw new RuntimeException("No, we do not support this for more than one observation!!");
		}
		double yPos=yVector.getValue(0);
		double xPos=xVector.getValue(0);

		double[] mask = new double [ny*nx];

		int idx=0;
		for (int ix=0;ix<this.nx;ix++){
			for (int iy=0;iy<this.ny;iy++){
				double dist=Math.sqrt((yPos - iy)*(yPos-iy)+(xPos-ix)*(xPos-ix));

				// Determine weights (Gaspari-Cohn formula)
				double a = Math.sqrt(10/3) * distance;        // characteristic length scale for model

				if (0.0 <= dist && dist <= a){
					mask[idx] = -0.25 * Math.pow((dist/a),5) + 0.5 * Math.pow((dist/a),4) + 5.0/8.0 * Math.pow((dist/a),3) - 5.0/3.0 * Math.pow((dist/a),2) + 1.0;
				} else if (a < dist && dist <= 2.0 * a) {
					mask[idx] = 1/12.0 * Math.pow((dist/a),5) - 0.5 * Math.pow((dist/a),4) + 5.0/8.0 * Math.pow((dist/a),3) + 5.0/3.0 * Math.pow((dist/a),2) - 5.0 * (dist/a) + 4.0 - 2.0/3.0 * (a/dist);
				} else if (2.0 * a < dist) {
					mask[idx] = 0.0;
				} else {
					throw new RuntimeException("There is a problem in the determination of the localisation weights.");
				}
				idx++;
			}

		}


		IVector [] retVec = new IVector[1];
		retVec[0] = new org.openda.utils.Vector(mask);
		return retVec;

	}

	/**
	 * Get the observed values of the Model.
	 *
	 * @param observationDescriptions observation description
	 * @return Model prediction interpolated to each observation (location).
	 */
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		System.out.println("Debug:Welcome to getObservedVales of the NEMO model");
		if (!(observationDescriptions instanceof NemoNetcdfStochObserver)){
			throw new RuntimeException("The NEMO model using the extended interface only supports the NemoNetcdfStochObserver");
		}
		NemoNetcdfStochObserver obsdescr=(NemoNetcdfStochObserver) observationDescriptions;

		// Get interesting information from the stochObserver
		int nObsAll=obsdescr.sshb.length+obsdescr.tb.length;
		int nObsValid=obsdescr.getCount();
		System.out.println("Number of observations is"+nObsAll+"\n");
		System.out.println("Number of valid observations is"+nObsValid+"\n");
		float[] Hi=obsdescr.Hi;
		float[] Hj=obsdescr.Hj;
		float[] Hs=obsdescr.Hs;
		//float[] sshb=obsdescr.sshb;
		//float[] tb=obsdescr.tb;

		// In the first step (Analysis001), there are 150+4270=4420 observations,
		// so H.i is included in [1 4420] (if -1 is present, it means this obs is
		//out of the model grid), H.j is included in [225424 - 333234] and [656668
		//		- 666468].

		int offset_sshb = 656668-1;
		int offset_tb   = 225424-1;

		// get values of sshb //
		IPrevExchangeItem sshbExchange = this.getExchangeItem("sshb");
		IPrevExchangeItem tbExchange   = this.getExchangeItem("tb");


		double [] sshb=sshbExchange.getValuesAsDoubles();
		double [] tb=tbExchange.getValuesAsDoubles();


		double [] HxAll= new double[nObsAll];

		for (int iObs=0; iObs<nObsAll; iObs++){ HxAll[iObs]=0.0;}

		//double[] observed=obsdescr.getValues().getValues();


		for (int iNnz=0; iNnz<Hi.length; iNnz++){
			int i=(int) Hi[iNnz]-1;   //Note move to 0-indexing
			int j=(int) Hj[iNnz]-1;   //Note move to 0-indexing
			double alpha= (double) Hs[iNnz];
			double x;
			if (debug) System.out.println("Handling observation "+i);
			if (i<obsdescr.sshb.length) {
				if (debug) System.out.println("sshb observation");
				    int jCorr=j-offset_sshb;
				    //if (debug) System.out.println(iNnz+" "+i+" "+j+" "+jCorr);
				    x=sshb[jCorr];
			}
			else {
				if (debug) System.out.println("tb observation");
				x = tb[j - offset_tb];
			}
			HxAll[i]+=alpha*x;
		}
		int nObs= obsdescr.getCount();
		double [] Hx= new double[nObs];


		int nOk=0;
		if (only_ssh){
			for (int i = 0; i < obsdescr.sshb.length; i++) {
				if (obsdescr.obsOK[i]) {
					Hx[nOk] = HxAll[i];
					nOk++;
				}
			}
		}
		else {
			for (int i = 0; i < HxAll.length; i++) {
				if (obsdescr.obsOK[i]) {
					Hx[nOk] = HxAll[i];
					nOk++;
				}
			}
		}
		if (nOk != Hx.length){
			throw new RuntimeException("Internal error numbver of observations does not match");

		}
		IVector retVec=new org.openda.utils.Vector(Hx);
		return retVec;
	}
}
