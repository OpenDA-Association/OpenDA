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
		return new IVector[0];
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
