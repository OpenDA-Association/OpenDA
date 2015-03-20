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

package org.costa;

import org.openda.interfaces.*;
import org.openda.utils.simple.SimpleStochVector;

import java.io.File;

public class CtaOpenDaModel extends CtaObject implements StochModelInstance{

	CtaOpenDaModel(String ModelCls, String Filename){

        // TEMPORARILY create a default simulation span (time horizon), and set initial time of the model

        // Create the model
        if (! new File(Filename).exists()){
            throw new RuntimeException("File:"+Filename+" does not exist");
        }

        this.ctaHandle=ctaCreate(ModelCls, Filename);
	}

	private native int ctaCreate(String ModelCls, String filename);

	public void announceObservedValues(
			ObservationDescriptions observationDescriptions) {

		if (observationDescriptions instanceof CtaObservationDescriptions ){
			ctaAnnounceObservedValues(observationDescriptions);
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.announceObservedValues not implemented yet for other types");
		}
	}

	private native void ctaAnnounceObservedValues(
			ObservationDescriptions observationDescriptions);

	public void axpyOnParameters(double alpha, Vector vector) {

		if (vector instanceof CtaTreeVector ){
			ctaAxpyOnParameters(alpha, vector);
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.axpyOnParameters not implemented yet for other types");
		}
	}

	private native void ctaAxpyOnParameters(double alpha, Vector vector);

	public void axpyOnState(double alpha, Vector vector) {
		// TODO Auto-generated method stub
		if (vector instanceof CtaTreeVector ){
			ctaAxpyOnState(alpha, vector);
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.axpyOnState not implemented yet for other types");
		}
	}

	private native void ctaAxpyOnState(double alpha, Vector vector);

	public void axpyOnWhiteNoise(double alpha, Vector[] vector) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.axpyOnWhiteNoise(): Not implemented yet.");
	}

	public void compute(Time targetTime) {
       // Convert time to avoid problems
        CtaTime ctaTarget= new CtaTime();
        ctaTarget.setMJD(targetTime.getMJD());
        // Do timestep(s)
        ctaCompute(ctaTarget);
    }

	private native void ctaCompute(Time targetTime);

	public native Time getCurrentTime();

	public String[] getExchangeItemIDs() {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getExchangeItemIDs(): Not implemented yet.");
	}

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        if (role == IPrevExchangeItem.Role.InOut) {
            return getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getExchangeItemIDs(): Role selection not implemented yet.");
    }

    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getExchangeItem(): Not implemented yet.");
    }

    public Vector getObservedValues(
			ObservationDescriptions observationDescriptions) {
		if (observationDescriptions instanceof CtaObservationDescriptions ){
            int n=observationDescriptions.getObservationCount();
            CtaVector values = new CtaVector(n);

			ctaGetObservedValues(observationDescriptions, values);
			return values;
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getObservedValues not implemented yet for other types");
		}
	}

	private native void ctaGetObservedValues(
			ObservationDescriptions observationDescriptions, CtaVector values);

	public StochVector getParameterUncertainty() {
        Vector params=this.getParameters();
        Vector std=params.clone();
        int npar=params.getSize();
        for (int i=0; i<npar; i++){
           std.setValue(i,1.0);
        }
        SimpleStochVector paramUncertainty= new SimpleStochVector(params,std);
        return paramUncertainty;
    }

	public native Vector getParameters();

	public native Vector getState();

	public native Vector getStateScaling();

	public Vector[] getStateScaling(
			ObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getStateScaling(): Not implemented yet.");
	}

	public StochVector getStateUncertainty() {
        System.err.println(this.getClass().getSimpleName() + ".getStateUncertainty(): Note: NOT IMPLEMENTED, returns null");
        return null;
    }

	public native Time getTimeHorizon();


	public Vector[] getWhiteNoise(Time timeSpan) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getWhiteNoise(): Not implemented yet.");
	}

	public Time[] getWhiteNoiseTimes(Time timeSpan) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getWhiteNoiseTimes(): Not implemented yet.");
	}

	public StochVector[] getWhiteNoiseUncertainty(Time time) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.getWhiteNoiseUncertainty(): Not implemented yet.");
	}

	public boolean isWhiteNoiseStationary() {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.isWhiteNoiseStationary(): Not implemented yet.");
	}

	public native void releaseInternalState(Object savedInternalState);

	public native Object saveInternalState();

	public native void restoreInternalState(Object savedInternalState);

	public native void setAutomaticNoiseGeneration(boolean value);

	public void setParameters(Vector parameters) {
		if (parameters instanceof CtaTreeVector ){
 			ctaSetParameters(parameters);
		}
		else {
			throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.setParameters not implemented yet for other types");
		}
	}

	private native void ctaSetParameters(Vector parameters);

	public void setWhiteNoise(Vector[] whiteNoise) {
		throw new UnsupportedOperationException("org.costa.CtaOpenDaModel.setWhiteNoise(): Not implemented yet.");
	}

    public File getModelRunDir() {
        return null;
    }
}
