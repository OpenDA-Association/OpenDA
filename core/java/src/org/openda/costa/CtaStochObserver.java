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

package org.openda.costa;

import org.openda.interfaces.*;
import org.openda.utils.IdentitySelector;
import org.openda.utils.SqrtCovariance;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

public class CtaStochObserver extends CtaObject implements IStochObserver {

    private CtaObservationDescriptions obsDescr;

    public CtaStochObserver() {
    }

    public void initialize(File workingDir, String[] arguments) {
		String[] argumentParts = arguments[0].split(";");

		obsDescr = null;

        /* Check number of arguments */
		if (argumentParts.length==1){
           /* Compatability mode */
           String configFile = argumentParts[0];
           String fullFilePath = new File(workingDir, configFile).getAbsolutePath();
           this.ctaHandle = ctaCreateNative("SQLITE",fullFilePath);
		}
		else if (argumentParts.length==2){
			/* first argument is the observer type and the second is the input */
		   String ObserverType = argumentParts[0].toUpperCase();
           String configFile   = argumentParts[1];
           String fullFilePath = new File(workingDir, configFile).getAbsolutePath();
           this.ctaHandle = ctaCreateNative(ObserverType,fullFilePath);
		}
		else {
			/* Error: Number of arguments is wrong */
			throw new RuntimeException("Number of arguments must be 1 or 2 but "+argumentParts.length+" arguments are specified: "+arguments);
		}
    }
    private native int ctaCreateNative(String type, String filename);

    public CtaStochObserver(int ctaHandle) {
        this();
        this.ctaHandle = ctaHandle;
        obsDescr = null;
        //obsDescr = new CtaObservationDescriptions();
        //obsDescr.ctaHandle = ctaGetObservationDescriptions();
    }

    public IStochObserver createSelection(String selection) {
        int handle = ctaCreateSelection(selection);
        return new CtaStochObserver(handle);
    }

    public IStochObserver createSelection(ITime selectionTimes) {
    	if (!(selectionTimes instanceof CtaTime)) {
    		selectionTimes = new CtaTime(selectionTimes);
    	}
    	int ctaTimeSel = ((CtaTime) selectionTimes).ctaHandle;
    	int handle = ctaCreateTimeSelection(ctaTimeSel);
    	return new CtaStochObserver(handle);
    }

	public IStochObserver createSelection(Type observationType) {
		if (observationType == Type.Assimilation) {
			return this;
		}
		throw new UnsupportedOperationException("org.openda.costa.CtaStochObserver.createSelection(): Not implemented yet.");
	}

    public ISelector createSelector(Type observationType) {
        return new IdentitySelector();        
    }

    public IVector evaluateMarginalPDFs(IVector values) {
        if (values instanceof CtaVector) {
            int ctaValues = ((CtaVector) values).ctaHandle;
            int n = values.getSize();
            CtaVector valuesPDF = new CtaVector(n);
            int ctaValuesPDF = valuesPDF.ctaHandle;

            ctaEvaluatePDF(ctaValues, ctaValuesPDF);

            return valuesPDF;
        } else {
            throw new UnsupportedOperationException("org.costa.CtaStochObserver.createSelection not implemented yet for other types of ITime");
        }
    }

    public ISqrtCovariance getSqrtCovariance() {
        // We will lie a bit and return a simple (diagonal) matrix
        // Quick and dirty but it works for now
        IVector std=this.getStandardDeviations();
        return new SqrtCovariance(std);
    }

    public IVector getStandardDeviations() {
        int n = this.getCount();
        CtaVector Std = new CtaVector(n);
        int ctaStd = Std.ctaHandle;
        ctaGetStandardDeviation(ctaStd);
        return Std;
    }

    private native void ctaGetStandardDeviation(int ctaStd);

    public native int getCount();

    public IVector getCovarianceMatrix() {
        throw new UnsupportedOperationException("org.costa.CtaStochObserver.getCovarianceMatrix not implemented yet");
    }

    public IVector getExpectations() {
        int n = this.getCount();
        CtaVector valuesExpec = new CtaVector(n);
        int ctaValuesExpec = valuesExpec.ctaHandle;

        ctaGetExpectations(ctaValuesExpec);

        return valuesExpec;
    }

    public double evaluatePDF(IVector values) {
        throw new UnsupportedOperationException("org.costa.CtaStochObserver.evaluatePDF(): Not implemented yet.");
    }

    private native void ctaGetExpectations(int ctaValuesExpec);

    public IObservationDescriptions getObservationDescriptions() {
        if (obsDescr == null){
			obsDescr = new CtaObservationDescriptions();
			obsDescr.ctaHandle = ctaGetObservationDescriptions();
		}
		return obsDescr;
    }

    private native int ctaGetObservationDescriptions();

    public IVector getRealizations() {
        int n = this.getCount();
        CtaVector valuesReal = new CtaVector(n);
        int ctaValuesReal = valuesReal.ctaHandle;

        ctaGetRealizations(ctaValuesReal);

        return valuesReal;
    }

    private native void ctaGetRealizations(int ctaValuesReal);

    public ITime[] getTimes() {

		if (obsDescr == null){
			obsDescr = new CtaObservationDescriptions();
			obsDescr.ctaHandle = ctaGetObservationDescriptions();
		}
        double [] timesAsDouble = obsDescr.getValueProperties("TIME").getValues();
        if ( timesAsDouble == null || timesAsDouble.length == 0 ) {
            return new ITime[0];
        }

        Arrays.sort(timesAsDouble);
        ArrayList<Double> listOfTime = new ArrayList<Double>();
        listOfTime.add(timesAsDouble[0]);
        for (int i = 1; i < timesAsDouble.length; i++) {
            if ( timesAsDouble[i] > listOfTime.get(listOfTime.size()-1 ) ) {
                listOfTime.add(timesAsDouble[i]);
            }
        }

        ITime[] times = new ITime[listOfTime.size()];
        for (int i = 0; i < times.length; i++) {
            CtaTime newTime = new CtaTime();
            newTime.setMJD(listOfTime.get(i));
            times[i] = newTime;
        }

        return times;
    }

    public IVector getValues() {
        int n = this.getCount();
        CtaVector values = new CtaVector(n);
        int ctaValues = values.ctaHandle;

        ctaGetValues(ctaValues);

        return values;
    }

    private native void ctaGetValues(int ctaValues);

    public IVector getVariances() {
        int n = this.getCount();
        CtaVector valuesVar = new CtaVector(n);
        int ctaValuesVar = valuesVar.ctaHandle;

        ctaGetVariances(ctaValuesVar);

        return valuesVar;
    }

    private native void ctaGetVariances(int ctaValuesVar);

    private native int ctaCreateSelection(String selection);

    private native int ctaCreateTimeSelection(int ctaTimeSel);

    private native void ctaEvaluatePDF(int ctaValues, int ctaValuesPDF);


}
