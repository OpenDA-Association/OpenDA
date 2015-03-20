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
import org.openda.utils.simple.SimpleSqrtCovariance;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

public class CtaStochObserver extends CtaObject implements StochObserver {

    private CtaObservationDescriptions obsDescr;

    public CtaStochObserver() {
    }

    public void initialize(File workingDir, String[] arguments) {
        String configFile = arguments[0];
        String fullFilePath = new File(workingDir, configFile).getAbsolutePath();
        this.ctaHandle = ctaCreateSQLITE(fullFilePath);
        obsDescr = new CtaObservationDescriptions();
        obsDescr.ctaHandle = ctaGetObservationDescriptions();
    }
    private native int ctaCreateSQLITE(String filename);

    public CtaStochObserver(int ctaHandle) {
        this();
        this.ctaHandle = ctaHandle;
        obsDescr = new CtaObservationDescriptions();
        obsDescr.ctaHandle = ctaGetObservationDescriptions();
    }

    public StochObserver createSelection(String selection) {
        int handle = ctaCreateSelection(selection);
        return new CtaStochObserver(handle);
    }

    public StochObserver createSelection(Time selectionTimes) {
        if (selectionTimes instanceof CtaTime) {
            int ctaTimeSel = ((CtaTime) selectionTimes).ctaHandle;
            int handle = ctaCreateTimeSelection(ctaTimeSel);
            return new CtaStochObserver(handle);

        } else {
            throw new UnsupportedOperationException("org.costa.CtaStochObserver.createSelection not implemented yet for other types of Time");
        }
    }

    public Vector evaluateMarginalPDFs(Vector values) {
        if (values instanceof CtaVector) {
            int ctaValues = ((CtaVector) values).ctaHandle;
            int n = values.getSize();
            CtaVector valuesPDF = new CtaVector(n);
            int ctaValuesPDF = valuesPDF.ctaHandle;

            ctaEvaluatePDF(ctaValues, ctaValuesPDF);

            return valuesPDF;
        } else {
            throw new UnsupportedOperationException("org.costa.CtaStochObserver.createSelection not implemented yet for other types of Time");
        }
    }

    public SqrtCovariance getSqrtCovariance() {
        // We will lie a bit and return a simple (diagonal) matrix
        // Quick and dirty but it works for now
        Vector std=this.getStandardDeviations();
        return new SimpleSqrtCovariance(std);
    }

    public Vector getStandardDeviations() {
        int n = this.getCount();
        CtaVector Std = new CtaVector(n);
        int ctaStd = Std.ctaHandle;
        ctaGetStandardDeviation(ctaStd);
        return Std;
    }

    private native void ctaGetStandardDeviation(int ctaStd);

    public native int getCount();

    public Vector getCovarianceMatrix() {
        throw new UnsupportedOperationException("org.costa.CtaStochObserver.getCovarianceMatrix not implemented yet");
    }

    public Vector getExpectations() {
        int n = this.getCount();
        CtaVector valuesExpec = new CtaVector(n);
        int ctaValuesExpec = valuesExpec.ctaHandle;

        ctaGetExpectations(ctaValuesExpec);

        return valuesExpec;
    }

    public double evaluatePDF(Vector values) {
        throw new UnsupportedOperationException("org.costa.CtaStochObserver.evaluatePDF(): Not implemented yet.");
    }

    private native void ctaGetExpectations(int ctaValuesExpec);

    public ObservationDescriptions getObservationDescriptions() {
        return obsDescr;
    }

    private native int ctaGetObservationDescriptions();

    public Vector getRealizations() {
        int n = this.getCount();
        CtaVector valuesReal = new CtaVector(n);
        int ctaValuesReal = valuesReal.ctaHandle;

        ctaGetRealizations(ctaValuesReal);

        return valuesReal;
    }

    private native void ctaGetRealizations(int ctaValuesReal);

    public Time[] getTimes() {

        double [] timesAsDouble = obsDescr.getValueProperties("TIME").getValues();
        if ( timesAsDouble == null || timesAsDouble.length == 0 ) {
            return new Time[0];
        }

        Arrays.sort(timesAsDouble);
        ArrayList<Double> listOfTime = new ArrayList<Double>();
        listOfTime.add(timesAsDouble[0]);
        for (int i = 1; i < timesAsDouble.length; i++) {
            if ( timesAsDouble[i] > listOfTime.get(listOfTime.size()-1 ) ) {
                listOfTime.add(timesAsDouble[i]);
            }
        }

        Time [] times = new Time[listOfTime.size()];
        for (int i = 0; i < times.length; i++) {
            CtaTime newTime = new CtaTime();
            newTime.setMJD(listOfTime.get(i));
            times[i] = newTime;
        }

        return times;         
    }

    public Vector getValues() {
        int n = this.getCount();
        CtaVector values = new CtaVector(n);
        int ctaValues = values.ctaHandle;

        ctaGetValues(ctaValues);

        return values;
    }

    private native void ctaGetValues(int ctaValues);

    public Vector getVariances() {
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
