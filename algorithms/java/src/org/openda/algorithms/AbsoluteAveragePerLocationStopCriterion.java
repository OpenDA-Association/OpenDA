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
package org.openda.algorithms;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IVector;

import java.util.List;

/**
 * Optional stopping criteria for optimization.
 * The algorithm will stop if the absolute average residual at all locations is smaller than a user defined threshold.
 */
public class AbsoluteAveragePerLocationStopCriterion implements IStopCriterion {
    IVector parameters;
    IVector residuals;
    double cost;
    int nLoc;
    boolean[] isSatisfied;
    String[] Ids;
    private double threshold;
    private double maxAbsAvg;

    public void initialize(IVector initialParameters, IVector initialResiduals, double initialCost) {
        this.parameters = initialParameters;
        this.residuals = initialResiduals;
        this.cost = initialCost;
    }

    public boolean checkForStop(IVector parameters, IVector residuals, double cost, double threshold) {
        // compute absolute average of residuals over all locations:
        boolean isStop = false;
        this.threshold = threshold;
        this.nLoc = 1;
        this.isSatisfied = new boolean[this.nLoc];
        this.Ids = new String[this.nLoc];
        this.Ids[0]="Overall";
        int nData = residuals.getSize();
        double absAvg = 0.0;
        for (int i=0; i<nData; i++){
            absAvg += 1/(double)nData * residuals.getValue(i);
        }
        absAvg = Math.abs(absAvg);

        if (absAvg < threshold) {
            isStop=true;
            this.isSatisfied[0]=true;
        } else {
            this.isSatisfied[0]=false;
        }
        this.maxAbsAvg = absAvg;
        return isStop;
    }

    public boolean checkForStop(IVector parameters, IVector residuals, IObservationDescriptions descriptions, double cost, double threshold) {
        boolean isStop = false;
        this.threshold = threshold;
        List<IPrevExchangeItem> items = descriptions.getExchangeItems();
        this.nLoc = items.size();
        this.isSatisfied = new boolean[this.nLoc];
        this.Ids = new String[this.nLoc];
        int indFirst = 0;
        int indLast = 0;
        double absAvg = 0.0;
        double maxAbsAvg = Double.NEGATIVE_INFINITY;
//        for(IPrevExchangeItem item : items){ // assume the exchangeItems are in the
        for (int i=0; i<nLoc; i++) {
            IPrevExchangeItem item = items.get(i);
            String id = item.getId();
            this.Ids[i] = id.trim();
            int n = 1;
            double times[] = item.getTimes();
            if (times != null) {
                n = times.length;
            }
            indFirst = indLast;
            indLast = indFirst + n;
            absAvg = computeAbsAvgThisId(indFirst,indLast,residuals,id);
            this.isSatisfied[i] = absAvg < threshold;
            maxAbsAvg = Math.max(absAvg,maxAbsAvg);
        }

        this.maxAbsAvg = maxAbsAvg;

        if (maxAbsAvg < threshold) isStop=true;

        return isStop;
    }

    private double computeAbsAvgThisId(int indFirst, int indLast, IVector residuals, String id) {
        // compute absolute average of residuals for a location:
        int nData = indLast - indFirst;
        double absAvg = 0.0d;
        for (int i=indFirst; i<indLast; i++){
            absAvg += 1/(double)nData * residuals.getValue(i);
        }
        absAvg = Math.abs(absAvg);
        return absAvg;
    }

    public String toString() {
        StringBuffer result = new StringBuffer();
        result.append("stop criterion AbsoluteAveragePerLocationStopCriterion: ").append(this.maxAbsAvg).append(" > ").append(this.threshold);
        int nSatisfied = 0;
        for (int i=0; i<this.nLoc; i++){
            if (this.isSatisfied[i]){
                nSatisfied++;
            }
        }
        if (nSatisfied == this.nLoc) {
            result.append("\n\t\tsatisfied at ALL locations!");
        } else if (nSatisfied == 0) {
            result.append("\n\t\tsatisfied at NO locations!");
        } else {
            StringBuffer notSatisfied = new StringBuffer();
            StringBuffer wellSatisfied = new StringBuffer();
            for (int i=0; i<this.nLoc; i++) {
                if (this.isSatisfied[i]){
                    wellSatisfied.append(" ").append(Ids[i]);
                } else {
                    notSatisfied.append(" ").append(Ids[i]);
                }
            }
            result.append("\n\t\tsatisfied at ");
            result.append(wellSatisfied);
            result.append("\n\t\tnot satisfied at ");
            result.append(notSatisfied);
        }
        return result.toString();
    }
}
