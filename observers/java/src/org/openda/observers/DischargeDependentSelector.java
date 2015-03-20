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
package org.openda.observers;
import org.openda.interfaces.ISelector;
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;
import java.util.List;

/**
 * Waterlevel observation selector based on a user specified discharge range.
 */
public class DischargeDependentSelector implements ISelector {
    List<Integer> selectedIndices = null;
    List<Double> selectedTimes = null;
    private boolean[] selectedMask = null;

    public IVector apply(IVector allValues){
        IVector selectedValues = null;

        int nMask = this.selectedMask.length;
        if (nMask != allValues.getSize()){
            throw new RuntimeException("DischargeDependentSelector: number of mask elements are not equal to number of prd-vector elements!");
        }

        if (this.selectedMask != null) {
            int nTrue = 0;
            for (int i=0; i<nMask; i++){
                if (this.selectedMask[i]) nTrue++;
            }
            selectedValues = new Vector(nTrue);
            int j=0;
            for (int i=0; i<nMask; i++){
                if (this.selectedMask[i]) {
                    selectedValues.setValue(j,allValues.getValue(i));
                    j++;
                }
            }
        } else {
            throw new RuntimeException("DischargeDependentSelector is not yet initialized.");
        }
        return selectedValues;
    }

//    public IStochObserver createSelection(IStochObserver inpObserver, TimeSeries[] inpVar, double minVal, double maxVal){
//        List<IPrevExchangeItem> items = inpObserver.getObservationDescriptions().getExchangeItems();
//        TimeSeries[] selectedTimeSeries = new TimeSeries[items.size()];
//        int k=0;
//        for (IPrevExchangeItem item : items) {
//            double[] values = item.getValuesAsDoubles();
//            double[] times = item.getTimes();
//            List<Double> newValues = new ArrayList<Double>();
//            List<Double> newTimes = new ArrayList<Double>();
//            int valLength = values.length;
//            int j=0;
//            for (int i=0; i<valLength; i++){
//                double thisInpValue = inpVar[k].getValue(times[i]);
//                if (thisInpValue>minVal && thisInpValue<maxVal){
//                    newValues.add(j,thisInpValue);
//                    newTimes.add(j,times[i]);
//                    j++;
//                }
//            }
//            double[] selectedTimes = new double[newTimes.size()];
//            double[] selectedValues = new double[newTimes.size()];
//            for (int i=0; i<newTimes.size(); i++){
//                selectedTimes[i] = newTimes.get(i);
//                selectedValues[i] = newValues.get(i);
//            }
//            item.setTimes(selectedTimes);
//            item.setValuesAsDoubles(selectedValues);
//            selectedTimeSeries[k] = new TimeSeries(selectedTimes,selectedValues);
//            selectedTimeSeries[k] = inpVar[k].selectValueSubset(minVal,maxVal);
//            selectedTimeSeries[k].getTimes();
//            k++;
//        }
//        NoosTimeSeriesStochObserver selectedStochObserver = new NoosTimeSeriesStochObserver();
//        selectedStochObserver.setSeries(selectedTimeSeries);
//        return selectedStochObserver;
//    }

//    public ISelector createSelector(TimeSeries inpVar, double minVal, double maxVal){
////        assume that input and output timeseries consists of the same time levels.
//        if (selectedIndices==null) {
//            selectedIndices = new ArrayList<Integer>();
//            selectedTimes = new ArrayList<Double>();
//        }
//        double[] times = inpVar.getTimes();
//        int j=0;
//        for (int i=0; i<times.length; i++){
//            double thisInpValue = inpVar.getValue(times[i]);
//            if (thisInpValue>minVal && thisInpValue<maxVal){
//                selectedTimes.add(j,times[i]);
//                selectedIndices.add(j,i);
//                j++;
//            }
//        }
//        return this;
//    }

    public void setMask(boolean[] selectedMask) {
        this.selectedMask = selectedMask;
    }
}
