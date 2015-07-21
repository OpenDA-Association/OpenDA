/* V2.2
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
package org.openda.model_RainfallRunoffZhang;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.observers.DischargeDependentSelector;
import org.openda.utils.*;
import org.openda.utils.Vector;

import java.io.File;
import java.util.*;

/**
 * Implementation of <code>IStochObserver</code> for data consisting of several timeseries.
 * The data is kept in memory,so the dataset should be of limited size.
 * All observations are assumed to be uncorrelated
 * All observation have normally distributed (Gaussian) errors
 * The property <code>standardDeviation</code> of each TimeSeries gives the spread of the errors
 * <p>
 * This is a slight modification of NOOS_TimeSeriesStochObserver by verlanm. 
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class ASCII_TimeSeriesStochObserver extends Instance implements IStochObserver {
	// all data in array of TimeSeries
	TimeSeries series[]=null;
	// tolerance for two times to be different or the same
	double tolerance = 1.0/24.0/60.0/2.0; // 0.5 minute
	// cache propertyLabels
	java.util.Vector<String> propertyLabels = new java.util.Vector<String>();
	private List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>();
    private HashMap<String,Integer> allObsIds = new HashMap<String,Integer>();


	/**
	 * Generic constructor.
	 */
	public ASCII_TimeSeriesStochObserver(){
		// Empty constructor to keep java happy
	}

	/**
	 * Generic constructor using a time series.
	 * @param series Time series for this observer.
	 */
	public ASCII_TimeSeriesStochObserver(TimeSeries series[]){
		this.setSeries(series);
	}

	/**
	 * Initialize can only be called for derived classes.
	 */
	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException(this.getClass().getName() + ".initialize can only be called for derived classes");
	}

	/**
	 * Save some property names to a list of time series. These properties are: location, quantity, source, unit, height, xposition, yposition, and id. 
	 * @param series[] A list of time series.
	 */
	public void setSeries(TimeSeries series[]){
		this.series = series;
		if (series != null) {
			//
			// cache labels of properties
			//
			// some labels always exist for a timeseries, although they may be empty
			this.propertyLabels.add("location");
			this.propertyLabels.add("quantity");
			this.propertyLabels.add("source");
			this.propertyLabels.add("unit");
			this.propertyLabels.add("height");
			this.propertyLabels.add("xposition");
			this.propertyLabels.add("yposition");
			this.propertyLabels.add("id");
			// look for others
			for (TimeSeries serie : series) {
				String[] propNames = serie.getPropertyNames();
				for (String propName : propNames) {
					if (!this.propertyLabels.contains(propName.toLowerCase())) {
						this.propertyLabels.add(propName.toLowerCase());
					}
				}
			}
			exchangeItems.addAll(Arrays.asList(series));
		}
	}

	/**
	 * Not implemented.
	 */
	public IStochObserver createSelection(String selection) {
		throw new RuntimeException("createSelection not implemented yet");
		//return null;
	}

	/**
	 * Returns an <code>IStochObserver</code> with shortened time series with begin and end time specified in <code>selectionTimes</code>
	 * @param selectionTimes A ITime-type specifying begin and end time of the selection.
	 */
	public IStochObserver createSelection(ITime selectionTimes) {
		// if observer is already empty, return an empty observer
		if (series == null){
			return new ASCII_TimeSeriesStochObserver();
		}
		TimeSeries selectedSeries[] = new TimeSeries[this.series.length];
		TimeSeries selectedSeriesCleaned[]=null;
		for(int i=0;i<this.series.length;i++){
			selectedSeries[i] = this.series[i].selectTimeSubset(selectionTimes.getBeginTime().getMJD(),
					selectionTimes.getEndTime().getMJD());
            if(selectedSeries[i]==null){
				throw new RuntimeException("TimeSeriesStochObserver: Selection of series nr. "+i+" is empty!");
			}
		}
		// Now clean empty selections
		int nSeries=0;
		for (int i=0;i<selectedSeries.length;i++){
			if (selectedSeries[i].getValuesRef()!=null) {
				nSeries++;
			}
		}
		if (nSeries>0){
			int iSeries=0;
			selectedSeriesCleaned = new TimeSeries[nSeries];
			for (int i=0;i<selectedSeries.length;i++){
				if (selectedSeries[i].getValuesRef()!=null) {
					selectedSeriesCleaned[iSeries]=selectedSeries[i];
					iSeries++;
				}
			}
		}
		return new ASCII_TimeSeriesStochObserver(selectedSeriesCleaned);
	}

	public IStochObserver createSelection(Type observationType) {
		// if observer is already empty, return an empty observer
		if (series == null){
			return new ASCII_TimeSeriesStochObserver();
		}
        List<TimeSeries> selectedSeries = new ArrayList<TimeSeries>();
        for (TimeSeries serie : series) {
            String status = serie.getProperty("status");
            if (status != null) {
                if (observationType.toString().toLowerCase().contentEquals(status.toLowerCase())) {
                    selectedSeries.add(serie);
                }
            }
        }

        TimeSeries arrSelectedSeries[] = selectedSeries.toArray(new TimeSeries[selectedSeries.size()]);
        return new ASCII_TimeSeriesStochObserver(arrSelectedSeries);
	}

    public ISelector createSelector(Type observationType) {
        if (series==null) return null;
        IndicesSelector indSelector = new IndicesSelector();
        int indLast=0;
        int indFirst;
        for (TimeSeries serie : series) {
            String status = serie.getProperty("status");
            indFirst = indLast;
            indLast += serie.getSize();
            if (status != null) {
                if (observationType.toString().toLowerCase().contentEquals(status.toLowerCase())) {
                    indSelector.addIndexRange(indFirst, indLast);
                }
            }
        }
        return indSelector;
    }

    private TimeSeries[] setObsValidation(IVector prdValidation){
		if (series==null) return null;
        int totalSizeTimeSeries = 0;
        for (TimeSeries serie : series){
            totalSizeTimeSeries += serie.getSize();
        }
        if (prdValidation.getSize() != totalSizeTimeSeries){
            throw new RuntimeException("The size of observation and prediction data is not the same.");
        }
        TimeSeries selectedSeries[] = new TimeSeries[this.series.length];
        int indFirst;
        int indLast = 0;
        int j=0;
        for (TimeSeries serie : series){
            String obsStatus = serie.getProperty("status");
            indFirst = indLast;
            indLast += serie.getSize();
            selectedSeries[j] = serie;
            if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())){
                int serieSize = serie.getSize();
                double[] values = new double[serieSize];
                for (int i=0; i<serieSize; i++){
                    values[i] = prdValidation.getValue(indFirst+i);
                }
                selectedSeries[j].setValuesAsDoubles(values);
            }
            allObsIds.put(serie.getId(),j);
            j++;
        }
        return selectedSeries;
    }

    private boolean[] getMaskSelectedIndices(TimeSeries serie, double minVal, double maxVal) {
        boolean[] selectedIndices = new boolean[serie.getSize()];
        double[] values = serie.getValuesAsDoubles();
        int nValues = values.length;
        for (int i=0; i<nValues; i++){
            selectedIndices[i] = values[i] >= minVal && values[i] <= maxVal;
        }
        return selectedIndices;
    }

    public IStochObserver createSelection(IVector prdValidation, double minVal, double maxVal) {
		// if observer is already empty, return an empty observer
		if (series == null){
			return new ASCII_TimeSeriesStochObserver();
		}
        HashMap<String,Integer> obsStatusLocation = new HashMap<String,Integer>();
        TimeSeries[] selectedSeries = setObsValidation(prdValidation);
        int nSeries = selectedSeries.length;
        List<Integer> assimLoopStep = new ArrayList<Integer>();
        int j=0;
        for (int i=0; i<nSeries; i++){
            TimeSeries serie = selectedSeries[i];
            String obsStatus = serie.getProperty("status");
            String obsLocation = serie.getLocation();
            obsStatusLocation.put(obsStatus+obsLocation,i);
            if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())){
                selectedSeries[i] = serie.selectValueSubset(minVal,maxVal);
            } else {
                assimLoopStep.add(j,i);
                j++;
            }
        }
        String obsStatus = Type.Validation.toString().toLowerCase();
        for (int i=0; i<j; i++){
            TimeSeries serie = selectedSeries[assimLoopStep.get(i)];
            String obsLocation = serie.getLocation();
            int iOrder = obsStatusLocation.get(obsStatus+obsLocation);
            TimeSeries serieValidation = selectedSeries[iOrder];
            double[] timeThisObs = serieValidation.getTimes();
            selectedSeries[assimLoopStep.get(i)] = serie.selectTimeSubset(timeThisObs);
        }
        return new ASCII_TimeSeriesStochObserver(selectedSeries);
    }

    public ISelector createSelector(IVector prdValidation, double minVal, double maxVal) {
        if (series==null) return null;

		DischargeDependentSelector indSelector = new DischargeDependentSelector();
        TimeSeries[] selectedSeries = setObsValidation(prdValidation);
        HashMap<String,boolean[]> obsSelectedElements = new HashMap<String,boolean[]>();
        int nSeries = selectedSeries.length;
		for (TimeSeries serie : selectedSeries) {
			String obsStatus = serie.getProperty("status");
			String obsLocation = serie.getLocation();
			if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())) {
				boolean[] maskSelectedIndices = getMaskSelectedIndices(serie, minVal, maxVal);
				obsSelectedElements.put(obsStatus + obsLocation, maskSelectedIndices);
				obsSelectedElements.put(Type.Assimilation.toString().toLowerCase() + obsLocation, maskSelectedIndices);
			}
		}

        boolean[] outMask = new boolean[this.getCount()];
        int nextindex = 0;
        for (TimeSeries serie : series) {
            String obsStatus = serie.getProperty("status");
            String obsLocation = serie.getLocation();
            boolean[] thisSerieMask = obsSelectedElements.get(obsStatus+obsLocation);
            int seriesLength = serie.getSize();
            System.arraycopy(thisSerieMask, 0, outMask, nextindex, seriesLength);
            nextindex += seriesLength;
        }
        indSelector.setMask(outMask);
        return indSelector;
    }

    public IStochObserver createSelection(HashMap pairsValidAssim, IVector prdValidation, HashMap<String, Double> dischargeAndMinVal, HashMap<String, Double> dischargeAndMaxVal) {
		// if observer is already empty, return an empty observer
		if (series == null){
			return new ASCII_TimeSeriesStochObserver();
		}
            checkIfPairsAreComplete(pairsValidAssim);
            TimeSeries[] selectedSeries = setObsValidation(prdValidation);
            int nSeries = selectedSeries.length;
            for (int i=0; i<nSeries; i++){
                TimeSeries serie = selectedSeries[i];
                String obsStatus = serie.getProperty("status");
                if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())){
                    String validId = serie.getId();
                    String assimId = (String) pairsValidAssim.get(validId);
                    double minVal = dischargeAndMinVal.get(validId);
                    double maxVal = dischargeAndMaxVal.get(validId);
                    selectedSeries[i] = serie.selectValueSubset(minVal,maxVal);
                    double[] timeThisObs = selectedSeries[i].getTimes();
                    int indexAssim;
                    try {
                       indexAssim = allObsIds.get(assimId);
                    } catch(Exception e){
                       throw new RuntimeException("Invalid ID of one or more validation and assimilation stations");
                    }
                    TimeSeries thisAssim = selectedSeries[indexAssim].selectTimeSubset(timeThisObs);
                    selectedSeries[indexAssim] = thisAssim;
                }
            }
            return new ASCII_TimeSeriesStochObserver(selectedSeries);
        }

    public ISelector createSelector(HashMap<String,String> pairsValidAssim, IVector prdValidation, HashMap<String, Double> dischargeAndMinVal, HashMap<String, Double> dischargeAndMaxVal) {
		if (series==null) return null;
		checkIfPairsAreComplete(pairsValidAssim);
		DischargeDependentSelector indSelector = new DischargeDependentSelector();
        TimeSeries[] selectedSeries = setObsValidation(prdValidation);
        HashMap<String,boolean[]> obsSelectedElements = new HashMap<String,boolean[]>();
		for (TimeSeries serie : selectedSeries) {
			String obsStatus = serie.getProperty("status");
			if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())) {
				String validId = serie.getId();
				String assimId = pairsValidAssim.get(validId);
				double minVal = dischargeAndMinVal.get(validId);
				double maxVal = dischargeAndMaxVal.get(validId);
				boolean[] maskSelectedIndices = getMaskSelectedIndices(serie, minVal, maxVal);
				obsSelectedElements.put(validId, maskSelectedIndices);
				obsSelectedElements.put(assimId, maskSelectedIndices);
			}
		}

        boolean[] outMask = new boolean[this.getCount()];
        for (int i=0; i<outMask.length; i++){
            outMask[i] = true;
        }

        for (TimeSeries serie : series) {
            String obsId = serie.getId();
            boolean[] thisSerieMask = obsSelectedElements.get(obsId);
			if (thisSerieMask == null) {
				throw new RuntimeException("No series mask available for " + obsId);
			}
            int indexObs = allObsIds.get(obsId);
            int nextindex = 0;
            for (int i=0; i<indexObs; i++){
                nextindex += selectedSeries[i].getSize();
            }
            int seriesLength = serie.getSize();
            System.arraycopy(thisSerieMask, 0, outMask, nextindex, seriesLength);
        }
        indSelector.setMask(outMask);
        return indSelector;
    }

    public IVector evaluateMarginalPDFs(IVector values) {
		IVector mean = this.getExpectations();
		IVector std = this.getStandardDeviations();
		StochVector sv = new StochVector(mean,std);
		return sv.evaluateMarginalPDFs(values);
	}

	public double evaluatePDF(IVector values) {
		IVector mean = this.getExpectations();
		IVector std = this.getStandardDeviations();
		IStochVector sv = new StochVector(mean,std);
		return sv.evaluatePdf(values);
	}

	public void free() {
		// nothing to do

	}

	public int getCount() {
		int count=0;
        if (this.series != null){
			for (TimeSeries sery : this.series) {
				if(sery.getTimesRef()!=null){
					count += sery.getTimesRef().length;
				}
			}
		}
		return count;
	}

	public IVector getExpectations() {
		if (series==null) return null;
		int n = this.getCount();
		int nextindex = 0;
		int seriesLength;
		double values[] = new double[n];
		double tempValues[] = null;
		for (TimeSeries sery : this.series) {
			tempValues = sery.getValuesRef();
			if(tempValues!=null){
				seriesLength = tempValues.length;
				System.arraycopy(sery.getValuesRef(), 0, values, nextindex, seriesLength);
				nextindex += seriesLength;
			}
		}
		return new Vector(values);
	}

	public IObservationDescriptions getObservationDescriptions() {
		return new ASCIITimeSeriesObservationDescriptions(this);
	}

	public IVector getRealizations() {
		IVector mean = this.getExpectations();
		IVector std = this.getStandardDeviations();
		IStochVector sv = new StochVector(mean,std);
		return sv.createRealization();
	}

	public ISqrtCovariance getSqrtCovariance() {
		IVector mean = this.getExpectations();
		IVector std = this.getStandardDeviations();
		IStochVector sv = new StochVector(mean,std);
		return sv.getSqrtCovariance();
	}

	public IVector getStandardDeviations() {
		int n = this.getCount();
		double values[] = new double[n];
		int nextindex = 0;
		int seriesLength;
		double stdSeries;
		double absStd;
		double relStd;
		for(int i=0;i<this.series.length;i++){
			double[] valuePart = this.series[i].getValuesRef();
			if(valuePart!=null){
				seriesLength = valuePart.length;
				absStd    = this.series[i].getDoubleProperty("standarddeviation",-1.0);
				if(absStd<0.){
					throw new RuntimeException("No standardDeviation was defined for series nr. "+i+"(first series has index 0)");
				}
				relStd    = this.series[i].getDoubleProperty("relativeStandardDeviation", 0.);
				
				// set values
				for(int j=0;j<seriesLength;j++){
					values[nextindex] = absStd + relStd*Math.abs(valuePart[j]);
					nextindex++;
				}
			}
		}
		return new Vector(values);
	}

	public ITime[] getTimes() {
		if (series==null) return null;
		int n = this.getCount();
		int nextindex = 0;
		int seriesLength;
		double unsortedTimes[] = new double[n];
		for (TimeSeries sery : this.series) {
			if(sery.getTimesRef()!=null){
				seriesLength = sery.getTimesRef().length;
				System.arraycopy(sery.getTimesRef(), 0, unsortedTimes, nextindex, seriesLength);
			}else{
				seriesLength=0;
			}
			nextindex += seriesLength;
		}
		//now sort times
		TreeSet<Double> temp = new TreeSet<Double>();
		for (double unsortedTime : unsortedTimes) {
			temp.add(unsortedTime);
		}
		java.util.Vector<Double> sorted = new java.util.Vector<Double>();
		java.util.Iterator<Double> it = temp.iterator();
		double previous=0.0;
		if(it.hasNext()){
			double current = it.next();
			sorted.add(current);
			previous = current;
		}
		while(it.hasNext()){
			double current = it.next();
			if(current>previous+this.tolerance){ //skip if too close
				sorted.add(current);
				previous=current;
			}
		}
		// convert from internal double representation to Time
		ITime[] result = new ITime[sorted.size()];
		for(int i=0;i<sorted.size();i++){
			Time simpleTime = new Time(sorted.get(i) - this.tolerance, sorted.get(i) + this.tolerance);
			simpleTime.setStep(this.tolerance);
			result[i] = simpleTime;
		}
		return result;
	}

	public IVector getValues() {
		return getExpectations();
	}

	/**
	 * Get a vector of times matching each value.
	 * @return IVector containing times.
	 */
	public IVector getAllTimes() {
		if (series==null) return null;
		int n = this.getCount();
		int nextindex = 0;
		int seriesLength;
		double times[] = new double[n];
		double tempTimes[] = null;
		for (TimeSeries sery : this.series) {
			tempTimes = sery.getTimesRef();
			if(tempTimes!=null){
				seriesLength = tempTimes.length;
				System.arraycopy(sery.getTimesRef(), 0, times, nextindex, seriesLength);
				nextindex += seriesLength;
			}
		}
		return new Vector(times);
	}

	public String toString(){
		String result ="StochObserver(\n";
		if (this.series != null) {
			for (TimeSeries sery : this.series) {
				result += sery.toString() + "\n";
			}
		} else {
			result += "<empty>";
		}
		result+=");";
		return result;
	}

	/*
	 * =====================================================================================================
	 * The methods below are not part of the IStochObserver interface, but are used by the
	 * TimeSeriesObservationDescriptions
	 * =====================================================================================================
	 */

	/** Get properties (values) that correspond to a given key.
	 * Returns null if property does not exist and throws a RuntimeException if
	 * the property can not be represented as double's.
	 * @param Key        I  key for which the value is asked
	 * @return Properties (column of data from observation descriptions)
	 */
	public IVector getValueProperties(String Key){
        if (series==null) return null;
		int nCount  = this.getCount();
		int nSeries = this.series.length;
		IVector result = null;
		double seriesValue;
		int nextIndex = 0;
		if(this.propertyLabels.contains(Key.toLowerCase())){
			double values[] = new double[nCount];
			for(int i=0;i<nSeries;i++){
				seriesValue = Double.NaN;
				if(Key.equalsIgnoreCase("Location")){
					throw new RuntimeException("Location is a string property");
				}
				if(Key.equalsIgnoreCase("Quantity")){
					throw new RuntimeException("Quantity is a string property");
				}
				if(Key.equalsIgnoreCase("Source")){
					throw new RuntimeException("Source is a string property");
				}
				if(Key.equalsIgnoreCase("Unit")){
					throw new RuntimeException("Unit is a string property");
				}
				if(Key.equalsIgnoreCase("Height")){
					seriesValue = this.series[i].getHeight();
				}
				if(Key.equalsIgnoreCase("xPosition")){
					double temp[] = this.series[i].getPosition();
					seriesValue   = temp[0];
				}
				if(Key.equalsIgnoreCase("yPosition")){
					double temp[] = this.series[i].getPosition();
					seriesValue   = temp[1];
				}
				if(this.series[i].getProperty(Key)!=null){
					String temp = this.series[i].getProperty(Key.toLowerCase());
					try {
						seriesValue = Double.parseDouble(temp);
					} catch (Exception e) {
						throw new RuntimeException("Could not parse value as double for series no."+i+
								". Value to parse was "+temp);
					}
				}
				int seriesLength = 0;
				if(this.series[i].getTimesRef()!=null){
					seriesLength = this.series[i].getTimesRef().length;
				}
				for(int j=0;j<seriesLength;j++){
					values[nextIndex] = seriesValue;
					nextIndex++;
				}
			}
			result = new Vector(values);
		}//
		return result;
	}

	/** Get properties (strings) that correspond to a given key.
	 * Returns null if the property does not exist
	 * @param key        I  key for which the value is asked
	 * @return Properties (column of data from observation descriptions)
	 */
	public String[] getStringProperties(String key){
		if (series==null) return null;
		int nCount  = this.getCount();
		int nSeries = this.series.length;
		String result[] = null;
		int nextIndex = 0;
		if(this.propertyLabels.contains(key.toLowerCase()) || key.equalsIgnoreCase("id")){
			result = new String[nCount];
			for(int i=0;i<nSeries;i++){
				String seriesValue;
				if(key.equalsIgnoreCase("Location")){
					seriesValue = this.series[i].getLocation();
				}
				else if(key.equalsIgnoreCase("Quantity")){
					seriesValue = this.series[i].getQuantityId();
				}
				else if(key.equalsIgnoreCase("Source")){
					seriesValue = this.series[i].getSource();
				}
				else if(key.equalsIgnoreCase("Unit")){
					seriesValue = this.series[i].getUnitId();
				}
				else if(key.equalsIgnoreCase("Id")){
					seriesValue = this.series[i].getId();
				}
				else if(key.equalsIgnoreCase("Height")){
					seriesValue = ""+this.series[i].getHeight();
				}
				else if(key.equalsIgnoreCase("xPosition")){
					double temp[] = this.series[i].getPosition();
					seriesValue   = ""+temp[0];
				}
				else if(key.equalsIgnoreCase("yPosition")){
					double temp[] = this.series[i].getPosition();
					seriesValue   = ""+temp[1];
				}
				else if(this.series[i].getProperty(key)!=null){
					seriesValue = this.series[i].getProperty(key);
				}
				else if (this.series[i].getTimesRef()==null){
					// key not found but serie for this station is empty anyway so it is not important
					seriesValue = null;
				} else  {
					throw new RuntimeException("Unknown key in getStringProperties: " + key);
				}
				double times[] = this.series[i].getTimesRef();
				if(times!=null){
					int seriesLength = times.length;
					for(int j=0;j<seriesLength;j++){
						result[nextIndex] = seriesValue;
						nextIndex++;
					}
				}
			} // loop over series
		} else { // end of if propertylabel was known
			if (key.equalsIgnoreCase("id")) {
				if(this.propertyLabels.contains("quantity") && this.propertyLabels.contains("location")){

				} else if(this.propertyLabels.contains("location")) {

				} else if(this.propertyLabels.contains("quantity")) {

				}
			}
		}
		return result;
	}

	/** Get names of all keys.
	 *
	 * @return error status: All keys of the observation descriptions
	 */
	public String[] getPropertyKeys(){
		String result[] = new String[this.propertyLabels.size()];
		for(int i=0;i<this.propertyLabels.size();i++){
			result[i] = this.propertyLabels.get(i);
		}
		return result; //TODO implement
	}

	/** Get number of properties/keys.
	 *
	 * @return number of properties
	 */
	public int getPropertyCount(){
		return this.propertyLabels.size();
	}

	public List<IPrevExchangeItem> getExchangeItems() {
		return exchangeItems;
	}

	private void checkIfPairsAreComplete(HashMap<String, String> pairsValidAssim){
		String message = "";
		Collection<String> assimStationIds = pairsValidAssim.values();
		for (TimeSeries serie : series) {
			String obsStatus = serie.getProperty("status");
			if (obsStatus.equalsIgnoreCase(Type.Validation.toString())){
				String validId = serie.getId();
				String assimId = pairsValidAssim.get(validId);
				if (validId == null){
					message += "\nAssimilation station " + assimId + " has no validation counterpart";
				}
				if (assimId == null){
					message += "\nValidation station " + validId + " has no assimilation counterpart";
				}
			} else {
				if (!assimStationIds.contains(serie.getId())){
					message += "\ncould not find " + serie.getId() + " in assimilation stations";
				}
			}
		}
		if (message.length() > 0) {
			throw new RuntimeException(
					"Error(s) in discharge dependent filter / the stochObserver input: " + message);
		}
	}
}



