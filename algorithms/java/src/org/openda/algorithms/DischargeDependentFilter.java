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
import org.openda.interfaces.IStochObserver;
import org.openda.observers.DischargeDependentSelector;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.observers.TimeSeriesStochObserver;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/**
 * Selecting observed waterlevel data based on a user defined discharge range.
 */
public class DischargeDependentFilter implements IObservationSpaceFilter {
    HashMap<String,String> dischargeAndWaterLevelId = new HashMap<String,String>();
    HashMap<String,Double> dischargeAndMinVal = new HashMap<String,Double>();
    HashMap<String,Double> dischargeAndMaxVal = new HashMap<String,Double>();
    DischargeDependentSelector dischSelector = null;
    File workingDir = null;
	private BufferedWriter debugFileWriter = null;

	public ObservationSpace applyFilter(ObservationSpace input) {
        ObservationSpace result = new ObservationSpace();
        if (this.workingDir==null){
            throw new RuntimeException("DischargeDependentFilter is not yet initialized.");
        }
        if (!(input.observer instanceof TimeSeriesStochObserver | input.observer instanceof NoosTimeSeriesStochObserver)) {
            throw new RuntimeException("DischargeDependentFilter is for the timebeing applicable " +
                    "only for (Noos)TimeSeriesStochObserver.");
        }
        TimeSeriesStochObserver newObserver = (TimeSeriesStochObserver) input.observer;
        if (dischSelector==null){
            dischSelector = (DischargeDependentSelector) newObserver.createSelector(dischargeAndWaterLevelId,input.predictedValues,dischargeAndMinVal,dischargeAndMaxVal);
        }
        result.predictedValues = dischSelector.apply(input.predictedValues);
        result.observer = newObserver.createSelection(dischargeAndWaterLevelId,input.predictedValues,dischargeAndMinVal,dischargeAndMaxVal);
        stopIfNoSelectedData(result.observer);
		if (debugFileWriter != null) {
			try {
				debugFileWriter.write("Filter Applied\n");
				debugFileWriter.write("Before Filtering: \n");
				logObservationSpace(input);
				debugFileWriter.write("After Filtering: \n");
				logObservationSpace(result);
				debugFileWriter.flush();
			} catch (IOException e) {
				throw new RuntimeException("Could write to log file: " + e.getMessage());
			}
		}
        return result;
    }

	private void stopIfNoSelectedData(IStochObserver observer) {
        IObservationDescriptions descr = observer.getObservationDescriptions();
        List<IPrevExchangeItem> items;
        try{
            items = descr.getExchangeItems();
        }catch (Exception e) {
            items = new ArrayList<IPrevExchangeItem>(); //Empty list
        }
        for(IPrevExchangeItem item : items){ // assume the exchangeItems are in the
            String id = item.getId();
            double times[] = item.getTimes();
            if (times == null) {
                throw new RuntimeException("DischargeDependentFilter: no data on "+id+" stations-pair is found to correspond to the specified discharge range. Please check your DischargeDependentFilter configuration file.");
            }
        }
    }

    public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
		if (arguments == null || arguments.length == 0) {
			throw new RuntimeException("\"DischargeDependentFilter: provide at least one item");
		}
        String configString = arguments[0];
        ConfigTree dischDependentFilter = new ConfigTree(workingDir, configString);
        ConfigTree partsObsFilter[] = dischDependentFilter.getSubTrees("listOfStationPairs/stationsPair");
        Results.putMessage("DischargeDependentFilter:");
        if (partsObsFilter != null) {
            for (ConfigTree part : partsObsFilter){
                String dischId = part.getAsString("@discharge", null);
                String waterlevelId = part.getAsString("@waterlevel", null);
                String minValStr = part.getAsString("@minValue", null);
                String maxValStr = part.getAsString("@maxValue", null);
                if (dischId == null){
                    throw new RuntimeException("'discharge' is not specified in "+ workingDir + configString);
                }
                if (waterlevelId == null){
                    throw new RuntimeException("'waterlevel' is not specified in "+ workingDir + configString);
                }
                if (minValStr == null){
                    throw new RuntimeException("'minValue' is not specified in "+ workingDir + configString);
                }
                if (maxValStr == null){
                    throw new RuntimeException("'maxValue' is not specified in "+ workingDir + configString);
                }
                double minVal = part.getAsDouble("@minValue",0.0);
                double maxVal = part.getAsDouble("@maxValue",Double.MAX_VALUE);
                Results.putMessage("        dischargeID: " + dischId + ", waterlevelID: " + waterlevelId + ", minVal: " + minVal + ", maxVal: " + maxVal);
                dischargeAndWaterLevelId.put(dischId,waterlevelId);
                dischargeAndMinVal.put(dischId,minVal);
                dischargeAndMaxVal.put(dischId,maxVal);
            }
        }
		if (arguments.length > 1) {
			String debugLogFileName = arguments[1];
			if (!debugLogFileName.toLowerCase().endsWith(".csv")) {
				debugLogFileName += ".csv";
			}
			File file = new File(workingDir, debugLogFileName);
			try {
				debugFileWriter = new BufferedWriter(new FileWriter(file));
			} catch (IOException e) {
				throw new RuntimeException("Could not open log file for writing: " + file.getAbsolutePath() +
				 "(" + e.getMessage() + ")");
			}
		}
	}

	private void logObservationSpace(ObservationSpace observationSpace) throws IOException {
		List<IPrevExchangeItem> observationItems =
				observationSpace.observer.getObservationDescriptions().getExchangeItems();
		String idLine = "obs.EI id's/size";
		List<Double> obsValues = new ArrayList<Double>();
		for	(IPrevExchangeItem exchangeItem : observationItems) {
			double[] eiValues = exchangeItem.getValuesAsDoubles();
			idLine += "," + exchangeItem.getId() + "=" + eiValues.length;
			for (double eiValue : eiValues) {
				obsValues.add(eiValue);
			}
		}
		double[] allObsValues = observationSpace.observer.getValues().getValues();
		double[] predValues = observationSpace.predictedValues.getValues();
		if (!(allObsValues.length == obsValues.size())) {
			throw new RuntimeException("Inconstent #values: " + allObsValues.length + "!=" + obsValues.size());
		}
		if (!(predValues.length == allObsValues.length)) {
			throw new RuntimeException("Inconstent #values: " + predValues.length + "!=" + allObsValues.length );
		}
		debugFileWriter.write(idLine + "\n");
		debugFileWriter.write("Obs.Indiv., Obs.All, Pred\n");
		for (int i = 0 ; i < allObsValues.length; i++) {
			debugFileWriter.write(obsValues.get(i) + "," +
					allObsValues[i] + "," + predValues[i] + "\n");
		}
	}
}
