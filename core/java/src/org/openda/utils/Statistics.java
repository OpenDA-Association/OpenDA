/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.utils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class Statistics {

    // max, min, number of data
	IVector values=null;
	IObservationDescriptions descriptions=null;
    private HashMap<String, Double> numberOfData = new HashMap<String, Double>();
    private HashMap<String, Double> max = new HashMap<String, Double>();
    private HashMap<String, Double> min = new HashMap<String, Double>();
    private HashMap<String, Double> bias = new HashMap<String, Double>();
    private HashMap<String, Double> stdev = new HashMap<String, Double>();
    private HashMap<String, Double> rms = new HashMap<String, Double>();

	public Statistics(IVector values){
		this.values=values;
        computeStatisticsOverAll();
	}

    private void computeStatisticsOverAll() {
        int nValues = this.values.getSize();
        this.numberOfData.put("Overall",(double)nValues);

        // find max and min:
        double max = Double.NEGATIVE_INFINITY;
        double min = Double.POSITIVE_INFINITY;
        for (int i=0; i<nValues; i++){
            if (max < this.values.getValue(i)){
                max = this.values.getValue(i);
            }
            if (min > this.values.getValue(i)){
                min = this.values.getValue(i);
            }
        }
        this.max.put("Overall",max);
        this.min.put("Overall",min);

        // compute bias:
        double bias = 0.0d;
        for (int i=0; i<nValues; i++){
            bias += (1 / (double)nValues) * this.values.getValue(i);
        }
        this.bias.put("Overall",bias);

        // compute std:
        double stdev = 0.0d;
        if (nValues>1) {
            for (int i=0; i<nValues; i++){
                stdev += (1 / (double)(nValues-1)) * Math.pow((this.values.getValue(i)-bias),2);
            }
            stdev = Math.pow(stdev,0.5);
        }
        this.stdev.put("Overall",stdev);

        // compute rms:
        double rms = 0.0d;
        for (int i=0; i<nValues; i++){
            rms += (1 / (double)nValues) * Math.pow(this.values.getValue(i),2);
        }
        rms = Math.pow(rms,0.5);
        this.rms.put("Overall",rms);
    }

    public Statistics(IVector values, IObservationDescriptions descriptions){
		this.values=values;
		this.descriptions=descriptions;
		List<IPrevExchangeItem> items = null;
		try{
			items = this.descriptions.getExchangeItems();
		}catch (Exception e) {
			items = new ArrayList<IPrevExchangeItem>(); //Empty list
		}
        int indFirst = 0;
        int indLast = 0;
		for(IPrevExchangeItem item : items){ // assume the exchangeItems are in the
			String id = item.getId();
            double times[] = item.getTimes();
            if (times != null) {
                int n = times.length;
				indLast = indFirst + n; //NOTE: indLast is the last index PLUS ONE, contrary to what the name suggests
				computeStatisticsThisId(indFirst,indLast,id);
				indFirst = indLast;
            } else {
				this.numberOfData.put(id,0.0);
				this.max.put(id,0.0);
        		this.min.put(id,0.0);
				this.bias.put(id,0.0);
				this.stdev.put(id,0.0);
				this.rms.put(id,0.0);
			}
		}
	}

    private void computeStatisticsThisId(int indFirst, int indLast, String id) {
        int nValues = indLast - indFirst;
        this.numberOfData.put(id,(double)nValues);

        // find max and min:
        double max = Double.NEGATIVE_INFINITY;
        double min = Double.POSITIVE_INFINITY;
        for (int i=indFirst; i<indLast; i++){
            if (max < this.values.getValue(i)){
                max = this.values.getValue(i);
            }
            if (min > this.values.getValue(i)){
                min = this.values.getValue(i);
            }
        }
        this.max.put(id,max);
        this.min.put(id,min);

        // compute bias:
        double bias = 0.0d;
        for (int i=indFirst; i<indLast; i++){
            bias += (1 / (double)nValues) * this.values.getValue(i);
        }
        this.bias.put(id,bias);

        // compute std:
        double stdev = 0.0d;
        if (nValues>1) {
            for (int i=indFirst; i<indLast; i++){
                stdev += (1 / (double)(nValues-1)) * Math.pow((this.values.getValue(i)-bias),2);
            }
            stdev = Math.pow(stdev,0.5);
        }
        this.stdev.put(id,stdev);

        // compute rms:
        double rms = 0.0d;
        for (int i=indFirst; i<indLast; i++){
            rms += (1 / (double)nValues) * Math.pow(this.values.getValue(i),2);
        }
        rms = Math.pow(rms,0.5);
        this.rms.put(id,rms);
    }

    public HashMap<String,Double> getNumberOfData(){
		return this.numberOfData;
	}

    public HashMap<String,Double> getMinimum(){
		return this.min;
	}

    public HashMap<String,Double> getMaximum(){
		return this.max;
	}

    public HashMap<String,Double> getBias(){
		return this.bias;
	}

    public HashMap<String,Double> getRMS(){
        return this.rms;
    }

    public HashMap<String,Double> getSTD(){
        return this.stdev;
    }

    public ITreeVector asTreeVector(HashMap<String,Double> stats){
        Set<String> paramIdsSet = stats.keySet();
        int nStats = paramIdsSet.size();
        String[] paramIds = new String[nStats];
        double[] paramValues = new double[nStats];
        for (int i=0; i<nStats; i++) {
            paramIds[i] = (String) paramIdsSet.toArray()[i];
            paramValues[i] = stats.get(paramIds[i]);
        }
        return new TreeVector("statistics",paramIds,paramValues);
    }

    public String toString(){
        StringBuffer result = new StringBuffer();
        result.append("\n");
        result.append(" -------------------------------------------------------------------------------------------------------------------------------------------\n");
        result.append(" RESIDUAL STATISTICS:\n");
        result.append(" -------------------------------------------------------------------------------------------------------------------------------------------\n");
        result.append(" ObsID\t\tRMS\t\tBias\t\tSTD\t\tNum of data\t\tMin\t\tMax\n");
        result.append(" -------------------------------------------------------------------------------------------------------------------------------------------\n");
        for (String key : bias.keySet()) {
            result.append(" ").append(key).append("\t\t");
            result.append(rms.get(key)).append("\t");
            result.append(bias.get(key)).append("\t");
            result.append(stdev.get(key)).append("\t");
            result.append(Math.round(numberOfData.get(key))).append("\t");
            result.append(min.get(key)).append("\t");
            result.append(max.get(key)).append("\n");
        }
        result.append(" -------------------------------------------------------------------------------------------------------------------------------------------\n");
        return result.toString();
    }
}
