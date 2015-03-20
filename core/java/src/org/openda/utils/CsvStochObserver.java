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
package org.openda.utils;
import org.openda.interfaces.*;
import org.openda.interfaces.IObservationDescriptions;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.TreeSet;

/**
 * Implements Stochastic observer interface.
 * <ul>
 * <li>assumes propertyKey 'time' for times</li>
 * <li>assumes propertyKey 'values' for observation values</li>
 * <li>assumes propertyKey 'std' for standard deviations</li>
 * <li>assumes uncorrelated Gaussian errors for all observations</li>
 * </ul>
 */
public class CsvStochObserver extends Instance implements IStochObserver {

    IVector values[] = null;
    /**
     * Internal data for this class
     */
    String keys[] = null;
    int noValues = 0;
	int noKeys = 0;
	int timeInd  = -1; //Column used for time
    int locInd  = -1; //Column used for location, if present
	int valueInd = -1; //Values
	int stdInd   = -1; //Standard deviations - ONLY INDEPENDENT OBS!
	public double tolerance = 1e-8; //times with a difference less than this are considered equal

    /**
     * Create an new EMPTY Stochastic Observer
     */
    public CsvStochObserver(){
    	// Create an empty StochObserverw
    }
	
    /**
     * Initialize an new Stochastic Observer from the contents of a file on a specific working directory
     * @param workingDir  working directory
     * @param arguments   one string, containing the name of file with the observation data
     */
    public void initialize(File workingDir, String[] arguments) {
    	if(workingDir==null){ // try 
    		String content = arguments[0];
        	// Split lines
            String[] rowStrings = content.split("\n");
            this.noValues = rowStrings.length-1;
            // Split first line
            String[] labelStrings = rowStrings[0].split(",");
            this.noKeys = labelStrings.length;
            this.keys = labelStrings;
            // extract labels
            java.util.Vector<String> labelVector = new java.util.Vector<String>();
            for(int i=0;i<this.noKeys;i++){
            	labelStrings[i]=labelStrings[i].trim();
            	labelVector.add(labelStrings[i]);
            }
            setTimeLocValueStdIndices(labelVector);
            // create space for the data
        	this.values = new Vector[this.noKeys];
            for(int i=0;i<this.noKeys;i++){ 
            	values[i] = new Vector(this.noValues);
            }
            // parse values
            for(int i=0;i<this.noValues;i++){
            	String[] valueStrings = rowStrings[i+1].split(",");
            	for(int j=0;j<this.noKeys;j++){
            		this.values[j].setValue(i, Double.parseDouble(valueStrings[j]));
            	}
            }
            if(this.noValues==0){
        		throw new RuntimeException("No values were found for content:"+content);
        	}
    	}else{
    		String configStringOrFileName = arguments[0];
    		try {
    			File file = new File(workingDir,configStringOrFileName);
    			if (!file.exists()) {
    				throw new FileNotFoundException ("File does not exist: " + file);
    			}
    			if (!file.isFile()) {
    				throw new IllegalArgumentException("Should be a file: " + file);
    			}
    			FileReader fileReader = new FileReader(file);
    			BufferedReader buff = new BufferedReader(fileReader);
    			// read header
    			String line = buff.readLine();
    			// Split first line
    			String[] labelStrings = line.split(",");
    			this.noKeys = labelStrings.length;
    			this.keys = labelStrings;
    			// extract labels
    			java.util.Vector<String> labelVector = new java.util.Vector<String>();
    			for(int i=0;i<this.noKeys;i++){
                	labelStrings[i]=labelStrings[i].trim();
    				labelVector.add(labelStrings[i]);
    			}
    			setTimeLocValueStdIndices(labelVector);
    			//
    			// read content
    			//
    			java.util.Vector<Double[]> tempValues = new java.util.Vector<Double[]>();
    			boolean eof = false;
    			while (!eof) {
    				line = buff.readLine();
    				if (line == null)
    					eof = true;
    				else{
    					//System.out.println(line);
    					// parse values
    					String[] valueStrings = line.split(",");
    					Double[] tempRow = new Double[this.noKeys];
    					for(int j=0;j<this.noKeys;j++){
    						tempRow[j]=Double.parseDouble(valueStrings[j]);
    					}
    					tempValues.add(tempRow);
    				}
    			}
    			//
    			// copy data to this.values
    			//
    			this.noValues = tempValues.size();
    			this.values = new Vector[this.noKeys];
    			for(int i=0;i<this.noKeys;i++){
    				this.values[i] = new Vector(this.noValues);
    			}
    			for(int i=0;i<this.noValues;i++){
    				Double[] tempRow = tempValues.get(i);
    				for(int j=0;j<this.noKeys;j++){
    					this.values[j].setValue(i, tempRow[j]);
    				}
    			}
    			buff.close();
    		} catch (IOException e) {
    			System.out.println("Error -- " + e.toString());
    			throw new RuntimeException("Cannot read CsvStochObserver from file");
    		}
    		if(this.noValues==0){
        		throw new RuntimeException("No values were found for:"+configStringOrFileName);
        	}
    	}
    }

    /**
     * Create an new Stochastic Observer from the contents of a file
     * @param content     name of file with the observation data
     */
    public CsvStochObserver(String content){
    	// Split lines
        String[] rowStrings = content.split("\n");
        this.noValues = rowStrings.length-1;
        // Split first line
        String[] labelStrings = rowStrings[0].split(",");
        this.noKeys = labelStrings.length;
        this.keys = labelStrings;
        // extract labels
        java.util.Vector<String> labelVector = new java.util.Vector<String>();
        for(int i=0;i<this.noKeys;i++){
        	labelStrings[i]=labelStrings[i].trim();
        	labelVector.add(labelStrings[i]);
        }
        setTimeLocValueStdIndices(labelVector);
        // create space for the data
    	this.values = new Vector[this.noKeys];
        for(int i=0;i<this.noKeys;i++){ 
        	values[i] = new Vector(this.noValues);
        }
        // parse values
        for(int i=0;i<this.noValues;i++){
        	String[] valueStrings = rowStrings[i+1].split(",");
        	for(int j=0;j<this.noKeys;j++){
        		this.values[j].setValue(i, Double.parseDouble(valueStrings[j]));
        	}
        }
    }

    /**
     * Create an new Stochastic Observer from several vectors
     * @param columns  with time,values,std,etc.
     * @param keys     with corresponding names of each vector
     */
    public CsvStochObserver(IVector[] columns, String[] keys){
        this.noKeys = keys.length;
        this.keys = keys;
        // extract labels
        java.util.Vector<String> labelVector = new java.util.Vector<String>();
        for(int i=0;i<this.noKeys;i++){
        	labelVector.add(this.keys[i]);
        }
        setTimeLocValueStdIndices(labelVector);
        // create space for the data
    	this.noValues = columns[0].getSize();
    	this.values = new Vector[this.noKeys];
        for(int i=0;i<this.noKeys;i++){ 
        	values[i] = columns[i].clone();
        	if(this.noValues!=columns[i].getSize()){
        		throw(new RuntimeException("Problems with size of col"+i+""));
        	}
        }
    }

    
    /**
     * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
     * The selection criterium is in the form of an SQLite query.
     * @param selection    Selection querry
     * @param reltab       Relation table on return it contains the relation between the originating relation table and the new relation table 
     * @return             Stochastic Observer containing the required selection.
     */
    public IStochObserver createSelection(String selection, IRelationTable reltab){
    	throw new RuntimeException("createSelection not implemented yet");
    	//CsvStochObserver result = null;
    	//return result;
    }

    /**
     * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
     * The selection criterium is in the form of an SQLite query.
     * @param selection    Selection querry
     * @return             Stochastic Observer containing the required selection.
     */
    public IStochObserver createSelection(String selection){
    	IStochObserver result = null;
    	return result;
    }

    
    /**
     *
     * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
     * The selection criteria is the list of time stamps that should be included in the selection.
     * @param selectionTimes    array with time stamps describing the selection criterium.
     * @param reltab       Relation table on return it contains the relation between the originating relation table and the new relation table
     * @return                  Stochastic Observer containing the required selection.
     */
    public IStochObserver createSelection(ITime selectionTimes, IRelationTable reltab){
    	IStochObserver result = null;
    	return result;
    }

    /**
    *
    * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
    * The selection criteria is the list of time stamps that should be included in the selection.
    * @param selectionTimes    array with time stamps describing the selection criterium.
    * @return                  Stochastic Observer containing the required selection.
    */
   public IStochObserver createSelection(ITime selectionTimes){
	//assume we want to select an interval
	if(selectionTimes.isStamp()){
		throw(new RuntimeException("createSelection: can only select for an interval"));
	}
	//copy metadata
   	CsvStochObserver result = new CsvStochObserver();
   	result.noKeys = this.noKeys;
   	result.stdInd = this.stdInd;
   	result.timeInd = this.timeInd;
   	result.valueInd = this.valueInd;
   	result.locInd = this.locInd;
   	result.keys = new String[this.noKeys];
   	for(int i=0;i<this.noKeys;i++){
   		result.keys[i]=this.keys[i];
   	}
   	result.values = new Vector[this.noKeys];
   	// make selection
   	java.util.Vector<Integer> index= new java.util.Vector<Integer>();
   	ITime times[] = this.getAllTimes();
   	for(int i=0;i<times.length;i++){
   		if((times[i]).inSpan(selectionTimes)){
   			// in interval?
   			index.add(i);
   		}
   	}
   	result.noValues = index.size();
   	// create space for the data
   	for(int i=0;i<this.noKeys;i++){
   		result.values[i] = new Vector(index.size());
   	}
   	//copy content
   	for(int j=0;j<index.size();j++){
   	for(int i=0;i<this.noKeys;i++){
   		result.values[i].setValue(j,this.values[i].getValue(index.get(j)));
   	}}
  	return result;
   }

	public IStochObserver createSelection(Type observationType) {
		if (observationType == Type.Assimilation) {
			return this;
		}
		throw new UnsupportedOperationException("org.openda.utils.CsvStochObserver.createSelection(): Not implemented yet.");
	}

    public ISelector createSelector(Type observationType) {
        return new IdentitySelector();
    }

    /**
     * Number of observations in the Stochastic Observer.
     * @return The number of observations.
     */
    public int getCount(){
    	return this.noValues;
    }

    /**
     * Get the values for all observations.
     * @return The observed values.
     */
    public IVector getValues(){
    	IVector result = this.values[this.valueInd].clone();
    	return result;
    }

    /**
     * Get realization values for all observations, for one ensemble member.
     * @return The realizations.
     */
    public IVector getRealizations(){
    	//TODO : Now assumes Gaussian distribution implicitly
    	IVector mean = this.values[this.valueInd];
    	IVector std = this.values[this.stdInd];
    	IStochVector sv = new StochVector(mean,std);
    	IVector result = sv.createRealization();
    	return result;
    }

    /**
     * Get expectation values for all stochastic observations.
     * @return The expectations.
     */
    public IVector getExpectations(){
    	IVector result = this.values[this.valueInd].clone();
    	return result;
    }

    public double evaluatePDF(IVector values) {
    	IVector mean = this.values[this.valueInd];
    	IVector std = this.values[this.stdInd];
    	IStochVector sv = new StochVector(mean,std);
    	double result = sv.evaluatePdf(values);
    	return result;
    }

    /**
     * Evaluate the PDF for stochastic observations, given the values for those observation.
     * @param values values for observation's PDF-evaluation.
     * @return The PDF evaluations.
     */
    public IVector evaluateMarginalPDFs(IVector values){
    	IVector mean = this.values[this.valueInd];
    	IVector std = this.values[this.stdInd];
    	StochVector sv = new StochVector(mean,std);
    	IVector result = sv.evaluateMarginalPDFs(values);
    	return result;
    }

    /**
     * Get the covariance matrix (as a vector) for the stochastic observations.
     * @return The covariance matrix.
     */
    public ISqrtCovariance getSqrtCovariance(){
    	IVector mean = this.values[this.valueInd];
    	IVector std = this.values[this.stdInd];
    	IStochVector sv = new StochVector(mean,std);
    	ISqrtCovariance result = sv.getSqrtCovariance();
    	return result;
    }

    /**
     * Get the variance of the stochastic observations.
     * @return The variances.
     */
    public IVector getStandardDeviations(){
    	IVector mean = this.values[this.valueInd];
    	IVector std = this.values[this.stdInd];
    	IStochVector sv = new StochVector(mean,std);
    	IVector result = sv.getStandardDeviations();
    	return result;
    }

    /**
     * free the Stochastic Observer.
     */
    public void free(){
    	// Nothing to do.
    }

    /**
     * Get the observation descriptions.
     * @return The Observation Descriptions 
     */
    public IObservationDescriptions getObservationDescriptions(){
    	IObservationDescriptions result = new CsvObservationDescriptions(this);
    	return result;
    }
    
    /**
     * Give string representation of the data
     * @return StochOsberver as a string 
     */
    public String toString(){
    	String result="";
    	// labels
    	for(int i=0;i<this.noKeys;i++){
    		result+=this.keys[i];
    		if(i<this.noKeys-1){
    			result+=",";
    		}
    	}
    	result+="\n";
    	// values
    	for(int j=0;j<this.noValues;j++){
        	for(int i=0;i<this.noKeys;i++){
        		result+=""+this.values[i].getValue(j);
        		if(i<this.noKeys-1) result+=",";        		
        	}
    		if(j<this.noValues-1) result+="\n";        	
    	}
    	return result;
    }

    /**
     * Write observations to a file
     * @param filename to write to 
     */
    public void toFile(File workingDir, String filename){
        try {
           	File file = new File(workingDir,filename);
        	//if (!aFile.canWrite()) {
        	//	throw new IllegalArgumentException("File cannot be written: " + aFile);
        	//}
            FileWriter out = new FileWriter(file);
	    	String line="";
	    	// labels
	    	for(int i=0;i<this.noKeys;i++){
	    		line+=this.keys[i];
	    		if(i<this.noKeys-1){
	    			line+=",";
	    		}
	    	}
	    	out.write(line); 
            out.write("\n");
	    	// values
	    	for(int j=0;j<this.noValues;j++){
	    		line="";
	        	for(int i=0;i<this.noKeys;i++){
	        		line+=""+this.values[i].getValue(j);
	        		if(i<this.noKeys-1) line+=",";        		
	        	}
	        	out.write(line);
	        	out.write("\n");
	    	}
        out.close();
        } catch (IOException e) {
            throw new RuntimeException("Error writing observations to file: " + e.getMessage());
        }
    }

    
    /**
     * Get index of a key. Or return -1 if the key does not exist
     * @param key
     * @return index
     */
    public int getKeyIndex(String key){        
    	java.util.Vector<String> labelVector = new java.util.Vector<String>();
        for(int i=0;i<this.noKeys;i++){
    	   labelVector.add(this.keys[i]);
        }
	    int result  = labelVector.indexOf(key); //Column used for time
    	return result;
    }

	/** Get properties (values) that correspond to a given key.
	 *
	 * @param key        I  key for which the value is asked
	 * @return Properties (column of data from observation descriptions)
	 */
    public IVector getValueProperties(String key){
    	IVector result = new Vector(this.noValues);
    	int index =this.getKeyIndex(key);
    	if(index>-1){
    		for(int j=0;j<this.noValues;j++){
    			result.setValue(j, this.values[index].getValue(j));
    		}
    		return result;
    	}else{
    		result=null;
    		return result;
    	}
    }


	/** Get names of all keys.
	 *
	 * @return error status: All keys of the observation descriptions
	 */
	public String[] CTA_ObsDescr_Get_Keys(){
		String result[] = new String[this.noKeys];
		for(int i=0;i<this.noKeys;i++){
			result[i] = this.keys[i];
		}
		return result;
	}

	/** Get number of properties/keys.
	 *
	 * @return number of properties 
	 */
	public int getPropertyCount(){
		return this.noKeys;
	}


	/** Get number of observations.
	 *
	 *noKeys @return number of observations
	 */
	public int getObservationCount(){
		return this.noValues;
	}

	   /**
	    * Get all different times in increasing order. There is at least one observation for each time.
	    * It is likely that observer.createSelection(time[i]) will be used to walk through the 
	    * observations. The implementation of the stochobserver should guarantee that all observations are
	    * returned in exactly one batch this way. 
	    * @return array with all uniquely different times.
	    */
	   public ITime[] getTimes(){
		    double[] unsortedTimes = this.values[this.timeInd].getValues();
           java.util.Vector<Double> sorted = sortDoubleArray(unsortedTimes);
			// convert from internal double representation to Time
			ITime[] result = new ITime[sorted.size()];
            for(int i=0;i<sorted.size();i++){
                Time simpleTime = new Time(sorted.get(i) - this.tolerance, sorted.get(i) + this.tolerance);
                simpleTime.setStep(this.tolerance);
                result[i] = simpleTime;
            }
			return result;

	   }

    /**
     * Get all different locations. There is at least one observation for each location.
     * @return array with all uniquely different location.
     */
    public String[] getIds(){

        if(locInd<0){
            throw(new RuntimeException(
                    "StochVector: measurements do not have \"id\", \"index\", \"loc\" or \"location\""));
        }
        double[] unsortedLocations = this.values[locInd].getValues();
        java.util.Vector<Double> sorted = sortDoubleArray(unsortedLocations);
         // convert from internal double representation to Time
         String[] result = new String[sorted.size()];
         for(int i=0;i<sorted.size();i++){
             result[i] = String.valueOf(sorted.get(i));
         }
         return result;

    }

    /**
     * The time stamp / time spans for all observations
     * @return time stamps / time spans.
     */
    public ITime[] getAllTimes(){
       	IVector times = this.values[this.timeInd];
       	ITime result[] = new Time[times.getSize()];
    	for(int i=0;i<times.getSize();i++){
    		result[i] = new Time(times.getValue(i));
    	}
    	return result;

    }
    
    private void setTimeLocValueStdIndices(java.util.Vector<String> labelVector) {
        this.timeInd  = labelVector.indexOf("time"); //Column used for time
        if(timeInd<0){
                throw(new RuntimeException("StochVector: Label <time> not found"));
        }
        this.valueInd = labelVector.indexOf("value"); //Values
        if(valueInd<0){
                throw(new RuntimeException("StochVector: Label <value> not found"));
        }
        this.stdInd   = labelVector.indexOf("std"); //Standard deviations - ONLY INDEPENDENT OBS!
        if(stdInd<0){
                throw(new RuntimeException("StochVector: Label <std> not found"));
        }
        this.locInd  = labelVector.indexOf("id"); //Column used for location, if present
        if(locInd<0) this.locInd  = labelVector.indexOf("index");
        if(locInd<0) this.locInd  = labelVector.indexOf("quant");
        if(locInd<0) this.locInd  = labelVector.indexOf("quantity");
        if(locInd<0) this.locInd  = labelVector.indexOf("loc");
        if(locInd<0) this.locInd  = labelVector.indexOf("location");
        if(locInd<0) this.locInd  = labelVector.indexOf("station");
    }

    private java.util.Vector<Double> sortDoubleArray(double[] unsortedLocations) {
        TreeSet<Double> temp = new TreeSet<Double>();
        for(int i=0;i<unsortedLocations.length;i++){
            temp.add(unsortedLocations[i]);
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
        return sorted;
    }

}
