/* OpenDA v2.4 
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

package org.openda.resultwriters;

import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.io.BufferedWriter;
import java.io.File;
//import java.io.FileWriter;
//import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;

/**
 * Result writer that produces output in a matlab script file.
 */
public class GlueCsvResultWriter extends CsvResultWriter implements IResultWriter {

    private boolean filesYetToBeOpened = true;
    private boolean obsDataYetToBeWritten = true;
    private HashMap<String, BufferedWriter> seriesOutputStream = new HashMap<String, BufferedWriter>();
    private ITreeVector paramAsTreeVector;
    private ITreeVector obsAsTreeVector = null;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public GlueCsvResultWriter(File workingDir, String configString) {
        super(workingDir,configString);
    }

    
    public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
    	// TODO create a counter for time being; one counter per id
    	Integer currentIter =0;
    	if(this.iter.containsKey(id)){
    		currentIter = this.iter.get(id);
    	}
    	this.iter.put(id, currentIter+1);

        if (result instanceof ITreeVector) {
            ITreeVector resultAsTreeVector = (ITreeVector) result;
            if (id.equalsIgnoreCase("evaluatedParameters")) {
                if (obsDataYetToBeWritten) {
                    // write observation data once on the top of the result file
                    if (this.obsAsTreeVector==null){
                        throw new RuntimeException(this.getClass().getName()+": observed vector is not yet initialized.");
                    }
                    outputStream.print("No.,ObservationId.,ObservationTimeseries");
                    outputStream.println();
                    int iObs = 0;
                    for (String obsId : this.obsAsTreeVector.getSubTreeVectorIds()){
                        iObs++;
                        outputStream.print(iObs+",");
                        outputStream.print(obsId);
                        double[] values= this.obsAsTreeVector.getSubTreeVector(obsId).getValues();
                        for (double value : values) {
                            outputStream.print("," + String.valueOf(value));
                        }
                        outputStream.println();
                    }
                    obsDataYetToBeWritten = false;
                }
                if (headerYetToBeWritten) {
                    // write header of model output data once
                    headerYetToBeWritten = false;
                    int iStart = 2;
                    int nColumns = resultAsTreeVector.getSubTreeVectorIds().size() + iStart;
                    String[] name = new String[nColumns];
                    name[0] = "RunID";
                    name[1] = "Location";
                    for (int i=iStart; i<nColumns; i++){
                        name[i]=resultAsTreeVector.getSubTreeVectorIds().get(i-iStart);
                    }
                    for (String aName : name) {
                        outputStream.print(aName + ",");
                    }
                    outputStream.print("RMSE,ModelOutputTimeseries");
                    outputStream.print('\n');
                }
                this.paramAsTreeVector = (ITreeVector) result;

            } else if (resultAsTreeVector.getId().equalsIgnoreCase("predictions")){
                int nObs = resultAsTreeVector.getSize();
                for (String predictionId : resultAsTreeVector.getSubTreeVectorIds()){
                    outputStream.print(currentIter);
                    outputStream.print(",");
                    outputStream.print(predictionId);
                    // print parameter values:
					Locale locale = new Locale("EN");
					String floatValueFormat = "%7.5f";

					for (String parameterId : this.paramAsTreeVector.getSubTreeVectorIds()) {
                        double[] values= this.paramAsTreeVector.getSubTreeVector(parameterId).getValues();
                        for (double value : values) {
                            outputStream.print("," + String.format(locale, floatValueFormat, value));
                        }
                    }
                    // get and store model output
                    IVector thisValuesVector = resultAsTreeVector.getSubTreeVector(predictionId);
                    double[] values= thisValuesVector.getValues();

                    // print RMSE:
                    thisValuesVector.axpy(-1.0,this.obsAsTreeVector.getSubTreeVector(predictionId));
                    double rmse = Math.sqrt(Math.pow(thisValuesVector.norm2(),2)/((double)thisValuesVector.getSize()));
                    outputStream.print("," + String.valueOf(rmse));

                    // print model output:
                    for (double value : values) {
                        outputStream.print("," + String.valueOf(value));
                    }
                    outputStream.println();
                }
            } else if (id.equalsIgnoreCase("observed")){
                this.obsAsTreeVector = (ITreeVector) result;
            }

        } else {
            // do nothing
            //TODO: extend this for other types of vector as well
        }
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    
    public void free(){
        outputStream.close();
    }

}
