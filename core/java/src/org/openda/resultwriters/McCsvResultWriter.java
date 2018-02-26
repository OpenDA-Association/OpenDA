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

package org.openda.resultwriters;

import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.io.*;
import java.util.HashMap;

/**
 * Result writer that produces output in a matlab script file.
 */
public class McCsvResultWriter extends CsvResultWriter implements IResultWriter {

    private boolean filesYetToBeOpened = true;
    private HashMap<String, BufferedWriter> seriesOutputStream = new HashMap<String, BufferedWriter>();
    private File workingDir;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public McCsvResultWriter(File workingDir, String configString) {
        super(workingDir,configString);
        this.workingDir = workingDir;
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
//            if (resultAsTreeVector.getId().equalsIgnoreCase("Params") ) {
            if (id.equalsIgnoreCase("evaluatedParameters")) {
                if (headerYetToBeWritten) {
                    headerYetToBeWritten = false;
                    int nColumns = resultAsTreeVector.getSize() + 1;
                    String[] name = new String[nColumns];
                    name[0] = "RunID";
                    for (int i=1; i<nColumns; i++){
                        name[i]=resultAsTreeVector.getSubTreeVectorIds().get(i-1);
                    }
                    for (int i = 0; i < name.length-1; i ++) {
                        outputStream.print(name[i]);
                        outputStream.print(",");
                    }
                    outputStream.print(name[name.length-1]);
                    outputStream.print('\n');
                }

                outputStream.print(currentIter);
                for (String subTreeVectorId : resultAsTreeVector.getSubTreeVectorIds()) {
                    double[] values= resultAsTreeVector.getSubTreeVector(subTreeVectorId).getValues();
                    for (double value : values) {
                        outputStream.print("," + String.valueOf(value));
                    }
                }
                outputStream.println();

            } else if (id.equalsIgnoreCase("residuals")){
//            } else if (resultAsTreeVector.getId().equalsIgnoreCase("residuals")){ --> getId() = ObsValues
                BufferedWriter outputFileBufferedWriter;
                if (filesYetToBeOpened) {
                    filesYetToBeOpened = false;
                    for (String childId : resultAsTreeVector.getSubTreeVectorIds()) {
                        FileWriter fileWriter;
                        try {
                            String fileName = childId + ".csv";
                            File file = new File(this.workingDir,fileName);
                            fileWriter = new FileWriter(file);
                        } catch (IOException e) {
                            throw new RuntimeException(this.getClass().getName()+" could not open file: "+childId+" ("+ e.getMessage()+").");
                        }
//                        seriesOutputStream.put(childId,new BufferedWriter(fileWriter));
                        try {
                            outputFileBufferedWriter = new BufferedWriter(fileWriter);
                            String residualHeader = "RunID, RMSE, Value(s)";
                            outputFileBufferedWriter.write(residualHeader);
                            outputFileBufferedWriter.newLine();
                            seriesOutputStream.put(childId,outputFileBufferedWriter);
                        } catch (IOException e) {
                            throw new RuntimeException(this.getClass().getName()+" could not write header of file: "+childId+" ("+ e.getMessage()+").");
                        }
                    }
                }

                for (String childId : resultAsTreeVector.getSubTreeVectorIds()){
//                    BufferedWriter outputFileBufferedWriter = seriesOutputStream.get(childId);
                    outputFileBufferedWriter = seriesOutputStream.get(childId);
                    try {
                        outputFileBufferedWriter.write(currentIter+",");
                        IVector thisValuesVector = resultAsTreeVector.getSubTreeVector(childId);
                        double rmse = Math.sqrt(Math.pow(thisValuesVector.norm2(),2)/((double)thisValuesVector.getSize()));
                        outputFileBufferedWriter.write(String.valueOf(rmse));
                        for (int i=0; i< thisValuesVector.getSize(); i++){
                            outputFileBufferedWriter.write(","+thisValuesVector.getValue(i));
                        }
                        outputFileBufferedWriter.newLine();
                    } catch (IOException e) {
                        throw new RuntimeException(this.getClass().getName()+" could not write file: "+childId+" ("+ e.getMessage()+").");
                    }

                }

            } else if (resultAsTreeVector.getId().equalsIgnoreCase("predictions")){
                // TODO: perhaps we need also to support the writing of model output?
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
        for (BufferedWriter thisBufferedWriter : seriesOutputStream.values()){
            try {
                thisBufferedWriter.close();
            } catch (IOException e) {
                throw new RuntimeException(this.getClass().getName()+" could not close file: "+thisBufferedWriter.toString()+" ("+ e.getMessage()+").");
            }
        }
        outputStream.close();
    }

}
