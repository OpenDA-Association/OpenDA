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

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.HashMap;

/**
 * Result writer that produces output in a matlab script file.
 */
public class CsvResultWriter implements IResultWriter {

    protected static final String commentPrefix = "% ";
    protected PrintStream outputStream = null;
    protected HashMap<String, Integer> iter = new HashMap<String, Integer>();
    protected boolean headerYetToBeWritten = true;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public CsvResultWriter(File workingDir, String configString) {
        if ( configString.startsWith("<xml") ) {  // TODO: right prefix
            // TODO: read from config file
        } else {
            if ( configString.toLowerCase().endsWith(".csv") ) {
                // configString directly indicates the matlab output file
                try {
                    outputStream = new PrintStream(new File(workingDir, configString));
                } catch (FileNotFoundException e) {
                    throw new RuntimeException("CsvResultWriter: could not open " +
                            " for writing (" + e.getMessage() + ")");
                }
            }
        }
    }

    public void free(){}

    public void putMessage(Source source, String comment) {
        // do nothing
    }

    public void putMessage(IInstance source, String comment) {
        // do nothing
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
    	// TODO create a counter for time being; one counter per id
    	Integer currentIter =0;
    	if(this.iter.containsKey(id)){
    		currentIter = this.iter.get(id);
    	}
    	this.iter.put(id, currentIter+1);

        if (result instanceof ITreeVector) {
            if (headerYetToBeWritten) {
                headerYetToBeWritten = false;
                String[] name = new String[4];
                name[0] = "Iteration";
                name[1] = "id";
                name[2] = "Vector.Id";
                name[3] = "Value(s)";
                for (int i = 0; i < name.length-1; i ++) {
                    outputStream.print(name[i]);
                    outputStream.print(",");
                }
                outputStream.print(name[name.length-1]);
                outputStream.print('\n');
            }

            boolean printComma = false;
            for (String subTreeVectorId : ((ITreeVector)result).getSubTreeVectorIds()) {
                outputStream.print(currentIter+",");
                outputStream.print(id+",");
                if (subTreeVectorId.contains(",")){
                    throw new RuntimeException("CsvResultWriter does not allow comma in TreeVectorId: "
                            + subTreeVectorId);
                }
                outputStream.print(subTreeVectorId);
                double[] values=((ITreeVector)result).getSubTreeVector(subTreeVectorId).getValues();
				for (double value : values) {
					outputStream.print("," + value);
				}
                outputStream.println();
            }
        } else {
            // do nothing
            //TODO: extend this for other types of vector as well
        }
	}

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }
}
