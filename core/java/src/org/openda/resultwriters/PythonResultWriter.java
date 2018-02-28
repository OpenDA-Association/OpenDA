/* OpenDA v2.4.3 
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

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IMatrix;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.Instance;
import org.openda.utils.Matrix;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.HashMap;

/**
 * Result writer that produces output in a python script file.
 */
public class PythonResultWriter implements IResultWriter {

    private static final String commentPrefix = "# ";
    private PrintStream outputStream = null;
    private HashMap<String, Integer> iter = new HashMap<String, Integer>();
    private int defaultMaxSize = 1000;

    public PythonResultWriter(File workingDir, String configString) {
        if ( configString.startsWith("<xml") ) {  // TODO: right prefix
            // TODO: read from config file
        } else {
            if ( configString.toLowerCase().endsWith(".py") ) {
                // configString directly indicates the python output file
                try {
                    outputStream = new PrintStream(new File(workingDir, configString));
                } catch (FileNotFoundException e) {
                    throw new RuntimeException("PythonResultWriter: could not open " +
                            " for writing (" + e.getMessage() + ")");
                }
            }
        }
        outputStream.println("import numpy as np");
    }

    public void putMessage(Source source, String comment) {
        comment = comment.replaceAll("\n", "\n"+commentPrefix);
        outputStream.println(commentPrefix + comment);
    }

    public void putMessage(IInstance source, String comment) {
        comment = comment.replaceAll("\n", "\n"+commentPrefix);
        outputStream.println(commentPrefix + " " + Instance.identifySource(source) + " " +  comment);
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		outputStream.println(commentPrefix + " resultItem id: "+ id +", outputLevel: "+ outputLevel.toString() +", context: "+ context);
        String prefix = commentPrefix;
    	// TODO create a counter for time being; one counter per id
    	Integer currentIter =0;
    	if(this.iter.containsKey(id)){
    		currentIter = this.iter.get(id);
    	}else{
    		outputStream.println(id+"=[]");
    	}
    	this.iter.put(id, currentIter+1);
        if (result instanceof ITreeVector) {
            outputStream.print(prefix + " " + ((ITreeVector)result).getId() + ": ");
            boolean printComma = false;
            for (String subTreeVectorId : ((ITreeVector)result).getSubTreeVectorIds()) {
                if (printComma) {
                    outputStream.print(", ");
                } else {
                    printComma = true;
                }
                outputStream.print(subTreeVectorId);
            }
            outputStream.println();
        }
        //MVL outputStream.print(id + (iteration>-1 ? "{"+(iteration+1)+"}" : "") + "=");
        outputStream.print(id + ".append(");
        if (result instanceof Matrix) {
            this.serializeMatrix(outputStream, ((Matrix)result));
        } else if (result instanceof Vector) {
            ((Vector)result).serialize(outputStream);
        } else if (result instanceof TreeVector) {
            ((TreeVector)result).serialize(outputStream);
        } else {
            outputStream.print(result.toString());
        }
        outputStream.println(")");
	}

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }
    
    public void free(){
    	outputStream.println(commentPrefix+"Try to merge lists into arrays");
    	
    	for( String key : this.iter.keySet()){
    		outputStream.println("try :");
    		outputStream.println("   "+key+"=np.vstack("+key+")");
    		outputStream.println("except :");
    		outputStream.println("   print(\"Could not merge list into array for "+key+"\")");
    	}
		outputStream.println("");
    };

    public void serializeMatrix(PrintStream outputStream, IMatrix matrix) {
        outputStream.print("[[");
        for(int i=0;i< Math.min(matrix.getNumberOfRows(),40);i++){
           if(i>0) outputStream.print("],[");
           for(int j=0;j<Math.min(matrix.getNumberOfColumns(),40);j++){
              if(j>0) {
                  outputStream.print(",");
              }
              outputStream.print(matrix.getValue(i, j));
           }
        }
        outputStream.print("]]");
    }

}

