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

import org.openda.interfaces.IInstance;
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
 * Result writer that produces output in a matlab script file.
 */
public class MatlabResultWriter implements IResultWriter {

    private static final String commentPrefix = "% ";
    private PrintStream outputStream = null;
    private HashMap<String, Integer> iter = new HashMap<String, Integer>();
    private int defaultMaxSize = 1000;

    public MatlabResultWriter(File workingDir, String configString) {
        if ( configString.startsWith("<xml") ) {  // TODO: right prefix
            // TODO: read from config file
        } else {
            if ( configString.toLowerCase().endsWith(".m") ) {
                // configString directly indicates the matlab output file
                try {
                    outputStream = new PrintStream(new File(workingDir, configString));
                } catch (FileNotFoundException e) {
                    throw new RuntimeException("MatlabResultWriter: could not open " +
                            " for writing (" + e.getMessage() + ")");
                }
            }
        }
    }

    public void free(){};

    public void putMessage(Source source, String comment) {
        comment = comment.replaceAll("\n", "\n%");
        outputStream.println(commentPrefix + comment);
    }

    public void putMessage(IInstance source, String comment) {
        comment = comment.replaceAll("\n", "\n%");
        outputStream.println(commentPrefix + " " + Instance.identifySource(source) + " " +  comment);
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		outputStream.println(commentPrefix + " resultItem id: "+ id +", outputLevel: "+ outputLevel.toString() +", context: "+ context);
        String prefix = commentPrefix;
    	// TODO create a counter for time being; one counter per id
    	Integer currentIter =0;
    	if(this.iter.containsKey(id)){
    		currentIter = this.iter.get(id);
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
        outputStream.print(id + "{"+(currentIter+1)+"}	=");
        if (result instanceof Matrix) {
            ((Matrix)result).serialize(outputStream);
        } else if (result instanceof Vector) {
            ((Vector)result).serialize(outputStream);
        } else if (result instanceof TreeVector) {
            ((TreeVector)result).serialize(outputStream);
        } else {
            outputStream.print(result.toString());
        }
        outputStream.println(";");
	}

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }
}
