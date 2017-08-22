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

package org.openda.resultwriters;

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

public class TextTableWriter implements IResultWriter {

    private PrintStream outputStream = null;
    private boolean writeHeader = false;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public TextTableWriter(File workingDir, String configString) {

        writeHeader = true;

        if (configString.startsWith("<xml")) {  // TODO: right prefix
            // TODO: read from config file
        } else {
            try {
                outputStream = new PrintStream(new File(workingDir, configString));
            } catch (FileNotFoundException e) {
                throw new RuntimeException("TextTableWriter: could not open " +
                    " for writing (" + e.getMessage() + ")");
            }
        }
    }


    public void free(){};

    public void putMessage(Source source, String message) {
		// Nothing to do
    }

    public void putMessage(IInstance source, String message) {
        // Nothing to do
    }

	
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
        // Nothing to do
	}

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {

		double[] parameterValues = parameters.getValues();

		if (writeHeader) {
			writeHeader = false;
			outputStream.print("Iteration   Cost");
			if (parameters instanceof ITreeVector) {
				for ( String treeVectorChildId : (((ITreeVector) parameters)).getSubTreeVectorIds() ) {
					outputStream.print(" \t" + treeVectorChildId);
                }
            } else {
				for (int i = 0; i < parameterValues.length; i ++) {
					outputStream.print(" \t" + "value[" + i + "]");
				}
			}
			outputStream.print('\n');
		}

		outputStream.print(iteration + " \t" + cost);

		for (int i = 0; i < parameterValues.length; i++) {
			outputStream.print(" \t" + parameterValues[i]);
		}
		outputStream.print('\n');
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }
}
