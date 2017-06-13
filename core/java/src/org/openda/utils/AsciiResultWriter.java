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

package org.openda.utils;

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

/**
 * Result writer that produces output in an ASCII file.
 */
public class AsciiResultWriter implements IResultWriter {

    private PrintStream outputStream;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public AsciiResultWriter(File workingDir, String configString) {
        if ( configString.startsWith("<xml") ) {  // TODO: right prefix
            // TODO: read from config file
        } else {
            if ( configString.toLowerCase().endsWith(".txt") ) {
                // configString directly indicates the text output file
                try {
                    outputStream = new PrintStream(new File(workingDir, configString));
                } catch (FileNotFoundException e) {
                    throw new RuntimeException("AsciiResultWriter: could not open " +
                            " for writing (" + e.getMessage() + ")");
                }
            }
        }
        if (outputStream == null) {
            throw new RuntimeException("No ascii result file (*.txt) specified in config string");
        }
    }

    public void free(){};

    public void putMessage(Source source, String comment) {
        outputStream.println(comment);
    }

    public void putMessage(IInstance source, String comment) {
        outputStream.println(Instance.identifySource(source) + comment);
    }

    public void putValue(Source source, String id, Object result) {
        outputStream.println(id + "=" + printObject(result));
    }

    public void putValue(IInstance source, String id, Object result) {
        outputStream.println(Instance.identifySource(source) + ": " + id + "=" + printObject(result));
    }

    public void putValue(Source source, String id, Object result, int iteration) {
        outputStream.println(id + " (iteration=" + iteration + "):");
        outputStream.println(id + "=" + printObject(result));
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		putValue(source, id, result, iteration);
	}

	public void putValue(IInstance source, String id, Object result, int iteration) {
        outputStream.println(Instance.identifySource(source) + ": " + id + " (iteration=" + iteration + "):");
        outputStream.println(Instance.identifySource(source) + ": " + id + "=" + printObject(result));
    }

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {}

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    // Copied from GUIResultWriter
    private String printObject(Object result) {
        if (result instanceof IVector) {
            return ((IVector) result).printString("");
        } else if (result instanceof Double) {
            return PrintNumber.printNumber(((Double) result).doubleValue());
        } else if (result instanceof Matrix) {
            return ((Matrix) result).printString();
        } else {
            return result.toString();
        }
    }

}
