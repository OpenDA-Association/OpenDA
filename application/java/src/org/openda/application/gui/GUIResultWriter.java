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
package org.openda.application.gui;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;
import org.openda.utils.Matrix;
import org.openda.utils.PrintNumber;

import javax.swing.*;

public class GUIResultWriter implements IResultWriter {

	static final int maxMatrixPrintSize = 80;		// TODO: make configurable

	JTextArea progressDisplayer;
    JTextArea resultDisplayer;
    JLabel statusDisplayer;

    /**
     * Results are only appended to this.resultDisplayer
     * when writeResults is true. Default is true.
     */
    private boolean writeResults = true;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public GUIResultWriter(JTextArea progress, JTextArea log, JLabel status) {
        this.progressDisplayer = progress;
        this.resultDisplayer = log;
        this.statusDisplayer = status;
    }

    public static String getFormattedResult(double[] result) {

        String formatResult = "";
        for (double d : result) {
            formatResult += (formatResult.length() > 0 ? ", " : "") + String.format("%1.3f", d);
        }

        return formatResult;

    }


    public void putMessage(Source source, String message) {
        synchronized (this) {
            progressDisplayer.append(message+"\n");
            setCursorToEnd(this.progressDisplayer);
        }
    }

    
    public void putMessage(IInstance source, String message) {
        synchronized (this) {
            progressDisplayer.append(message + "\n");
            setCursorToEnd(this.progressDisplayer);
        }
    }

    public void putStatus(IInstance source, String message) {
        synchronized (this) {
            statusDisplayer.setText(message);
        }
    }


//    
//    public void putValue(Source source, String id, Object result) {
//        if (!writeResults) {
//            return;
//        }
//
//        //resultDisplayer.setText(id + "= " + result.toString());
//        resultDisplayer.append(id + "= " + printObject(result)+"\n");
//        setCursorToEnd(this.resultDisplayer);
//    }
//
//    
//    public void putValue(IInstance source, String id, Object result) {
//        if (!writeResults) {
//            return;
//        }
//
//        //resultDisplayer.setText(id + "= " + result.toString());
//        resultDisplayer.append(id + "= " + printObject(result) + "\n");
//        setCursorToEnd(this.resultDisplayer);
//    }

//    
//    public void putValue(Source source, String id, Object result, int iteration) {
//        if (!writeResults) {
//            return;
//        }
//
//        String display = id + " (iteration=" + iteration + "):\n";
//        display += id + "= " + printObject(result) + "\n";
//        //resultDisplayer.setText(display);
//        resultDisplayer.append(display);
//        setCursorToEnd(this.resultDisplayer);
//    }

	
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {

        if (!writeResults) {
            return;
        }
        synchronized (this) {
            String display = id + " (iteration=" + iteration + "):\n";
            display += id + "= " + printObject(result) + "\n";
            //resultDisplayer.setText(display);
            resultDisplayer.append(display);
            setCursorToEnd(this.resultDisplayer);
        }
	}

//	
//    public void putValue(IInstance source, String id, Object result, int iteration) {
//        if (!writeResults) {
//            return;
//        }
//
//        String display = Instance.identifySource(source) + " " + id +
//                " (iteration=" + iteration + "):\n";
//        display += id + "= " + printObject(result) + "\n";
//        //resultDisplayer.setText(display);
//        resultDisplayer.append(display);
//        setCursorToEnd(this.resultDisplayer);
//    }

    private String printObject(Object result) {
        if (result instanceof IVector) {
            return ((IVector) result).printString("");
        } else if (result instanceof Double) {
            return PrintNumber.printNumber(((Double) result).doubleValue());
        } else if (result instanceof Matrix) {
			Matrix matrix = (Matrix) result;
			if (matrix.getNumberOfColumns()* matrix.getNumberOfRows() < maxMatrixPrintSize) {
				return matrix.printString();
			} else {
				return "(Matrix too large to print)";
			}
        } else {
            return result.toString();
        }
    }

    private void setCursorToEnd(JTextArea box){
    	box.setCaretPosition(box.getText().length());
    }

    public void reset() {
        progressDisplayer.setText("");
        resultDisplayer.setText("");
    }

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    ;

      public void free(){};

    /**
     * @param writeResults
     */
    public void setWriteResults(boolean writeResults) {
        this.writeResults = writeResults;
    }

    /**
     * @return writeResults.
     */
    public boolean isWriteResults() {
        return this.writeResults;
    }
}
