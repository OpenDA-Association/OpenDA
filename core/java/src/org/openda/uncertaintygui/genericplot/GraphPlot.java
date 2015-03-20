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
package org.openda.uncertaintygui.genericplot;
import due.utilities.matrix.DoubleMatrix2D;

/**
 * Utility for plotting the functions associated with a quantitative probability
 * model.
 *
 * @author brown@science.uva.nl
 * @version 3.0
 */

public class GraphPlot extends Plot {

/*******************************************************************************
 *                                                                             *
 *                               CONSTRUCTORS                                  *
 *                                                                             *
 ******************************************************************************/

    /**
     * Create a new plotting object with a default plotting window and a specified
     * title.
     */

    public GraphPlot() {
        setButtons(false);
    }


/*******************************************************************************
 *                                                                             *
 *                             INSTANCE VARIABLES                              *
 *                                                                             *
 ******************************************************************************/

    /**
     * Is true for a bar plot, false for a line plot.
     */

    private boolean bars = false;

    /**
     * Is true to clear the display
     */

    private boolean clear = true;

/*******************************************************************************
 *                                                                             *
 *                                  METHODS                                    *
 *                                                                             *
 ******************************************************************************/

    /**
     * Function for plotting points.
     *
     * @param input the array of points for plotting
     * @param dataset the dataset ID for plotting
     * @param connected is true to connect points
     */

    public void plotPoints(final DoubleMatrix2D input, final int dataset, final boolean connected) {
        Runnable doAction = new Runnable() {
            public void run() {
                if(clear) {
                    clear(true);
                }
                setBars(false);
                bars = false;
                _rightPadding = 5;
                _leftPadding = 10;
                _bottomPadding = 10;
                _topPadding = 0;
                if(! connected) {
                    setMarksStyle("dots",dataset);
                }
                if(dataset < 0 || dataset > 9) {
                    throw new IllegalArgumentException("Unsupported dataset ID: use 0 - 9.");
                }
                int length = input.getNumberOfRows();
                addPoint(dataset,input.getElement(0,0),input.getElement(0,1),false);
                for (int i = 1; i < length; i++) {
                    addPoint(dataset,input.getElement(i,0),input.getElement(i,1),connected);
                }
            }
        };
        deferIfNecessary(doAction);
    }

   /**
     * Function for plotting points with custom ticks on the x axis.  Use for
     * displaying discrete pdfs.
     *
     * @param input the array of points for plotting
     * @param dataset the dataset ID for plotting
     */

    public void plotDiscretePoints(final DoubleMatrix2D input, final int dataset) {
        if(dataset < 0 || dataset > 9) {
            throw new IllegalArgumentException("Unsupported dataset ID: use 0 - 9.");
        }
        Runnable doAction = new Runnable() {
            public void run() {
                if(clear) {
                    clear(true);
                }
                setBars(true);
                bars = true;
                setBars(0.8,0);
                setConnected(false,dataset);
                _rightPadding = 5;
                _leftPadding = 10;
                _bottomPadding = 10;
                _topPadding = 0;

                boolean first = true;
                int length = input.getNumberOfRows();
                if(input.getIndexKey() instanceof Integer) {
                    for (int i = 0; i < length; i++) {
                        addPoint(dataset,input.getElement(i,0),input.getElement(i,1),!first);
                        int axis = (int)input.getElement(i,0);
                        addXTick(""+axis+"",axis);
                        first = false;
                    }
                }
                else {
                    for (int i = 0; i < length; i++) {
                        addPoint(dataset,i,input.internalGetElement(i,1),!first);
                        String axis = input.getRowKey(i).toString();
                        addXTick(""+axis+"",i);
                        first = false;
                    }
                }
             }
         };
        deferIfNecessary(doAction);
    }

    /**
     * Rescale so that the data that is currently plotted just fits.
     * This is with the protected variables _xBottom, _xTop,
     * _yBottom, and _yTop.  It is up to derived classes to ensure that
     * variables are valid.
     *
     * This method calls repaint(), which eventually causes the display
     * to be updated.
     */

    public synchronized void fillPlot() {
        boolean sup = false;
        Runnable doAction = new Runnable() {
            public void run() {
                //Set additional padding for bars
                if(bars) {
                    double xPadding = 0.1 * (_xTop - _xBottom) + 0.5 * getBarWidth();
                    double yPadding = 0.01 * (_yTop - 0);
                    setXRange(_xBottom - xPadding, _xTop + xPadding );
                    setYRange(0 - yPadding, _yTop + yPadding);
                    //repaint();
                }
            }
        };
        deferIfNecessary(doAction);
        if(! bars) {
            super.fillPlot();
        }
    }

    /**
     * Sets the clear display status.
     *
     * @param clear is true to clear on adding a new dataset
     */

    public void setClearDisplay(boolean clear) {
        this.clear = clear;
    }

}
