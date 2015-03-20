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

import java.io.Serializable;

//////////////////////////////////////////////////////////////////////////
//// PlotPoint
/**
A simple structure for storing a plot point.
@author Edward A. Lee
@version $Id: PlotPoint.java,v 1.30 2003/01/08 02:39:14 ptII Exp $
@since Ptolemy II 0.2
*/
public class PlotPoint implements Serializable {

    ///////////////////////////////////////////////////////////////////
    ////                         public variables                  ////

    /** True if this point is connected to the previous point by a line. */
    public boolean connected = false;

    /** True if the yLowEB and yHighEB fields are valid. */
    public boolean errorBar = false;

    /** Original value of x before wrapping. */
    public double originalx;

    /** X value after wrapping (if any). */
    public double x;

    /** Y value. */
    public double y;

    /** Error bar Y low value. */
    public double yLowEB;

    /** Error bar Y low value. */
    public double yHighEB;
}
