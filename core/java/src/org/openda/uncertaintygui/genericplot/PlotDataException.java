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

//////////////////////////////////////////////////////////////////////////
//// PlotDataException
/**
Exception thrown by plot classes if there are format
problems with the data to be plotted.

@author Christopher Hylands
@version $Id: PlotDataException.java,v 1.22 2003/01/08 02:38:54 ptII Exp $
@since Ptolemy II 0.2
 */
class PlotDataException extends Throwable {
    public PlotDataException() { super(); }
    public PlotDataException(String s) { super(s); }

}
