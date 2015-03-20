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
//// EditListener
/**
Interface for listeners that are informed of plot edit events.
These events are generated when a user modifies the plot data using
interactive facilities of an editable subclass of Plot.

@author  Edward A. Lee
@version $Id: EditListener.java,v 1.6 2003/01/08 02:38:25 ptII Exp $
@since Ptolemy II 0.4
@see EditablePlot

*/
public interface EditListener {

    ///////////////////////////////////////////////////////////////////
    ////                         public methods                    ////

    /** Notify that data in the specified plot has been modified
     *  by a user edit action.
     *  @param source The plot containing the modified data.
     *  @param dataset The data set that has been modified.
     */
    public void editDataModified(EditablePlot source, int dataset);
}
