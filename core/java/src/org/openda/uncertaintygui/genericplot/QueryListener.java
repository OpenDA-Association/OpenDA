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

import java.util.EventListener;

//////////////////////////////////////////////////////////////////////////
//// QueryListener
/**
Listener interface for changes in the query box.

@author Edward A. Lee
@version $Id: QueryListener.java,v 1.7 2003/01/08 02:25:04 ptII Exp $
@since Ptolemy II 0.3
*/
public interface QueryListener extends EventListener {

    ///////////////////////////////////////////////////////////////////
    ////                         public methods                    ////

    /** Called to notify that one of the entries has changed.
     *  The name of the entry is passed as an argument.
     *  @param name The name of the entry.
     */
    public abstract void changed(String name);
}
