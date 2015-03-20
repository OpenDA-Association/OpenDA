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

import java.awt.Window;

//////////////////////////////////////////////////////////////////////////
//// CloseListener
/**
This is an interface for listeners that need to be informed when a
window closes.  Note that this is a very small subset of what Java's
WindowListener interface does.  This class is a workaround for a bug
in Java's AWT, where components are not informed in any way when the
window that contains them is closed, even though they can have
registered listeners.  The listeners are never called, unless the
component is a top-level window. A listener that implements this
interface, by contrast, is informed regardless of whether it is
at the top level. This is used, for example, by the ComponentDialog
class.
@see ComponentDialog

@author Edward A. Lee
@version $Id: CloseListener.java,v 1.6 2003/01/08 02:24:31 ptII Exp $
@since Ptolemy II 1.0
*/
public interface CloseListener {

    /** Notify that the specified window has closed.  The second argument,
     *  if non-null, gives the name of the button that was used to close
     *  the window.
     *  @param window The window that closed.
     *  @param button The name of the button that was used to close the window.
     */
    public void windowClosed(Window window, String button);
}
