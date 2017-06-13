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

package org.openda.costa;

import org.openda.utils.Instance;

/**
 * Costa Object (stores Costa handle, initializes DLL)
 */


public class CtaObject extends Instance {

    protected final int CtaNULL = 0;
    protected final int CtaOK = 0;
	private Object lock1 = new Object();

    static {
		CtaInitialize.initialize();
        // load OpenDA/Costa bridging DLL once.
        //System.loadLibrary("opendabridge");
        //ctaInit();
    }

    protected int ctaHandle = CtaNULL; // Costa handle to tree vector
    protected int ctaLastError = CtaOK; // Result of last error call

    
    public void finalize() throws Throwable {
        if (ctaHandle!=0){this.free();}
        super.finalize();
    }


    public void free() {
        synchronized (lock1) {
            if ( ctaHandle != CtaNULL ) {
                this.ctaFree();
                ctaHandle=CtaNULL;
            }
        }
    }

    public native void ctaFree();

	// Return the native handle of the Object
	// This method can only be used with care and only for linking user routines to
	// native code!
	public int getOpenDANativeHandle(){
		return ctaHandle;
	}

}
