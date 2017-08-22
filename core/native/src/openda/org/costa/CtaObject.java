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

package org.costa;

import org.openda.utils.Instance;

/**
 * Costa Object (stores Costa handle, initializes DLL)
 */


public class CtaObject extends Instance {

    protected final int CtaNULL = 0;
    protected final int CtaOK = 0;

    static {
        // load OpenDA/Costa bridging DLL once.
		// Libxml2.dll is part of jre\bin and not jdk\bin, so explicitly load OpenDA's version.
		// Only relevant outside of IDE's - which always use JDK - so check on OPENDA_BINDIR.
		String openda_bindir = System.getenv("OPENDA_BINDIR");
		if (openda_bindir == null) {
			String user_dir = System.getProperty("user.dir");
			File openda_bin_directory = new File(user_dir, "bin");
			if (openda_bin_directory.getPath().endsWith("\\public\\bin")) {
				openda_bindir = openda_bin_directory.getPath();
			}
		}
		if (openda_bindir != null) {
			File lib2xml_directory = new File(openda_bindir);
			if (lib2xml_directory.isDirectory()) {
				File libxml2_filepath = new File(lib2xml_directory.getPath(), "libxml2.dll");
				System.load(libxml2_filepath.getPath());
			}
		}
		System.loadLibrary("opendabridge"); //Should be lowercase on linux; case does not matter on windows.
        ctaInit();
    }
    
    protected int ctaHandle = CtaNULL; // Costa handle to tree vector
    protected int ctaLastError = CtaOK; // Result of last error call

    
    public void finalize() throws Throwable {
        this.free();
        super.finalize();
    }

    public void free() {
        if ( ctaHandle != CtaNULL ) {
            this.ctaFree();
        }
    }

    private static native void ctaInit();

    public native void ctaFree();

}
