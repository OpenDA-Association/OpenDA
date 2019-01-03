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
package org.openda.costa;
import java.io.File;

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 7/12/13
 * Time: 3:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class CtaInitialize {


	static {
	        // load OpenDA/Costa bridging DLL once.
		    System.out.println("Load library to native implementations opendabridge:");
		    // Debugging all libraries except libcta.dll are loading on win-xp
//		    System.loadLibrary("libxml2");
//		    System.loadLibrary("netcdf");
//		    System.loadLibrary("blas_lapack");
//		    System.loadLibrary("pthreadvce2");
//		    System.loadLibrary("mpich2mpi");
//		    System.loadLibrary("svml_dispmd");
//		    System.loadLibrary("libmmd");
//		    System.loadLibrary("libifcoremd");
//		    System.loadLibrary("libifportmd");
//		    System.loadLibrary("msvcr100");
// -->	    System.loadLibrary("cta"); problems loading this one on windows MVL

			try {
				System.loadLibrary("opendabridge"); //Should be lowercase on linux; case does not matter on windows.
			} catch (java.lang.UnsatisfiedLinkError exception) {
				System.out.println("java.library.path: " + System.getProperty("java.library.path"));
				System.out.println("sun.arch.data.model: " + System.getProperty("sun.arch.data.model"));
				throw exception;
			}
			ctaInit();
		    System.out.println("Set default random seed for native implementations:");
		    setRandomSeed(2101975);
	}

    static public void initialize(){
		System.out.println("Initializing native OpenDA");
//		ctaInit();
	}
	static private native void ctaInit();
	static public native void setRandomSeed(int seed);


}
