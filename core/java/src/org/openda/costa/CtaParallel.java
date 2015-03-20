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
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: Apr 21, 2010
 * Time: 9:30:32 AM
 *
 */
public class CtaParallel {

    /** initialize the native parallel environment,
     * create MPI process groups and only the master process (proc 0) well exit this routine
     *  the other processes will become worker processes waiting for commands from the master
     *
     * @param configFileName   Name of the configuration file defining the process groups
     * @throws RuntimeException
     */

    public static void initParallelEnvironment(String configFileName) throws RuntimeException {
       // Check whether the input file exists
       File configFile = new File(configFileName);
       if (configFile.exists()){

           // Force to init COSTA by creating an object
           CtaObject anObject = new CtaObject();

           
          nativeInit(configFile.getAbsolutePath());
       }
       else {
           // throw error when the config file cannot be found
           throw new RuntimeException("Parallel configuration file "+configFileName+" does not exist");
       }
    }

    public static native void finalizeParallelEnvironment();
        
    private static native void nativeInit(String configFile);

}
