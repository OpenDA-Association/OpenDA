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
package org.openda.examples.simplef90model;

import java.io.File;

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IModelInstance;

/**
 * Executable version (i.e. version with static main method) of SimpleModelInstanceTest
 */
public class SimpleModelInstanceTestExe {

    public static void main(String[] args) {

        File modelInstancesParentDir = new File(".");

        File simpleFortranDll;
        if (SimpleModelDLL.RUNNING_ON_WINDOWS) {
            simpleFortranDll = new File(".", "lib/win32_ifort/simplefortrandll.dll");
        } else {
            simpleFortranDll = new File(".", "lib/linux32_gnu/libsimplefortran.so");
        }

        if (args.length > 0 ) {
            if (isHelpArgument(args[0])) {
                printUsage();
                return;
            } else {
                for (String arg : args) {
                    if (!arg.startsWith("-P") && !arg.startsWith("-N")) {
                        printUsage();
                        return;
                    }
                    String userFilePath = arg.substring(2);
                    File userFile;
                    if (new File(userFilePath).isAbsolute()) {
                         userFile = new File(userFilePath);
                    } else {
                        userFile = new File(".", userFilePath);
                    }
                    if (arg.startsWith("-P")) {
                        modelInstancesParentDir = userFile;
                    } else { // "-N"
                        simpleFortranDll = userFile;
                    }
                }
            }
        }

        /*
         * Start real test
         */
        //TODO
    }

    private static void printUsage() {
        System.out.println("Usage: <progName> [ -P<modelInstancesParentDir> ] [ -N<native DLL/SO> ]");
        System.out.println("       default modelInstancesParentDir: . (current working directory)");
        System.out.println("       default modelInstancesParentDir, Windows: ./simplefortrandll.dll");
        System.out.println("       default modelInstancesParentDir,   Linux: ./libsimplefortran.so");
    }

    private static boolean isHelpArgument(String arg) {
        String[] helpArgs = new String[] {"-h", "-h", "-help", "help", "-?", "?"};
        for (String helpArg : helpArgs) {
            if (arg.equalsIgnoreCase(helpArg)) {
                return true;
            }
        }
        return false;
    }

}
