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

package org.openda.model_hspf;

import java.io.File;

import org.openda.interfaces.IConfigurable;
import org.openda.utils.Results;

/**
 * Utility class to delete all data in an existing wdm file.
 * For each dataSet in the wdm file all data is removed, so that
 * the dataSet becomes empty. The resulting wdm file still
 * contains the same dataSets, only all dataSets are empty.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class ClearWdmFile implements IConfigurable {

    /**
     * Utility class to delete all data in an existing wdm file.
     * For each dataSet in the wdm file all data is removed, so that
     * the dataSet becomes empty. The resulting wdm file still
     * contains the same dataSets, only all dataSets are empty.
     *
     * To manually open and edit a wdm file use WDMUtil, which
     * is installed as part of the BASINS package, which is available from:
     * http://water.epa.gov/scitech/datait/models/basins/index.cfm
     *
     * @param workingDir the working directory.
     * @param arguments the first argument should be the path of the wdm.dll file (relative to the working directory),
     *                  the second argument should be the path of the message file (relative to the working directory),
     *                  the third argument should be the path of the wdm file to clear (relative to the working directory).
     *                  for which exchange items should be made.
     */
    @Override
    public void initialize(File workingDir, String[] arguments) {
        //initialize wdmDll.
        if (arguments == null || arguments.length < 1) {
            throw new IllegalArgumentException("ClearWdmFile: No arguments specified."
                    + " The first argument should be the path of the wdm.dll file (relative to working directory).");
        }
        File wdmDllFile = new File(workingDir, arguments[0]);
        WdmDll.initialize(wdmDllFile);
        WdmDll wdmDll = WdmDll.getInstance();

        //initialize wdmMessageFile.
        if (arguments.length < 2) {
            throw new IllegalArgumentException("ClearWdmFile: No message file argument specified."
                    + " The second argument should be the path of the message file (relative to the working directory).");
        }
        File wdmMessageFile = new File(workingDir, arguments[1]);
        if (!wdmMessageFile.exists()) {
            throw new IllegalArgumentException("ClearWdmFile: Time series file '"
                    + wdmMessageFile.getAbsolutePath() + "' does not exist.");
        }

        //initialize wdmTimeSeriesFile.
        if (arguments.length < 3) {
            throw new IllegalArgumentException("ClearWdmFile: No wdmFile argument specified."
                    + " The third argument should be the path of the wdm file to clear (relative to the working directory).");
        }
        File wdmTimeSeriesFile = new File(workingDir, arguments[2]);
        if (!wdmTimeSeriesFile.exists()) {
            throw new IllegalArgumentException("ClearWdmFile: Time series file '"
                    + wdmTimeSeriesFile.getAbsolutePath() + "' does not exist.");
        }

        clearWdmFile(wdmDll, wdmMessageFile, wdmTimeSeriesFile);
    }

    private static void clearWdmFile(WdmDll wdmDll, File wdmMessageFile, File wdmTimeSeriesFile) {
        //open wdm file.
        //create a unique wdmFileNumber to use for this wdm file.
        int wdmTimeSeriesFileNumber = WdmUtils.generateUniqueFortranFileUnitNumber();
        Results.putMessage(ClearWdmFile.class.getSimpleName() + ": deleting times and values in wdm file "
                + wdmTimeSeriesFile.getAbsolutePath() + " with fortran unit number " + wdmTimeSeriesFileNumber + ".");
        WdmUtils.openWdmFile(wdmDll, wdmTimeSeriesFileNumber, wdmTimeSeriesFile.getAbsolutePath(), wdmMessageFile.getAbsolutePath());

        //delete values.
        WdmUtils.deleteTimesAndValues(wdmDll, wdmTimeSeriesFileNumber);

        //close wdm file.
        WdmUtils.closeWdmFile(wdmDll, wdmTimeSeriesFileNumber);
    }
}
