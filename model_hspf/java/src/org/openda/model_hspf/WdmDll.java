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

package org.openda.model_hspf;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

import org.openda.blackbox.config.BBUtils;
import org.openda.utils.Results;

import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;

/**
 * Thin 'wrapping class' around the native fortran dll for the wdm library.
 * The only purposes of this class is to translate java arguments into fortran arguments and vice versa.
 *
 * The subroutines and functions in the dll file were compiled from the
 * libanne 4.0 library (see http://water.usgs.gov/software/LIBANNE/).
 *
 * @author Arno Kockx
 */
public class WdmDll {

    // Flag indicating whether the native code has been initialized or not 
    private static boolean dllYetToBeInitialized = true;

    // native library
    private static IWdmFortranNativeDLL nativeDLL;

    /**
     * Initialize the wdm library.
     *
     * @param wdmDllPath Full path specification of the native DLL.
     */
    public static void initialize(File wdmDllPath) {

        // Initialize the DLL, if not done yet
        if (dllYetToBeInitialized) {
            Results.putMessage(WdmDll.class.getSimpleName() + ": initializing wdm dll " + wdmDllPath.getAbsolutePath());

            String nativeDllPath = wdmDllPath.getAbsolutePath();
            File nativeDll = new File(nativeDllPath);
            if (!nativeDll.exists()) {
                throw new RuntimeException("WdmDll: Native DLL/SO does not exist: " + wdmDllPath.getAbsolutePath());
            }

            if(BBUtils.RUNNING_ON_WINDOWS) {
                nativeDLL = (IWdmFortranNativeDLL) Native.loadLibrary(nativeDllPath, IWdmFortranNativeDLL.class);
            } else {
            	// For now assumes that gfortran is used for linux and ifort for windows
            	GfortranWdmFunctionMapper gfortranMapper = new GfortranWdmFunctionMapper();
            	HashMap<String, String> gfortranMap = gfortranMapper.getMap();
                nativeDLL = (IWdmFortranNativeDLL) Native.loadLibrary(nativeDllPath, IWdmFortranNativeDLL.class, gfortranMap);
            }

            dllYetToBeInitialized = false;

            Results.putMessage(WdmDll.class.getSimpleName() + ": wdm dll initialized.");
        }
    }

    public static void free() {
        // todo: check how DLL can be unloaded
        // nativeDLL.destroy();
    }

    public static WdmDll getInstance() {
        if (dllYetToBeInitialized) {
            throw new IllegalStateException("WdmDll has not been initialized yet.");
        }
        return new WdmDll();
    }

    private WdmDll() {
    }

    /**
     * Opens the given wdm file at the given fortran unit number.
     *
     * @param wdmFileNumber
     * @param wdmFilePath
     * @param readOnlyFlag
     */
    public void openWdmFile(int wdmFileNumber, String wdmFilePath, int readOnlyFlag) {
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdbopn_(new IntByReference(wdmFileNumber), wdmFilePath,
                new IntByReference(readOnlyFlag), returnCode, wdmFilePath.length());
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdbopn_ , returnCode = " + returnCode.getValue());
        }
    }

    /**
     * Removes the WDM file at the given fortran unit number
     * from the open WDM buffer and adjust buffer accordingly.
     * For this to work a valid WDM message file must be open.
     *
     * @param wdmFileNumber
     */
    public void removeWdmFileFromBuffer(int wdmFileNumber) {
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdflcl_(new IntByReference(wdmFileNumber), returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdflcl_ , returnCode = " + returnCode.getValue());
        }
    }

    /**
     * Closes the file at the given fortran unit number.
     *
     * @param fileNumber
     */
    public void closeFile(int fileNumber) {
        nativeDLL.wdm_close_(new IntByReference(fileNumber));
    }

    /**
     * Note: this method throws an exception if there is no data available for the given dataSetNumber. So it is needed to check if there is data before calling this method.
     */
    public int[] getStartDate(int wdmFileNumber, int dataSetNumber) {
        int[] startDate = new int[6];
        int[] endDate = new int[6];
        Arrays.fill(startDate, 0);
        Arrays.fill(endDate, 0);

        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdatim_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                startDate, endDate, new IntByReference(), new IntByReference(), returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdatim_ , returnCode = " + returnCode.getValue());
        }

        return startDate;
    }

    /**
     * Note: this method throws an exception if there is no data available for the given dataSetNumber. So it is needed to check if there is data before calling this method.
     */
    public int[] getEndDate(int wdmFileNumber, int dataSetNumber) {
        int[] startDate = new int[6];
        int[] endDate = new int[6];
        Arrays.fill(startDate, 0);
        Arrays.fill(endDate, 0);

        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdatim_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                startDate, endDate, new IntByReference(), new IntByReference(), returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdatim_ , returnCode = " + returnCode.getValue());
        }

        return endDate;
    }

    /**
     * Note: this method throws an exception if there is no data available for the given dataSetNumber. So it is needed to check if there is data before calling this method.
     */
    public int getTimeStep(int wdmFileNumber, int dataSetNumber) {
        IntByReference timeStep = new IntByReference();
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdatim_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                new int[6], new int[6], timeStep, new IntByReference(), returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdatim_ , returnCode = " + returnCode.getValue());
        }
        return timeStep.getValue();
    }

    /**
     * Note: this method throws an exception if there is no data available for the given dataSetNumber. So it is needed to check if there is data before calling this method.
     */
    public int getTimeUnit(int wdmFileNumber, int dataSetNumber) {
        IntByReference timeUnit = new IntByReference();
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdatim_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                new int[6], new int[6], new IntByReference(), timeUnit, returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdatim_ , returnCode = " + returnCode.getValue());
        }
        return timeUnit.getValue();
    }

    /**
     * Returns the next existing dataSetNumber equal to or greater than the given dataSetNumber
     * in the given wdmFile.
     *
     * @param wdmFileNumber
     * @param dataSetNumber
     * @return next existing dataSetNumber.
     */
    public int getNextDataSetNumber(int wdmFileNumber, int dataSetNumber) {
        IntByReference dsn = new IntByReference(dataSetNumber);
        nativeDLL.wddsnx_(new IntByReference(wdmFileNumber), dsn);
        return dsn.getValue();
    }

    /**
     * Returns the attribute value for the given attributeIndex, length, dataSetNumber and wdmFile.
     * Returns null if attribute not defined.
     *
     * Important note:
     * See https://github.com/djlampert/PyHSPF/blob/master/src/pyhspf/core/wdmutil.py (get_attribute method) and model_hspf/doc/WDM_attributes.xlsx for
     * information about the location (indices and lengths) of the different attributes in the WDM file attribute memory. This can be used to read specific attributes directly.
     *
     * @param wdmFileNumber
     * @param dataSetNumber
     * @param attributeIndex
     * @param length
     * @return String attributeValue.
     */
    public String getAttributeValue(int wdmFileNumber, int dataSetNumber, int attributeIndex, int length) {
        byte[] result = new byte[length];
        //clear result bytes.
        final byte clearByte = 0;
        Arrays.fill(result, clearByte);

        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdbsgc_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                new IntByReference(attributeIndex), new IntByReference(length),
                result, returnCode, 1);
        if (returnCode.getValue() == 0) {
            //convert result bytes to String.
            return new String(result);
        }
        if (returnCode.getValue() == -107) {//if attribute not defined.
            //throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdbsgc_ , returnCode = -107 (attribute with index " + attributeIndex + " not present on data set with number " + dataSetNumber + ").");
            return null;
        }
        if (returnCode.getValue() == -81) {
            throw new IllegalStateException("WdmDll: Invalid result from call to subroutine dll.wdbsgc_ , returnCode = -81 (data set with number " + dataSetNumber + " does not exist).");
        }

        throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdbsgc_ , returnCode = " + returnCode.getValue());
    }

    public double[] getValues(int wdmFileNumber, int dataSetNumber, int timeStep, int[] startDate,
            int numberOfValues, int transformationCode, int allowedQualityCode, int timeUnit) {

        float[] retrievedValues = new float[numberOfValues];
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdtget_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                new IntByReference(timeStep), startDate, new IntByReference(numberOfValues),
                new IntByReference(transformationCode), new IntByReference(allowedQualityCode),
                new IntByReference(timeUnit), retrievedValues, returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdtget_ , returnCode = " + returnCode.getValue());
        }

        //convert float values to doubles.
        return BBUtils.toDoubleArray(retrievedValues);
    }

    public void putValues(int wdmFileNumber, int dataSetNumber, int timeStep, int[] startDate,
            int overwriteData, int qualityCode, int timeUnit, double[] values) {

        IntByReference numberOfValues = new IntByReference(values.length);
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wdtput_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                new IntByReference(timeStep), startDate, numberOfValues,
                new IntByReference(overwriteData), new IntByReference(qualityCode),
                new IntByReference(timeUnit), BBUtils.toFloatArray(values), returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wdtput_ , returnCode = " + returnCode.getValue());
        }
    }

    public void deleteValues(int wdmFileNumber, int dataSetNumber, int[] deleteFromDate, int deleteAll) {
        IntByReference returnCode = new IntByReference(-1);
        nativeDLL.wtddel_(new IntByReference(wdmFileNumber), new IntByReference(dataSetNumber),
                deleteFromDate, new IntByReference(deleteAll), returnCode);
        if (returnCode.getValue() != 0) {
            throw new RuntimeException("WdmDll: Invalid result from call to subroutine dll.wtddel_ , returnCode = " + returnCode.getValue());
        }
    }

    /**
     * Returns the number of timeSteps between the given dates.
     *
     * @param startDate
     * @param endDate
     * @param timeUnit
     * @param timeStep
     * @return numberOfTimeSteps.
     */
    public int getNumberOfTimeSteps(int[] startDate, int[] endDate, int timeUnit, int timeStep) {
        IntByReference numberOftimeSteps = new IntByReference(-1);
        nativeDLL.timdif_(startDate, endDate, new IntByReference(timeUnit), new IntByReference(timeStep), numberOftimeSteps);
        return numberOftimeSteps.getValue();
    }

    public int[] getNextValidDate(int[] startDate, int timeUnit, int timeStep, int numberOftimeSteps) {
        int[] nextDate = new int[6];
        Arrays.fill(nextDate, 0);
        nativeDLL.timadd_(startDate, new IntByReference(timeUnit), new IntByReference(timeStep),
                new IntByReference(numberOftimeSteps), nextDate);
        return nextDate;
    }
}
