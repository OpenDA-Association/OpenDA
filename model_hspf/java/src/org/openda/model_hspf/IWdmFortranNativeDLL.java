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

import com.sun.jna.Library;
import com.sun.jna.ptr.IntByReference;

/**
 * This interface lists the subroutines and functions that are available
 * in the native fortran dll for the wdm library.
 * It is a Fortran DLL, so:
 * - all arguments go by reference
 * - at the end of a method with (a) string argument(s), the string length(s) are given (#char.s)
 *
 * The subroutines and functions in the dll file were compiled from the
 * libanne 4.0 library (see http://water.usgs.gov/software/LIBANNE/).
 *
 * @author Arno Kockx
 */
public interface IWdmFortranNativeDLL extends Library {

    /**
     * Open a WDM file.  File is opened as new or old, depending on
     * the value of RONWFG.  The common block related to the WDM record
     * buffer are initialized the first time this routine is called.
     *
     * @param wdmFileFortranUnitNumber I*4   (In:)  Fortran unit number of the WDM file
     * @param wdmFilePath              C*(*) (In:)  path name of the WDM file
     * @param readOnlyFlag             I*4   (In:)  read only/new file flag
     *                                              0- normal open of existing WDM file,
     *                                              1- open WDM file as read only (system dependent),
     *                                              2- open new WDM file
     * @param returnCode               I*4   (Out:) return code
     *                                               0 - successful open
     *                                               1 - successful open, but invalid WDM file
     *                                              <0 - error on open, -IOSTAT, compiler specific
     * @param wdmFilePathLength                     length of wdmFilePath String
     */
    void wdbopn_(IntByReference wdmFileFortranUnitNumber, String wdmFilePath,
            IntByReference readOnlyFlag, IntByReference returnCode, int wdmFilePathLength);

    /**
     * Remove a WDM file from the open WDM buffer and adjust
     * buffer accordingly.
     *
     * @param wdmFileFortranUnitNumber I*4   (In:)  Fortran unit number of WDM file
     * @param returnCode               I*4   (Out:) return code
     *                                                0 - everything ok
     *                                              -87 - can't remove message WDM file from buffer
     */
    void wdflcl_(IntByReference wdmFileFortranUnitNumber, IntByReference returnCode);

    /**
     * gets timeseries information from the WDMSFL
     *
     * @param wdmFileFortranUnitNumber I*4     (In:)  watershed data management file unit number
     * @param dataSetNumber            I*4     (In:)  data-set number
     * @param timeStep                 I*4     (In:)  time step for get
     * @param startDate                I*4 (6) (In:)  starting date
     * @param numberOfValues           I*4     (In:)  number of values
     * @param transformationCode       I*4     (In:)  transformation code
     *                                                0 - ave,same                                              
     *                                                1 - sum,div                                               
     *                                                2 - max                                                   
     *                                                3 - min  
     * @param allowedQualityCode       I*4     (In:)  allowed quality code. For values with a quality code
     *                                                that is higher than the given quality code, this
     *                                                method returns 0 values.
     * @param timeUnit                 I*4     (In:)  time units for get
     * @param retrievedValues          R   (V) (Out:) array to place retrieved values in
     * @param returnCode               I*4     (Out:) return code
     *                                                  0 - everything O.K.
     *                                                 -8 - invalid date
     *                                                -14 - date specified not within valid range for data set
     *                                                -20 - problem with one or more of following:
     *                                                      GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
     *                                                -21 - date from WDM doesn't match expected date
     *                                                -81 - data set does not exist
     *                                                -82 - data set exists, but is wrong DSTYP
     *                                                -84 - data set number out of range
     */
    void wdtget_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber, IntByReference timeStep,
            int[] startDate, IntByReference numberOfValues, IntByReference transformationCode,
            IntByReference allowedQualityCode, IntByReference timeUnit, float[] retrievedValues,
            IntByReference returnCode);

    /**
     * Puts time series data into a WDM file.  This routine traps the
     * problem with overwritting existing data.
     *
     * @param wdmFileFortranUnitNumber I*4     (In:)  watershed data management file unit number
     * @param dataSetNumber            I*4     (In:)  data-set number
     * @param timeStep                 I*4     (In:)  time step for put
     * @param startDate                I*4 (6) (In:)  starting date
     * @param numberOfValues           I*4     (In:)  number of values
     * @param overwriteData            I*4     (In:)  data overwrite flag,
     *                                                0 - dont overwrite
     *                                                1 - overwrite O.K.
     * @param qualityCode              I*4     (In:)  quality code for the written values
     * @param timeUnit                 I*4     (In:)  time units for put
     * @param values                   R   (V) (In:)  array for writing out values
     * @param returnCode               I*4     (Out:) return code
     *                                                  0 - everything is O.K.
     *                                                 -8 - invalid date
     *                                                 -9 - data not present in current group
     *                                                -10 - no data in this group
     *                                                -11 - no non missing data, data has not started yet
     *                                                -14 - date specified not within valid range for data set
     *                                                -15 - VBTIME=1 and DELT,TUNITS do not agree
     *                                                      with the data set
     *                                                -20 - problem with one or more of following:
     *                                                      DTOVWR, NVAL, QUALFG, TUNITS, DELT
     *                                                -21 - date from WDM doesn't match expected date
     *                                                -81 - data set does not exist
     *                                                -82 - data set exists, but is wrong DSTYP
     *                                                -84 - data set number out of range
     *                                                -85 - trying to write to a read-only data set
     */
    void wdtput_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber, IntByReference timeStep,
            int[] startDate, IntByReference numberOfValues, IntByReference overwriteData,
            IntByReference qualityCode, IntByReference timeUnit, float[] values,
            IntByReference returnCode);

    /**
     * delete all data following a specified date in the given
     * data set
     *
     * @param wdmFileFortranUnitNumber I*4     (In:)  watershed data management file unit number
     * @param dataSetNumber            I*4     (In:)  data-set number
     * @param deleteFromDate           I*4 (6) (In:)  delete from date array
     * @param deleteAll                I*4     (In:)  delete all flag, 0 - only delete group
                                                                       1 - all data after date
     * @param returnCode               I*4     (Out:) return code
     *                                                  0 - everything is O.K.
     *                                                 -6 - no data present
     *                                                -10 - no data in this group
     *                                                -11 - no non missing data, data has not started yet
     *                                                -14 - date specified not within valid range for data set
     *                                                -21 - date from WDM doesn't match expected date
     *                                                -81 - data set does not exist
     *                                                -82 - data set exists, but is wrong DSTYP
     *                                                -84 - data set number out of range
     *                                                -85 - trying to write to a read-only data set
     */
    void wtddel_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber,
            int[] deleteFromDate, IntByReference deleteAll, IntByReference returnCode);

    /**
     * Gets the start and end dates of the data in a time series data set
     * and the time step of the data.
     *
     * @param wdmFileFortranUnitNumber I*4     (In:)  Fortran unit number of WDM file
     * @param dataSetNumber            I*4     (In:)  data set number
     * @param startDate                I*4 (6) (Out:) start of record
     * @param endDate                  I*4 (6) (Out:) end of record
     * @param timeStep                 I*4     (Out:) time step of data, in TCODE units
     * @param timeUnit                 I*4     (Out:) time units of data
     *                                                1 - second        4 - day                                 
     *                                                2 - minute        5 - month                               
     *                                                3 - hour          6 - year
     * @param returnCode               I*4     (Out:) return code
     *                                                    0 - all information successfully retrieved
     *                                                   -6 - no data in the data set
     *                                                  -81 - data set does not exist
     *                                                  -82 - data set is not a time-series data set
     *                                                 -107 - one or more attributes not present in data set
     */
    int wdatim_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber,
            int[] startDate, int[] endDate, IntByReference timeStep, IntByReference timeUnit,
            IntByReference returnCode);

    /**
     * Check data set for existance and type, returns:
     *     0 - data set does not exist
     * or data-set type
     *     1 - time series      6 - rastor
     *     2 - table            7 - space-time
     *     3 - schematic        8 - attribute
     *     4 - project          9 - message
     *     5 - vector
     *
     * @param wdmFileFortranUnitNumber I*4     (In:)  Fortran unit number of WDM file
     * @param dataSetNumber            I*4     (In:)  data-set number to be checked
     * @return data-set type, if data set exists.
     */
    int wdckdt_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber);

    /**
     * Calls WDDSNP to search for the next data set greater than or equal to DSN.
     *
     * @param wdmFileFortranUnitNumber I*4  (In:)    Fortran unit number of WDM file
     * @param dataSetNumber            I*4  (InOut:) input:  first data set to be checked
     *                                                       valid range is 1 to 32000, inclusive
     *                                               output: -1 if the input DSN is outside the valid range
     *                                                   or, the input DSN if it exists
     *                                                   or, the next existing data set after DSN
     *                                                   or, -1 if no data sets >= the input DSN exist
     */
    void wddsnx_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber);

    /**
     * gets values of character search attribute for a dsn
     *
     * @param wdmFileFortranUnitNumber I*4     (In:)  watershed data management file unit number
     * @param dataSetNumber            I*4     (In:)  data-set number to check
     * @param attributeIndex           I*4     (In:)  index number of attribute
     * @param attributeLength          I*4     (In:)  length of attribute
     * @param attributeValue           C*1 (V) (Out:) value of attribute
     * @param returnCode               I*4     (Out:) return code,
     *                                                   0 - attribute value returned
     *                                                 -81 - data set does not exist
     *                                                -107 - attribute not present on this data set
     * @param attributeValueStringLength this must always be 1.
     *                                   See definition of fortran subroutine WDBSGC:
     *                                   CHARACTER*1 SAVAL(SALEN)
     *                                   this means that SAVAL is a String array with length SALEN
     *                                   that contains Strings with numberOfCharacters = 1.
     *                                   Here argument attributeLength is SALEN and
     *                                   argument attributeValueStringLength is numberOfCharacters = 1.  
     */
    void wdbsgc_(IntByReference wdmFileFortranUnitNumber, IntByReference dataSetNumber,
            IntByReference attributeIndex, IntByReference attributeLength, byte[] attributeValue,
            IntByReference returnCode, int attributeValueStringLength);

    /**
     * Calculate the number of time steps between two dates.  Part
     * intervals at a time step less than TCODE and TSSTEP are not
     * included.  If the second date is before the first date, or the
     * second date is the same as the first date, the number of time
     * steps will be returned as 0.  Dates are assumed to be valid.
     *
     * @param startDate                I*4 (6) (In:)  first (starting) date
     * @param endDate                  I*4 (6) (In:)  second (ending) date
     * @param timeUnit                 I*4     (In:)  time units code
     *                                                1 - seconds     5 - months
     *                                                2 - minutes     6 - years
     *                                                3 - hours       7 - centuries
     *                                                4 - days
     * @param timeStep                 I*4     (In:)  time step in TCODE units
     * @param numberOfTimeSteps        I*4     (Out:) number of time steps between DATE1 and DATE2
     */
    void timdif_(int[] startDate, int[] endDate, IntByReference timeUnit, IntByReference timeStep, IntByReference numberOftimeSteps);

    /**
     * Add NVALS time steps to first date to compute second date.
     * The first date is assumed to be valid.
     *
     * @param startDate                I*4 (6) (In:)  starting date
     * @param timeUnit                 I*4     (In:)  time units
     *                                                1 - second     5 - month
     *                                                2 - minute     6 - year
     *                                                3 - hour       7 - century
     *                                                4 - day
     * @param timeStep                 I*4     (In:)  time step in TCODE units
     * @param numberOfTimeSteps        I*4     (In:)  number of time steps to be added
     * @param nextDate                 I*4 (6) (Out:) new date
     */
    void timadd_(int[] startDate, IntByReference timeUnit, IntByReference timeStep, IntByReference numberOftimeSteps,
            int[] nextDate);

    /**
     * Open a file for a particular logical unit number.
     *
     * @param fileFortranUnitNumber (In:)  Logical unit number for the file
     * @param filePath              (In:)  Name of the file
     * @param filePathLength               length of filePath String
     */
    void wdm_open_(IntByReference fileFortranUnitNumber, String filePath, int filePathLength);

    /**
     * Close a file for a particular logical unit number.
     *
     * @param fileFortranUnitNumber (In:)  Logical unit number for the file
     */
    void wdm_close_(IntByReference fileFortranUnitNumber);
}
