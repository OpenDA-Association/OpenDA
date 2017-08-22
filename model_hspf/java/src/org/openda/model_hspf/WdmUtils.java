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
import java.util.*;

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * Util methods for reading/writing data from/to a wdm file.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class WdmUtils {

    /**
     * Counter to keep track of generated fortranFileUnitNumbers that have already
     * been returned by method generateUniqueFortranFileUnitNumber.
     *
     * Start with 10.
     */
    private static int currentFortranFileUnitNumber = 10;

    /**
     * Generates and returns a unique wdmFileNumber that can be used
     * for one wdm file.
     *
     * @return unique wdmFileNumber.
     */
    public static synchronized int generateUniqueFortranFileUnitNumber() {
        //see http://docs.cray.com/books/S-3695-35/html-S-3695-35/pdollsmg.html
        //and http://gcc.gnu.org/onlinedocs/gcc-3.3.6/g77/Large-File-Unit-Numbers.html
        //The reserved fortran file unit numbers are 0, 5, 6, 100, 101 and 102
        //and in some libraries numbers above 99 are not available.
        //To be on the safe side here use only 11 to 90 and 110 and higher,
        //which should be enough for most applications.
        currentFortranFileUnitNumber++;
        //skip 90 to 110.
        if (currentFortranFileUnitNumber > 90 && currentFortranFileUnitNumber < 110) {
            currentFortranFileUnitNumber = 110;
        }

        return currentFortranFileUnitNumber;
    }

    /**
     * Always use the same fortran file unit number for the wdm message file, otherwise
     * exceptions occur when opening and closing multiple wdm files multiple times.
     */
    private static final int WDM_MESSAGE_FILE_UNIT_NUMBER = generateUniqueFortranFileUnitNumber();

    /**
     * Opens the given wdm time series file at the given fortran unit number.
     *
     * @param wdmDll
     * @param wdmTimeSeriesFileNumber the fortran unit number for the time series file to open.
     * @param wdmTimeSeriesFilePath absolute path of the wdm time series file to open.
     * @param wdmMessageFilePath absolute path of the wdm message file.
     */
    public static void openWdmFile(WdmDll wdmDll, int wdmTimeSeriesFileNumber, String wdmTimeSeriesFilePath, String wdmMessageFilePath) {
        //Note: for the fortran wdm library methods to work properly
        //the wdm message file must be open whenever wdm time series files are used.

        //open message file read only.
        wdmDll.openWdmFile(WDM_MESSAGE_FILE_UNIT_NUMBER, wdmMessageFilePath, 1);
        //open time series file.
        wdmDll.openWdmFile(wdmTimeSeriesFileNumber, wdmTimeSeriesFilePath, 0);
    }

    /**
     * Closes the wdm file at the given fortran unit number.
     *
     * @param wdmDll
     * @param wdmTimeSeriesFileNumber the fortran unit number of the time series file to close.
     */
    public static void closeWdmFile(WdmDll wdmDll, int wdmTimeSeriesFileNumber) {
        //Note: for the fortran wdm library methods to work properly
        //the wdm message file must be open whenever wdm time series files are used.

        //close message file.
        wdmDll.removeWdmFileFromBuffer(WDM_MESSAGE_FILE_UNIT_NUMBER);
        //close time series file.
        wdmDll.removeWdmFileFromBuffer(wdmTimeSeriesFileNumber);
        //call the normal fortran close statement for both files just to be on the safe side.
        wdmDll.closeFile(wdmTimeSeriesFileNumber);
        wdmDll.closeFile(WDM_MESSAGE_FILE_UNIT_NUMBER);
    }

    public static WdmDll initializeWdmDll(File workingDir, String relativeWdmDllPath) {
        File wdmDllFile = new File(workingDir, relativeWdmDllPath);
        WdmDll.initialize(wdmDllFile);
        return WdmDll.getInstance();
    }

    public static String initializeWdmMessageFilePath(File workingDir, String relativeWdmMessageFilePath) {
        File wdmMessageFile = new File(workingDir, relativeWdmMessageFilePath);
        if (!wdmMessageFile.exists()) {
            throw new IllegalArgumentException(WdmUtils.class.getSimpleName() + ": Message file '" + wdmMessageFile.getAbsolutePath() + "' does not exist.");
        }
        return wdmMessageFile.getAbsolutePath();
    }

    public static IPrevExchangeItem.Role initializeRole(String roleString) {
        if ("input".equalsIgnoreCase(roleString)) return IPrevExchangeItem.Role.Input;
        if ("output".equalsIgnoreCase(roleString)) return IPrevExchangeItem.Role.Output;

        throw new IllegalArgumentException(WdmUtils.class.getSimpleName() + ": unknown role type '" + roleString + "' specified.");
    }

    /**
     * Reads all available times and values for the given wdmTimeSeriesExchangeItem
     * from the given wdm file and sets these in the given wdmTimeSeriesExchangeItem.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number.
     * @param wdmFilePath
     * @param wdmTimeSeriesExchangeItem
     * @param timeZone
     */
    /*
    public static void readTimesAndValues(WdmDll wdmDll, int wdmFileNumber, String wdmFilePath,
            WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem, TimeZone timeZone) {

        //get dataSetNumber that corresponds to the time series for the given wdmTimeSeriesExchangeItem.
        int dataSetNumber = getDataSetNumber(wdmDll, wdmFileNumber, wdmFilePath, wdmTimeSeriesExchangeItem);

        //get period to read.
        int[] startDateArray = getAvailableDataStartDateArray(wdmDll, wdmFileNumber, dataSetNumber);
        int[] endDateArray = getAvailableDataEndDateArray(wdmDll, wdmFileNumber, dataSetNumber);
        if (startDateArray == null || endDateArray == null) {
            //if no data available, then clear data in wdmTimeSeriesExchangeItem using empty arrays.
            Results.putMessage(WdmUtils.class.getSimpleName() + ": no data available for location "
                    + wdmTimeSeriesExchangeItem.getLocation() + " and quantity "
                    + wdmTimeSeriesExchangeItem.getQuantityId() + " in wdm file " + wdmFilePath);
            wdmTimeSeriesExchangeItem.setData(new double[0], new double[0]);
            return;
        }

        //read data.
        double[] times = readTimes(wdmDll, wdmFileNumber, dataSetNumber, startDateArray, endDateArray, timeZone);
        double[] values = readValues(wdmDll, wdmFileNumber, dataSetNumber, startDateArray, endDateArray);
        wdmTimeSeriesExchangeItem.setData(times, values);
    }
    */

    /**
     * Reads all available times and values between the given requestedStartTime and requestedEndTime
     * for the given wdmTimeSeriesExchangeItem
     * from the given wdm file and sets these in the given wdmTimeSeriesExchangeItem.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number.
     * @param wdmFilePath
     * @param wdmTimeSeriesExchangeItem
     * @param requestedStartTime
     * @param requestedEndTime
     * @param timeZone
     */
    public static void readTimesAndValues(WdmDll wdmDll, int wdmFileNumber, String wdmFilePath,
           WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem,
           double requestedStartTime, double requestedEndTime, TimeZone timeZone) {

        //get dataSetNumber that corresponds to the time series for the given wdmTimeSeriesExchangeItem.
        int dataSetNumber = getDataSetNumber(wdmDll, wdmFileNumber, wdmFilePath, wdmTimeSeriesExchangeItem);

        //get period to read.
        int[] availableDataStartDateArray = getAvailableDataStartDateArray(wdmDll, wdmFileNumber, dataSetNumber);
        int[] availableDataEndDateArray = getAvailableDataEndDateArray(wdmDll, wdmFileNumber, dataSetNumber);
        if (availableDataStartDateArray == null || availableDataEndDateArray == null) {
            //if no data available, then clear data in wdmTimeSeriesExchangeItem using empty arrays.
            Results.putMessage(WdmUtils.class.getSimpleName() + ": no data available for location "
                    + wdmTimeSeriesExchangeItem.getLocation() + " and quantity "
                    + wdmTimeSeriesExchangeItem.getQuantityId() + " in wdm file " + wdmFilePath);
            wdmTimeSeriesExchangeItem.setData(new double[0], new double[0]);
            return;
        }

        //determine period between requestedStartTime and requestedEndTime for which there is data available.
        double availableDataStartTime = convertDateArrayToMjd(availableDataStartDateArray, timeZone);
        double availableDataEndTime = convertDateArrayToMjd(availableDataEndDateArray, timeZone);
        double startTime = Math.max(requestedStartTime, availableDataStartTime);
        double endTime = Math.min(requestedEndTime, availableDataEndTime);
        if (startTime > endTime) {//if no data available between requestedStartTime and requestedEndTime.
            //clear data in wdmTimeSeriesExchangeItem using empty arrays.
            Results.putMessage(WdmUtils.class.getSimpleName() + ": no data available for location "
                    + wdmTimeSeriesExchangeItem.getLocation() + " and quantity "
                    + wdmTimeSeriesExchangeItem.getQuantityId() + " between "
                    + new Date(Time.mjdToMillies(requestedStartTime)) + " and "
                    + new Date(Time.mjdToMillies(requestedEndTime)) + " in wdm file " + wdmFilePath);
            wdmTimeSeriesExchangeItem.setData(new double[0], new double[0]);
            return;
        }

        //read data.
        int[] startDateArray = convertMjdToDateArray(startTime, timeZone);
        int[] endDateArray = convertMjdToDateArray(endTime, timeZone);
        double[] times = readTimes(wdmDll, wdmFileNumber, dataSetNumber, startDateArray, endDateArray, timeZone);
        double[] values = readValues(wdmDll, wdmFileNumber, dataSetNumber, startDateArray, endDateArray);
        wdmTimeSeriesExchangeItem.setData(times, values);
    }

    private static int[] getAvailableDataStartDateArray(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber) {
        int[] availableDataStartDateArray;
        try {
            //this call throws an exception if there is no data available for the given dataSetNumber, however this is the only way to check if there is data for the given dataSetNumber.
            availableDataStartDateArray = wdmDll.getStartDate(wdmFileNumber, dataSetNumber);
        } catch (Exception e) {
            //an Exception is thrown if startDate or endDate cannot be determined,
            //this usually means that the dataSet is empty,
            //so no data available, so return null.
            return null;
        }
        return availableDataStartDateArray;
    }

    private static int[] getAvailableDataEndDateArray(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber) {
        int[] availableDataEndDateArray;
        try {
            //this call throws an exception if there is no data available for the given dataSetNumber, however this is the only way to check if there is data for the given dataSetNumber.
            availableDataEndDateArray = wdmDll.getEndDate(wdmFileNumber, dataSetNumber);
        } catch (Exception e) {
            //an Exception is thrown if startDate or endDate cannot be determined,
            //this usually means that the dataSet is empty,
            //so no data available, so return null.
            return null;
        }
        return availableDataEndDateArray;
    }

    /**
     * Reads all times between the given startDate and endDate
     * for the given wdmTimeSeriesExchangeItem from the given wdm file.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number.
     * @param dataSetNumber
     * @param startDate
     * @param endDate
     * @param timeZone
     * @return times.
     */
    private static double[] readTimes(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber,
            int[] startDate, int[] endDate, TimeZone timeZone) {

        //read times.
        int timeUnit = wdmDll.getTimeUnit(wdmFileNumber, dataSetNumber);
        int timeStep = wdmDll.getTimeStep(wdmFileNumber, dataSetNumber);
        int numberOfTimeSteps = wdmDll.getNumberOfTimeSteps(startDate, endDate, timeUnit, timeStep);
        double[] retrievedTimes = new double[numberOfTimeSteps];
        int[] currentDate = startDate;
        retrievedTimes[0] = convertDateArrayToMjd(currentDate, timeZone);
        for (int n = 1; n < retrievedTimes.length; n++) {
            currentDate = wdmDll.getNextValidDate(currentDate, timeUnit, timeStep, 1);
            retrievedTimes[n] = convertDateArrayToMjd(currentDate, timeZone);
        }

        return retrievedTimes;
    }

    /**
     * Reads all values between the given startDate and endDate
     * for the given wdmTimeSeriesExchangeItem from the given wdm file.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number.
     * @param dataSetNumber
     * @param startDate
     * @param endDate
     * @return retrievedValues.
     */
    private static double[] readValues(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber,
            int[] startDate, int[] endDate) {

        //read values.
        int timeUnit = wdmDll.getTimeUnit(wdmFileNumber, dataSetNumber);
        int timeStep = wdmDll.getTimeStep(wdmFileNumber, dataSetNumber);
        int numberOftimeSteps = wdmDll.getNumberOfTimeSteps(startDate, endDate, timeUnit, timeStep);
        //use maximum allowedQualityCode (31) so that all values are read.
        return wdmDll.getValues(wdmFileNumber, dataSetNumber, timeStep, startDate,
                numberOftimeSteps, 0, 31, timeUnit);
    }

    /**
     * Writes all values for the given wdmTimeSeriesExchangeItem
     * to the given wdm file. Overwrites existing data.
     * If no values available, then does nothing.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number.
     * @param wdmFilePath
     * @param wdmTimeSeriesExchangeItem
     * @param timeZone
     */
    /*
    public static void writeTimesAndValues(WdmDll wdmDll, int wdmFileNumber, String wdmFilePath,
            WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem, TimeZone timeZone) {

        double[] times = wdmTimeSeriesExchangeItem.getTimesRef();
        double[] values = wdmTimeSeriesExchangeItem.getValuesRef();
        if (times != null && values != null && times.length > 0 && values.length > 0) {
            //get dataSetNumber that corresponds to the time series for the given wdmTimeSeriesExchangeItem.
            int dataSetNumber = getDataSetNumber(wdmDll, wdmFileNumber, wdmFilePath, wdmTimeSeriesExchangeItem);

            //get timeStep/Unit from dataSet, since the timeStep/Unit of a given dataSet can never change.
            int timeUnit = wdmDll.getTimeUnit(wdmFileNumber, dataSetNumber);
            int timeStep = wdmDll.getTimeStep(wdmFileNumber, dataSetNumber);

            int[] startDate = convertMjdToDateArray(times[0], timeZone);
            //write reliable values, i.e. qualityCode 0.
            wdmDll.putValues(wdmFileNumber, dataSetNumber, timeStep, startDate,
                    1, 0, timeUnit, values);
        }
    }
    */

    /**
     * Writes all values for the given wdmTimeSeriesExchangeItem
     * to the given wdm file. Overwrites existing data.
     *
     * The given startTime and endTime indicate the model run period.
     * The HSPF model cannot cope with missing values within the
     * model run period. Therefore, if there are missing values
     * within the model run period for the given wdmTimeSeriesExchangeItem,
     * then throw an exception here. Throwing an exception here will result in
     * a clear error message for the user. If this exception would not be thrown here,
     * then in case of missing values the HSPF model would give an obscure
     * and unclear error message, e.g.:
     *  ERROR/WARNING ID:   217 103
     *  VOLNO:   616
     *  WDMSFL:     2
     *  Time span of simulation is not within time span of specified WDM
     *  source dataset.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number.
     * @param wdmFilePath
     * @param wdmTimeSeriesExchangeItem
     * @param startTime
     * @param endTime
     * @param timeZone
     */
    public static void writeTimesAndValues(WdmDll wdmDll, int wdmFileNumber, String wdmFilePath,
            WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem, double startTime,
            double endTime, TimeZone timeZone) {

        //get dataSetNumber that corresponds to the time series for the given wdmTimeSeriesExchangeItem.
        int dataSetNumber = getDataSetNumber(wdmDll, wdmFileNumber, wdmFilePath, wdmTimeSeriesExchangeItem);

        //get times and values to write.
        double[] times = wdmTimeSeriesExchangeItem.getTimesRef();
        double[] values = wdmTimeSeriesExchangeItem.getValuesRef();

        //get timeStep.
        double timeStepInDays;
        int timeUnit;
        int timeStepInUnits;
        int[] availableDataStartDateArray = getAvailableDataStartDateArray(wdmDll, wdmFileNumber, dataSetNumber);
        int[] availableDataEndDateArray = getAvailableDataEndDateArray(wdmDll, wdmFileNumber, dataSetNumber);
        if (availableDataStartDateArray == null || availableDataEndDateArray == null) {//if no data available.
            //determine timeStep and timeUnit from the data to write.
            if (times == null || times.length < 2) {
                Results.putMessage(WdmUtils.class.getSimpleName() + ": cannot write data for location "
                        + wdmTimeSeriesExchangeItem.getLocation() + " and quantity "
                        + wdmTimeSeriesExchangeItem.getQuantityId() + " in wdm file " + wdmFilePath
                        + " Reason: cannot determine timeStep, since there is no data in the wdm file and there are less than two values to write. This time series will be skipped.");
                return;
            }
            //this code assumes that the timeStep is the same for all times.
            timeStepInDays = times[1] - times[0];
            //timeStepInUnits and timeUnit are both integers, so timeStep must always be an integer number of years, months, days, hours, minutes OR seconds.
            //Here always use timeUnit second so that it works for most timeSteps.
            timeUnit = 1;//second.
            timeStepInUnits = (int) Math.round(timeStepInDays * 24 * 3600);

        } else {
            //get timeStep/Unit from dataSet (this only works if there is data), since the timeStep/Unit of a given dataSet can never change.
            timeUnit = wdmDll.getTimeUnit(wdmFileNumber, dataSetNumber);
            timeStepInUnits = wdmDll.getTimeStep(wdmFileNumber, dataSetNumber);
            double timeUnitInDays;
            switch(timeUnit) {
                case 1:
                    //second.
                    timeUnitInDays = (double) 1/(24.0 * 3600.0);
                    break;
                case 2:
                    //minute
                    timeUnitInDays = (double) 1/(24.0 * 60.0);
                    break;
                case 3:
                    //hour.
                    timeUnitInDays = (double) 1/24.0;
                    break;
                case 4:
                    //day.
                    timeUnitInDays = 1;
                    break;
                default:
                    throw new RuntimeException("WdmUtils.writeTimesAndValues: TimeUnit " + timeUnit + " currently not supported.");
            }
            timeStepInDays = timeStepInUnits * timeUnitInDays;
        }

        //subtract tolerance of about 1 second to avoid rounding errors.
        int numberOfRequiredTimes = 1 + (int) Math.ceil((endTime - startTime - 1e-5) / timeStepInDays);
        double[] newValues = new double[numberOfRequiredTimes];
        int previousIndex = -1;
        for (int n = 0; n < newValues.length; n++) {
            double searchedTime = startTime + n*timeStepInDays;

            double newValue = Double.NaN;
            if (times != null && values != null) {
                if (previousIndex + 1 >= 0 && previousIndex + 1 < times.length
                        && Math.abs(times[previousIndex + 1] - searchedTime) < 1e-6) {
                    newValue = values[previousIndex + 1];
                    previousIndex = previousIndex + 1;
                } else {
                    for (int i = 0; i < times.length; i++) {
                        if (Math.abs(times[i] - searchedTime) < 1e-6) {
                            newValue = values[i];
                            previousIndex = i;
                            break;
                        }
                    }
                }
            }

            if (Double.isNaN(newValue)) {
                //note: replacing missing values with either -999, -990, -1.00E+30 or NaN results in
                //an overflow error during the model run.
                //throw an exception if there are missing values within the model run period.
                Calendar calendar = Calendar.getInstance();
                calendar.setTimeZone(timeZone);
                calendar.setTimeInMillis(Time.mjdToMillies(searchedTime));
                throw new RuntimeException("Missing data value at "
                        + calendar.getTime().toString() + " for time series with id '"
                        + wdmTimeSeriesExchangeItem.getId() + "' as input for the model."
                        + " The HSPF model cannot handle missing values within the model run period."
                        + " Please check if the input data is correct and if id '" + wdmTimeSeriesExchangeItem.getId()
                        + "' is not duplicated in the model adapter config xml files.");
            }

            newValues[n] = newValue;
        }

        //write values.
        if (newValues != null && newValues.length > 0) {
            int[] startDate = convertMjdToDateArray(startTime, timeZone);
            //write reliable values, i.e. qualityCode 0.
            try {
                wdmDll.putValues(wdmFileNumber, dataSetNumber, timeStepInUnits, startDate, 1, 0, timeUnit, newValues);
            } catch (RuntimeException e) {
                throw new RuntimeException("Error while writing times and values for time series with id '" + wdmTimeSeriesExchangeItem.getId() + "' as input for the model."
                        + " Please check if the input data is correct and if id '" + wdmTimeSeriesExchangeItem.getId() + "' is not duplicated in the model input files.", e);
            }
        }
    }

    /**
     * Deletes all times and values for all dataSets in the wdm file
     * that corresponds to the given wdmFileNumber. The dataSets remain,
     * only they will be empty afterwards.
     *
     * @param wdmDll
     * @param wdmFileNumber
     */
    public static void deleteTimesAndValues(WdmDll wdmDll, int wdmFileNumber) {

        int dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, 1);
        while (dataSetNumber != -1) {
            try {
                int[] startDate = wdmDll.getStartDate(wdmFileNumber, dataSetNumber);
                //delete values.
                wdmDll.deleteValues(wdmFileNumber, dataSetNumber, startDate, 1);
            } catch (Exception e) {
                //an Exception is thrown if startDate cannot be determined,
                //this usually means that the dataSet is already empty,
                //so continue with next dataSet.
            }

            //get next existing dataSet and put its number in the variable dataSetNumber.
            dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, dataSetNumber + 1);
        }
    }

    /**
     * Creates an exchange item with the given role for each dataSet in the wdm file with the given unit number.
     */
    public static List<WdmTimeSeriesExchangeItem> createExchangeItemsFromFile(WdmDll wdmDll, int wdmFileNumber, IPrevExchangeItem.Role role) {
        List<WdmTimeSeriesExchangeItem> exchangeItems = new ArrayList<WdmTimeSeriesExchangeItem>();

        //get first existing dataSet and put its number in the variable dataSetNumber.
        //for dataSetNumber the valid range is 1 (inclusive) to 32000 (inclusive), so start with 1.
        int dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, 1);
        while (dataSetNumber != -1) {
            //get the first attributeValue.
            String constituent = WdmUtils.getConstituentAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            String stationName = WdmUtils.getStationNameAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            //if stationName is empty, use location.
            if (stationName == null) stationName = WdmUtils.getLocationAttribute(wdmDll, wdmFileNumber, dataSetNumber);

            if (constituent != null && stationName != null) {
                String id = stationName + '.' + constituent;
                WdmTimeSeriesExchangeItem exchangeItem = new WdmTimeSeriesExchangeItem(id, role);
                exchangeItem.setDataSetNumber(dataSetNumber);
                exchangeItems.add(exchangeItem);
            } else {//if cannot get location and parameter from attribute.
                //ignore this dataSet (do nothing).
            }

            //get next existing dataSet and put its number in the variable dataSetNumber.
            dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, dataSetNumber + 1);
        }

        return exchangeItems;
    }

    /**
     * Creates an exchange item with the given role for each dataSet in the wdm file that is also in the given timeSeriesIdList.
     */
    public static List<WdmTimeSeriesExchangeItem> createExchangeItemsFromList(WdmDll wdmDll, int wdmFileNumber, String wdmFilePath,
            IPrevExchangeItem.Role role, String[] timeSeriesIdList) {
        if (timeSeriesIdList == null || timeSeriesIdList.length == 0) throw new IllegalArgumentException("timeSeriesIdList == null || timeSeriesIdList.length == 0");
        Set<String> timeSeriesIdSet = new HashSet<String>(Arrays.asList(timeSeriesIdList));

        List<WdmTimeSeriesExchangeItem> exchangeItems = new ArrayList<WdmTimeSeriesExchangeItem>();

        //get first existing dataSet and put its number in the variable dataSetNumber.
        //for dataSetNumber the valid range is 1 (inclusive) to 32000 (inclusive), so start with 1.
        int dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, 1);
        while (dataSetNumber != -1) {
            //get the first attributeValue.
            String constituent = WdmUtils.getConstituentAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            String stationName = WdmUtils.getStationNameAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            //if stationName is empty, use location.
            if (stationName == null) stationName = WdmUtils.getLocationAttribute(wdmDll, wdmFileNumber, dataSetNumber);

            if (constituent != null && stationName != null) {
                String id = stationName + '.' + constituent;
                if (timeSeriesIdSet.contains(id)) {
                    WdmTimeSeriesExchangeItem exchangeItem = new WdmTimeSeriesExchangeItem(id, role);
                    exchangeItem.setDataSetNumber(dataSetNumber);
                    exchangeItems.add(exchangeItem);
                    timeSeriesIdSet.remove(id);
                }
            } else {//if cannot get location and parameter from attribute.
                //ignore this dataSet (do nothing).
            }

            //get next existing dataSet and put its number in the variable dataSetNumber.
            dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, dataSetNumber + 1);
        }

        if (!timeSeriesIdSet.isEmpty()) {
            throw new RuntimeException(WdmUtils.class.getSimpleName() + ": Data sets with exchangeItem ids " + Arrays.asList(timeSeriesIdSet) + " not found in wdm file " + wdmFilePath);
        }

        return exchangeItems;
    }

    /**
     * Returns the dataSetNumber that corresponds to the time series for the given wdmTimeSeriesExchangeItem.
     */
    private static int getDataSetNumber(WdmDll wdmDll, int wdmFileNumber, String wdmFilePath, WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem) {
        int dataSetNumber = wdmTimeSeriesExchangeItem.getDataSetNumber();
        if (dataSetNumber != -1) return dataSetNumber;

        //if dataSetNumber not cached.
        dataSetNumber = searchDataSetNumber(wdmDll, wdmFileNumber, wdmTimeSeriesExchangeItem.getLocation(), wdmTimeSeriesExchangeItem.getQuantityId());
        if (dataSetNumber != -1) {
            wdmTimeSeriesExchangeItem.setDataSetNumber(dataSetNumber);
            return dataSetNumber;
        }

        //if dataSet not found.
        throw new RuntimeException("WdmUtils: Data set for location " + wdmTimeSeriesExchangeItem.getLocation()
                + " and quantity " + wdmTimeSeriesExchangeItem.getQuantityId()
                + " not found in wdm file " + wdmFilePath);
    }

    /**
     * Searches the wdm file with the given unit number for a dataSet that has data for the
     * given location (stationName) and parameter. If found, then returns the number of the found dataSet.
     * If no such dataSet found, then returns -1.
     *
     * @param wdmDll
     * @param wdmFileNumber watershed data management file unit number
     * @param location
     * @param parameter
     * @return found dataSet.
     */
    static int searchDataSetNumber(WdmDll wdmDll, int wdmFileNumber, String location, String parameter) {
        //get first existing dataSet and put its number in the variable dataSetNumber.
        //for dataSetNumber the valid range is 1 (inclusive) to 32000 (inclusive), so start with 1.
        int dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, 1);
        while (dataSetNumber != -1) {
            //get the first attributeValue.
            String currentConstituent = WdmUtils.getConstituentAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            String currentStationName = WdmUtils.getStationNameAttribute(wdmDll, wdmFileNumber, dataSetNumber);
            //if stationName is empty, use location.
            if (currentStationName == null) currentStationName = WdmUtils.getLocationAttribute(wdmDll, wdmFileNumber, dataSetNumber);

            if (currentConstituent != null && currentStationName != null) {
                if (location.equalsIgnoreCase(currentStationName) && parameter.equalsIgnoreCase(currentConstituent)) {
                    return dataSetNumber;
                }
            } else {//if cannot get location and parameter from attribute.
                //ignore this dataSet (do nothing).
            }

            //get next existing dataSet and put its number in the variable dataSetNumber.
            dataSetNumber = wdmDll.getNextDataSetNumber(wdmFileNumber, dataSetNumber + 1);
        }

        //if dataSet not found.
        return -1;
    }

    /**
     * Returns the trimmed constituent attribute for the given dataSet from the given wdmFile.
     * Returns null if attribute is empty.
     */
    public static String getConstituentAttribute(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber) {
        //info from attributes list for attribute IDCONS:
        //desc: Constituent id
        //type: CHARACTER
        //default: None
        //index: 289
        //length: 8
        int index = 289;
        int length = 8;
        String string = wdmDll.getAttributeValue(wdmFileNumber, dataSetNumber, index, length);

        if (string == null) return null;
        string = string.trim();
        if (string.isEmpty()) return null;
        return string;
    }

    /**
     * Returns the trimmed location attribute for the given dataSet from the given wdmFile.
     * Returns null if attribute is empty.
     */
    public static String getLocationAttribute(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber) {
        //info from attributes list for attribute IDLOCN:
        //desc: Location id
        //type: CHARACTER
        //default: None
        //index: 290
        //length: 8
        int index = 290;
        int length = 8;
        String string = wdmDll.getAttributeValue(wdmFileNumber, dataSetNumber, index, length);

        if (string == null) return null;
        string = string.trim();
        if (string.isEmpty()) return null;
        return string;
    }

    /**
     * Returns the trimmed station name attribute for the given dataSet from the given wdmFile.
     * Returns null if attribute is empty.
     */
    public static String getStationNameAttribute(WdmDll wdmDll, int wdmFileNumber, int dataSetNumber) {
        //info from attributes list for attribute STANAM:
        //desc: Station name (48 characters).
        //help: Short name or description of the data set.
        //type: CHARACTER
        //default: None
        //index: 45
        //length: 48
        int index = 45;
        int length = 48;
        String string = wdmDll.getAttributeValue(wdmFileNumber, dataSetNumber, index, length);

        if (string == null) return null;
        string = string.trim();
        if (string.isEmpty()) return null;
        return string;
    }

    private static double convertDateArrayToMjd(int[] date, TimeZone timeZone) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(timeZone);
        calendar.set(date[0], date[1] - 1, date[2], date[3], date[4], date[5]);
        calendar.set(Calendar.MILLISECOND, 0);

        return Time.milliesToMjd(calendar.getTimeInMillis());
    }

    private static int[] convertMjdToDateArray(double time, TimeZone timeZone) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(timeZone);
        calendar.setTimeInMillis(Time.mjdToMillies(time));

        int[] date = new int[6];
        date[0] = calendar.get(Calendar.YEAR);
        date[1] = calendar.get(Calendar.MONTH) + 1;
        date[2] = calendar.get(Calendar.DAY_OF_MONTH);
        date[3] = calendar.get(Calendar.HOUR_OF_DAY);
        date[4] = calendar.get(Calendar.MINUTE);
        date[5] = calendar.get(Calendar.SECOND);
        return date;
    }
}
