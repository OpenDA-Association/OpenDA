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

package org.openda.model_hspf;

import java.io.File;
import java.text.ParseException;
import java.util.*;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

/**
 * IoObject for one WDM (Watershed Data Management) file.
 *
 * See http://water.usgs.gov/cgi-bin/man_wrdapp?wdm(1) :
 * A WDM file is a binary, direct-access file used to store
 * hydrologic, hydraulic, meteorologic, water-quality, and
 * physiographic data.  The WDM file is organized into data
 * sets (DSN = Data Set Number).  Each data set contains a specific type of data, such
 * as streamflow at a specific site or air temperature at a
 * weather station.  Each data set contains attributes that
 * describe the data, such as station identification number,
 * time step of data, latitude, and longitude.  A WDM file may
 * contain a single data set or as many as 200,000 data sets.
 * A data set may be described by a few attributes or by
 * hundreds of attributes.  Data can be added, deleted, and
 * modified without restructuring the data in the file.  Space
 * from deleted data sets is reused.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * The HSPF model can be installed as part of the BASINS package,
 * which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
@Deprecated
//use WdmTimeSeriesDataObject instead.
public class WdmTimeSeriesIoObject implements IoObjectInterface {

    /**
     * The timeZone that is used by the model.
     * This is required to convert the times of the data values
     * to/from the timeZone that is used by the model.
     * Default is GMT.
     */
    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    /**
     * Absolute path name of the file containing the data for this IoObject.
     */
    private String wdmTimeSeriesFilePath;
    private int wdmTimeSeriesFileNumber;
    /**
     * Absolute path name of the wdm message file.
     * The wdm message file is required for the fortran wdm library
     * methods to work properly.
     */
    private String wdmMessageFilePath;

    private WdmDll wdmDll;
    private Role role;
    private IPrevExchangeItem startTimeExchangeItem = null;
    private IPrevExchangeItem endTimeExchangeItem = null;
    private double startTimeDouble = Double.NaN;
    private double endTimeDouble = Double.NaN;
    private List<WdmTimeSeriesExchangeItem> wdmTimeSeriesExchangeItems = new ArrayList<WdmTimeSeriesExchangeItem>();

    /**
     * @param workingDir the working directory.
     * @param fileName the pathname of the file containing the data for this IoObject (relative to the working directory).
     * @param arguments the first argument should be the pathname of the wdm.dll file (relative to the working directory),
     *                  the second argument should be the pathname of the message file (relative to working directory),
     *                  the third argument should be the role of this IoObject. Role can be 'input' or 'output',
     *                  the fourth argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  for role INPUT the fifth and sixth arguments should be the ids of the startTime and endTime exchangeItems respectively,
     *                  for role OUTPUT the fifth and sixth arguments should be respectively the startTime and endTime of the model run,
     *                  the (optional) seventh and further arguments should be the location and parameter ids of the time series for which exchange items should be made,
     *                  if no seventh and further arguments present then exchange items will be created for all time series in the file.
     */
    public void initialize(File workingDir, String fileName, String[] arguments) {
        //initialize wdmTimeSeriesFilePath.
        File wdmTimeSeriesFile = new File(workingDir, fileName);
        if (!wdmTimeSeriesFile.exists()) {
            throw new IllegalArgumentException(getClass().getSimpleName() + ": Time series file '" + wdmTimeSeriesFile.getAbsolutePath() + "' does not exist.");
        }
        this.wdmTimeSeriesFilePath = wdmTimeSeriesFile.getAbsolutePath();
        //create a unique fortran file unit number to use for the wdmTimeSeriesFile.
        this.wdmTimeSeriesFileNumber = WdmUtils.generateUniqueFortranFileUnitNumber();

        //initialize wdmDll.
        if (arguments == null || arguments.length < 1) {
            throw new IllegalArgumentException(getClass().getSimpleName() + ": No arguments specified. The first argument should be the path of the wdm.dll file (relative to working directory).");
        }
        this.wdmDll = WdmUtils.initializeWdmDll(workingDir, arguments[0]);

        //initialize wdmMessageFilePath.
        if (arguments.length < 2) {
            throw new IllegalArgumentException(getClass().getSimpleName() + ": No arguments specified. The second argument should be the path of the message file (relative to working directory).");
        }
        this.wdmMessageFilePath = WdmUtils.initializeWdmMessageFilePath(workingDir, arguments[1]);

        //initialize role.
        if (arguments.length < 3) {
            throw new IllegalArgumentException(getClass().getSimpleName() + ": No role argument specified. The third argument should be the role of this IoObject. Role can be 'input' or 'output'.");
        }
        this.role = WdmUtils.initializeRole(arguments[2]);

        //get timeZone.
        if (arguments.length < 4) {
            throw new IllegalArgumentException("No timeZone argument specified for " + getClass().getSimpleName()
                    + ". The fourth argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12).");
        }
        try {
            double timeZoneOffsetInHours = Double.parseDouble(arguments[3]);
            this.timeZone = TimeUtils.createTimeZoneFromDouble(timeZoneOffsetInHours);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse fourth argument '" + arguments[3] + "' for " + getClass().getSimpleName()
                    + ". The fourth argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12).", e);
        }

        //this object can be used for either input or output. In both cases the run startTime and endTime
        //are needed. However the timeInfoExchangeItems are only set for input ioObjects (see BBModelInstance.compute)
        //and the aliases are only available for output ioObjects (because the aliases are set after the input ioObjects
        //have already been initialized). Therefore depending on the role use different arguments to get the
        //startTime and endTime here.
        //Note: the timeInfoExchangeItems are also set after the input ioObjects have already been initialized,
        //but the timeInfoExchangeItems need only be created during ioObject initialization and can then be
        //used later, after they have been set.
        if (this.role == IPrevExchangeItem.Role.Input) {
            //create exchange items.
            if (arguments.length < 6) {
                throw new IllegalArgumentException("No start/endTime exchange item ids arguments specified for " + getClass().getSimpleName()
                        + ". For role INPUT the fifth and sixth arguments should be the ids of the startTime and endTime exchangeItems respectively.");
            }
            //get start and end time.
            this.startTimeExchangeItem = new DoubleExchangeItem(arguments[4], 0);
            this.endTimeExchangeItem = new DoubleExchangeItem(arguments[5], 0);

        } else {//if this.role == IPrevExchangeItem.Role.Output.
            if (arguments.length < 6) {
                throw new IllegalArgumentException("No start/endTime arguments specified for " + getClass().getSimpleName()
                        + ". For role OUTPUT the fifth and sixth arguments should be respectively the startTime and endTime of the model run.");
            }
            //get start time.
            try {
                this.startTimeDouble = TimeUtils.date2Mjd(arguments[4]);
            } catch (ParseException e) {
                throw new IllegalArgumentException("Invalid startTime argument specified for " + getClass().getSimpleName()
                        + ". Cannot parse fifth argument '" + arguments[4] + "'. For role OUTPUT the fifth and sixth arguments should be respectively the startTime and endTime of the model run.", e);
            }
            //get end time.
            try {
                this.endTimeDouble = TimeUtils.date2Mjd(arguments[5]);
            } catch (ParseException e) {
                throw new IllegalArgumentException("Invalid endTime argument specified for " + getClass().getSimpleName()
                        + ". Cannot parse sixth argument '" + arguments[5] + "'. For role OUTPUT the fifth and sixth arguments should be respectively the startTime and endTime of the model run.", e);
            }
        }

        //initialize wdmTimeSeriesExchangeItems.
        if (arguments.length < 7) {
            Results.putMessage(getClass().getSimpleName() + ": No time series ids arguments specified. Exchange items will be created for all time series in the file.");
            createWdmTimeSeriesExchangeItemsFromFile();
        } else {
            //create exchange items for specified time series only.
            Results.putMessage(getClass().getSimpleName() + ": Time series ids arguments specified. Exchange items will be created for specified time series ids only.");
            String[] timeSeriesIdList = Arrays.copyOfRange(arguments, 6, arguments.length);
            createWdmTimeSeriesExchangeItemsFromList(timeSeriesIdList);
        }

        //read all values for all exchangeItems in one go, so that not needed to open/close same file for each read separately.
        readValuesAndTimesFromFile();
    }

    private void createWdmTimeSeriesExchangeItemsFromList(String[] timeSeriesIdList) {
        //reset this.timeSeriesExchangeItems list.
        this.wdmTimeSeriesExchangeItems.clear();

        //create exchange items for all time series ids in the list.
        WdmUtils.openWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, this.wdmMessageFilePath);
        this.wdmTimeSeriesExchangeItems.addAll(WdmUtils.createExchangeItemsFromList(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, this.role, timeSeriesIdList));
        WdmUtils.closeWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber);

        if (this.wdmTimeSeriesExchangeItems.isEmpty()) throw new IllegalStateException(this.getClass().getSimpleName() + ": this.wdmTimeSeriesExchangeItems.isEmpty()");
    }

    private void createWdmTimeSeriesExchangeItemsFromFile() {
        //reset this.timeSeriesExchangeItems list.
        this.wdmTimeSeriesExchangeItems.clear();

        //create exchange items for all time series in the file.
        WdmUtils.openWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, this.wdmMessageFilePath);
        this.wdmTimeSeriesExchangeItems.addAll(WdmUtils.createExchangeItemsFromFile(this.wdmDll, this.wdmTimeSeriesFileNumber, this.role));
        WdmUtils.closeWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber);

        if (this.wdmTimeSeriesExchangeItems.isEmpty()) throw new IllegalArgumentException(this.getClass().getSimpleName() + ": No time series found in time series file '" + this.wdmTimeSeriesFilePath + "'.");
    }

    public IPrevExchangeItem[] getExchangeItems() {
        //return all available exchange items.
        List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>(this.wdmTimeSeriesExchangeItems);
        if (this.startTimeExchangeItem != null && this.endTimeExchangeItem != null) {
            exchangeItems.add(this.startTimeExchangeItem);
            exchangeItems.add(this.endTimeExchangeItem);
        }
        return exchangeItems.toArray(new IPrevExchangeItem[exchangeItems.size()]);
    }

    /**
     * If this IoObject has role Output (i.e. output from the model), then this method
     * reads the data from the wdm file and stores it in the wdmTimeSeriesExchangeItems
     * in this IoObject.
     *
     * Updates the in-memory stored values and times by reading from the wdm file.
     */
    private void readValuesAndTimesFromFile() {
        if (this.role == IPrevExchangeItem.Role.Input) {
            return;
        }

        if (Double.isNaN(this.startTimeDouble) || Double.isNaN(this.endTimeDouble)) {
            throw new IllegalStateException(getClass().getSimpleName() + " not initialized yet.");
        }

        Results.putMessage(getClass().getSimpleName() + ": reading " + this.wdmTimeSeriesExchangeItems.size()
                + " output time series from file " + this.wdmTimeSeriesFilePath  + " with fortran unit number " + this.wdmTimeSeriesFileNumber + ".");

        //open wdm file.
        WdmUtils.openWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, this.wdmMessageFilePath);

        for (WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem : this.wdmTimeSeriesExchangeItems) {
            //read data from file and set in wdmTimeSeriesExchangeItem.
            WdmUtils.readTimesAndValues(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, wdmTimeSeriesExchangeItem, this.startTimeDouble, this.endTimeDouble, this.timeZone);
        }

        //close wdm file.
        WdmUtils.closeWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber);
    }

    /**
     * If this IoObject has role Input (i.e. input for the model), then this method writes the data
     * from all wdmTimeSeriesExchangeItems in this IoObject to the wdm file
     * so that it can be used as input by the model.
     */
    public void finish() {
        if (this.role == IPrevExchangeItem.Role.Output) {
            return;
        }

        if (this.startTimeExchangeItem == null || this.endTimeExchangeItem == null) {
            throw new IllegalStateException(getClass().getSimpleName() + " not initialized yet.");
        }

        Results.putMessage(getClass().getSimpleName() + ": writing " + this.wdmTimeSeriesExchangeItems.size()
                + " input time series to file " + this.wdmTimeSeriesFilePath  + " with fortran unit number " + this.wdmTimeSeriesFileNumber + ".");

        //get start and end times.
        double startTime = (Double) this.startTimeExchangeItem.getValues();
        double endTime = (Double) this.endTimeExchangeItem.getValues();

        //open wdm file.
        WdmUtils.openWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, this.wdmMessageFilePath);

        for (WdmTimeSeriesExchangeItem wdmTimeSeriesExchangeItem : this.wdmTimeSeriesExchangeItems) {
            //write data from wdmTimeSeriesExchangeItem to file.
            WdmUtils.writeTimesAndValues(this.wdmDll, this.wdmTimeSeriesFileNumber, this.wdmTimeSeriesFilePath, wdmTimeSeriesExchangeItem, startTime, endTime, this.timeZone);
        }

        //close wdm file.
        WdmUtils.closeWdmFile(this.wdmDll, this.wdmTimeSeriesFileNumber);
    }
}
