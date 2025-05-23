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

package org.openda.model_efdc;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.model_efdc.timeseriesformat.*;
import org.openda.utils.Results;
import org.openda.utils.Time;

import java.io.File;
import java.util.*;

/**
 * IoObject for a .INP (EFDC input) file that contains one or more time series.
 * This can be used for the files QSER.INP, TSER.INP, PSER.INP, ASER.INP
 * and CWQSR**.INP that are input for the EFDC (Environmental Fluid Dynamics Code) model.
 *
 * @author Arno Kockx
 */
public class EfdcTimeSeriesDataObject extends AbstractDataObject {

    /**
     * The times in the .INP input files are in this time unit.
     * The EFDC model uses this timeUnit to convert the times to seconds.
     * This timeUnit can be chosen here, as long as it is
     * applied consistently.
     *
     * Default is day.
     */
    public static final double SECONDS_PER_TIME_UNIT = 24*60*60;

    /**
     * The timeZone that is used by the model.
     * This is required to convert the times of the data values
     * to/from the timeZone that is used by the model.
     * Default is GMT.
     */
    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    private File timeSeriesFile;
    private EfdcTimeSeriesFormatter timeSeriesFormatter;
	private String startTimeID;

	/**
     * @param workingDir the working directory.
     * @param arguments the first argument should be the name of the file containing the data for this IoObject (relative to the working directory),
	 *                  the second argument should be the type of file, e.g. QSER, TSER or ASER,
     *                  the third argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  the fourth argument should be the id of the startTime exchangeItem,
     *                  the fifth and further arguments should be the ids of the time series
     *                  for which exchange items should be made.
     */

    @Override
    public void initialize(File workingDir, String[] arguments) {
        this.timeSeriesFile = new File(workingDir, arguments[0]);

        //create time series formatter.
        if (arguments.length < 2) {
            throw new IllegalArgumentException("No arguments specified for " + this.getClass().getSimpleName()
                    + ". The second argument should be the type of file, e.g. QSER, TSER or ASER.");
        }
        String fileType = arguments[1];
        createTimeSeriesFormatter(fileType);

        //get timeZone.
        if (arguments.length < 3) {
            throw new IllegalArgumentException("No timeZone argument specified for " + this.getClass().getSimpleName()
                    + ". The third argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).");
        }
        try {
            double timeZoneOffsetInHours = Double.parseDouble(arguments[2]);
            this.timeZone = TimeUtils.createTimeZoneFromDouble(timeZoneOffsetInHours);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse third argument '" + arguments[2]
                    + "' for " + this.getClass().getSimpleName()
                    + ". The third argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).", e);
        }

        //create start time exchange item.
        if (arguments.length < 4) {
            throw new IllegalArgumentException("No exchange item ids arguments specified for " + this.getClass().getSimpleName()
                    + ". The fourth argument should be the id of the startTime exchangeItem.");
        }
		startTimeID = arguments[3];
        exchangeItems.put(startTimeID,  new DoubleExchangeItem(startTimeID, 0));

        //create exchange items.
        if (arguments.length < 5) {
            throw new IllegalArgumentException("No time series ids arguments specified for " + this.getClass().getSimpleName()
                    + ". The fifth and further arguments should be the ids of time series.");
        }
        String[] timeSeriesIdList = Arrays.copyOfRange(arguments, 4, arguments.length);
        createTimeSeriesExchangeItems(timeSeriesIdList);

        //read file.
        if (!this.timeSeriesFile.exists()) {
            throw new RuntimeException("Input file '" + this.timeSeriesFile.getAbsolutePath() + "' does not exist.");
        }
        Results.putMessage(this.getClass().getSimpleName() + ": reading header lines in existing input file "
                + this.timeSeriesFile.getAbsolutePath());
        this.timeSeriesFormatter.readFile(this.timeSeriesFile);
    }

    private void createTimeSeriesFormatter(String fileType) {

        if (EfdcTimeSeriesFormatter.ASER_FILE_TYPE.equalsIgnoreCase(fileType)) {
            this.timeSeriesFormatter = new EfdcAserTimeSeriesFormatter(fileType);

        } else if (EfdcTimeSeriesFormatter.PSER_FILE_TYPE.equalsIgnoreCase(fileType)) {
            this.timeSeriesFormatter = new EfdcPserTimeSeriesFormatter(fileType);

        } else if (EfdcTimeSeriesFormatter.QSER_FILE_TYPE.equalsIgnoreCase(fileType)) {
            this.timeSeriesFormatter = new EfdcQserTimeSeriesFormatter(fileType);

        } else if (EfdcTimeSeriesFormatter.TSER_FILE_TYPE.equalsIgnoreCase(fileType)) {
            this.timeSeriesFormatter = new EfdcTserTimeSeriesFormatter(fileType);

        } else if (fileType.startsWith(EfdcTimeSeriesFormatter.CWQSR_FILE_TYPE)) {
            this.timeSeriesFormatter = new EfdcCwqsrTimeSeriesFormatter(fileType);

        } else {
            throw new IllegalArgumentException("Unknown file type '" + fileType
                    + "' specified for " + this.getClass().getSimpleName());
        }
    }

    private void createTimeSeriesExchangeItems(String[] timeSeriesIdList) {
        //for all identifiers in the idList create a TimeSeriesExchangeItem.
        List<String> ids = new ArrayList<>();
        for (String timeSeriesId : timeSeriesIdList) {
            //create timeSeriesExchangeItem.
            EfdcTimeSeriesExchangeItem timeSeriesExchangeItem = new EfdcTimeSeriesExchangeItem();
            String location = BBUtils.getLocationFromId(timeSeriesId);
            String parameter = BBUtils.getParameterFromId(timeSeriesId);
            timeSeriesExchangeItem.setLocation(location);
            timeSeriesExchangeItem.setQuantity(parameter);

            //check on duplicate ids (not locationIds).
            String id = timeSeriesExchangeItem.getId();
            if (ids.contains(id)) {
                throw new IllegalArgumentException("Duplicate time series with id '" + id
                        + "' found for time series file '" + this.timeSeriesFile.getAbsolutePath() + "'.");
            }

            //add timeSeriesExchangeItem.
            exchangeItems.put(timeSeriesExchangeItem.getId(), timeSeriesExchangeItem);
            ids.add(id);
        }

        if (exchangeItems.isEmpty()) {
            throw new IllegalArgumentException("No time series found for time series file '"
                    + this.timeSeriesFile.getAbsolutePath() + "'.");
        }
    }

    @Override
    public void finish() {
        if (exchangeItems.isEmpty()) {
            throw new IllegalStateException("EfdcTimeSeriesIoObject not initialized yet.");
        }

        //get startTime.
        double startTime = (Double) exchangeItems.get(startTimeID).getValues();

        //determine referenceTime.
        long referenceTime = EfdcUtils.getReferenceTime(Time.mjdToMillies(startTime), this.timeZone);

        //write data to file for each time series exchange item.
        this.timeSeriesFormatter.setReferenceTime(referenceTime);
        this.timeSeriesFormatter.setTimeUnit(SECONDS_PER_TIME_UNIT);
		Map<String, IExchangeItem> exchangeItemsWithoutStartTime = new LinkedHashMap<>(this.exchangeItems);
		exchangeItemsWithoutStartTime.remove(startTimeID);
		this.timeSeriesFormatter.writeTimeSeriesToFile(this.timeSeriesFile, exchangeItemsWithoutStartTime);
    }
}
