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

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

/**
 * The UCI (User Control Input) file for the HSPF (Hydrological Simulation Program - FORTRAN) model
 * is searched for keywords in the format $<KEY>$. All "$<KEY>$" strings found
 * in the UCI template file will be replaced with the values found by the
 * exchangeItems of this UciIoObject.
 *
 * The dateFormat for dates in the UCI file is yyyy/MM/dd HH:mm, e.g.:
 * "  START       2004/01/01 00:00  END    2004/01/10 00:00"
 *
 * The HSPF model can be installed as part of the BASINS package,
 * which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class UciIoObject implements IoObjectInterface {

    private static final String TAG = "$";
    private static final String TSTART_TAG = "TSTART";
    private static final String TSTOP_TAG = "TSTOP";

    /**
     * The timeZone that is used by the model.
     * This is required to convert the times of the data values
     * to/from the timeZone that is used by the model.
     * Default is GMT.
     */
    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    private IPrevExchangeItem startTimeExchangeItem = null;
    private IPrevExchangeItem endTimeExchangeItem = null;
    private File uciFile;
    private DateFormat dateFormat;
    /**
     * This is the startTime of the runPeriod for the HSPF model in hours relative to the startTime of the OpenDA runPeriod.
     * This can be used to extend the runPeriod for the HSPF model (the runPeriod as it will be put in the uci file)
     * with respect to the OpenDA runPeriod. This is useful when there is no restart file (warm state) available for the forecast.
     * In other words the extended period will allow the model to get to a warm state before the forecast starts.
     *
     * This is 0 by default.
     */
    private double startTimeExtension = 0;

    /**
     * @param workingDir the working directory.
     * @param fileName the name of the file containing the data for this IoObject (relative to the working directory).
     * @param arguments the first argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  the second and third arguments should be the ids of the startTime and endTime exchangeItems respectively,
     *                  the (optional) fourth argument should be the startTimeExtension in hours relative to the startTime of the OpenDA runPeriod.
     */
    
    public void initialize(File workingDir, String fileName, String[] arguments) {
        this.uciFile = new File(workingDir, fileName);

        //get timeZone.
        if (arguments == null || arguments.length < 1) {
            throw new IllegalArgumentException("No timeZone argument specified for " + this.getClass().getSimpleName()
                    + ". The first argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).");
        }
        try {
            double timeZoneOffsetInHours = Double.parseDouble(arguments[0]);
            this.timeZone = TimeUtils.createTimeZoneFromDouble(timeZoneOffsetInHours);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse first argument '" + arguments[0]
                    + "' for " + this.getClass().getSimpleName()
                    + ". The first argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).", e);
        }

        //create exchange items.
        if (arguments.length < 3) {
            throw new IllegalArgumentException("No exchange item ids arguments specified for " + this.getClass().getSimpleName()
                    + ". The second and third arguments should be the ids of the startTime and endTime exchangeItems respectively.");
        }
        this.startTimeExchangeItem = new DoubleExchangeItem(arguments[1], 0);
        this.endTimeExchangeItem = new DoubleExchangeItem(arguments[2], 0);

        //get optional startTimeExtension.
        if (arguments.length > 3) {
            try {
                this.startTimeExtension = Double.parseDouble(arguments[3]);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Cannot parse fourth argument '" + arguments[3]
                        + "' for " + this.getClass().getSimpleName()
                        + ". The (optional) fourth argument should be the startTimeExtension"
                        + " in hours relative to the startTime of the OpenDA runPeriod.", e);
            }
        }

        //The dateFormat for dates in the UCI file is yyyy/MM/dd HH:mm, e.g.:
        //"  START       2004/01/01 00:00  END    2004/01/10 00:00"
        this.dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm");
        this.dateFormat.setTimeZone(this.timeZone);
    }

    
    public IPrevExchangeItem[] getExchangeItems() {
        return new IPrevExchangeItem[]{this.startTimeExchangeItem, this.endTimeExchangeItem};
    }

    
    public void finish() {
        writeDefinitionFile();
    }

    /**
     * The uciFile is read and a search and replace is done for the $<KEY>$ keywords.
     */
    private void writeDefinitionFile() {
        if (this.startTimeExchangeItem == null || this.endTimeExchangeItem == null) {
            throw new IllegalStateException("UciIoObject not initialized yet.");
        }
        if (!this.uciFile.exists()) {
            throw new RuntimeException("UciIoObject: Uci file '" + this.uciFile + "' does not exist.");
        }
        Results.putMessage(this.getClass().getSimpleName() + ": replacing tags in file " + this.uciFile.getAbsolutePath());

        //read file.
        List<String> content = AsciiFileUtils.readLines(uciFile);

        //get start and stop times.
        double startTime = (Double) this.startTimeExchangeItem.getValues();
        double stopTime = (Double) this.endTimeExchangeItem.getValues();
        String startTimeString = UciUtils.getStartTimeString(startTime, this.dateFormat, this.timeZone, this.startTimeExtension);
        String stopTimeString = UciUtils.getEndTimeString(stopTime, this.dateFormat, this.timeZone);

        //replace key words for start and end time.
        final String startTimeTag = TAG + TSTART_TAG + TAG;
        final String stopTimeTag = TAG + TSTOP_TAG + TAG;
        for (int n = 0; n < content.size(); n++) {
            String line = content.get(n);
            if (line.contains(startTimeTag)) {
                line = line.replace(startTimeTag, startTimeString);
                int lineNumber = n + 1;
                Results.putMessage(this.getClass().getSimpleName() + ": replaced '" + startTimeTag + "' with '"
                        + startTimeString + "' on line " + lineNumber + " in file " + this.uciFile.getAbsolutePath());
            }
            if (line.contains(stopTimeTag)) {
                line = line.replace(stopTimeTag, stopTimeString);
                int lineNumber = n + 1;
                Results.putMessage(this.getClass().getSimpleName() + ": replaced '" + stopTimeTag + "' with '"
                        + stopTimeString + "' on line " + lineNumber + " in file " + this.uciFile.getAbsolutePath());
            }
            content.set(n, line);
        }

        //write file.
        AsciiFileUtils.writeLines(this.uciFile, content);
    }
}
