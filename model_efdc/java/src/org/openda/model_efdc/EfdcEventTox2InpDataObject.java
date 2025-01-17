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

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

/**
 * The EVENT_TOX2.INP control input file for the EFDC (Environmental Fluid Dynamics Code) model
 * is searched for keywords in the format $<KEY>$. All "$<KEY>$" strings found
 * in the EVENT_TOX2.INP template file will be replaced with the required values
 * using the exchangeItems of this EfdcEventTox2InpIoObject.
 *
 * The dateFormat for the start and end dates in the EVENT_TOX2.INP file is yyyy MM dd HH mm, e.g.:
 * "2009 01 01 09 00"
 * "2009 01 03 09 00"
 *
 * @author Arno Kockx
 */
public class EfdcEventTox2InpDataObject extends AbstractDataObject {

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

    private File eventTox2InpFile;
    private DateFormat dateFormat;
	private String startTimeID;
	private String endTimeID;

	/**
     * @param workingDir the working directory.
     * @param arguments the first argument should be the the name of the file containing the data for this IDataObject (relative to the working directory),
	 *                  the second argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  the third and fourth arguments should be the ids of the startTime and endTime exchangeItems respectively.
     */

    @Override
    public void initialize(File workingDir, String[] arguments) {
		//get timeZone.
		if (arguments == null || arguments.length < 2) {
			throw new IllegalArgumentException("No timeZone argument specified for " + this.getClass().getSimpleName()
				+ ". The second argument should be the timeZone that is used by the model"
				+ " (in hours with respect to GMT, between -12 and 12).");
		}

        this.eventTox2InpFile = new File(workingDir, arguments[0]);

        try {
            double timeZoneOffsetInHours = Double.parseDouble(arguments[1]);
            this.timeZone = TimeUtils.createTimeZoneFromDouble(timeZoneOffsetInHours);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse first argument '" + arguments[1]
                    + "' for " + this.getClass().getSimpleName()
                    + ". The first argument should be the timeZone that is used by the model"
                    + " (in hours with respect to GMT, between -12 and 12).", e);
        }

        //create exchange items.
        if (arguments.length < 4) {
            throw new IllegalArgumentException("No exchange item ids arguments specified for " + this.getClass().getSimpleName()
                    + ". The third and fourth arguments should be the ids of the startTime and endTime exchangeItems respectively.");
        }
		startTimeID = arguments[2];
		endTimeID = arguments[3];
		exchangeItems.put(startTimeID, new DoubleExchangeItem(startTimeID, 0));
		exchangeItems.put(endTimeID, new DoubleExchangeItem(endTimeID, 0));

        //The dateFormat for the start and end dates in the EVENT_TOX2.INP file is yyyy MM dd HH mm, e.g.:
        //"2009 01 01 09 00"
        //"2009 01 03 09 00"
        this.dateFormat = new SimpleDateFormat("yyyy MM dd HH mm");
        this.dateFormat.setTimeZone(this.timeZone);
    }

    @Override
    public void finish() {
        writeControlFile();
    }

    /**
     * The EVENT_TOX2.INP file is read and a search and replace is done for the $<KEY>$ keywords
     * that correspond to the ids of the exchangeItems.
     */
    public void writeControlFile() {
        if (exchangeItems.isEmpty()) {
            throw new IllegalStateException("EfdcEventTox2InpIoObject not initialized yet.");
        }
        if (!this.eventTox2InpFile.exists()) {
            throw new RuntimeException("EVENT_TOX2.INP file '" + this.eventTox2InpFile.getAbsolutePath()
                    + "' does not exist.");
        }
        Results.putMessage(this.getClass().getSimpleName() + ": replacing tags in file " + this.eventTox2InpFile.getAbsolutePath());

        //read file.
        List<String> content = AsciiFileUtils.readLines(this.eventTox2InpFile);

        //get start and stop times.
        double startTime = (Double) this.exchangeItems.get(startTimeID).getValues();
        double endTime = (Double) this.exchangeItems.get(endTimeID).getValues();

        //get keyword values.
        String startTimeString = getTimeString(startTime);
        String endTimeString = getTimeString(endTime);

        //replace keywords.
        final String startTimeTag = TAG + TSTART_TAG + TAG;
        final String endTimeTag = TAG + TSTOP_TAG + TAG;
        for (int n = 0; n < content.size(); n++) {
            String line = content.get(n);
            if (line.contains(startTimeTag)) {
                line = line.replace(startTimeTag, startTimeString);
                int lineNumber = n + 1;
                Results.putMessage(this.getClass().getSimpleName() + ": replaced '" + startTimeTag + "' with '" + startTimeString
                        + "' on line " + lineNumber + " in file " + this.eventTox2InpFile.getAbsolutePath());
            }
            if (line.contains(endTimeTag)) {
                line = line.replace(endTimeTag, endTimeString);
                int lineNumber = n + 1;
                Results.putMessage(this.getClass().getSimpleName() + ": replaced '" + endTimeTag + "' with '" + endTimeString
                        + "' on line " + lineNumber + " in file " + this.eventTox2InpFile.getAbsolutePath());
            }
            content.set(n, line);
        }

        //write file.
        AsciiFileUtils.writeLines(this.eventTox2InpFile, content);
    }

    /**
     * The dateFormat for the start and end dates in the EVENT_TOX2.INP file is yyyy MM dd HH mm, e.g.:
     * "2009 01 01 09 00"
     * "2009 01 03 09 00"
     *
     * @param timeDouble
     * @return String startTime formatted for uci file.
     */
    private String getTimeString(double timeDouble) {
        long time = Time.mjdToMillies(timeDouble);
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(this.timeZone);
        calendar.setTimeInMillis(time);
        return this.dateFormat.format(calendar.getTime());
    }
}
