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

import java.io.File;
import java.util.ArrayList;
import java.util.Locale;
import java.util.TimeZone;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * This IoObject changes the relative startTime in the RESTART.INP file,
 * which is used for the EFDC (Environmental Fluid Dynamics Code) model.
 *
 * @author Arno Kockx
 */
public class EfdcRestartFileIoObject implements IoObjectInterface {

    /**
     * The timeZone that is used by the model.
     * This is required to convert the times of the data values
     * to/from the timeZone that is used by the model.
     * Default is GMT.
     */
    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    private IPrevExchangeItem startTimeExchangeItem = null;
    private File efdcRestartFile;

    /**
     * @param workingDir the working directory.
     * @param fileName the name of the restart file for this IoObject (relative to the working directory).
     * @param arguments the first argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  the second argument should be the id of the startTime exchangeItem.
     */
    @Override
    public void initialize(File workingDir, String fileName, String[] arguments) {
        this.efdcRestartFile = new File(workingDir, fileName);

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
        if (arguments.length < 2) {
            throw new IllegalArgumentException("No exchange item id argument specified for " + this.getClass().getSimpleName()
                    + ". The second argument should be the id of the startTime exchangeItem.");
        }
        this.startTimeExchangeItem = new DoubleExchangeItem(arguments[1], 0);
    }

    @Override
    public IPrevExchangeItem[] getExchangeItems() {
        return new IPrevExchangeItem[]{this.startTimeExchangeItem};
    }

    @Override
    public void finish() {
        changeRelativeStartTimeInRestartFile();
    }

    /**
     * This method changes the relative startTime in the RESTART.INP file.
     */
    private void changeRelativeStartTimeInRestartFile() {
        if (this.startTimeExchangeItem == null) {
            throw new IllegalStateException("EfdcRestartFileIoObject not initialized yet.");
        }
        if (!this.efdcRestartFile.exists()) {
            throw new RuntimeException("RESTART.INP file '" + this.efdcRestartFile.getAbsolutePath()
                    + "' does not exist.");
        }

        //get start time.
        double startTimeDouble = (Double) this.startTimeExchangeItem.getValues();
        //get relative start time.
        double relativeStartTime = getRelativeStartTime(startTimeDouble,
                EfdcTimeSeriesIoObject.SECONDS_PER_TIME_UNIT, this.timeZone);

        //read file.
        ArrayList<String> content = EfdcUtils.readFile(this.efdcRestartFile);
        if (content == null || content.size() < 1) {
            throw new RuntimeException("Invalid RESTART.INP file. File is empty.");
        }

        //replace start time.
        String firstLine = content.get(0);
        int indexOfWhiteSpaceAfterFirstNumber = getIndexOfWhiteSpaceAfterFirstNumber(firstLine);
        if (indexOfWhiteSpaceAfterFirstNumber == -1) {
            throw new RuntimeException("Invalid RESTART.INP file. First line expected to contain at least two columns.");
        }
        String firstHalfOfFirstLine = firstLine.substring(0, indexOfWhiteSpaceAfterFirstNumber);
        //format relativeStartTime.
        String secondHalfOfFirstLine = String.format(Locale.UK, "%#16.4f", relativeStartTime);
        String newFirstLine = firstHalfOfFirstLine + secondHalfOfFirstLine;
        content.set(0, newFirstLine);
        Results.putMessage(this.getClass().getSimpleName() + ": replaced first line '" + firstLine + "' with '"
                + newFirstLine + "' in file " + this.efdcRestartFile.getAbsolutePath());

        //write file.
        EfdcUtils.writeFile(this.efdcRestartFile, content);
    }

    private int getIndexOfWhiteSpaceAfterFirstNumber(String line) {
        boolean nonWhiteSpaceCharacterFound = false;
        for (int index = 0; index < line.length(); index++) {
            String character = new String(new char[]{line.charAt(index)});

            if (!nonWhiteSpaceCharacterFound) {
                if (character.matches("\\S")) {
                    nonWhiteSpaceCharacterFound = true;
                }

            } else {//if nonWhiteSpaceCharacterFound is true.
                if (character.matches("\\s")) {
                    return index;
                }
            }
        }

        //if not found.
        return -1;
    }

    private double getRelativeStartTime(double startTimeDouble, double secondsPerTimeUnit, TimeZone timeZone) {
        long startTime = Time.mjdToMillies(startTimeDouble);
        long referenceTime = EfdcUtils.getReferenceTime(startTime, timeZone);
        return EfdcUtils.getRelativeTimeInTimeUnits(startTime, referenceTime, secondsPerTimeUnit);
    }
}
