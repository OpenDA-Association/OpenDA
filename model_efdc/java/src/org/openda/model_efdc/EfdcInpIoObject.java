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
import java.util.Iterator;
import java.util.TimeZone;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * The EFDC.INP control input file for the EFDC (Environmental Fluid Dynamics Code) model
 * is searched for keywords in the format $<KEY>$. All "$<KEY>$" strings found
 * in the EFDC.INP template file will be replaced with the required values
 * using the exchangeItems of this EfdcInpIoObject.
 *
 * @author Arno Kockx
 */
public class EfdcInpIoObject implements IoObjectInterface {

    private static final String TAG = "$";
    private static final String RELATIVE_TSTART_TAG = "RELATIVE_TSTART";
    private static final String N_REF_PERIODS_TAG = "N_REF_PERIODS";

    /**
     * The timeZone that is used by the model.
     * This is required to convert the times of the data values
     * to/from the timeZone that is used by the model.
     * Default is GMT.
     */
    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    private IPrevExchangeItem startTimeExchangeItem = null;
    private IPrevExchangeItem endTimeExchangeItem = null;
    private File efdcInpFile;

    /**
     * @param workingDir the working directory.
     * @param fileName the name of the file containing the data for this IoObject (relative to the working directory).
     * @param arguments the first argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
     *                  the second and third arguments should be the ids of the startTime and endTime exchangeItems respectively.
     */
    @Override
    public void initialize(File workingDir, String fileName, String[] arguments) {
        this.efdcInpFile = new File(workingDir, fileName);

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
    }

    @Override
    public IPrevExchangeItem[] getExchangeItems() {
        return new IPrevExchangeItem[]{this.startTimeExchangeItem, this.endTimeExchangeItem};
    }

    @Override
    public void finish() {
        writeControlFile();
    }

    /**
     * The EFDC.INP file is read and a search and replace is done for the $<KEY>$ keywords
     * that correspond to the ids of the exchangeItems.
     */
    private void writeControlFile() {
        if (this.startTimeExchangeItem == null || this.endTimeExchangeItem == null) {
            throw new IllegalStateException("EfdcInpIoObject not initialized yet.");
        }
        if (!this.efdcInpFile.exists()) {
            throw new RuntimeException("EFDC.INP file '" + this.efdcInpFile.getAbsolutePath()
                    + "' does not exist.");
        }
        Results.putMessage(this.getClass().getSimpleName() + ": replacing tags in file " + this.efdcInpFile.getAbsolutePath());

        //get start and stop times.
        double startTimeDouble = (Double) this.startTimeExchangeItem.getValues();
        double endTimeDouble = (Double) this.endTimeExchangeItem.getValues();

        //read file.
        ArrayList<String> content = EfdcUtils.readFile(this.efdcInpFile);
        double secondsPerTimeUnit = getSecondsPerTimeUnit(content);
        double secondsPerReferencePeriod = getSecondsPerReferencePeriod(content);

        //get keyword values.
        String relativeStartTimeString = getRelativeStartTimeString(startTimeDouble, secondsPerTimeUnit, this.timeZone);
        String numberOfReferencePeriodsString = String.valueOf(getNumberOfReferencePeriods(startTimeDouble,
                endTimeDouble, secondsPerReferencePeriod));

        //replace keywords.
        final String relativeStartTimeTag = TAG + RELATIVE_TSTART_TAG + TAG;
        final String numberOfReferencePeriodsTag = TAG + N_REF_PERIODS_TAG + TAG;
        for (int n = 0; n < content.size(); n++) {
            String line = content.get(n);
            if (line.contains(relativeStartTimeTag)) {
                line = line.replace(relativeStartTimeTag, relativeStartTimeString);
                int lineNumber = n + 1;
                Results.putMessage(this.getClass().getSimpleName() + ": replaced '" + relativeStartTimeTag + "' with '"
                        + relativeStartTimeString + "' on line " + lineNumber + " in file " + this.efdcInpFile.getAbsolutePath());
            }
            if (line.contains(numberOfReferencePeriodsTag)) {
                line = line.replace(numberOfReferencePeriodsTag, numberOfReferencePeriodsString);
                int lineNumber = n + 1;
                Results.putMessage(this.getClass().getSimpleName() + ": replaced '" + numberOfReferencePeriodsTag + "' with '"
                        + numberOfReferencePeriodsString + "' on line " + lineNumber + " in file " + this.efdcInpFile.getAbsolutePath());
            }
            content.set(n, line);
        }

        //write file.
        EfdcUtils.writeFile(this.efdcInpFile, content);
    }

    /**
     * Returns secondsPerTimeUnit, which is TCON in card image 8 in EFDC.INP file:
     *
     * -------------------------------------------------------------------------------
     * C8 TIME-RELATED REAL PARAMETERS
     * *
     * *  TCON:     CONVERSION MULTIPLIER TO CHANGE TBEGIN TO SECONDS
     * *  TBEGIN:   TIME ORIGIN OF RUN
     * *  TREF:     REFERENCE TIME PERIOD IN sec (i.e. 44714.16S OR 86400S)
     * *  CORIOLIS: CONSTANT CORIOLIS PARAMETER IN 1/sec =2*7.29E-5*SIN(LAT)
     * *  ISCORV:   1 TO READ VARIABLE CORIOLIS COEFFICIENT FROM LXLY.INP FILE
     * *  ISCCA:    WRITE DIAGNOSTICS FOR MAX CORIOLIS-CURV ACCEL TO FILEEFDC.LOG
     * *  ISCFL:    1 WRITE DIAGNOSTICS OF MAX THEORETICAL TIME STEP TO CFL.OUT
     * *            GT 1  TIME STEP ONLY AT INTERVAL ISCFL FOR ENTIRE RUN
     * *  ISCFLM:   1  TO MAP LOCATIONS OF MAX TIME STEPS OVER ENTIRE RUN
     * *  DTSSFAC:  DYNAMIC TIME STEPPING IF 0.0.LT.DTSSFAC.LT.1.0
     * *
     * C8  TCON  TBEGIN    TREF CORIOLIS ISCORV   ISCCA   ISCFL  ISCFLM DTSSFAC
     *    86400   $RELATIVE_TSTART$   86400 8.5E-05       0       0       0       0       0
     * -------------------------------------------------------------------------------
     *
     * @param content
     * @return secondsPerTimeUnit.
     */
    private double getSecondsPerTimeUnit(ArrayList<String> content) {
        String parameterLine = getFirstParameterLineForCardImage(content, 8);
        if (parameterLine == null) {
            throw new RuntimeException("Card image with number 8 not found in EFDC.INP file.");
        }
        String[] strings = parameterLine.trim().split("\\s+");
        if (strings == null || strings.length < 1) {
            throw new RuntimeException("TCON not found in EFDC.INP file. Card image with number 8 contains not enough parameters.");
        }
        String tCon = strings[0];

        double secondsPerTimeUnit;
        try {
            secondsPerTimeUnit = Double.parseDouble(tCon);
        } catch (NumberFormatException e) {
            throw new RuntimeException("Cannot parse TCON in EFDC.INP file: " + tCon, e);
        }

        return secondsPerTimeUnit;
    }

    /**
     * Returns secondsPerReferencePeriod, which is TREF in card image 8 in EFDC.INP file:
     *
     * -------------------------------------------------------------------------------
     * C8 TIME-RELATED REAL PARAMETERS
     * *
     * *  TCON:     CONVERSION MULTIPLIER TO CHANGE TBEGIN TO SECONDS
     * *  TBEGIN:   TIME ORIGIN OF RUN
     * *  TREF:     REFERENCE TIME PERIOD IN sec (i.e. 44714.16S OR 86400S)
     * *  CORIOLIS: CONSTANT CORIOLIS PARAMETER IN 1/sec =2*7.29E-5*SIN(LAT)
     * *  ISCORV:   1 TO READ VARIABLE CORIOLIS COEFFICIENT FROM LXLY.INP FILE
     * *  ISCCA:    WRITE DIAGNOSTICS FOR MAX CORIOLIS-CURV ACCEL TO FILEEFDC.LOG
     * *  ISCFL:    1 WRITE DIAGNOSTICS OF MAX THEORETICAL TIME STEP TO CFL.OUT
     * *            GT 1  TIME STEP ONLY AT INTERVAL ISCFL FOR ENTIRE RUN
     * *  ISCFLM:   1  TO MAP LOCATIONS OF MAX TIME STEPS OVER ENTIRE RUN
     * *  DTSSFAC:  DYNAMIC TIME STEPPING IF 0.0.LT.DTSSFAC.LT.1.0
     * *
     * C8  TCON  TBEGIN    TREF CORIOLIS ISCORV   ISCCA   ISCFL  ISCFLM DTSSFAC
     *    86400   $RELATIVE_TSTART$   86400 8.5E-05       0       0       0       0       0
     * -------------------------------------------------------------------------------
     *
     * @param content
     * @return secondsPerReferencePeriod.
     */
    private double getSecondsPerReferencePeriod(ArrayList<String> content) {
        String parameterLine = getFirstParameterLineForCardImage(content, 8);
        if (parameterLine == null) {
            throw new RuntimeException("Card image with number 8 not found in EFDC.INP file.");
        }
        String[] strings = parameterLine.trim().split("\\s+");
        if (strings == null || strings.length < 3) {
            throw new RuntimeException("TREF not found in EFDC.INP file. Card image with number 8 contains not enough parameters.");
        }
        String tRef = strings[2];

        double secondsPerReferencePeriod;
        try {
            secondsPerReferencePeriod = Double.parseDouble(tRef);
        } catch (NumberFormatException e) {
            throw new RuntimeException("Cannot parse TREF in EFDC.INP file: " + tRef, e);
        }

        return secondsPerReferencePeriod;
    }

    /**
     * Searches the given file content and returns the first line with parameters
     * of the cardImage with the given cardImageNumber.
     * If cardImage not found, then returns null.
     *
     * @param content
     * @param cardImageNumber
     * @return String.
     */
    private String getFirstParameterLineForCardImage(ArrayList<String> content, int cardImageNumber) {
        String searchString = "C" + cardImageNumber;

        Iterator<String> iterator = content.iterator();
        boolean firstLineFound = false;
        while (iterator.hasNext()) {
            String line = iterator.next();

            String[] strings = line.trim().split("\\s+");
            if (strings != null && strings.length > 0 && searchString.equalsIgnoreCase(strings[0])) {
                if (!firstLineFound) {
                    firstLineFound = true;
                    continue;
                }

                //return next line.
                if (iterator.hasNext()) {
                    return iterator.next();
                }
            }
        }

        //if not found.
        return null;
    }

    private String getRelativeStartTimeString(double startTimeDouble, double secondsPerTimeUnit, TimeZone timeZone) {
        long startTime = Time.mjdToMillies(startTimeDouble);
        long referenceTime = EfdcUtils.getReferenceTime(startTime, timeZone);
        double relativeStartTime = EfdcUtils.getRelativeTimeInTimeUnits(startTime, referenceTime, secondsPerTimeUnit);
        return String.valueOf(relativeStartTime);
    }

    private int getNumberOfReferencePeriods(double startTimeDouble, double endTimeDouble, double secondsPerReferencePeriod) {
        long startTime = Time.mjdToMillies(startTimeDouble);
        long endTime = Time.mjdToMillies(endTimeDouble);
        double numberOfReferencePeriodsDouble = (double) (endTime - startTime)/1000.0/secondsPerReferencePeriod;
        //numberOfReferencePeriods must be an integer in EFDC.INP file.
        int numberOfReferencePeriodsInt = (int) Math.floor(numberOfReferencePeriodsDouble);
        if (Math.abs(numberOfReferencePeriodsDouble - numberOfReferencePeriodsInt) > 1e-10) {//if rounded.
            //log warning.
            Results.putMessage(this.getClass().getSimpleName() + ": WARNING: Number of reference periods " + numberOfReferencePeriodsDouble
                    + " rounded to " + numberOfReferencePeriodsInt + " in file " + this.efdcInpFile.getAbsolutePath());
        }
        return numberOfReferencePeriodsInt;
    }
}
