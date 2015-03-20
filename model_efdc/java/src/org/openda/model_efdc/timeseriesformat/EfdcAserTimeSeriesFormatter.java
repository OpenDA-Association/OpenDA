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

package org.openda.model_efdc.timeseriesformat;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.model_efdc.EfdcTimeSeriesExchangeItem;
import org.openda.model_efdc.EfdcUtils;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * Formatter for EFDC ASER.INP file.
 *
 * Shortened example of a ASER.INP file:
 *
 * C ** aser.inp file, in free format across line, repeats naser=1 times
 * C **
 * C **  ATMOSPHERIC FORCING FILE, USE WITH 28 JULY 96 AND LATER VERSIONS OF EFDC
 * C **
 * C **  MASER     =NUMBER OF TIME DATA POINTS
 * C **  TCASER    =DATA TIME UNIT CONVERSION TO SECONDS
 * C **  TAASER    =ADDITIVE ADJUSTMENT OF TIME VALUES SAME UNITS AS INPUT TIMES
 * C **  IRELH     =0 VALUE TWET COLUMN VALUE IS TWET, =1 VALUE IS RELATIVE HUMIDITY
 * C **  RAINCVT   =CONVERTS RAIN TO UNITS OF M/SEC , inch/day=0.0254m/86400s=2.94E-7m/s, inch/h=7.05556E-6m/s
 * C **  EVAPCVT   =CONVERTS EVAP TO UNITS OF M/SEC, IF EVAPCVT<0 EVAP IS INTERNALLY COMPUTED
 * C **  SOLRCVT   =CONVERTS SOLAR SW RADIATION TO JOULES/S/SQ METER (Watts/m^2)
 * C **  CLDCVT    =MULTIPLIER FOR ADJUSTING CLOUD COVER
 * C **  IASWRAD   =O DISTRIBUTE SW SOL RAD OVER WATER COL AND INTO BED, =1 ALL TO SURF LAYER
 * C **  REVC      =1000*EVAPORATIVE TRANSFER COEF, REVC<0 USE WIND SPD DEPD DRAG COEF
 * C **  RCHC      =1000*CONVECTIVE HEAT TRANSFER COEF, REVC<0 USE WIND SPD DEPD DRAG COEF
 * C **  SWRATNF   =FAST SCALE SOLAR SW RADIATION ATTENUATION COEFFCIENT 1./METERS
 * C **  SWRATNS   =SLOW SCALE SOLAR SW RADIATION ATTENUATION COEFFCIENT 1./METERS
 * C **  FSWRATF   =FRACTION OF SOLSR SW RADIATION ATTENUATED FAST  0<FSWRATF<1
 * C **  DABEDT    =DEPTH OR THICKNESS OF ACTIVE BED TEMPERATURE LAYER, METERS
 * C **  TBEDIT    =INITIAL BED TEMPERATURE
 * C **  HTBED1    =CONVECTIVE HT COEFFCIENT BETWEEN BED AND BOTTOM WATER LAYER  NO DIM
 * C **  HTBED2    =HEAT TRANS COEFFCIENT BETWEEN BED AND BOTTOM WATER LAYER  M/SEC
 * C **  PATM      =ATM PRESS MILLIBAR
 * C **  TDRY/TEQ  =DRY ATM TEMP ISOPT(2)=1 OR EQUIL TEMP ISOPT(2)=2
 * C **  TWET/RELH =WET BULB ATM TEMP IRELH=0, RELATIVE HUMIDITY IRELH=1
 * C **  RAIN      =RAIN FALL RATE LENGTH/TIME
 * C **  EVAP      =EVAPORATION RATE IS EVAPCVT>0.
 * C **  SOLSWR    =SOLAR SHORT WAVE RADIATION AT WATER SURFACE  ENERGY FLUX/UNIT AREA
 * C **  CLOUD     =FRACTIONAL CLOUD COVER
 * EE    EFDC_DS_SOLRAD (COMPUTE SOLAR RADIATION FROM LAT/LONG & CLOUD COVER): False
 * EE    EFDC_DS_LATITUDE  (DEC DEGREES):35.520
 * EE    EFDC_DS_LONGITUDE (DEC DEGREES):0.000
 * EE    EFDC_DS_USESHADE  (T/F): False
 * C **   MASER     TCASER   TAASER   IRELH    RAINCVT  EVAPCVT  SOLRCVT   CLDCVT
 * C **  IASWRAD     REVC    RCHC    SWRATNF  SWRATNS   FSWRATF  DABEDT    TBEDIT  HTBED1  HTBED2
 * C **   TASER(M)  PATM(M)  TDRY(M)  TWET(M)  RAIN(M)  EVAP(M)  SOLSWR(M) CLOUD(M)
 *       8761 86400.000     0.000         1  1.1574E-05     -1          1.000     1.000   ! *** ASER_1
 *          0       1.5       1.5         1         0         1         5         7         0   2.8E-06
 *      0.000 1014.9000   -3.3000    0.5200    0.0000    0.0000    0.0000    0.0670
 *      0.040 1014.9000   -3.3000    0.5200    0.0000    0.0000    0.0000    0.0670
 *      0.080 1014.7000   -3.4000    0.5400    0.0000    0.0000    0.0000    0.1330
 *
 * ...
 *
 *    364.960 1015.2000   -6.8000    0.3900    0.0000    0.0000    0.0000    0.0000
 *    365.000 1015.2000   -6.8000    0.3900    0.0000    0.0000    0.0000    0.0000
 *
 *
 *
 * @author Arno Kockx
 */
public class EfdcAserTimeSeriesFormatter extends EfdcTimeSeriesFormatter {

    private Map<String, String[]> timeSeriesHeadersMap = new HashMap<String, String[]>();
    private int locationIdCount = 0;

    public EfdcAserTimeSeriesFormatter(String fileType) {
        super(fileType);
    }

    @Override
    protected String getFileType() {
        return this.fileType;
    }

    /**
     * Reads the parameter values in the time series headers in the given inputFile.
     *
     * @param inputFile
     */
    @Override
    public void readFile(File inputFile) {
        if (!inputFile.exists()) {
            throw new RuntimeException("Input file '" + inputFile.getAbsolutePath()
                    + "' does not exist.");
        }

        //read header.
        List<String> lines = EfdcUtils.readFile(inputFile);
        removeAndStoreFileHeader(lines);

        //read time series header parameters.
        readTimeSeriesHeaderParameters(lines);
    }

    /**
     * For each time series reads and stores the relevant parameters.
     * This method assumes that there are no comment lines or empty lines in the given list.
     *
     * @param lines
     */
    private void readTimeSeriesHeaderParameters(List<String> lines) {
        this.timeSeriesHeadersMap.clear();

        Iterator<String> iterator = lines.iterator();
        int locationIdNumber = 1;
        while (iterator.hasNext()) {
            String timeSeriesHeaderLine1 = iterator.next();
            if (!iterator.hasNext()) {
                break;
            }
            String timeSeriesHeaderLine2 = iterator.next();

            //store first and second line of time series header.
            this.timeSeriesHeadersMap.put(String.valueOf(locationIdNumber), new String[]{timeSeriesHeaderLine1, timeSeriesHeaderLine2});

            String[] strings = timeSeriesHeaderLine1.trim().split("\\s+");
            String dataPointCountString = strings[0];
            int dataPointCount;
            try {
                dataPointCount = Integer.parseInt(dataPointCountString);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Invalid data point count '" + dataPointCountString
                        + "' specified for time series ASER_" + locationIdNumber
                        + " in existing ASER.INP file. Cannot write input time series if existing ASER.INP file is not valid.", e);
            }

            //read until start of next time series.
            int n = 0;
            while (n < dataPointCount && iterator.hasNext()) {
                iterator.next();
                n++;
            }

            locationIdNumber++;
        }

        this.locationIdCount = locationIdNumber - 1;
    }

    /**
     * Writes all given timeSeries to the given file.
     *
     * @param outputFile
     * @param timeSeriesList
     */
    @Override
    public void writeTimeSeriesToFile(File outputFile, Map<String, IPrevExchangeItem> timeSeriesMap) {
        if (timeSeriesMap == null || timeSeriesMap.isEmpty()) {
            throw new RuntimeException("No time series to write to file '" + outputFile.getAbsolutePath() + "'.");
        }

        //write.
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new FileWriter(outputFile));

            //write file header.
            writeFileHeader(writer);

            //write time series.
            //the model only uses the index number of the time series, i.e. the place in which it
            //is present in the .INP file. Therefore the time series must always be in the correct order.
            //Therefore here write the time series in the order of their location id number.
            Results.putMessage(this.getClass().getSimpleName() + ": writing input time series for "
                    + this.locationIdCount + " locations to file " + outputFile.getAbsolutePath());
            for (int locationId = 1; locationId <= this.locationIdCount; locationId++) {
                writeTimeSeriesForLocation(writer, locationId, timeSeriesMap);
            }

            writer.flush();

        } catch (Exception e) {
            throw new RuntimeException("Exception while writing to file '" + outputFile.getAbsolutePath()
                    + "', message was: " + e.getMessage(), e);
        } finally {
            if (writer != null) {
                writer.close();
            }
        }
    }

    /**
     * Writes all values in the TimeSeries for the given locationId
     * in the given timeSeriesMap to the given writer in EFDC input format.
     *
     * @param writer
     * @param locationId
     * @param timeSeriesMap
     */
    @Override
    protected void writeTimeSeriesForLocation(PrintWriter writer, int locationId, Map<String, IPrevExchangeItem> timeSeriesMap) {

        //get data.
        String[] parameterIds = {"PATM", "TDRY", "TWET", "RAIN", "EVAP", "SOLSWR", "CLOUD"};
        EfdcTimeSeriesExchangeItem[] timeSeries = new EfdcTimeSeriesExchangeItem[parameterIds.length];
        double[][] times = new double[parameterIds.length][];
        double[][] values = new double[parameterIds.length][];
        for (int k = 0; k < parameterIds.length; k++) {
            timeSeries[k] = (EfdcTimeSeriesExchangeItem) timeSeriesMap.get(locationId + "." + parameterIds[k]);
            if (timeSeries[k] == null) {
                throw new RuntimeException("No time series available to write to file " + fileType
                        + ".INP for locationId " + locationId + " and parameter " + parameterIds[k] + ".");
            }

            times[k] = timeSeries[k].getTimesRef();
            values[k] = timeSeries[k].getValuesRef();
            if (times[k] == null || values[k] == null || times[k].length == 0 || values[k].length == 0
                    || EfdcUtils.containsOnlyMissing(times[k]) || EfdcUtils.containsOnlyMissing(values[k])) {
                throw new RuntimeException("No data to write for time series with id '" + timeSeries[k].getId()
                        + "', time series is empty or contains only missing values.");
            }
        }

        //Note: the model always expects the same total number of time series to be present
        //in the .INP file (see EFDC.INP file in sections C22 and C23).
        //Furthermore the model uses the index number of the time series,
        //i.e. the place in which it is present in the .INP file to refer to it in the
        //EFDC.INP file in section C24. Therefore all time series must always be present
        //and in the correct order. Therefore if only missing values, then
        //throw an exception.
        //assume that times are the same for all parameters.
        //assume no missing values.
        int numberOfValues = times[0].length;

        //validate data.
        for (int k = 0; k < parameterIds.length; k++) {
            if (values[k].length != numberOfValues) {
                throw new RuntimeException("Number of data values to write to file " + fileType
                        + ".INP for locationId " + locationId + " and parameter " + parameterIds[k]
                        + " does not match number of values for other parameters, which is " + numberOfValues + ".");
            }
        }

        //write time series header.
        writeTimeSeriesHeaderForLocation(writer, locationId, numberOfValues);

        //write data.
        for (int n = 0; n < numberOfValues; n++) {
            double timeInTimeUnits = EfdcUtils.getRelativeTimeInTimeUnits(Time.mjdToMillies(times[0][n]),
                    this.referenceTime, this.secondsPerTimeUnit);

            StringBuffer buffer = new StringBuffer();
            buffer.append(" ").append(timeInTimeUnits);
            for (int k = 0; k < parameterIds.length; k++) {
                buffer.append(" ").append(values[k][n]);
            }

            writer.println(buffer.toString());
        }
    }

    @Override
    protected void writeTimeSeriesHeaderForLocation(PrintWriter printer, int locationId, int numberOfNonMissingValues) {
        String[] timeSeriesHeaderLines = this.timeSeriesHeadersMap.get(String.valueOf(locationId));
        if (timeSeriesHeaderLines == null || timeSeriesHeaderLines.length != 2
                || timeSeriesHeaderLines[0] == null || timeSeriesHeaderLines[1] == null) {
            throw new RuntimeException("Invalid time series header lines in file " + fileType
                    + ".INP for locationId " + locationId + ".");
        }

        String[] firstHeaderLineStrings = timeSeriesHeaderLines[0].trim().split("\\s+");
        if (firstHeaderLineStrings == null || firstHeaderLineStrings.length < 3) {
            throw new RuntimeException("Invalid time series header lines in file " + fileType
                    + ".INP for locationId " + locationId + ".");
        }
        String[] restOfFirstHeaderLineStrings = Arrays.copyOfRange(firstHeaderLineStrings, 2, firstHeaderLineStrings.length);
        StringBuffer buffer = new StringBuffer();
        for (int n = 0; n < restOfFirstHeaderLineStrings.length; n++) {
            buffer.append(" ").append(restOfFirstHeaderLineStrings[n]);
        }
        String restOfFirstHeaderLine = buffer.toString();
        String secondHeaderLine = timeSeriesHeaderLines[1];

        //Comments are taken from the EFDC User Manual.
        //MASER (the number of time data points)
        int dataPointCount = numberOfNonMissingValues;
        //TCASER (a multiplying conversion factor changing the input time units to seconds)
        double secondsPerTimeUnit = this.secondsPerTimeUnit;

        //write times series header.
        buffer = new StringBuffer();
        buffer.append(" ").append(dataPointCount);
        buffer.append(" ").append(secondsPerTimeUnit);
        buffer.append(restOfFirstHeaderLine);
        printer.println(buffer.toString());
        printer.println(secondHeaderLine);
    }
}
