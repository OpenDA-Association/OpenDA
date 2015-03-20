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
import java.io.PrintWriter;
import java.util.List;

import org.openda.model_efdc.EfdcUtils;

/**
 * Formatter for EFDC QSER.INP file.
 *
 * Shortened example of a QSER.INP file:
 *
 * C qser.inp file, in free format across line, repeats nqser times  DDD  2011-05-27 16:00:00
 * C
 * C ISTYP MQSER(NS) TCQSER(NS) TAQSER(NS) RMULADJ(NS) ADDADJ(NS)
 * C
 * C if istyp.eq.1 then read depth weights and single value of QSER
 * C
 * C (WKQ(K),K=1,KC)
 * C
 * C TQSER(M,NS) QSER(M,1,NS) !(mqser(ns) pairs for ns=1,nqser series)
 * C
 * C else read a value of qser for each layer
 * C
 * C TQSER(M,NS) (QSER(M,K,NS),K=1,KC) !(mqser(ns) pairs)
 * C
 *        0    8760   86400       0       1       0       0 ' *** QSER_1
 *      0.000   23.2400
 *      0.040   23.2400
 *      0.080   13.6580
 *
 * ...
 *
 *    364.960   14.4330
 *    365.000   14.4330
 *        0      50   86400       0       1       0       0 ' *** QSER_2
 *      0.000    0.2200
 *      6.500    0.2200
 *     12.500    0.1330
 *
 * ...
 *
 *    356.500    0.8350
 *    365.000    0.8350
 *
 *
 *
 * @author Arno Kockx
 */
public class EfdcQserTimeSeriesFormatter extends EfdcTimeSeriesFormatter {

    public EfdcQserTimeSeriesFormatter(String fileType) {
        super(fileType);
    }

    @Override
    protected String getFileType() {
        return this.fileType;
    }

    /**
     * Reads the file header in the given inputFile.
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
    }

    @Override
    protected void writeTimeSeriesHeaderForLocation(PrintWriter writer,
            int locationId, int numberOfNonMissingValues) {

        //Comments are taken from the EFDC User Manual.
        //ISTYP (the time series format identifier)
        int isType = 0;
        //MCSER (the number of time data points)
        int dataPointCount = numberOfNonMissingValues;
        //TCCSER (a multiplying conversion factor changing the input time units to seconds)
        double secondsPerTimeUnit = this.secondsPerTimeUnit;
        //TACSER (an additive time adjustment, applied before unit conversion)
        double additiveTimeAdjustment = 0;
        //RMULADJ (a multiplying conversion for the concentration)
        double multiplyingConversion = 1;
        //ADDADJ (an additive conversion for concentration, applied before the multiplier)
        double additiveConversion = 0;
        //not clear what this integer does, since this integer is present in the QSER.INP
        //file example in the EFDC User Manual, but it is not described.
        int zero = 0;
        //timeSeriesId is not used by the EFDC model, but it makes the file human readable.
        String timeSeriesId = getFileType() + "_" + locationId;

        StringBuffer buffer = new StringBuffer();
        buffer.append(" ").append(isType);
        buffer.append(" ").append(dataPointCount);
        buffer.append(" ").append(secondsPerTimeUnit);
        buffer.append(" ").append(additiveTimeAdjustment);
        buffer.append(" ").append(multiplyingConversion);
        buffer.append(" ").append(additiveConversion);
        buffer.append(" ").append(zero);
        buffer.append(" ' *** ").append(timeSeriesId);
        writer.println(buffer.toString());
    }
}
