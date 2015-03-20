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
 * Formatter for EFDC CWQSR**.INP file where ** is an integer id number.
 *
 * Shortened example of a CWQSR**.INP file:
 *
 * C cwqsr02.inp, Diatoms (mg/l as C)  DDD  2011-05-27 16:00:00
 * C
 * C ISTYP MCSER(NS,8) TCCSER(NS,8) TACSER(NS,8) RMULADJ(NS,8) ADDADJ(NS,8)
 * C
 * C if istyp.eq.1 then read depth weights and single value of CSER
 * C
 * C (WKQ(K),K=1,KC)
 * C
 * C TCSER(M,NS,8) CSER(M,NS,8) !(mcser(ns,8) pairs for ns=8,ncser(8) series)
 * C
 * C else read a value of dser for each layer
 * C
 * C TCSER(M,NS,8) (CSER(M,K,NS,8),K=1,KC) !(mcser(ns,8) pairs)
 * C
 *        0      14   86400       0       1       0
 *      0.000    6.7181
 *      7.500    6.7181
 *     34.500    7.1966
 *     69.500    5.7133
 *     91.500    4.5170
 *    122.500    1.3589
 *    155.500    0.4306
 *    182.500    1.1867
 *    213.500    1.2728
 *    245.500    1.7896
 *    274.500    2.1628
 *    308.500    1.7800
 *    337.500    0.1436
 *    366.500    0.1436
 *
 *
 *
 * @author Arno Kockx
 */
public class EfdcCwqsrTimeSeriesFormatter extends EfdcTimeSeriesFormatter {

    public EfdcCwqsrTimeSeriesFormatter(String fileType) {
        super(fileType);
    }

    @Override
    protected String getFileType() {
        return this.fileType;
    }

    /**
     * Reads the file header in the given inputFile.
     * The file header is different for different CWQSR**.INP files.
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
        //timeSeriesId is not used by the EFDC model, but it makes the file human readable.
        String timeSeriesId = getFileType() + "_" + locationId;

        StringBuffer buffer = new StringBuffer();
        buffer.append(" ").append(isType);
        buffer.append(" ").append(dataPointCount);
        buffer.append(" ").append(secondsPerTimeUnit);
        buffer.append(" ").append(additiveTimeAdjustment);
        buffer.append(" ").append(multiplyingConversion);
        buffer.append(" ").append(additiveConversion);
        buffer.append(" ' *** ").append(timeSeriesId);
        writer.println(buffer.toString());
    }
}
