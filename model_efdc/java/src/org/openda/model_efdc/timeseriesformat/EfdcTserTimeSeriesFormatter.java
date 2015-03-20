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
 * Formatter for EFDC TSER.INP file.
 *
 * Shortened example of a TSER.INP file:
 *
 * C tser.inp file, in free format across line, repeats ntser times  DDD  2011-05-27 16:00:00
 * C
 * C ISTYP MTSER(NS) TCTSER(NS) TATSER(NS) RMULADJ(NS) ADDADJ(NS)
 * C
 * C if istyp.eq.1 then read depth weights and single value of TSER
 * C
 * C (WKQ(K),K=1,KC)
 * C
 * C TTSER(M,NS) TSER(M,1,NS) !(mqser(ns) pairs for ns=1,nqser series)
 * C
 * C else read a value of qser for each layer
 * C
 * C TTSER(M,NS) (TSER(M,K,NS),K=1,KC) !(mqser(ns) pairs)
 * C
 *        0      49   86400       0       1       0 ' *** TEMP_1
 *      0.000    6.0000
 *      5.500    6.0000
 *     11.500    5.0000
 *
 * ...
 *
 *    355.500    8.0000
 *    365.500    8.0000
 *        0      50   86400       0       1       0 ' *** TEMP_2
 *      0.000    4.0000
 *      6.500    4.0000
 *     12.500    3.0000
 *
 * ...
 *
 *    356.500    3.0000
 *    365.500    3.0000
 *
 *
 *
 * @author Arno Kockx
 */
public class EfdcTserTimeSeriesFormatter extends EfdcTimeSeriesFormatter {

    public EfdcTserTimeSeriesFormatter(String fileType) {
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
        //MTSER (the number of time data points)
        int dataPointCount = numberOfNonMissingValues;
        //TCTSER (a multiplying conversion factor changing the input time units to seconds)
        double secondsPerTimeUnit = this.secondsPerTimeUnit;
        //TATSER (an additive time adjustment, applied before unit conversion)
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
