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
 * Formatter for EFDC PSER.INP file.
 *
 * Shortened example of a PSER.INP file:
 *
 * C pser.inp file, in free format across line, repeats npser times
 * C
 * C MPSER(NS) TCPSER(NS) TAPSER(NS) RMULADJ(NS) ADDADJ(NS)
 * C
 * C TPSER(M,NS) PSER(M,NS) !(mpser(ns) pairs for ns=1,npser series)
 * C
 *        4   86400       0       1       0 ' *** PSER_1
 *     265.00      4.90
 *     270.00      4.90
 *     273.00      4.90
 *     275.00      2.06
 *
 *
 *
 * @author Arno Kockx
 */
public class EfdcPserTimeSeriesFormatter extends EfdcTimeSeriesFormatter {

    public EfdcPserTimeSeriesFormatter(String fileType) {
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
        //MPSER (the number of time data points)
        int dataPointCount = numberOfNonMissingValues;
        //TCPSER (a multiplying conversion factor changing the input time units to seconds)
        double secondsPerTimeUnit = this.secondsPerTimeUnit;
        //TAPSER (an additive time adjustment, applied before unit conversion)
        double additiveTimeAdjustment = 0;
        //RMULADJ (a multiplying conversion for the concentration)
        double multiplyingConversion = 1;
        //ADDADJ (an additive conversion for concentration, applied before the multiplier)
        double additiveConversion = 0;
        //timeSeriesId is not used by the EFDC model, but it makes the file human readable.
        String timeSeriesId = getFileType() + "_" + locationId;

        StringBuffer buffer = new StringBuffer();
        buffer.append(" ").append(dataPointCount);
        buffer.append(" ").append(secondsPerTimeUnit);
        buffer.append(" ").append(additiveTimeAdjustment);
        buffer.append(" ").append(multiplyingConversion);
        buffer.append(" ").append(additiveConversion);
        buffer.append(" ' *** ").append(timeSeriesId);
        writer.println(buffer.toString());
    }
}
