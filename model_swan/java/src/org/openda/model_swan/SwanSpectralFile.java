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

package org.openda.model_swan;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;

import java.io.*;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Reading and writing of any SWAN spectral files.
 */
public class SwanSpectralFile {
    private boolean isStationary;
    private Integer nLocations;
    private Integer nTimes = 1;
    private Integer nFreq;
    private Integer nDir;
    private String quantity;
    private String unit;
    private double exceptionValue;
    private ArrayList<SpectralValuesPerGrid> spectralVal = new ArrayList<SpectralValuesPerGrid>();
    private File spectralFile;
    private int nSpectralDataPerLine;
    private boolean isNegativeFactor = false;
    private int negGrid = Integer.MIN_VALUE;
    private double negFactor = Double.MIN_VALUE;
    private double zeroEnergy = 1.0E-100;
    private double epsilon = 0.1E-100;
    private boolean isNegativeSpectrum = false;
    private ArrayList<Double> dirArray = new ArrayList<Double>();


    public SwanSpectralFile(File spectralFile){
        if (!(spectralFile.exists())) {
            throw new RuntimeException("Spectral file does not exist: " + spectralFile.getAbsolutePath());
        }
        this.spectralFile = spectralFile;
        readSpectralFile(spectralFile);
    }

    public boolean isStationary() {
        return isStationary;
    }

    public Integer getNLocations() {
        return nLocations;
    }

    public Integer getNFreq() {
        return nFreq;
    }

    public Integer getNDir() {
        return nDir;
    }

    public String getQuantity() {
        return quantity;
    }

    public String getUnit() {
        return unit;
    }

    public double getExceptionValue() {
        return exceptionValue;
    }

    public Integer getNTimes() {
        return nTimes;
    }

    public String[] getTimesStr() {
        String[] times = new String[nTimes];
        if (nTimes == 1) {
            times = null;
        } else {
            for (int i=0; i<nTimes; i++){
                int index = i*nLocations;
                times[i] = spectralVal.get(index).time;
            }
        }
        return times;
    }

    public double[] getTimesDbl() {
        double[] times = new double[nTimes];
        if (nTimes == 1) {
            times = null;
        } else {
            for (int i=0; i<nTimes; i++){
                int index = i*nLocations;
                String timeLine = spectralVal.get(index).time;
                timeLine = timeLine.replace(".","");
                try {
                    times[i] = TimeUtils.date2Mjd(timeLine);
                } catch (ParseException e) {
                    throw new RuntimeException("Could not parse timeLine: "+timeLine+" from file "+spectralFile.getAbsolutePath());
                }
            }
        }
        return times;
    }


    public double[] getSpectralValuesAsDouble(int iTime) {
        int indexFirst = iTime*nLocations;
        int indexMax = nDir*nFreq;
        double[] dblValues = new double[nLocations*indexMax];

        for (int iGrid=0; iGrid < nLocations; iGrid++){
            int thisGridTime = indexFirst+iGrid;
            for (int index=0; index < indexMax; index++){
                double dblValue = spectralVal.get(thisGridTime).factor * spectralVal.get(thisGridTime).values[index];
                dblValues[index+iGrid*indexMax]=dblValue;
            }
        }
        return dblValues;
    }

    public void setSpectralValuesAsDouble(int iTime, double[] spcValues){
        int indexFirst = iTime*nLocations;
        int indexMax = nDir*nFreq;
        double[] dblValues = new double[indexMax];
        double factor;
//        double orgfactor;
        for (int iGrid=0; iGrid < nLocations; iGrid++){
            int thisGridTime = indexFirst+iGrid;
            ArrayList<Integer> negativeSpectrumIndices = new ArrayList<Integer>();
            for (int index=0; index < indexMax; index++){
                double dblValue = spcValues[index+iGrid*indexMax];
                dblValues[index] = dblValue;
                if (dblValue < 0.0){
                    isNegativeSpectrum = true;
                    negativeSpectrumIndices.add(index);
                }
            }

            if (isNegativeSpectrum) {
                correctNegativeSpectrum(dblValues,nDir,nFreq,negativeSpectrumIndices);
                isNegativeSpectrum = false;
            }
            negativeSpectrumIndices.clear();

            factor = getFactor(dblValues);
            int[] intValues = new int[dblValues.length];
            for (int index=0; index < dblValues.length; index++){
                intValues[index] = (int) Math.round(dblValues[index]/factor);
            }

            double location_m = spectralVal.get(thisGridTime).location_m;
            double location_n = spectralVal.get(thisGridTime).location_n;
            String time = spectralVal.get(thisGridTime).time;
            SpectralValuesPerGrid newSpectral = new SpectralValuesPerGrid(location_m,location_n,time,factor,intValues);
            spectralVal.set(thisGridTime,newSpectral);
        }
    }

    private void correctNegativeSpectrum(double[] dblValues, int nDir, int nFreq, ArrayList<Integer> negativeSpectrumIndices) {
        // written based on the subroutine RESCALE in swancom1.ftn of SWAN source code.
        int[] indexEndQuadrant = new int[5];
        indexEndQuadrant[0] = -1;
        for (int i=0; i<dirArray.size(); i++){
            double dir = dirArray.get(i);
            if (dir<90.0){
                indexEndQuadrant[1] = i;
            } else if (dir >= 90.0 && dir < 180.0) {
                indexEndQuadrant[2] = i;
            } else if (dir >= 180.0 && dir < 270.0) {
                indexEndQuadrant[3] = i;
            } else {
                indexEndQuadrant[4] = i;
            }
        }

        int i;
        i = 0;
        while (i<negativeSpectrumIndices.size()) {
            int thisIndex = negativeSpectrumIndices.get(i);
            if (dblValues[thisIndex] < 0.0) {
                int thisFreq = (int) Math.floor(thisIndex/nDir);
                int thisDir = thisIndex-thisFreq*nDir;

                int iQuadrant;
                if (thisDir <= indexEndQuadrant[1]) {
                    iQuadrant = 1;
                } else if (thisDir > indexEndQuadrant[1] && thisDir <= indexEndQuadrant[2]) {
                    iQuadrant = 2;
                } else if (thisDir > indexEndQuadrant[2] && thisDir <= indexEndQuadrant[3]) {
                    iQuadrant = 3;
                } else {
                    iQuadrant = 4;
                }

                double sumAll = 0.0;
                double sumPositive = 0.0;
                double sumAbsNegative = 0.0;
                for (int iDir=indexEndQuadrant[iQuadrant]; iDir>indexEndQuadrant[iQuadrant-1]; iDir--){
                    int indValue = thisFreq*nDir + iDir;
                    sumAll += dblValues[indValue];
                    if (dblValues[indValue] < 0.0){
                        sumAbsNegative += Math.abs(dblValues[indValue]);
                        dblValues[indValue] = 0.0;
                    } else {
                        sumPositive += dblValues[indValue];
                    }
                }
                if (sumPositive < 1.0E-15) sumPositive=1.0E-15;
                double rescaleFactor = sumAll/sumPositive;
                if (rescaleFactor > 0.0){
                    for (int iDir=indexEndQuadrant[iQuadrant]; iDir>indexEndQuadrant[iQuadrant-1]; iDir--){
                        int indValue = thisFreq*nDir + iDir;
    //                    dblValues[indValue] = dblValues[indValue]*sumAll/sumAbsNegative;
                        dblValues[indValue] = dblValues[indValue]*rescaleFactor;
                    }
                }
            }
            i++;
        }

        double minDblValues = Double.MAX_VALUE;
        for (int j=0; j<dblValues.length; j++){
            if (minDblValues > dblValues[j]) {
                minDblValues = dblValues[j];
            }
        }
        if (minDblValues<0.0){
//            System.out.println("Hoii...kok masih ada yang negative?????");
            throw new RuntimeException("Negative spectrum found in " + spectralFile.getAbsolutePath());
        }
    }

    public void multiplySpectralValues(int iTime, double[] alpha){
        double[] values = getSpectralValuesAsDouble(iTime);
        for (int i = 0; i < values.length; i++) {
            values[i] = alpha[0] * values[i];
        }
        setSpectralValuesAsDouble(iTime,values);

    }

    public void axpyOnSpectralValues(int iTime, double alpha, double[] axpyValues){
        double[] values = getSpectralValuesAsDouble(iTime);
        for (int i = 0; i < values.length; i++) {
            values[i] += alpha * axpyValues[i];
        }
        setSpectralValuesAsDouble(iTime,values);
    }

    public void writeSpectralFile(){
        Locale locale = new Locale("EN");
//        String factorValueFormat = "    %14.8E";
        String factorValueFormat = "%18.8E";
        String integerValueFormat;
        int maxValues;
        int nDigits;
        FileWriter fileWriter;
        SpectralValuesPerGrid stateValuesThisGrid;
        try {
            File tempFile = new File(this.spectralFile.getParent(), "temp.SPECTRAL");
            fileWriter = new FileWriter(tempFile);
            BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

            FileReader fileReader = new FileReader(this.spectralFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

            // write header:
            String line = inputFileBufferedReader.readLine();
            while (line != null) {
              if (  !(line.toUpperCase().contains("FACTOR"))
            	  & !(line.toUpperCase().contains("ZERO"))){
                  outputFileBufferedWriter.write(line);
                  outputFileBufferedWriter.newLine();
                  line = inputFileBufferedReader.readLine();
              } else {
                inputFileBufferedReader.close();
                break;
              }
            }

            if ( isStationary ) {

                // write state values:
                for (int iGrid=0; iGrid<nLocations; iGrid++){
                    stateValuesThisGrid = spectralVal.get(iGrid);
                    maxValues = max(stateValuesThisGrid.values);
//                    nDigits = Integer.toString(maxValues).length()+1;
                    nDigits = 6;
//                    integerValueFormat = "%"+nDigits+"d";
                    integerValueFormat = " %"+nDigits+"d";

                    outputFileBufferedWriter.write("FACTOR");
                    outputFileBufferedWriter.newLine();
                    outputFileBufferedWriter.write(String.format(locale, factorValueFormat, stateValuesThisGrid.factor));
                    outputFileBufferedWriter.newLine();
                    int ival = 0;
                    for (int r = 0; r < nFreq; r++) {
                        int iLine = 1;
                        for (int c=0; c < nDir; c++) {
                            if (c == iLine*nSpectralDataPerLine) {
                                outputFileBufferedWriter.newLine();
                                iLine++;
                            }
                            outputFileBufferedWriter.write(String.format(locale, integerValueFormat, stateValuesThisGrid.values[ival]));
                            ival++;
                        }
                        outputFileBufferedWriter.newLine();
                    }
                }

            } else {

                String time;
                int indSpectralVal;
                // write state values:
                for (int iTime=0; iTime<nTimes; iTime++) {
                    indSpectralVal = iTime*nLocations;
                    time = spectralVal.get(indSpectralVal).time;
                    if (iTime>0) {
                      // skip the first one
                      outputFileBufferedWriter.write(time);
                      outputFileBufferedWriter.newLine();
                    }
                    for (int iGrid=0; iGrid<nLocations; iGrid++){
                        stateValuesThisGrid = spectralVal.get(iGrid+indSpectralVal);
                        maxValues = max(stateValuesThisGrid.values);
                        nDigits = Integer.toString(maxValues).length()+1;
                        integerValueFormat = "%"+nDigits+"d";

                        outputFileBufferedWriter.write("FACTOR");
                        outputFileBufferedWriter.newLine();
                        outputFileBufferedWriter.write(String.format(locale, factorValueFormat, stateValuesThisGrid.factor));
                        outputFileBufferedWriter.newLine();
                        int ival = 0;
                        for (int r = 0; r < nFreq; r++) {
                            int iLine = 1;
                            for (int c=0; c < nDir; c++) {
                                if (c == iLine*nSpectralDataPerLine) {
                                    outputFileBufferedWriter.newLine();
                                    iLine++;
                                }
                                outputFileBufferedWriter.write(String.format(locale, integerValueFormat, stateValuesThisGrid.values[ival]));
                                ival++;
                            }
                            outputFileBufferedWriter.newLine();
                        }
                    }
                }

            }
            outputFileBufferedWriter.close();

            // move temp.HOT to the actual restart file:
            BBUtils.copyFile(tempFile,this.spectralFile);
            tempFile.deleteOnExit();

            if (isNegativeFactor) {
              throw new RuntimeException("Negative spectrum found in a.o. grid "+negGrid+", factor "+negFactor+" in " + spectralFile.getAbsolutePath());
            }
            
        } catch (IOException e) {
            throw new RuntimeException("Could not write to " + this.spectralFile.getAbsolutePath());
        }
    }

    private void readSpectralFile(File spectralFile){
        FileReader fileReader = null;
        try {
            fileReader = new FileReader(spectralFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);
            String[] fields;
            String line = inputFileBufferedReader.readLine();
            if (!line.toUpperCase().contains("SWAN")){
                throw new RuntimeException("Keyword SWAN is not found in " + spectralFile.getAbsolutePath());
            }
            line = inputFileBufferedReader.readLine();
            if (line.contains("TIME")){
                isStationary = false;
                line = inputFileBufferedReader.readLine();
                line = inputFileBufferedReader.readLine();
            } else {
                isStationary = true;
            }
            if (line.contains("LOCATIONS") || line.contains("LONLAT")){
                line = inputFileBufferedReader.readLine();
                fields = line.trim().split("[ /t]");
                nLocations = Integer.parseInt(fields[0]);
            } else {
                throw new RuntimeException("Keyword LOCATIONS or LONLAT is not found in " + spectralFile.getAbsolutePath());
            }
            double[] location_m = new double[nLocations];
            double[] location_n = new double[nLocations];
            for (int i=0; i<nLocations; i++){
                line = inputFileBufferedReader.readLine();
                fields = line.trim().split("[ ]");
                location_m[i] = Double.parseDouble(fields[0]);
                location_n[i] = Double.parseDouble(fields[fields.length-1]);
            }

            line = inputFileBufferedReader.readLine();
            if (line.contains("AFREQ") || line.contains("RFREQ")) {
                line = inputFileBufferedReader.readLine();
                fields = line.trim().split("[ /t]");
                nFreq = Integer.parseInt(fields[0]);
            } else {
                throw new RuntimeException("Keyword AFREQ or RFREQ is not found in " + spectralFile.getAbsolutePath());
            }
            for (int i=0; i<nFreq; i++){
                line = inputFileBufferedReader.readLine();
            }

            line = inputFileBufferedReader.readLine();
            if (line.contains("NDIR") || line.contains("CDIR")) {
                line = inputFileBufferedReader.readLine();
                fields = line.trim().split("[ /t]");
                nDir = Integer.parseInt(fields[0]);
            } else {
                throw new RuntimeException("Keyword NDIR or CDIR is not found in " + spectralFile.getAbsolutePath());
            }
            for (int i=0; i<nDir; i++){
                line = inputFileBufferedReader.readLine();
                fields = line.trim().split("[ /t]");
                double dir = Double.parseDouble(fields[0]);
                dirArray.add(dir);
            }

            line = inputFileBufferedReader.readLine();
            if (line.contains("QUANT")) {
                line = inputFileBufferedReader.readLine();
                fields = line.trim().split("[ /t]");
                Integer nQuant = Integer.parseInt(fields[0]);
                if (nQuant != 1){
                    throw new RuntimeException("Number of QUANT is not equal to one. 1D spectra is not supported.");
                }
            } else {
                throw new RuntimeException("Keyword QUANT is not found in " + spectralFile.getAbsolutePath());
            }
            line = inputFileBufferedReader.readLine();
            fields = line.trim().split("[ /t]");
            quantity = fields[0];
            line = inputFileBufferedReader.readLine();
            fields = line.trim().split("[ ]");
            unit = fields[0];
            line = inputFileBufferedReader.readLine();
            fields = line.trim().split("[ ]");
            exceptionValue = Double.parseDouble(fields[0]);

            String time = null;
            if ( !isStationary ) {
              line = inputFileBufferedReader.readLine();
              fields = line.trim().split("[ /t]");
              time = fields[0];
            }

            int[] values = new int[nDir*nFreq];
            int iLoc = 0;
            line = inputFileBufferedReader.readLine();
            boolean firstSpectralLine = true;
            while ( line!= null ){
                if (line.contains("ZERO") || line.contains("NODATA")){
                    //throw new RuntimeException("Spectral file contains keyword ZERO or NODATA: not yet supported."+
                    //"file="+spectralFile.getAbsolutePath()+"line="+line);
                    // TO DO: for line.contains "ZERO" and "NODATA"
                	// MVL
                	// fill with zeros
                	for(int i=0;i<values.length;i++){
                		values[i]=0;
                	}
                	double factor=1.0;
                    spectralVal.add(new SpectralValuesPerGrid(location_m[iLoc],location_n[iLoc],time,factor,values));
                    iLoc++;
                }else if (line.contains("FACTOR")){
                    line = inputFileBufferedReader.readLine();
                    fields = line.trim().split("[ /t]+");
                    double factor = Double.parseDouble(fields[0]);
                    int nvalues = 0;
                    int ivalues = 0;
                    while (nvalues<nDir*nFreq-1) {
                        line = inputFileBufferedReader.readLine();
                        fields = line.trim().split("[ /t]+");
                        nvalues = nvalues + fields.length;
                        for (int d=0; d<fields.length; d++){
                            values[ivalues] = Integer.parseInt(fields[d]);
                            ivalues++;
                        }
                        if (firstSpectralLine) {
                            firstSpectralLine = false;
                            nSpectralDataPerLine = fields.length;
                        }
                    }
                    spectralVal.add(new SpectralValuesPerGrid(location_m[iLoc],location_n[iLoc],time,factor,values));
                    iLoc++;
                }
                line = inputFileBufferedReader.readLine();
                // case of nonstationary: get time information
                if ( !isStationary && iLoc==nLocations && line != null) {
                    fields = line.trim().split("[ /t]");
                    iLoc = 0;
                    time = fields[0];
                    nTimes++;
                }
            }

            inputFileBufferedReader.close();
            fileReader.close();

        } catch (IOException e) {
            throw new RuntimeException("Could not read from " + spectralFile.getAbsolutePath());
        }

    }

    private double getFactor(double[] factors){
//        double factor = Double.MAX_VALUE;
//        for (double factor1 : factors) {
//            if (factor1 > epsilon) {
//                if (factor1 < factor) {
//                    factor = factor1;
//                }
//            }
//        }
//        return factor;
        double factor = 0.0;
        for (double factor1 : factors) {
            if (factor1 >= 0.0){
                if (factor<factor1){
                    factor=factor1;
                }
            } else {
                if (factor<10*Math.abs(factor1)){
                    factor=10*Math.abs(factor1);
                }
            }
        }
//        int DEC_SPEC = 4; // see swmod2.ftn and subroutine WRSPEC() in swanser.ftn
        int DEC_SPEC = 6; // from the resulting HOT file, I concluded that DEC_SPEC should be equal to 6

        factor = 1.01 * factor * Math.pow(10,-DEC_SPEC);

        return factor;
    }

    private static int max(int[] array) {
        return max(array, 0, array.length);
    }

    private static int max(int[] array, int pos, int length) {
        if (length == 0) return Integer.MIN_VALUE;
        int res = array[pos];
        for (int i = pos + 1, n = pos + length; i < n; i++) {
            int v = array[i];
            if (v > res) res = v;
        }

        return res;
    }

    private class SpectralValuesPerGrid {
        public double factor;
        public int[] values;
        public double location_m;
        public double location_n;
        public String time;
        public SpectralValuesPerGrid(double location_m, double location_n, String time, double factor, int[] values){
            this.location_m = location_m;
            this.location_n = location_n;
            this.time = time;
            if (factor < 0.0){
                throw new RuntimeException("Factor is negative in " + spectralFile.getAbsolutePath()+
                		"\n m="+location_m+" n="+location_n+" time="+time+" factor="+factor);
            }
            this.factor=factor;
            this.values = new int[values.length];
            int i=0;
            while (i<values.length) {
                if (values[i] < 0.0){
                    throw new RuntimeException("Spectral value is negative in " + spectralFile.getAbsolutePath()+
                    		"\n m="+location_m+" n="+location_n+" time="+time+" i="+i+" value="+values[i]	);
                }
                this.values[i]=values[i];
                i++;
            }
        }
    }
}
