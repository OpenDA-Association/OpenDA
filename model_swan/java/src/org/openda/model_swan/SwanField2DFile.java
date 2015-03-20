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
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

/**
 * Reading and writing scalar input/output files of SWAN with 2D field data format.
 */
public class SwanField2DFile implements IDataObject {

    private static String[] knownFileTypes = {"WLEV"};
    private IPrevExchangeItem[] exchangeItems;

    private File field2dFile;
    private File field2dSeriesFile =null;
    private ArrayList<String> listField2DFiles = new ArrayList<String>();
    private Field2dValuesPerTimeLevel field2dSeriesVal;
    private int mXInp;
    private int mYInp;
    private int mnMax;
    private int nTimeLevelField2D;
    private double facField2D;
//    private int nhedfField2DXY;
//    private int nhedtField2DXY;
//    private int nhedvecField2DXY;
//    private boolean isField2DXYSeries;
    private int nDataFirstLine;
    private int idlaWLevel;
    private String swanField2DFileId = "waterlevel";

    public int getNTimes(){
        return nTimeLevelField2D;
    }

    public double[] getValueField2D(int tIndexField2D) {
        double[] xComp;
        readField2DFile(tIndexField2D);
        xComp= field2dSeriesVal.valueField2D;
        return xComp;
    }

    public void multiplyField2D(int tIndexField2D, double[] alpha) {
            if (tIndexField2D!= field2dSeriesVal.intTime) {
                throw new RuntimeException("Wind data at time index "+tIndexField2D+" is not yet loaded.");
            }
            double[] xComp = getValueField2D(tIndexField2D);
            for (int i = 0; i < xComp.length; i++) {
                xComp[i] = alpha[0] * xComp[i];
            }
            System.arraycopy(xComp,0, field2dSeriesVal.valueField2D,0,xComp.length);
            writeField2DFile();
    }

    public void axpyField2D(int tIndexField2D, double alpha, double[] axpyField2D) {
        if (tIndexField2D!= field2dSeriesVal.intTime) {
            throw new RuntimeException("Field2D data at time index "+tIndexField2D+" is not yet loaded.");
        }
        double[] xComp = getValueField2D(tIndexField2D);
        for (int i = 0; i < xComp.length; i++) {
            xComp[i] += alpha * axpyField2D[i];
        }
        System.arraycopy(xComp,0, field2dSeriesVal.valueField2D,0,xComp.length);
        writeField2DFile();
    }

    public void setField2D(int tIndexField2D, double[] xComp) {
            if (tIndexField2D!= field2dSeriesVal.intTime) {
                throw new RuntimeException("Field2D data at time index "+tIndexField2D+" is not yet loaded.");
            }
            System.arraycopy(xComp,0, field2dSeriesVal.valueField2D,0,xComp.length);
            writeField2DFile();
    }

    public void writeField2DFile(){
        Locale locale = new Locale("EN");
        String field2DValueFormat = " %10.5f";
        FileWriter fileWriter;
        Field2dValuesPerTimeLevel field2dValuesPerTimeLevel;
        try {
            File tempFile = new File(this.field2dFile.getParent(), "temp.WND");
            fileWriter = new FileWriter(tempFile);
            BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

            field2dValuesPerTimeLevel = field2dSeriesVal;
            double[] valueField2D = field2dValuesPerTimeLevel.valueField2D;
            int iValueField2D = 0;
            if (idlaWLevel==5 | idlaWLevel==6) {
                for (int m=0;m< mXInp;m++){
                    int iLine = 1;
                    for (int n=0;n< mYInp;n++) {
                        double thisValue = valueField2D[iValueField2D]/this.facField2D;
                        if (n==iLine*nDataFirstLine) {
                            outputFileBufferedWriter.newLine();
                            iLine++;
                        }
                        outputFileBufferedWriter.write(String.format(locale, field2DValueFormat, thisValue));
                        iValueField2D++;
                    }
                    outputFileBufferedWriter.newLine();
                }
            } else {
                for (int n=0;n< mYInp;n++) {
                    int iLine = 1;
                    for (int m=0;m< mXInp;m++){
                        double thisValue = valueField2D[iValueField2D]/this.facField2D;
                        if (m==iLine*nDataFirstLine) {
                            outputFileBufferedWriter.newLine();
                            iLine++;
                        }
                        outputFileBufferedWriter.write(String.format(locale, field2DValueFormat, thisValue));
                        iValueField2D++;
                    }
                    outputFileBufferedWriter.newLine();
                }
            }
            outputFileBufferedWriter.close();

//            for (int m=0;m< mXInp;m++){
//                for (int n=0;n< mYInp;n++) {
//                    double thisValue = valueField2D[iValueField2D]/this.facField2D;
//                    outputFileBufferedWriter.write(String.format(locale, field2DValueFormat, thisValue));
//                    iValueField2D++;
//                }
//                outputFileBufferedWriter.newLine();
//            }
//            outputFileBufferedWriter.close();

            // move temp.WND to the actual restart file:
            BBUtils.copyFile(tempFile,this.field2dFile);
            tempFile.deleteOnExit();

        } catch (IOException e) {
            throw new RuntimeException("Could not write to " + this.field2dFile.getAbsolutePath());
        }
    }

    private void readField2DFile(int tIndexField2D) {
        this.field2dFile = new File(field2dSeriesFile.getParent(), listField2DFiles.get(tIndexField2D));
        double[] value = new double[mnMax];
//        double[] yComp = new double[mnMax];
        try {
            int iValue = 0;
//            int iYComp = 0;
            String[] lineFields;
            BufferedReader windFileBufferedReader = new BufferedReader(new FileReader(this.field2dFile));
            String line = windFileBufferedReader.readLine();
            boolean firstLine = true;
            int nData = 0;
            while (line != null) {
                if (nData<mnMax) {
                    lineFields = line.trim().split("[ \t]+");
                    for (String lineField : lineFields) {
                        value[iValue] = this.facField2D * Double.parseDouble(lineField);
                        iValue++;
                        nData++;
                    }
                } else {
                      throw new RuntimeException("Number of grid data is more than mnMax: " + this.field2dFile.getAbsolutePath());
                }
                if (firstLine) {
                    nDataFirstLine = lineFields.length;
                    firstLine = false;
                }
                line = windFileBufferedReader.readLine();
            }
            if (iValue<mnMax){
                throw new RuntimeException("Number of grid data is not complete: " + this.field2dFile.getAbsolutePath());
            } else if (iValue>mnMax) {
                throw new RuntimeException("Number of grid data is larger than grid points: " + this.field2dFile.getAbsolutePath());
            }
//            if (iYComp<mnMax){
//                throw new RuntimeException("Number of Y-component of grid data is not complete: " + this.field2dFile.getAbsolutePath());
//            } else if (iYComp>mnMax) {
//                throw new RuntimeException("Number of Y-component of grid data is larger than grid points: " + this.field2dFile.getAbsolutePath());
//            }
            if (nData<mnMax){
                throw new RuntimeException("Number of grid data is not complete: " + this.field2dFile.getAbsolutePath());
            } else if (nData>2*mnMax) {
                throw new RuntimeException("Number of grid data is larger than grid points: " + this.field2dFile.getAbsolutePath());
            }
            windFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read swan wind file " + this.field2dFile.getAbsolutePath());
        }
        field2dSeriesVal = new Field2dValuesPerTimeLevel(tIndexField2D,value);
    }

    private void readListField2DSeries() {
        try {
            BufferedReader windSeriesFileBufferedReader = new BufferedReader(new FileReader(this.field2dSeriesFile));
            String line = windSeriesFileBufferedReader.readLine();
            while (line != null) {
                listField2DFiles.add(line);
                line = windSeriesFileBufferedReader.readLine();
            }
            windSeriesFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read swan wind file " + this.field2dSeriesFile.getAbsolutePath());
        }

        if (listField2DFiles.size()<this.nTimeLevelField2D){
            throw new RuntimeException("Number of wind files *.WND is less than the specified wind time levels.");
        }

    }

    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);
    }

    private class Field2dValuesPerTimeLevel {
        public double[] valueField2D;
        public int intTime;

        public Field2dValuesPerTimeLevel(int intTime, double[] valueField2D) {
            this.intTime = intTime;
            this.valueField2D = new double[valueField2D.length];
            System.arraycopy(valueField2D,0,this.valueField2D,0, valueField2D.length);
        }
    }

    private void initialize(File workingDir, String fileName, String[] arguments) {

        if (arguments.length != 2) {
            throw new RuntimeException("Expected two arguments: 1. SWAN input *.swn file name, 2. the 2D-field type: " +
                    knownFileTypes[0]);
        }

        File swnFile = new File(workingDir,arguments[0]);
        if (!(swnFile.exists())) {
            throw new RuntimeException("Input file does not exist: " + swnFile.getAbsolutePath());
        }
        SwanParameters swanParams = new SwanParameters(swnFile);
        String tStart1 = null;
        String dtField2D = null;
        String dtUnitField2D = null;
        String tStop1 = null;
        if (arguments[1].toUpperCase().contentEquals(knownFileTypes[0])) {
            mXInp = swanParams.getMXInpWLevel();
            mYInp = swanParams.getMYInpWLevel();
            mnMax = mXInp * mYInp;
            tStart1 = swanParams.getTStartWLevel();
            dtField2D = swanParams.getDtWLevel();
            dtUnitField2D = swanParams.getDtUnitWLevel();
            tStop1 = swanParams.getTStopWLevel();
            facField2D = swanParams.getFactorWLevel();
            idlaWLevel = swanParams.getIDLAWLevel();
//            nhedfField2DXY = swanParams.getNHEDFWind();
//            nhedtField2DXY = swanParams.getNHEDTWind();
//            nhedvecField2DXY = swanParams.getNHEDVECWind();
    //        isField2DXYSeries = swanParams.isWindSeries();

//            if (!(idlaWind==1 | idlaWind==3)) {
//                throw new RuntimeException("Wind data format with idla != 1 or 3 is not yet supported.");
//            }

            Date tStart = null;
            Date tStop = null;
            try {
                SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd.HHmm", Locale.UK);
                tStart = formatter.parse(tStart1);
                tStop = formatter.parse(tStop1);
            } catch (ParseException e) {
                e.printStackTrace();
            }
            double dt = Double.parseDouble(dtField2D);
            double dtField2DmSec;
            if (dtUnitField2D.equalsIgnoreCase("SEC")) {
                dtField2DmSec = dt*1000;
            } else if (dtUnitField2D.equalsIgnoreCase("MIN")) {
                dtField2DmSec = dt*60*1000;
            } else if (dtUnitField2D.equalsIgnoreCase("HR")) {
                dtField2DmSec = dt*60*60*1000;
            } else if (dtUnitField2D.equalsIgnoreCase("DAY")) {
                dtField2DmSec = dt*24*60*60*1000;
            } else {
                throw new RuntimeException("Unit of time interval is not available in *.swn.");
            }
            assert tStop != null;
            this.nTimeLevelField2D = (int) ((tStop.getTime()-tStart.getTime())/ dtField2DmSec +1);

            if (!swanParams.getFileNameWLevel().contentEquals(fileName)){
                throw new RuntimeException("Input grid waterlevel file name in swn file is not equal with the one specified in xml file.");
            }
            this.field2dSeriesFile = new File(swnFile.getParent(),swanParams.getFileNameWLevel());
            readListField2DSeries();

            exchangeItems = new SwanField2DFileExchangeItem[1];
            exchangeItems[0] = new SwanField2DFileExchangeItem(swanField2DFileId,this);

//        } else if (arguments[1].toUpperCase().contentEquals(knownFileTypes[1])) {
//            throw new RuntimeException("Field2D file type "+arguments[1]+" is not yet implemented");
        } else {
            throw new RuntimeException("Field2D file is not of known types: "+arguments[1]+". Known types are: " +
                    knownFileTypes[0]);
        }
    }

    public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId()};
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanField2DFile.getDataObjectExchangeItem(): Not implemented yet.");
	}

    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        if (!exchangeItemID.equals(swanField2DFileId)) {
            throw new RuntimeException("unknown exchange item: " + exchangeItemID);
        }
        return exchangeItems[0];
    }

    public void finish() {
        // no action since writefile is executed everytime setX/Y is called.
    }

}