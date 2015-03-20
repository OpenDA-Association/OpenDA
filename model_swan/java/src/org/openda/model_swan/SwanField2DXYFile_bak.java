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
import org.openda.model_swan.SwanParameters;

import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

/**
 * Reading and writing vector components (XY) input/output files of SWAN, each in 2D field data format.
 */
public class SwanField2DXYFile_bak implements IDataObject {

    private static String[] knownFileTypes = {"WIND", "CURRENT"};
    private IExchangeItem[] exchangeItems;

    private File field2dXYFile;
    private File field2dXYSeriesFile =null;
    private ArrayList<String> listWindFiles = new ArrayList<String>();
//    private ArrayList<String> lines = new ArrayList<String>();
//    private ArrayList<Field2dXYValuesPerTimeLevel> field2dXYVal = new ArrayList<Field2dXYValuesPerTimeLevel>();
    private Field2dXYValuesPerTimeLevel field2dXYSeriesVal;
    private int mXInp;
    private int mYInp;
    private int mnMax;
    private int nTimeLevelField2DXY;
    private double facField2DXY;
    private int nhedfField2DXY;
    private int nhedtField2DXY;
    private int nhedvecField2DXY;
    private int nDataFirstLine;
    private int idlaField2DXY;
    private String[] swanField2DXYFileId = new String[]{"wind.x","wind.y","wind.xy"};

    public int getNTimes(){
        return nTimeLevelField2DXY;
    }

    public double[] getXYComp(int tIndexField2DXY) {
        double[] xComp = getXComp(tIndexField2DXY);
        double[] yComp = getYComp(tIndexField2DXY);
        double[] xyComp = new double[xComp.length+yComp.length];
        System.arraycopy(xComp,0,xyComp,0,xComp.length);
        System.arraycopy(yComp,0,xyComp,xComp.length,yComp.length);
        return xyComp;
    }

    public double[] getXComp(int tIndexField2DXY) {
        double[] xComp;
//        if (isField2DXYSeries) {
            readField2DXYFile(tIndexField2DXY);
            xComp= field2dXYSeriesVal.xComp;
//        } else {
//            valueField2D= field2dXYVal.get(tIndexField2DXY).valueField2D;
//        }
        return xComp;
    }

    public double[] getYComp(int tIndexField2DXY) {
        double[] yComp;
//        if (isField2DXYSeries) {
            readField2DXYFile(tIndexField2DXY);
            yComp= field2dXYSeriesVal.yComp;
//        } else {
//            yComp= field2dXYVal.get(tIndexField2DXY).yComp;
//        }
        return yComp;
    }

    public void multiplyXYComp(int tIndexField2DXY, double[] alpha) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Wind data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            double[] xComp = getXComp(tIndexField2DXY);
            double[] yComp = getYComp(tIndexField2DXY);
            for (int i = 0; i < xComp.length; i++) {
                xComp[i] = alpha[0] * xComp[i];
                yComp[i] = alpha[0] * yComp[i];
            }
            System.arraycopy(xComp,0, field2dXYSeriesVal.xComp,0,xComp.length);
            System.arraycopy(yComp,0, field2dXYSeriesVal.yComp,0,yComp.length);
            writeField2DXYFile();
    }

    public void multiplyXComp(int tIndexField2DXY, double[] alpha) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Wind data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            double[] xComp = getXComp(tIndexField2DXY);
            for (int i = 0; i < xComp.length; i++) {
                xComp[i] = alpha[0] * xComp[i];
            }
            System.arraycopy(xComp,0, field2dXYSeriesVal.xComp,0,xComp.length);
            writeField2DXYFile();
    }

    public void multiplyYComp(int tIndexField2DXY, double[] alpha) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Wind data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            double[] yComp = getYComp(tIndexField2DXY);
            for (int i = 0; i < yComp.length; i++) {
                yComp[i] = alpha[0] * yComp[i];
            }
            System.arraycopy(yComp,0, field2dXYSeriesVal.yComp,0,yComp.length);
            writeField2DXYFile();
    }

    public void axpyXYComp(int tIndexField2DXY, double alpha, double[] axpyXComp, double[] axpyYComp) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Wind data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            double[] xComp = getXComp(tIndexField2DXY);
            double[] yComp = getYComp(tIndexField2DXY);
            for (int i = 0; i < xComp.length; i++) {
                xComp[i] += alpha * axpyXComp[i];
                yComp[i] += alpha * axpyYComp[i];
            }
            System.arraycopy(xComp,0, field2dXYSeriesVal.xComp,0,xComp.length);
            System.arraycopy(yComp,0, field2dXYSeriesVal.yComp,0,yComp.length);
            writeField2DXYFile();
    }

    public void axpyXYComp(int tIndexField2DXY, double alpha, double[] axpyXYValues) {
        int lengthX = getXComp(tIndexField2DXY).length;
        int lengthY = getYComp(tIndexField2DXY).length;
        double[] xComp = new double[lengthX];
        double[] yComp = new double[lengthY];
        System.arraycopy(axpyXYValues,0,xComp,0,lengthX);
        System.arraycopy(axpyXYValues,lengthX,yComp,0,lengthY);
        axpyXYComp(tIndexField2DXY,alpha,xComp,yComp);
    }

    public void axpyXComp(int tIndexField2DXY, double alpha, double[] axpyXComp) {
        double[] yComp = new double[axpyXComp.length];
        for (int i=0; i<yComp.length; i++) {
          yComp[i] = 0.0;
        }
        axpyXYComp(tIndexField2DXY,alpha,axpyXComp,yComp);
    }

    public void axpyYComp(int tIndexField2DXY, double alpha, double[] axpyYComp) {
        double[] xComp = new double[axpyYComp.length];
        for (int i=0; i<xComp.length; i++) {
          xComp[i] = 0.0;
        }
        axpyXYComp(tIndexField2DXY,alpha,xComp,axpyYComp);
    }

    public void setXYComp(int tIndexField2DXY, double[] xComp, double[] yComp) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Wind data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            System.arraycopy(xComp,0, field2dXYSeriesVal.xComp,0,xComp.length);
            System.arraycopy(yComp,0, field2dXYSeriesVal.yComp,0,yComp.length);
            writeField2DXYFile();
    }

    public void setXYComp(int tIndexField2DXY, double[] xyComp) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Field2DXY data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            int lengthX = getXComp(tIndexField2DXY).length;
            int lengthY = getYComp(tIndexField2DXY).length;
            double[] xComp = new double[lengthX];
            double[] yComp = new double[lengthY];
            System.arraycopy(xyComp,0,xComp,0,lengthX);
            System.arraycopy(xyComp,lengthX,yComp,0,lengthY);
            setXYComp(tIndexField2DXY,xComp,yComp);
    }

    public void setXComp(int tIndexField2DXY, double[] xComp) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Field2DXY data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            setXYComp(tIndexField2DXY,xComp, field2dXYSeriesVal.yComp);
    }

    public void setYComp(int tIndexField2DXY, double[] yComp) {
            if (tIndexField2DXY!= field2dXYSeriesVal.intTime) {
                throw new RuntimeException("Field2DXY data at time index "+tIndexField2DXY+" is not yet loaded.");
            }
            setXYComp(tIndexField2DXY, field2dXYSeriesVal.xComp,yComp);
    }

    public void writeField2DXYFile(){
        Locale locale = new Locale("EN");
        String windValueFormat = "%8.2f";
        FileWriter fileWriter;
        Field2dXYValuesPerTimeLevel field2dXYValuesPerTimeLevel;
        try {
            File tempFile = new File(this.field2dXYFile.getParent(), "temp.WND");
            fileWriter = new FileWriter(tempFile);
            BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

            field2dXYValuesPerTimeLevel = field2dXYSeriesVal;
            if (idlaField2DXY == 5 | idlaField2DXY == 6) {
                double[] wind = field2dXYValuesPerTimeLevel.xComp;
                int iXComp = 0;
                for (int m=0;m< mXInp;m++){
                    int iLine = 1;
                    for (int n=0;n< mYInp;n++) {
                        double valWind = wind[iXComp]/this.facField2DXY;
                        if (n==iLine*nDataFirstLine) {
                            outputFileBufferedWriter.newLine();
                            iLine++;
                        }
                        outputFileBufferedWriter.write(String.format(locale, windValueFormat, valWind));
                        iXComp++;
                    }
                    outputFileBufferedWriter.newLine();
                }
                wind = field2dXYValuesPerTimeLevel.yComp;
                int iYComp = 0;
                for (int m=0;m< mXInp;m++){
                    int iLine = 1;
                    for (int n=0;n< mYInp;n++) {
                        double valWind = wind[iYComp]/this.facField2DXY;
                        if (n==iLine*nDataFirstLine) {
                            outputFileBufferedWriter.newLine();
                            iLine++;
                        }
                        outputFileBufferedWriter.write(String.format(locale, windValueFormat, valWind));
                        iYComp++;
                    }
                    outputFileBufferedWriter.newLine();
                }
            } else {
                double[] wind = field2dXYValuesPerTimeLevel.xComp;
                int iXComp = 0;
                for (int n=0;n< mYInp;n++) {
                    int iLine = 1;
                    for (int m=0;m< mXInp;m++){
                        double valWind = wind[iXComp]/this.facField2DXY;
                        if (m==iLine*nDataFirstLine) {
                            outputFileBufferedWriter.newLine();
                            iLine++;
                        }
                        outputFileBufferedWriter.write(String.format(locale, windValueFormat, valWind));
                        iXComp++;
                    }
                    outputFileBufferedWriter.newLine();
                }
                wind = field2dXYValuesPerTimeLevel.yComp;
                int iYComp = 0;
                for (int n=0;n< mYInp;n++) {
                    int iLine = 1;
                    for (int m=0;m< mXInp;m++){
                        double valWind = wind[iYComp]/this.facField2DXY;
                        if (m==iLine*nDataFirstLine) {
                            outputFileBufferedWriter.newLine();
                            iLine++;
                        }
                        outputFileBufferedWriter.write(String.format(locale, windValueFormat, valWind));
                        iYComp++;
                    }
                    outputFileBufferedWriter.newLine();
                }
            }

            outputFileBufferedWriter.close();

            // move temp.WND to the actual restart file:
            BBUtils.copyFile(tempFile,this.field2dXYFile);
            tempFile.deleteOnExit();

        } catch (IOException e) {
            throw new RuntimeException("Could not write to " + this.field2dXYFile.getAbsolutePath());
        }
    }

    private void readField2DXYFile(int tIndexField2DXY) {
        this.field2dXYFile = new File(field2dXYSeriesFile.getParent(),listWindFiles.get(tIndexField2DXY));
        double[] xComp = new double[mnMax];
        double[] yComp = new double[mnMax];
        try {
            int iXComp = 0;
            int iYComp = 0;
            String[] lineFields;
            BufferedReader windFileBufferedReader = new BufferedReader(new FileReader(this.field2dXYFile));
            String line = windFileBufferedReader.readLine();
            boolean firstLine = true;
            int nData = 0;
            while (line != null) {
                if (nData<mnMax) {
                    lineFields = line.trim().split("[ \t]+");
                    for (String lineField : lineFields) {
                        xComp[iXComp] = this.facField2DXY * Double.parseDouble(lineField);
                        iXComp++;
                        nData++;
                    }
                } else {
                    lineFields = line.trim().split("[ \t]+");
                    for (String lineField : lineFields) {
                        yComp[iYComp] = this.facField2DXY * Double.parseDouble(lineField);
                        iYComp++;
                        nData++;
                    }
                }
                if (firstLine) {
                    nDataFirstLine = lineFields.length;
                    firstLine = false;
                }
                line = windFileBufferedReader.readLine();
            }
            if (iXComp<mnMax){
                throw new RuntimeException("Number of X-component of grid data is not complete: " + this.field2dXYFile.getAbsolutePath());
            } else if (iXComp>mnMax) {
                throw new RuntimeException("Number of X-component of grid data is larger than grid points: " + this.field2dXYFile.getAbsolutePath());
            }
            if (iYComp<mnMax){
                throw new RuntimeException("Number of Y-component of grid data is not complete: " + this.field2dXYFile.getAbsolutePath());
            } else if (iYComp>mnMax) {
                throw new RuntimeException("Number of Y-component of grid data is larger than grid points: " + this.field2dXYFile.getAbsolutePath());
            }
            if (nData<2*mnMax){
                throw new RuntimeException("Number of grid data is not complete: " + this.field2dXYFile.getAbsolutePath());
            } else if (nData>2*mnMax) {
                throw new RuntimeException("Number of grid data is larger than grid points: " + this.field2dXYFile.getAbsolutePath());
            }
            windFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read swan wind file " + this.field2dXYFile.getAbsolutePath());
        }
        field2dXYSeriesVal = new Field2dXYValuesPerTimeLevel(tIndexField2DXY,xComp,yComp);
    }

    private void readListField2DXYSeries() {
        try {
            BufferedReader windSeriesFileBufferedReader = new BufferedReader(new FileReader(this.field2dXYSeriesFile));
            String line = windSeriesFileBufferedReader.readLine();
            while (line != null) {
                listWindFiles.add(line);
                line = windSeriesFileBufferedReader.readLine();
            }
            windSeriesFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read swan wind file " + this.field2dXYSeriesFile.getAbsolutePath());
        }

        if (listWindFiles.size()<this.nTimeLevelField2DXY){
            throw new RuntimeException("Number of wind files *.WND is less than the specified wind time levels.");
        }

    }

    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);
    }

    private class Field2dXYValuesPerTimeLevel {
        public double[] xComp;
        public double[] yComp;
        public int intTime;

        public Field2dXYValuesPerTimeLevel(int intTime, double[] xComp, double[] yComp) {
            this.intTime = intTime;
            this.xComp = new double[xComp.length];
            this.yComp = new double[yComp.length];
            System.arraycopy(xComp,0,this.xComp,0,xComp.length);
            System.arraycopy(yComp,0,this.yComp,0,xComp.length);
        }
    }

    private void initialize(File workingDir, String fileName, String[] arguments) {

        if (arguments.length != 2) {
            throw new RuntimeException("Expected two arguments: 1. SWAN input *.swn file name, 2. the 2D-field-XY type: " +
                    knownFileTypes[0]+", "+knownFileTypes[1]);
        }

        File swnFile = new File(workingDir,arguments[0]);
        if (!(swnFile.exists())) {
            throw new RuntimeException("Input file does not exist: " + swnFile.getAbsolutePath());
        }
        SwanParameters swanParams = new SwanParameters(swnFile);
        String tStart1 = null;
        String dtField2DXY = null;
        String dtUnitField2DXY = null;
        String tStop1 = null;
        if (arguments[1].toUpperCase().contentEquals(knownFileTypes[0])) {
            mXInp = swanParams.getMXInpWind();
            mYInp = swanParams.getMYInpWind();
            mnMax = mXInp * mYInp;
            tStart1 = swanParams.getTStartWind();
            dtField2DXY = swanParams.getDtWind();
            dtUnitField2DXY = swanParams.getDtUnitWind();
            tStop1 = swanParams.getTStopWind();
            facField2DXY = swanParams.getFacWind();
            idlaField2DXY = swanParams.getIDLAWind();
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
            double dt = Double.parseDouble(dtField2DXY);
            double dtWindmSec;
            if (dtUnitField2DXY.equalsIgnoreCase("SEC")) {
                dtWindmSec = dt*1000;
            } else if (dtUnitField2DXY.equalsIgnoreCase("MIN")) {
                dtWindmSec = dt*60*1000;
            } else if (dtUnitField2DXY.equalsIgnoreCase("HR")) {
                dtWindmSec = dt*60*60*1000;
            } else if (dtUnitField2DXY.equalsIgnoreCase("DAY")) {
                dtWindmSec = dt*24*60*60*1000;
            } else {
                throw new RuntimeException("Unit of time interval is not available in *.swn.");
            }
            assert tStop != null;
            this.nTimeLevelField2DXY = (int) ((tStop.getTime()-tStart.getTime())/ dtWindmSec +1);

            if (!swanParams.getFileNameWind().contentEquals(fileName)){
                throw new RuntimeException("Wind file name in swn file is not equal with the one specified in xml file.");
            }
            this.field2dXYSeriesFile = new File(swnFile.getParent(),swanParams.getFileNameWind());
            readListField2DXYSeries();

            exchangeItems = new SwanField2DXYFileExchangeItem_bak[3];
            exchangeItems[0] = new SwanField2DXYFileExchangeItem_bak(swanField2DXYFileId[0],this);
            exchangeItems[1] = new SwanField2DXYFileExchangeItem_bak(swanField2DXYFileId[1],this);
            exchangeItems[2] = new SwanField2DXYFileExchangeItem_bak(swanField2DXYFileId[2],this);

        } else if (arguments[1].toUpperCase().contentEquals(knownFileTypes[1])) {
            throw new RuntimeException("Field2DXY file type "+arguments[1]+" is not yet implemented");
        } else {
            throw new RuntimeException("Field2DXY file is not of known types: "+arguments[1]+". Known types are: " +
                    knownFileTypes[0]+", "+knownFileTypes[1]);
        }
    }

    public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId(),exchangeItems[1].getId(),exchangeItems[2].getId()};
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int indexExchangeItem;
		if (exchangeItemID.equals(swanField2DXYFileId[0])) {
			indexExchangeItem = 0;
		} else if (exchangeItemID.equals(swanField2DXYFileId[1])) {
			indexExchangeItem = 1;
		} else if (exchangeItemID.equals(swanField2DXYFileId[2])) {
			indexExchangeItem = 2;
		} else {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		}
		return exchangeItems[indexExchangeItem];
	}

    public void finish() {
        // no action since writefile is executed everytime setX/Y is called.
    }

}
