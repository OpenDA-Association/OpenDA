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

import java.io.*;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Class containing the lines of a swan parameter file
 */
public class SwanParameters {

    private static final String gridString = "CGRID";
    private static final String waterLevelGridString = "INPGRID WLEV";
    private static final String[] waterLevelReadString = new String[] {"READINP WLEV","READ WLEV"};
    private static final String windGridString = "INPGRID WIND";
    private static final String windReadString = "READINP WIND";
    private static final String simulationTime = "COMPUTE";

    public static final String[] groupKeys = new String[]{
            "GEN3", "WCAP", "WCAP1", "QUAD", "FRIC", "BREA", "BREA1", "TRIAD", "NUMREFRL", "NUM"
    };

    private ArrayList<String> lines = new ArrayList<String>();
    private int mMax = Integer.MIN_VALUE;
    private int nMax = Integer.MIN_VALUE;
    private int nmMax = Integer.MIN_VALUE;
    private int rFreq = Integer.MIN_VALUE;
    private int cDir = Integer.MIN_VALUE;
    private int mXInpWind = Integer.MIN_VALUE;
    private int mYInpWind = Integer.MIN_VALUE;
    private double xInpWind = Double.MIN_VALUE;
    private double yInpWind = Double.MIN_VALUE;
    private double angleWind = Double.MIN_VALUE;
    private double dXWind = Double.MIN_VALUE;
    private double dYWind = Double.MIN_VALUE;

    private String tStartWind = null;
    private String dtWind = null;
    private String dtUnitWind = null;
    private String tStopWind = null;
    private double facWind = Double.MIN_VALUE;
    private int idlaWind = Integer.MIN_VALUE;
    private int nhedfWind = Integer.MIN_VALUE;
    private int nhedtWind = Integer.MIN_VALUE;
    private int nhedvecWind = Integer.MIN_VALUE;
    private String formatWind = null;
    private String finameWind = null;
    private boolean isWindSeries = false;
    private String finameOpenBoundary = null;
    private String typeOpenBoundary = null;
    private String gridType = null;
    private String nodeFileName = null;
    private File swanInputFile;
    private String tStartSimulation;
    private String dtSimulation;
    private String dtUnitSimulation;
    private String tStopSimulation;
    private int mxInpWLevel = Integer.MIN_VALUE;
    private int myInpWLevel = Integer.MIN_VALUE;
    private String tStartWLevel = null;
    private String dtWLevel = null;
    private String dtUnitWLevel = null;
    private String tStopWLevel = null;
    private double factorWLevel = Double.MIN_VALUE;
    private double exceptionWLevel = Double.MIN_VALUE;
    private String formatWLevel = null;
    private String finameWLevel = null;
    private int idlaWLevel = Integer.MIN_VALUE;
    private double exceptionWind = Double.MIN_VALUE;

    public SwanParameters(File swanInputTemplateFile) {
        try {
            BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(swanInputTemplateFile));
            String line = inputFileBufferedReader.readLine();
            while (line != null) {
                lines.add(line);
                line = inputFileBufferedReader.readLine();
            }
            inputFileBufferedReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not read swan input file " + swanInputTemplateFile.getAbsolutePath());
        }
        this.swanInputFile = swanInputTemplateFile;
    }

    private int getMMax() {
        if (mMax == Integer.MIN_VALUE) {
            determineSizes();
        }
        return mMax;
    }

    private int getNMax() {
        if (nMax == Integer.MIN_VALUE) {
            determineSizes();
        }
        return nMax;
    }

    public int getNMMax() {
        if (nmMax == Integer.MIN_VALUE) {
            determineSizes();
        }
        return nmMax;
    }

    public int getRFreq() {
        if (rFreq == Integer.MIN_VALUE) {
            determineSizes();
        }
        return rFreq;
    }

    public int getCDir() {
        if (cDir == Integer.MIN_VALUE) {
            determineSizes();
        }
        return cDir;
    }

    public String getGridType() {
        if (gridType == null) {
            determineSizes();
        }
        return gridType;
    }

    public String getNodeFileName() {
        if (nodeFileName == null) {
            determineSizes();
        }
        return nodeFileName;
    }

    public double getXInpWind() {
        if (this.xInpWind == Double.MIN_VALUE) {
            determineSizesWind();
        }
        return xInpWind;
    }

    public double getYInpWind() {
        if (this.yInpWind == Double.MIN_VALUE) {
            determineSizesWind();
        }
        return yInpWind;
    }

    public double getDXInpWind() {
        if (this.dXWind == Double.MIN_VALUE) {
            determineSizesWind();
        }
        return dXWind;
    }

    public double getDYInpWind() {
        if (this.dYWind == Double.MIN_VALUE) {
            determineSizesWind();
        }
        return dYWind;
    }

    public double getAngleInpWind() {
        if (this.angleWind == Double.MIN_VALUE) {
            determineSizesWind();
        }
        return angleWind;
    }

    public int getMXInpWind() {
        if (mXInpWind == Integer.MIN_VALUE) {
            determineSizesWind();
        }
        return mXInpWind;
    }

    public int getMYInpWind() {
        if (mYInpWind == Integer.MIN_VALUE) {
            determineSizesWind();
        }
        return mYInpWind;
    }

    public String getTStartWind() {
        if (tStartWind == null) {
            determineSizesWind();
        }
        return tStartWind;
    }

    public String getDtWind() {
        if (dtWind == null) {
            determineSizesWind();
        }
        return dtWind;
    }

    public String getDtUnitWind() {
        if (dtUnitWind == null) {
            determineSizesWind();
        }
        return dtUnitWind;
    }

    public String getTStopWind() {
        if (tStopWind == null) {
            determineSizesWind();
        }
        return tStopWind;
    }

    public Double getExceptionWind() {
        if (exceptionWind == Double.MIN_VALUE) {
            determineSizesWind();
        }
        return exceptionWind;
    }

    public String getTStartSimulation() {
        if (tStartSimulation == null) {
            determineSimulationTime();
        }
        return tStartSimulation;
    }

    public String getDtSimulation() {
        if (dtSimulation == null) {
            determineSimulationTime();
        }
        return dtSimulation;
    }

    public String getDtUnitSimulation() {
        if (dtUnitSimulation == null) {
            determineSimulationTime();
        }
        return dtUnitSimulation;
    }

    public String getTStopSimulation() {
        if (tStopSimulation == null) {
            determineSimulationTime();
        }
        return tStopSimulation;
    }

    public double getFacWind() {
        if (facWind == Double.MIN_VALUE) {
            determineSizesReadWind();
        }
        return facWind;
    }

    public int getIDLAWind() {
        if (idlaWind == Integer.MIN_VALUE) {
            determineSizesReadWind();
        }
        return idlaWind;
    }

    public int getNHEDFWind() {
        if (nhedfWind == Integer.MIN_VALUE) {
            determineSizesReadWind();
        }
        return nhedfWind;
    }

    public int getNHEDTWind() {
        if (nhedtWind == Integer.MIN_VALUE) {
            determineSizesReadWind();
        }
        return nhedtWind;
    }

    public int getNHEDVECWind() {
        if (nhedvecWind == Integer.MIN_VALUE) {
            determineSizesReadWind();
        }
        return nhedvecWind;
    }

    public String getFormatWind() {
        if (formatWind == null) {
            determineSizesReadWind();
        }
        return formatWind;
    }

    public String getFileNameWind() {
        if (finameWind == null) {
            determineSizesReadWind();
        }
        return finameWind;
    }

    public boolean isWindSeries() {
        if (finameWind == null) {
            determineSizesReadWind();
        }
        return isWindSeries;
    }

    public int getMXInpWLevel() {
        if (mxInpWLevel == Integer.MIN_VALUE) {
            determineSizesInputGrid2D("WLEV");
        }
        return mxInpWLevel;
    }

    public int getMYInpWLevel() {
        if (myInpWLevel == Integer.MIN_VALUE) {
            determineSizesInputGrid2D("WLEV");
        }
        return myInpWLevel;
    }

    public String getTStartWLevel() {
        if (tStartWLevel == null) {
            determineSizesInputGrid2D("WLEV");
        }
        return tStartWLevel;
    }

    public String getDtWLevel() {
        if (dtWLevel == null) {
            determineSizesInputGrid2D("WLEV");
        }
        return dtWLevel;
    }

    public String getDtUnitWLevel() {
        if (dtUnitWLevel == null) {
            determineSizesInputGrid2D("WLEV");
        }
        return dtUnitWLevel;
    }

    public String getTStopWLevel() {
        if (tStopWLevel == null) {
            determineSizesInputGrid2D("WLEV");
        }
        return tStopWLevel;
    }

    public Double getExceptionWLevel() {
        if (exceptionWLevel == Double.MIN_VALUE) {
            determineSizesInputGrid2D("WLEV");
        }
        return exceptionWLevel;
    }

    public Double getFactorWLevel() {
        if (factorWLevel == Double.MIN_VALUE) {
            determineSizesReadInputGrid2D("WLEV");
        }
        return factorWLevel;
    }

    public String getFileNameWLevel() {
        if (finameWLevel == null) {
            determineSizesReadInputGrid2D("WLEV");
        }
        return finameWLevel;
    }

    public int getIDLAWLevel() {
        if (idlaWLevel == Integer.MIN_VALUE) {
            determineSizesReadInputGrid2D("WLEV");
        }
        return idlaWLevel;
    }

    public String getFileNameOpenBoundary() {
        if (finameOpenBoundary == null) {
            determineOpenBoundaryParameter();
        }
        return finameOpenBoundary;
    }

    public String getTypeOpenBoundary() {
        if (typeOpenBoundary == null) {
            determineOpenBoundaryParameter();
        }
        return typeOpenBoundary;
    }

    public void setParameterValue(String parameterKeyword, Object parameterValue, boolean keywordMaybeAbsent) {
        Locale locale = new Locale("EN");
        String parameterValueAsString;
        if (parameterValue instanceof Double) {
            parameterValueAsString = String.format(locale, "%9g", (Double)parameterValue).trim();
        } else if (parameterValue instanceof Integer) {
            parameterValueAsString = String.format(locale, "%d", (Integer)parameterValue).trim();
        } else if (parameterValue instanceof String) {
            parameterValueAsString = ((String)parameterValue).trim();
        } else {
            throw new RuntimeException("Unknown value type for parameter " + parameterKeyword);
        }
        substituteParameterValueString(parameterKeyword, parameterValueAsString, keywordMaybeAbsent);
    }

    private void substituteParameterValueString(String parameterKeyword, String parameterValue, boolean keywordMaybeAbsent) {
        parameterKeyword = composeKeyString(parameterKeyword);
        for (int i = 0; i < lines.size() ; i++) {
            String line = lines.get(i);
            int startOfParameterKeyword = line.indexOf(parameterKeyword);
            if ( startOfParameterKeyword >= 0 ) {
                int startOfRestOfLine = startOfParameterKeyword + parameterKeyword.length();
                String newLine = line.substring(0, startOfParameterKeyword) +
                        parameterValue + line.substring(startOfRestOfLine);
                lines.set(i, newLine);
                return;
            }
        }
        if ( ! keywordMaybeAbsent ) {
            if (! keywordMayHaveBeenSetSpecificForNestedRun(parameterKeyword)) {
                throw new RuntimeException("Could not find parameter keyword " + parameterKeyword);
            }
        }
    }

    private boolean keywordMayHaveBeenSetSpecificForNestedRun(String parameterKeyword) {
        return parameterKeyword.contains("cfjon") ||
                parameterKeyword.contains("dabs") ||
                parameterKeyword.contains("drel") ||
                parameterKeyword.contains("curvat") ||
                parameterKeyword.contains("npnts") ||
                parameterKeyword.contains("mxitst");
    }

    public void write(File parameterFile) throws IOException {
        BufferedWriter outputFileBufferedWriter = new BufferedWriter(new FileWriter(parameterFile));
        for (String line : lines) {
            outputFileBufferedWriter.write(line);
            outputFileBufferedWriter.newLine();
        }
        outputFileBufferedWriter.close();
    }

    public void setGroupActive(String groupKey, boolean groupActive) {
        String groupOnString = composeKeyString(groupKey+"ON");
        String groupOffString = composeKeyString(groupKey+"OFF");
        ArrayList<String> adjustedLines = new ArrayList<String>();
        for (String line : lines) {
            if (line.contains(groupOnString)) {
                if (groupActive) {
                    adjustedLines.add(line.replace(groupOnString, ""));
                }
            } else if (line.contains(groupOffString)) {
                if (!groupActive) {
                    adjustedLines.add(line.replace(groupOffString, ""));
                }
            } else {
                adjustedLines.add(line);
            }
        }
        lines = adjustedLines;
    }

    private String composeKeyString(String keyword) {
        return "<#" + keyword + "#>";
    }

    private void determineSizes() {
        // TO DO: so far, this is only for REGULAR grid. It's not applicable yet for CURVILINEAR and UNSTRUCTURED grid.
        String gridStringLine = null;
        String readUnstruc = null;
        for (int i=0;i<lines.size();i++) {
            String line = lines.get(i);
            if (line.toUpperCase().contains(gridString)) {
                gridStringLine = line;
                if (gridStringLine.toUpperCase().contains("REG")) {
                    this.gridType = "REGULAR";
                    break;
                } else if (gridStringLine.toUpperCase().contains("UNSTRUC")) {
                    this.gridType = "UNSTRUCTURED";
                    readUnstruc = lines.get(i+1);
                    if (readUnstruc.toUpperCase().contains("TRIANGLE")){
                        break;
                    } else {
                        //TO DO: other formats besides TRIANGLE, i.e. ADCirc and EASYmesh
                        throw new RuntimeException("Only TRIANGLE is supported for UNSTRUCTURED grid.");
                    }                    
                } else {
                    throw new RuntimeException("Only REGULAR and UNSTRUCTURED grid types are accomodated. Put keyword REGULAR or UNSTRUCTURED explicitly on *swn file.");
                }                
            }
        }
        if (gridStringLine == null) {
            throw new RuntimeException("String cgrid not found");
        }
        if (this.gridType.contentEquals("REGULAR")) {
            int beginIndexDataRegular = 2;
            String lineFields[] = gridStringLine.trim().split("[ \t]+");
            mMax = Integer.parseInt(lineFields[beginIndexDataRegular+5])+1;
            nMax = Integer.parseInt(lineFields[beginIndexDataRegular+6])+1;
            this.nmMax = mMax*nMax;
            if (lineFields[beginIndexDataRegular+7].equalsIgnoreCase("SECTOR")) {
    //            double dir1 = Double.parseDouble(lineFields[beginIndexDataRegular+8]);
    //            double dir2 = Double.parseDouble(lineFields[beginIndexDataRegular+9]);
                int mdc = Integer.parseInt(lineFields[beginIndexDataRegular+10]);
                cDir = mdc+1;
                // TO DO: On page 31 in SWAN Manual, it says that user can specify either the combination of [flow]+[msc],
                // [fhigh]+[msc], and [flow]+[figh]. How to indicate if it is combination-1 or -2 or -3??
                if (lineFields.length==16) { // if user specifies everything
                    int msc = Integer.parseInt(lineFields[beginIndexDataRegular+13]);
                    rFreq = msc+1;
                } else { // if user specifies only [flow]+[fhigh]:
                    double flow = Double.parseDouble(lineFields[beginIndexDataRegular+11]);
                    double fhigh = Double.parseDouble(lineFields[beginIndexDataRegular+12]);
                    double mscD = Math.log(fhigh/flow)/Math.log(1.1);
                    int msc = (int) Math.round(mscD);
                    rFreq = msc+1;
                }
            } else if (lineFields[beginIndexDataRegular+7].equalsIgnoreCase("CIRCLE")) {
                int mdc;
                mdc = Integer.parseInt(lineFields[beginIndexDataRegular+8]);
                cDir = mdc;
                // TO DO: On page 31 in SWAN Manual, it says that user can specify either the combination of [flow]+[msc],
                // [fhigh]+[msc], and [flow]+[figh]. How to indicate if it is combination-1 or -2 or -3??
                if (lineFields.length==14) { // if user specifies everything
                    int msc = Integer.parseInt(lineFields[beginIndexDataRegular+11]);
                    rFreq = msc+1;
                } else { // if user specifies only [flow]+[fhigh]:
                    double flow = Double.parseDouble(lineFields[beginIndexDataRegular+9]);
                    double fhigh = Double.parseDouble(lineFields[beginIndexDataRegular+10]);
                    double mscD = Math.log(fhigh/flow)/Math.log(1.1);
                    int msc = (int) Math.round(mscD);
                    rFreq = msc+1;
                }
            } else {
                throw new RuntimeException("Both keywords SECTOR and CIRCLE are not found in *.swn");
            }
        } else if (this.gridType.contentEquals("UNSTRUCTURED")) {
//            mMax = 0;
//            nMax = 0;
            int beginIndexDataUnstruc = 1;
            String lineFields[] = gridStringLine.trim().split("[ \t]+");
            if (lineFields[beginIndexDataUnstruc +1].equalsIgnoreCase("SECTOR")) {
    //            double dir1 = Double.parseDouble(lineFields[beginIndexDataUnstruc+1]);
    //            double dir2 = Double.parseDouble(lineFields[beginIndexDataUnstruc+2]);
                int mdc = Integer.parseInt(lineFields[beginIndexDataUnstruc +3]);
                cDir = mdc+1;
                // TO DO: On page 31 in SWAN Manual, it says that user can specify either the combination of [flow]+[msc],
                // [fhigh]+[msc], and [flow]+[figh]. How to indicate if it is combination-1 or -2 or -3??
                if (lineFields.length==9) { // if user specifies everything
                    int msc = Integer.parseInt(lineFields[beginIndexDataUnstruc +7]);
                    rFreq = msc+1;
                } else { // if user specifies only [flow]+[fhigh]:
                    double flow = Double.parseDouble(lineFields[beginIndexDataUnstruc +5]);
                    double fhigh = Double.parseDouble(lineFields[beginIndexDataUnstruc +6]);
                    double mscD = Math.log(fhigh/flow)/Math.log(1.1);
                    int msc = (int) Math.round(mscD);
                    rFreq = msc+1;
                }
            } else if (lineFields[beginIndexDataUnstruc +1].equalsIgnoreCase("CIRCLE")) {
                int mdc;
                mdc = Integer.parseInt(lineFields[beginIndexDataUnstruc+2]);
                cDir = mdc;
                // TO DO: On page 31 in SWAN Manual, it says that user can specify either the combination of [flow]+[msc],
                // [fhigh]+[msc], and [flow]+[figh]. How to indicate if it is combination-1 or -2 or -3??
                if (lineFields.length==7) { // if user specifies everything
                    int msc = Integer.parseInt(lineFields[beginIndexDataUnstruc+5]);
                    rFreq = msc+1;
                } else { // if user specifies only [flow]+[fhigh]:
                    double flow = Double.parseDouble(lineFields[beginIndexDataUnstruc +3]);
                    double fhigh = Double.parseDouble(lineFields[beginIndexDataUnstruc +4]);
                    double mscD = Math.log(fhigh/flow)/Math.log(1.1);
                    int msc = (int) Math.round(mscD);
                    rFreq = msc+1;
                }
            } else {
                throw new RuntimeException("Both keywords SECTOR and CIRCLE are not found in *.swn");
            }

            lineFields = readUnstruc.trim().split("[ \t]+");
            nodeFileName = lineFields[3].substring(1,lineFields[3].length()-1)+".node";
            // get number of nodes (nmMax) for unstructured grid (TRIANGLE):
            File unstructGridNodeFile = new File(this.swanInputFile.getParent(),nodeFileName);
            FileReader fileReader = null;
            try {
                fileReader = new FileReader(unstructGridNodeFile);
                BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);
                String line = inputFileBufferedReader.readLine();
                if (line!=null) {
                    String[] fields = line.trim().split("[ /t]+");
                    this. nmMax = Integer.parseInt(fields[0]);
                    inputFileBufferedReader.close();
                    fileReader.close();
                }
            } catch (IOException e) {
                throw new RuntimeException("Could not read from " + unstructGridNodeFile.getAbsolutePath());
            }
        }
    }

    private void determineSizesInputGrid2D(String keyString) {
        String gridStringLine = null;
        int indPlusException = 0;
        if (waterLevelGridString.contains(keyString)) {
            for (String line : lines) {
                if (line.toUpperCase().contains(waterLevelGridString)) {
                    gridStringLine = line;
                    break;
                }
            }
            if (gridStringLine == null) {
                throw new RuntimeException("String INPGRID WLEVEL not found in *.swn");
            }
            if (!gridStringLine.toUpperCase().contains("NONSTAT")) {
                throw new RuntimeException("Water level data should be nonstationary. Keyword NONSTAT is not found in *.swn");
            }
            if (gridStringLine.contains("REG")) {
                String lineFields[] = gridStringLine.trim().split("[ \t]+");
                if (lineFields.length<15){
                    throw new RuntimeException("Incomplete INPGRID WLEVEL parameters in *.swn: please set all parameters explicitly.");
                }
                if (gridStringLine.toUpperCase().contains("EXC")) {
                    indPlusException = 2;
                    exceptionWLevel = Double.parseDouble(lineFields[11]);
                }
                mxInpWLevel = Integer.parseInt(lineFields[6])+1;
                myInpWLevel = Integer.parseInt(lineFields[7])+1;
                tStartWLevel = lineFields[11+indPlusException];
                dtWLevel = lineFields[12+indPlusException];
                dtUnitWLevel = lineFields[13+indPlusException];
                tStopWLevel = lineFields[14+indPlusException];
            } else if (gridStringLine.contains("CURV")) {
                //TO DO: implement for CURVILINEAR grid
                throw new RuntimeException("Reading water level file with CURVilinear grid is not yet implemented.");
            } else {
                throw new RuntimeException("String REG or CURV not found for waterlevel INPgrid WLEVel.");
            }
        } else if (windGridString.contains(keyString)) {
            for (String line : lines) {
                if (line.toUpperCase().contains(windGridString)) {
                    gridStringLine = line;
                    break;
                }
            }
            if (gridStringLine == null) {
                throw new RuntimeException("String INPGRID WIND not found in *.swn");
            }
//            if (gridStringLine.contains("EXC")) {
//                //TO DO: implement also for the case of using EXCeption
//                throw new RuntimeException("Reading wind file with EXCeption is not yet implemented.");
//            }
            if (!gridStringLine.toUpperCase().contains("NONSTAT")) {
                throw new RuntimeException("Wind data should be nonstationary. Keyword NONSTAT is not found in *.swn");
            }
            if (gridStringLine.contains("REG")) {
                String lineFields[] = gridStringLine.trim().split("[ \t]+");
                if (lineFields.length<15){
                    throw new RuntimeException("Incomplete INPGRID WIND parameters in *.swn: please set all parameters explicitly.");
                }
                if (gridStringLine.toUpperCase().contains("EXC")) {
                    indPlusException = 2;
                    exceptionWind = Double.parseDouble(lineFields[11]);
                }
                mXInpWind = Integer.parseInt(lineFields[6])+1;
                mYInpWind = Integer.parseInt(lineFields[7])+1;
                tStartWind = lineFields[11+indPlusException];
                dtWind = lineFields[12+indPlusException];
                dtUnitWind = lineFields[13+indPlusException];
                tStopWind = lineFields[14+indPlusException];
            } else if (gridStringLine.contains("CURV")) {
                //TO DO: implement for CURVILINEAR grid
                throw new RuntimeException("Reading wind file with CURVilinear grid is not yet implemented.");
            } else {
                throw new RuntimeException("String REG or CURV not found for wind INPgrid WInd.");
            }
        }
    }

    private void determineSizesReadInputGrid2D(String keyString) {
        String gridStringLine = null;
        if (waterLevelReadString[0].contains(keyString) | waterLevelReadString[1].contains(keyString)) {
            for (String line : lines) {
                if (line.toUpperCase().contains(waterLevelReadString[0]) | line.toUpperCase().contains(waterLevelReadString[1])) {
                    gridStringLine = line;
                    break;
                }
            }
            if (gridStringLine == null) {
                throw new RuntimeException("String READINP WLEVel not found in *.swn.");
            }
            if (!gridStringLine.toUpperCase().contains("SERI")){
                throw new RuntimeException("Water level should be available in SERIES format. String SERIES for WLEVEL not found in *.swn");
            }
            String lineFields[] = gridStringLine.trim().split("[ \t]+");
            factorWLevel = Double.parseDouble(lineFields[2]);
            finameWLevel = lineFields[4];
            finameWLevel = finameWLevel.substring(1,finameWLevel.length()-1);
            idlaWLevel = Integer.parseInt(lineFields[5]);

        } else if (windReadString.contains(keyString)) {
            for (String line : lines) {
                if (line.toUpperCase().contains(windReadString)) {
                    gridStringLine = line;
                    break;
                }
            }
            if (gridStringLine == null) {
                throw new RuntimeException("String READINP WIND not found in *.swn");
            }
            isWindSeries = gridStringLine.contains("SERI");
            String lineFields[] = gridStringLine.trim().split("[ \t]+");
            if (isWindSeries) {
                // For the case of wind data is available in separated files without headers,
                // we support only the case where idla is specified without name in swn file, but the other header parameters
                // are default (=0). Moreover, the wind format = FREE.
                facWind = Double.parseDouble(lineFields[2]);
                finameWind = lineFields[4];
                finameWind = finameWind.substring(1,finameWind.length()-1);
                idlaWind = Integer.parseInt(lineFields[5]);
                nhedfWind = 0;
                nhedtWind = 0;
                nhedvecWind = 0;
                formatWind = "FREE";
            } else {
                // For the case of wind data is available at one file, we support only the case where all header parameters
                // are specified explicitly without names.
                facWind = Double.parseDouble(lineFields[2]);
                idlaWind = Integer.parseInt(lineFields[4]);
                nhedfWind = Integer.parseInt(lineFields[5]);
                nhedtWind = Integer.parseInt(lineFields[6]);
                nhedvecWind = Integer.parseInt(lineFields[7]);
                formatWind = lineFields[8];
                finameWind = lineFields[3];
                finameWind = finameWind.substring(1,finameWind.length()-1);
            }
        }
    }
    
    /**
     * Parse wind input lines
     * Only regulare grids are implemented!
     * [INPgrid] [WInd|WX|WY] REGular [xpinp] [ypinp] [alpinp] [mxinp] [myinp] [dxinp] [dyinp]
     *    (NONSTATionary [tbeginp] [deltinp] [Sec|Min|HR|DAy] [tendinp]
     * example:
     * INPGRID WIND REG 0. 0. 0 1 1 2000 2000 NONSTAT 20081121.0000 12 HR 20081125.0000 
     */
    private void determineSizesWind() {
        String gridStringLine = null;
        for (String line : lines) {
            if (line.toUpperCase().contains(windGridString)) {
                gridStringLine = line;
                break;
            }
        }
        if (gridStringLine == null) {
            throw new RuntimeException("String INPGRID WIND not found in *.swn");
        }
        if (gridStringLine.contains("EXC")) {
            //TO DO: implement also for the case of using EXCeption
            throw new RuntimeException("Reading wind file with EXCeption is not yet implemented.");
        }
        if (gridStringLine.contains("REG")) {
            String lineFields[] = gridStringLine.trim().split("[ \t]+");
            if (lineFields.length<15){
                throw new RuntimeException("Incomplete INPGRID WIND parameters in *.swn: please set all parameters explicitly.");
            }
            // [INPgrid] [WInd] REGular [xpinp] [ypinp] [alpinp] [mxinp] [myinp] [dxinp] [dyinp]
            xInpWind = Double.parseDouble(lineFields[3]);
            yInpWind = Double.parseDouble(lineFields[4]);
            angleWind = Double.parseDouble(lineFields[5]);
            mXInpWind = Integer.parseInt(lineFields[6])+1;
            mYInpWind = Integer.parseInt(lineFields[7])+1;
            dXWind = Double.parseDouble(lineFields[8]);
            dYWind = Double.parseDouble(lineFields[9]);
            tStartWind = lineFields[11];
            dtWind = lineFields[12];
            dtUnitWind = lineFields[13];
            tStopWind = lineFields[14];
        } else if (gridStringLine.contains("CURV")) {
            //TO DO: implement for CURVILINEAR grid
            throw new RuntimeException("Reading wind file with CURVilinear grid is not yet implemented.");
        } else {
            throw new RuntimeException("String REG or CURV not found for wind INPgrid WInd.");
        }
    }

    /**
     * Parse wind input lines
     * READinp WInd fac [fname1 | SERIES fname2] idla [nhedf] ([nhedt]) ([nhedvec])
     * example:
     * READINP WIND 1.0 'rotating.WND' 1 1 1 1 FREE
     * READINP WIND 1.0 SERIes 'rotating.SER' 3 FREE
     */
    private void determineSizesReadWind() {
        String gridStringLine = null;
        for (String line : lines) {
            if (line.toUpperCase().contains(windReadString)) {
                gridStringLine = line;
                break;
            }
        }
        if (gridStringLine == null) {
            throw new RuntimeException("String READINP WIND not found in *.swn");
        }
        isWindSeries = gridStringLine.contains("SERI");
        String lineFields[] = gridStringLine.trim().split("[ \t]+");
        if (isWindSeries) {
            // For the case of wind data is available in separated files without headers,
            // we support only the case where idla is specified without name in swn file, but the other header parameters
            // are default (=0). Moreover, the wind format = FREE.
            facWind = Double.parseDouble(lineFields[2]);
            finameWind = lineFields[4];
            finameWind = finameWind.substring(1,finameWind.length()-1);
            idlaWind = Integer.parseInt(lineFields[5]);
            nhedfWind = 0;
            nhedtWind = 0;
            nhedvecWind = 0;
            formatWind = "FREE";
        } else {
            // For the case of wind data is available at one file, we support only the case where all header parameters
            // are specified explicitly without names.
            facWind = Double.parseDouble(lineFields[2]);
            idlaWind = Integer.parseInt(lineFields[4]);
            nhedfWind = Integer.parseInt(lineFields[5]);
            nhedtWind = Integer.parseInt(lineFields[6]);
            nhedvecWind = Integer.parseInt(lineFields[7]);
            formatWind = lineFields[8];
            finameWind = lineFields[3];
            finameWind = finameWind.substring(1,finameWind.length()-1);
        }
        
    }

    private void determineOpenBoundaryParameter(){
        // For now we focus only on getting OBD file names
        String boundStringLine = null;
        String boundFileName = null;
        for (String line : lines) {
            if (line.toUpperCase().contains("BOUNDNEST1") || line.toUpperCase().contains("BOUNDSPEC") && line.toUpperCase().contains("SIDE")) {
                boundStringLine = line;
                break;
            }
        }
        if (boundStringLine == null) {
            throw new RuntimeException("String BOUNDSPEC SIDE or BOUNDNEST1 not found in *.swn");
        }
        if (boundStringLine.toUpperCase().contains("SPEC")){
            String lineFields[] = boundStringLine.trim().split("[ \t]+");
            int i=0;
            while (i<lineFields.length) {
                if (lineFields[i].equalsIgnoreCase("FILE")){
                    boundFileName = lineFields[i+1];
                    boundFileName=boundFileName.substring(1,boundFileName.length()-1);
                    break;
                }
                i++;
            }
            finameOpenBoundary = boundFileName;
            typeOpenBoundary = "SHAPE";
        } else if (boundStringLine.toUpperCase().contains("NEST")) {
            String lineFields[] = boundStringLine.trim().split("[ \t]+");
            boundFileName = lineFields[2];
            finameOpenBoundary = boundFileName.substring(1,boundFileName.length()-1);
            typeOpenBoundary = "NEST";
        } else {
            throw new RuntimeException("String SPEC or NEST not found in *.swn");
        }

    }

    private void determineSimulationTime() {
        String gridStringLine = null;
        for (String line : lines) {
            if (line.toUpperCase().contains(simulationTime)) {
                gridStringLine = line;
                break;
            }
        }
        if (gridStringLine == null) {
            throw new RuntimeException("String "+simulationTime+" not found in *.swn");
        }
        if (!gridStringLine.toUpperCase().contains("NONSTAT")) {
            throw new RuntimeException("Keyword NONSTAT not found in *.swn");
        }
        String lineFields[] = gridStringLine.trim().split("[ \t]+");
        if (lineFields.length != 6) {
            throw new RuntimeException("Line COMPUTE in the swn input file is not complete or not written as one line. Specify all necessary keywords and values for nonstationary run in one line.");
        }
        tStartSimulation = lineFields[2];
        dtSimulation = lineFields[3];
        dtUnitSimulation = lineFields[4];
        tStopSimulation = lineFields[5];
    }
}