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

import static java.lang.Double.NaN;
import static java.lang.Double.valueOf;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Determine the wind dependent parameters
 */
public class SwanWindDepParms {

    ArrayList<InterpolTableRow> tableRows = new ArrayList<InterpolTableRow>();

    public SwanWindDepParms(File interpTableFile) {

        try {
            BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(interpTableFile));
            String line = inputFileBufferedReader.readLine();
            int lineNr=0;
            while (line != null) {
                lineNr++;
                line = line.trim();
                if ( ! line.startsWith("#") && ! (line.length()==0) ) {
                    String fieldSeparators = "[ \t,']+";
                    String[] fields = line.split(fieldSeparators);
                    if ( fields.length != 4 ) {
                        throw new RuntimeException(this.getClass() + ": Unexpected number of fields on line " +
                                lineNr + interpTableFile.getAbsolutePath());
                    }
                    tableRows.add(new InterpolTableRow(fields));
                }
                line = inputFileBufferedReader.readLine();
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException(this.getClass() + ": Could not open interpolation table file " +
                    interpTableFile.getAbsolutePath());
        } catch (IOException e) {
            throw new RuntimeException(this.getClass() + ": Could not read values from interpolation table file " +
                    interpTableFile.getAbsolutePath() + " (error:" + e.getMessage() + ")");
        }
    }

    public double getHS(double actualWindDir, double actualWindVel) {

        return getInterpolatedValue("HS", actualWindDir, actualWindVel);
    }

    public double getTp(double actualWindDir, double actualWindVel) {
        return getInterpolatedValue("Tp", actualWindDir, actualWindVel);
    }

    private double getInterpolatedValue(String variableID, double actualWindDir, double actualWindVel) {
        double dirUpperValue=Math.ceil(actualWindDir/30)*30;
        double dirLowerValue=Math.floor(actualWindDir/30)*30;
        if ( epsilonCompare(dirLowerValue,0) ) {
            dirLowerValue=360;
        }
        double velUpperValue=Math.ceil(actualWindVel/5)*5;
        double velLowerValue=Math.floor(actualWindVel/5)*5;

        int lowDirLowVelRow = -1;
        int upDirLowVelRow = -1;
        int lowDirUpVelRow = -1;
        int upDirUpVelRow = -1;
        int numRows = tableRows.size();
        for ( int rowIndex = 0 ;  rowIndex < numRows ; rowIndex++ ) {
            double rowWindDir = tableRows.get(rowIndex).windDir;
            double rowWindVel = tableRows.get(rowIndex).windVel;
            if ( epsilonCompare(dirLowerValue, rowWindDir) && epsilonCompare(velLowerValue,rowWindVel) ) {
                lowDirLowVelRow = rowIndex;
            }
            if ( epsilonCompare(dirUpperValue,rowWindDir) && epsilonCompare(velLowerValue,rowWindVel) ) {
                upDirLowVelRow = rowIndex;
            }
            if ( epsilonCompare(dirLowerValue,rowWindDir) && epsilonCompare(velUpperValue,rowWindVel) ) {
                lowDirUpVelRow = rowIndex;
            }
            if ( epsilonCompare(dirUpperValue,rowWindDir) && epsilonCompare(velUpperValue,rowWindVel) ) {
                upDirUpVelRow = rowIndex;
            }
        }
        if ( lowDirLowVelRow == -1 || upDirLowVelRow == -1 ||  lowDirUpVelRow == -1 ||  upDirUpVelRow == -1 ) {
            throw new RuntimeException(this.getClass()+ ": Invalid interpolation table");
        }

        double valueAtlowDirUpVel = NaN;
        double valueAtUpDirUpVel = NaN;
        double valueAtLowDirLowVel = NaN;
        double valueAtUpDirLowVel = NaN;
        if (variableID.toUpperCase().equals("HS")) {
            valueAtlowDirUpVel = tableRows.get(lowDirUpVelRow).HS;
            valueAtUpDirUpVel = tableRows.get(upDirUpVelRow).HS;
            valueAtLowDirLowVel = tableRows.get(lowDirLowVelRow).HS;
            valueAtUpDirLowVel = tableRows.get(upDirLowVelRow).HS;
        } else if (variableID.toUpperCase().equals("TP")) {
            valueAtlowDirUpVel = tableRows.get(lowDirUpVelRow).Tp;
            valueAtUpDirUpVel = tableRows.get(upDirUpVelRow).Tp;
            valueAtLowDirLowVel = tableRows.get(lowDirLowVelRow).Tp;
            valueAtUpDirLowVel = tableRows.get(upDirLowVelRow).Tp;
        }

        double dirInterpolatedValueAtVelUp;
        double dirInterpolatedValueAtVelLow;

        if ( lowDirUpVelRow == upDirUpVelRow ) {
            dirInterpolatedValueAtVelUp = valueAtlowDirUpVel;
        } else {
            double factor = (actualWindDir - dirLowerValue) / (dirUpperValue - dirLowerValue);
            dirInterpolatedValueAtVelUp = valueAtlowDirUpVel + factor*(valueAtUpDirUpVel-valueAtlowDirUpVel);
        }

        if ( lowDirLowVelRow == upDirLowVelRow ) {
            dirInterpolatedValueAtVelLow = valueAtLowDirLowVel;
        } else {
            double factor = (actualWindDir - dirLowerValue) / (dirUpperValue - dirLowerValue);
            dirInterpolatedValueAtVelLow = valueAtLowDirLowVel + factor*(valueAtUpDirLowVel-valueAtLowDirLowVel);
        }

        double velInterpolatedValue;
        if ( epsilonCompare(dirInterpolatedValueAtVelLow,dirInterpolatedValueAtVelUp) ) {
            velInterpolatedValue = dirInterpolatedValueAtVelLow;
        } else {
            double factor = (actualWindVel - velLowerValue) / (velUpperValue - velLowerValue);
            velInterpolatedValue = dirInterpolatedValueAtVelLow + factor * (dirInterpolatedValueAtVelUp - dirInterpolatedValueAtVelLow);
        }

        return velInterpolatedValue;
    }

    private boolean epsilonCompare(double d1, double d2) {
        final double epsilon = 1.e-8;
        return (d1 + epsilon > d2) && (d2 + epsilon > d1);
    }

    private class InterpolTableRow {

        public int windDir;
        public int windVel;
        public double HS;
        public double Tp;

        public InterpolTableRow(String[] fields) {
            windDir = Integer.valueOf(fields[0]);
            windVel = Integer.valueOf(fields[1]);
            HS = valueOf(fields[2]);
            Tp = valueOf(fields[3]);
        }
    }
}
