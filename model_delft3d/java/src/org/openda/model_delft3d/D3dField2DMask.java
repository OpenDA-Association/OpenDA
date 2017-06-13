/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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

package org.openda.model_delft3d;

import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.utils.Vector;
import org.openda.interfaces.IVector;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * M,N point sub mask selector for D3d 2d field files (depth, roughness)
 */
public class D3dField2DMask implements SelectorInterface {

    ArrayList<MnPoint> mnPoints = null;
    private D3dField2D d3dField2D = null;

    public void initialize(File workingDir, String[] arguments) {
        if (arguments.length != 1) {
            throw new IllegalArgumentException("Wrong #arguments (1 expected: mask file or sub-area)");
        }
        File maskFile = new File(workingDir, arguments[0]);
        if (maskFile.exists()) {
            mnPoints = readMnMaskFile(maskFile);
        } else {
            String configString = arguments[0].trim();
            if (!arguments[0].contains(":") || !arguments[0].contains(",")) {
                throw new IllegalArgumentException("Wrong argument (expected: mask file or sub-area): " + arguments[0]);
            }
            else {
                int startCommaPos = configString.indexOf(",");
                int colonPos = configString.indexOf(":");
                if (colonPos < 0) {
                    throw new IllegalArgumentException("D3dField2DMask: Invalid subarea specification: " + configString);
                }
                int endCommaPos = configString.lastIndexOf(",");
                if (endCommaPos < 0) {
                    throw new IllegalArgumentException("D3dField2DMask: Invalid subarea specification: " + configString);
                }
                int mStart = Integer.parseInt(configString.substring(0, startCommaPos).trim());
                int nStart = Integer.parseInt(configString.substring(startCommaPos + 1, colonPos).trim());
                int mEnd = Integer.parseInt(configString.substring(colonPos + 1, endCommaPos).trim());
                int nEnd = Integer.parseInt(configString.substring(endCommaPos + 1).trim());
                mnPoints = new ArrayList<MnPoint>();
                for (int m = mStart; m <= mEnd ; m++) {
                    for (int n = nStart; n <= nEnd ; n++) {
                        mnPoints.add(new MnPoint(m, n));
                    }
                }
            }
        }
    }

    private ArrayList<MnPoint> readMnMaskFile(File maskFile) {
        ArrayList<MnPoint> mnPoints = new ArrayList<MnPoint>();

        try {
            FileReader fileReader = new FileReader(maskFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

            String line = inputFileBufferedReader.readLine();
            while (line != null) {
                String trimmedLine = line.trim();
                if ( !trimmedLine.startsWith("#") && !(trimmedLine.length() == 0) ) {
                    String[] fields = trimmedLine.split("[\t ]*,[\t ]*");
                    if (fields.length != 2) {
                        throw new RuntimeException("Invalid line \n\t\"" + line + "\"\nin mask file " + maskFile.getAbsolutePath());
                    }
                    mnPoints.add(new MnPoint(fields[0], fields[1]));

                }
                line = inputFileBufferedReader.readLine();
            }

            inputFileBufferedReader.close();
            fileReader.close();
        } catch (IOException e) {
            throw new RuntimeException("Error when reading: "  + maskFile.getAbsolutePath() + ": " + e.getMessage());
        }

        return mnPoints;
    }

    public Object select(Object inputObject) {
        if (!(inputObject instanceof D3dField2D)) {
            throw new RuntimeException("D3dField2DMask.select: unexpected object type: " + inputObject.getClass().getName());
        }
        d3dField2D = (D3dField2D) inputObject;
        double[] selectedValues = d3dField2D.getValues(mnPoints);
        return new Vector(selectedValues);
    }

    public Object deselect(Object selection) {
        if (d3dField2D == null) {
            throw new RuntimeException("D3dField2DMask.deselect: select() method not yet called for this selector");
        }
        double[] selectionValues;
        if (selection instanceof IVector) {
            selectionValues = ((IVector)selection).getValues();
        } else if (selection instanceof double[]) {
            selectionValues = (double[]) selection;
        } else {
            throw new RuntimeException("D3dField2DMask.deselect: unexpected object type: " + selection.getClass().getName());
        }
        d3dField2D.setValues(mnPoints, selectionValues);
        return d3dField2D;
    }
}
