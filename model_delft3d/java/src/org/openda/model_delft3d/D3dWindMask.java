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
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * M,N point sub mask selector for D3d 2d field files (depth, roughness)
 */
public class D3dWindMask implements SelectorInterface {

    ArrayList<MnPoint> mnPoints = null;
    private List<D3dValuesOnGrid2D> d3dField2Ds = null;
	private boolean FlipMaskinXaxis = false;

	/**
	 *
	 * @param workingDir Working directory
	 * @param arguments Configuration arguments
	 *        argument1: required: name of of the mask file
	 *        argument2: optional: flag "yes" or "no" if the indices have to be mirrored
	 *        ni the x-axis.
	 */
	public void initialize(File workingDir, String[] arguments) {
        if (arguments.length != 1 && arguments.length !=2) {
            throw new IllegalArgumentException("Wrong #arguments (required: mask file or sub-area; optionally: mirror flag )"+arguments.length);
        }
        File maskFile = new File(workingDir, arguments[0]);
		if (arguments.length == 2){
			FlipMaskinXaxis = arguments[1].equalsIgnoreCase("yes");
		}

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
		if (!(inputObject instanceof List)) {
			throw new RuntimeException("D3dWindMask.select: unexpected object type: " + inputObject.getClass().getName());
		}
		d3dField2Ds = (List<D3dValuesOnGrid2D>) inputObject;
		int nLevels = d3dField2Ds.size();

			// if the mns coordinates should be flipped, do it now, once!
		    // this routine will always be called if an mns-selection is needed.
		if (FlipMaskinXaxis) {
		    performMirroring(d3dField2Ds.get(0).getGrid().getNmax());
			FlipMaskinXaxis = false;
		}
		double[] selectedValues = new double[mnPoints.size() * nLevels];
		int index = 0;
		for (D3dValuesOnGrid2D onefield2D : d3dField2Ds) {
			double[] timeValues = onefield2D.getValues(mnPoints);
			for (int i = 0; i < timeValues.length; i++) {
				selectedValues[i+index] = timeValues[i];
			}
			index = index + timeValues.length;
		}

		return new Vector(selectedValues);
	}

	private void performMirroring(int nmax){
		for (int i=0; i < mnPoints.size(); i++)   {
		    MnPoint  tmppair = mnPoints.get(i);
			int mirroredVal = nmax + 1 - tmppair.getN();
			mnPoints.set(i,new MnPoint(tmppair.getM(),mirroredVal));
		}
	}

    public Object deselect(Object selection) {
        if (d3dField2Ds == null) {
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
		int index = 0;
		for (D3dValuesOnGrid2D onefield2D : d3dField2Ds) {
			double[] selectionTimeValues = new double[mnPoints.size()];
			for (int i = 0; i < selectionTimeValues.length; i++) {
				selectionTimeValues[i] = selectionValues[i+index];
			}
			onefield2D.setValues(mnPoints,selectionTimeValues);
			index = index + selectionTimeValues.length;
		}
        return d3dField2Ds;
    }
}
