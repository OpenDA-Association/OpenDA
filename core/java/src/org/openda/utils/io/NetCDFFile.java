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
package org.openda.utils.io;

import ucar.ma2.*;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;
import ucar.nc2.write.NetcdfFormatWriter;

import java.io.IOException;
import java.io.File;

/**
 * This is a simple utility class to write variables to NetCDF. The current implementation is extremely simple but can/will be extended in the future.
 *
 * @author nils van Velzen on 25/09/15.
 */

public class NetCDFFile {
	private final File fileName;
	private boolean fileIsNotCreated=true;
    private Dimension nDim;


	//Create and initialize a new NetCDFFile. Nothing happens yet (no files are created)
	public NetCDFFile(File fileName) {
		this.fileName = fileName;
	}


	//Write an array to file. Currently, we only support a single array per file. The time-index specifies the additional axis along we can extend the array in an existing NetCDF file
	public void writeArray(double[] vals, int iTime, String shortName) throws IOException, InvalidRangeException {
        //Check whether we have to define de header and create the file on first write

        int n=vals.length;
		NetcdfFormatWriter.Builder NetcdfBuilder;
		NetcdfFormatWriter NetcdfWriter;
		Variable myVar;

    	if (this.fileIsNotCreated){
			// Create a new file
			NetcdfBuilder = NetcdfFormatWriter.createNewNetcdf3(fileName.getAbsolutePath());

			// Setup Header
			this.nDim  = NetcdfBuilder.addDimension("n", n);
			NetcdfBuilder.addUnlimitedDimension("time");
			NetcdfBuilder.addVariable(shortName, DataType.DOUBLE, "time n");

			// create the file
			NetcdfWriter = NetcdfBuilder.build();
			this.fileIsNotCreated=false;
		}
		else {
			NetcdfBuilder = NetcdfFormatWriter.openExisting(this.fileName.getAbsolutePath());
			NetcdfWriter = NetcdfBuilder.build();
		}
		myVar = NetcdfWriter.findVariable(shortName);

		ArrayDouble.D2 values = new ArrayDouble.D2(1, nDim.getLength());
        // Copy the values
		for (int i=0; i<n; i++){
			values.set(0,i,vals[i]);
		}

		int[] origin = new int[]{iTime, 0};

		NetcdfWriter.write(myVar, origin, values);

        //Always close
		NetcdfWriter.close();
	}
}



