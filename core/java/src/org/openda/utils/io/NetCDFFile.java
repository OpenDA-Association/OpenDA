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
import ucar.nc2.*;

import java.io.IOException;
import java.io.File;

/**
 * Created by nils van Velzen on 25/09/15.
 *
 * This is a simple utility class to write variables to NetCDF. The current implementation is extremely simple but can/will be extended in the future.
 *
 */

public class NetCDFFile {
	private File fileName;
	private NetcdfFileWriter netcdfFileWriter;
	private boolean fileIsNotCreated=true;
    private Dimension nDim;
	private Dimension timeDim;
	private Variable variable;


	//Create and initialize a new NetCDFFile. Nothing happens yet (no files are created)
	public NetCDFFile(File fileName) {
		this.fileName = fileName;
	}


	//Write an array to file. Currently we only support a single array per file. The timeindex species the additional axis along we can extend the array in an existing NetCDF file
	public void writeArray(double[] vals, int[] nDims, int iTime, String shortName) throws IOException, InvalidRangeException {
        //Check wether we have to define de header and create the file on first write

        int n=vals.length;
		NetcdfFileWriter netcdfFileWriter;


		Variable myVar;
    	if (this.fileIsNotCreated){
			// Create a new file
			netcdfFileWriter = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, fileName.getAbsolutePath());

			// Setup Header
			this.nDim  = netcdfFileWriter.addDimension(null,"n", n);
			this.timeDim = netcdfFileWriter.addUnlimitedDimension("time");
			myVar = netcdfFileWriter.addVariable(null, shortName, DataType.DOUBLE, "time n");

			// create the file
			netcdfFileWriter.create();
			this.fileIsNotCreated=false;
		}
		else {
			netcdfFileWriter = NetcdfFileWriter.openExisting(this.fileName.getAbsolutePath());
			myVar = netcdfFileWriter.findVariable(shortName);
		}

		ArrayDouble.D2 values = new ArrayDouble.D2(1, nDim.getLength());
        // Copy the values
		for (int i=0; i<n; i++){
			values.set(0,i,vals[i]);
		}

		int[] origin = new int[]{iTime, 0};

		netcdfFileWriter.write(myVar, origin, values);

        //Always close
		netcdfFileWriter.close();
	}
}



