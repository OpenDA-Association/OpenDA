/* OpenDA v2.4.1 
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

package org.openda.resultwriters;

import org.openda.costa.CtaObject;
import org.openda.costa.CtaTreeVector;
import org.openda.costa.CtaVector;
import org.openda.interfaces.*;

import java.io.File;
import java.util.HashMap;

/**
 * Result writer that produces output in a netcdf file.
 */
public class NetcdfResultWriter extends CtaObject implements IResultWriter {

    private HashMap<String, Integer> iter = new HashMap<String, Integer>();

    // A list that contains the opened netcdf files: each variable (id),
    // needs its own netcdfile. The netcdffile should be opened before/during first writing ,
    // All netcdf files should be closed together in a finalize call.

    private HashMap<String, Integer> idlist = new HashMap<String, Integer>();

    private String netcdfnameprefix = " ";
    private int ctaFilehandle = 0;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public NetcdfResultWriter(File workingDir, String configString) {
        if (configString.startsWith("<xml")) {  // TODO: right prefix
            // TODO: read from config file
        	throw new RuntimeException("NetcdfResultWriter: expecting a filename, but found xml-code");
        } else {
            if (configString.toLowerCase().endsWith(".nc")) {
                String prefixname = configString.substring(0, configString.length() - 3);
                netcdfnameprefix = new File(workingDir, prefixname).getAbsolutePath();
            }else{
            	throw new RuntimeException("NetcdfResultWriter: expecting a filename ending on .nc,"
            			+" but found "+configString);
            }
        }
    }

	public void putMessage(Source source, String comment) {
        // NO ACTION NEEDED (TOOD: can messages be added to netcdf file?
    }

    public void putMessage(IInstance source, String comment) {
        // NO ACTION NEEDED (TOOD: can messages be added to netcdf file?
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
        // TODO create a counter for time being; one counter per id
        Integer currentIter = 0;
        if (this.iter.containsKey(id)) {
            currentIter = this.iter.get(id);
        }
        this.iter.put(id, currentIter + 1);

        // Administration for opening a netcdf file:
        if (this.idlist.containsKey(id)) {
            // the variable has already written before; get filehandle
            ctaFilehandle = this.idlist.get(id);
        } else {
            // first time this variable occurs; first initialize the netcdffile!

            String netcdfnamevar = netcdfnameprefix.concat(id);
            netcdfnamevar = netcdfnamevar.concat(".nc");
            ctaFilehandle = ctaNetcdfInit(netcdfnamevar,"w");
            this.idlist.put(id, ctaFilehandle);
        }
		writeObjectToNetCdfFile(ctaFilehandle, result);
	}

	public static void writeToNetCdf(String fileName, Object obj) {
		int fileHandle = createNetCdfHandle(fileName, "w");
		writeObjectToNetCdfFile(fileHandle, obj);
		closeNetcdfHandle(fileHandle);
	}

	private static void writeObjectToNetCdfFile(int aCtaFileHandle, Object obj) {
		if (obj instanceof CtaTreeVector) {
			int retval = ((CtaTreeVector) obj).export(aCtaFileHandle);
			if (retval != 0) {
				throw new RuntimeException("NetcdfResultWriter: putValue for CtaTreeVector not succeeded ");
			}
		}
		else if (obj instanceof IMatrix) {
			int ncols = ((IMatrix) obj).getNumberOfColumns();
			int nrows = ((IMatrix) obj).getNumberOfRows();
			IVector vec = new CtaVector(ncols * nrows);
			for (int i = 0; i < nrows; i++) {
				for (int j = 0; j < ncols; j++) {
					vec.setValue(j * nrows + i, ((IMatrix) obj).getValue(i, j));
				}
			}
			CtaTreeVector ctaTreeVector = new CtaTreeVector("from_SimpleMatrix", "from_SimpleMatrix", vec);
			ctaTreeVector.setReggrid(nrows,ncols,1,0.0,0.0,0.0,1.0,1.0,1.0);

			int retval = ctaTreeVector.export(aCtaFileHandle);
			if (retval != 0) {
				throw new RuntimeException("NetcdfResultWriter: putValue for Matrix not succeeded ");
			}

		} else if (obj instanceof CtaVector) {
			int retval = ((CtaVector) obj).export(aCtaFileHandle);
			if (retval != 0) {
				throw new RuntimeException("NetcdfResultWriter: putValue for CtaVector not succeeded ");
			}
		} else if (obj instanceof ITreeVector) {
			ITreeVector treeVector = (ITreeVector) obj;
			double[] values = ((ITreeVector)obj).getValues();
			IVector vec = new CtaVector(values.length);
			vec.setValues(values);
			CtaTreeVector ctaTreeVector = new CtaTreeVector(treeVector.getCaption(), treeVector.getId(), vec);
			int retval = ctaTreeVector.export(aCtaFileHandle);
			if (retval != 0) {
				throw new RuntimeException("NetcdfResultWriter: putValue for TreeVector not succeeded ");
			}
		} else if (obj instanceof IVector) {
			CtaVector ctaVector = new CtaVector( (IVector) obj);
			int retval = ctaVector.export(aCtaFileHandle);
			if (retval != 0) {
				throw new RuntimeException("NetcdfResultWriter: putValue for Vector not succeeded ");
			}
		} else if (obj instanceof Double) {
			CtaVector ctaVector = new CtaVector(1);
			ctaVector.setValue(0,(Double)obj);
			int retval = ctaVector.export(aCtaFileHandle);
			if (retval != 0) {
				throw new RuntimeException("NetcdfResultWriter: putValue for Vector not succeeded ");
			}
		} else {
			throw new RuntimeException("NetcdfResultWriter: putValue for this object not implemented: " +
					obj.getClass().getName());
		}
	}

	public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    public void free() {
        // Get list of all Keys;

        for (String key2 : idlist.keySet()) {

            ctaFilehandle = idlist.get(key2);
            int retval = ctaNetcdfClose(ctaFilehandle);

            if (retval != 0) {
                throw new RuntimeException("NetcdfResultWriter: closing the netcdf file has not succeeded ");

            }
        }
        idlist.clear();


    }

    public native int ctaNetcdfInit(String netcdfname, String action);

    public native int ctaNetcdfClose(int ctafilehandle);

	public static int createNetCdfHandle(String netcdfname, String action) {
		NetcdfResultWriter writer = new NetcdfResultWriter();
		return writer.ctaNetcdfInit(netcdfname, action);
	}

	public static int closeNetcdfHandle(int ctafilehandle) {
		NetcdfResultWriter writer = new NetcdfResultWriter();
		return writer.ctaNetcdfClose(ctafilehandle);
	}

	public NetcdfResultWriter() {
		// empty constructor. Can be removed want static methods above are avaiable in the right way
	}
}
