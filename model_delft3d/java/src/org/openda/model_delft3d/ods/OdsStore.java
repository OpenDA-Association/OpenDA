/* OpenDA v2.4.3 
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
package org.openda.model_delft3d.ods;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Reads data from the model output files using ODS library.
 * Uses OdsLib wrappers of the native ods.dll
 */
public class OdsStore {

	public enum TimeSeriesType {
		POINT, GRID
	}

	public class GridDimensions {
		public int xSteps;
		public int ySteps;
		public int zSteps;
		public int noCells;
	}

	private GridDimensions gridDimensions = new GridDimensions();

	private OdsDll odsDll = null;

	String[] fname = new String[3];
	String[] fnameGrd = new String[3];
	private int[] ftype = new int[1]; // see OdsLib for codes
	private int[] ftypeGrd = new int[1];

	String pardef = "*";
	int[] maxdef = new int[1];
	int[] maxlst = new int[1];
	int[] lang = new int[1];
	int[] nrlst = new int[1];
	int[] pardep = new int[1];
	int[] timdep = new int[1];
	int[] locdep = new int[1];
	int[] ndim = new int[5];
	int[] error = new int[1];
	int[] lgrid = null;

	StringBuffer option = new StringBuffer(256);

	Map<String, Integer> parameterIds = new HashMap<String, Integer>();
	Map<String, Integer> locationIds = new HashMap<String, Integer>();

	String currentFileNameGrid = null;
	int currentNoCells = 0;

	public OdsStore(File f1) throws IOException {
		this(f1, autoDetectSecondFile(f1));
	}

	public OdsStore(File f1, File f2) throws IOException {
		this(f1, f2, null, null);
	}

	public OdsStore(File f1, File f2, File f3) throws IOException {
		this(f1, f2, f3, null);
	}

	public OdsStore(File f1, String nativeDllPath) throws IOException {
		this(f1, autoDetectSecondFile(f1), nativeDllPath);
	}

	public OdsStore(File f1, File f2, String nativeDllPath) throws IOException {
		this(f1, f2, null, nativeDllPath);
	}

	public OdsStore(File f1, File f2, File f3, String nativeDllPath) throws IOException {

		odsDll = new OdsDll(nativeDllPath);

		fname[0] = f1.getAbsolutePath();
		fname[1] = f2 == null ? "" : f2.getAbsolutePath();
		fname[2] = f3 == null ? "" : f3.getAbsolutePath();

		if (!f1.exists()) {
			throw new IOException("File does not exist (f1): " + f1.getAbsolutePath());
		}
		if (f2 != null && !f2.exists()) {
			throw new IOException("File does not exist (f2): " + f2.getAbsolutePath());
		}
		if (f3 != null && !f3.exists()) {
			throw new IOException("File does not exist (f3): " + f3.getAbsolutePath());
		}

		ftype[0] = getFileType(fname);
	}

	public String[] getParameters() throws Exception {
		odsDll.getdim(fname, ftype, "P", pardep, timdep, locdep, ndim, error, option);


		/* ndim:
				   ndim[0] = number of dimensions (loc) or 1 (par, tim)
				   ndim[1] = number of parameters */

		assert ndim[0] == 1; // TODO: support only point-based for not, should be extended

		// get names and units for all parameters
		String[] parname = new String[ndim[1]];
		String[] parunit = new String[ndim[1]];
		int[] partype = new int[ndim[1]];
		int[] parcode = new int[ndim[1]];
		pardef = "*";
		lang[0] = 0;
		maxdef[0] = 1;
		maxlst[0] = ndim[1];

		odsDll.getpar(fname, ftype, pardef, maxdef, timdep, locdep, maxlst, lang, parname, parunit, partype, parcode, nrlst, error, option);

		// fill in parameter - code map to be used later
		int i = 0;
		for (String parameter : parname) {
			parameterIds.put(parameter, parcode[i++]);
		}

		return parname;
	}

	public TimeSeriesType getTimeSeriesType(String parameter) throws Exception {
		pardep[0] = parameterIds.get(odsName(parameter));
		odsDll.getdim(fname, ftype, "L", pardep, timdep, locdep, ndim, error, option);

		if (ndim[0] == 1 || fname[0].contains("trih-")) {
			return TimeSeriesType.POINT;
		} else {
			return TimeSeriesType.GRID;
		}
	}

	public int[] getDimensions(String parameterFullName) throws Exception {
		if (parameterIds.size() == 0) {
			getParameters();
		}
		String parameter = odsName(parameterFullName);
		pardep[0] = parameterIds.get(parameter);

		// in case of WAQ grid dimensions are stored in a separate files <runid>.lga, <runid>.cco
		fnameGrd[0] = fname[0];
		fnameGrd[1] = fname[1];
		fnameGrd[2] = fname[2];
		ftypeGrd[0] = ftype[0];

		if (ftype[0] == odsDll.ODS_DELWAQ_MAP_BIN || ftype[0] == odsDll.ODS_DELPAR_PLO_BIN) {
			if (ftype[0] == odsDll.ODS_DELWAQ_MAP_BIN) {
				String directoryPath = new File(fname[0]).getParent();
				String fileName = new File(fname[0]).getName();

				fnameGrd[0] = directoryPath + File.separatorChar + "com-" + fileName.substring(0, fileName.length() - 3) + "lga";
				fnameGrd[1] = directoryPath + File.separatorChar + "com-" + fileName.substring(0, fileName.length() - 3) + "cco";
				fnameGrd[2] = "";
			}

			ftypeGrd[0] = getFileType(fnameGrd);

			pardep[0] = -1;

			odsDll.getdim(fnameGrd, ftypeGrd, "loc", pardep, timdep, locdep, ndim, error, option);

			if (lgrid == null || !fnameGrd[0].equals(currentFileNameGrid)) {
				int[] noCells = new int[1];
				int[] gisType = new int[1];
				int[] loc_index = new int[9];
				loc_index[0] = 0;
				loc_index[1] = ndim[1] - 1;
				loc_index[2] = 1;
				loc_index[3] = 0;
				loc_index[4] = ndim[2] - 1;
				loc_index[5] = 1;
				loc_index[6] = 0;
				loc_index[7] = 0;
				loc_index[8] = 1;

				lgrid = new int[ndim[1] * ndim[2]];
				currentFileNameGrid = fnameGrd[0];

				odsDll.getgrd(fnameGrd, ftypeGrd, loc_index, lgrid, noCells, gisType, error);
				currentNoCells = noCells[0];
			}

			ndim[4] = currentNoCells;
		} else {
			odsDll.getdim(fname, ftype, "L", pardep, timdep, locdep, ndim, error, option);
		}
		return ndim;
	}

	public GridDimensions getGridDimensions(String parameter) throws Exception {
		int[] dimensions = getDimensions(odsName(parameter));

		gridDimensions.xSteps = dimensions[1];
		gridDimensions.ySteps = dimensions[2];
		gridDimensions.zSteps = dimensions[3];
		gridDimensions.noCells = dimensions[4];

		return gridDimensions;
	}

	public String[] getLocations(String parameter) throws Exception {
		pardep[0] = parameterIds.get(odsName(parameter));

		odsDll.getdim(fname, ftype, "L", pardep, timdep, locdep, ndim, error, option);

		/*
				ndim:
					ndim[0] = number of dimensions (loc) or 1 (par, tim)
					ndim[1] = number of locations, if the file contains _named_ locations but also the number of layers in ndim[3]

					printf( "Dimension information: %d %d %d %d %d\n", ndim[0], ndim[1], ndim[2], ndim[3], ndim[4] );
				*/

		// get names for all locations for a selected parameter

		String[] locname = new String[ndim[1]];
		int[] loctype = new int[ndim[1]];
		int[] locid = new int[ndim[1]];

		String locdef = "*";
		lang[0] = 0;
		maxdef[0] = 1;
		maxlst[0] = ndim[1];
		odsDll.getloc(fname, ftype, locdef, maxdef, pardep, timdep, maxlst, locname, loctype, locid, nrlst, error, option);

		int i = 0;
		for (String location : locname) {
			locationIds.put(location, locid[i++]);
		}

		return locname;
	}

	public double[] getTimes(String parameter) throws Exception {
		if (parameterIds.size() == 0) {
			getParameters();
		}
		Integer parDep = parameterIds.get(odsName(parameter));
		if (parDep == null) {
			return null;
		}
		pardep[0] = parDep;
		odsDll.getdim(fname, ftype, "T", pardep, timdep, locdep, ndim, error, option);

		//  ndim:
		//       ndim[0] = number of dimensions (loc) or 1 (par, tim)
		//       ndim[1] = number of locations, if the file contains _named_ locations
		//                 but also the number of layers in ndim[3]

		double[] times = new double[ndim[1]];
		int[] timetype = new int[ndim[1]];
		double[] timdef = new double[2];

		timdef[0] = 0.0;
		timdef[1] = Double.MAX_VALUE;
		lang[0] = 0;
		maxdef[0] = 2;
		maxlst[0] = ndim[1];
		odsDll.gettme(fname, ftype, timdef, maxdef, pardep, timdep, maxlst, times, timetype, nrlst, error, option);

		return times;
	}

	public float[] getLocationTimeSeriesValues(String parameter, String location, double startTime, double endTime, int zLayer) throws Exception {
		getTimes(odsName(parameter)); // make sure all variables are inifialised

		// Now we need to set the parameter that we are interested in
		pardep[0] = parameterIds.get(odsName(parameter));
		locdep[0] = locationIds.get(location);

		int[] loc_index = new int[9];

		loc_index[0] = locdep[0];
		loc_index[1] = locdep[0];
		loc_index[2] = 1; // Not used for named locations
		loc_index[3] = 0;
		loc_index[4] = 0;
		loc_index[5] = 1;
		loc_index[6] = zLayer - 1;
		loc_index[7] = zLayer - 1;
		loc_index[8] = 1;

		double[] timdef = new double[3];
		timdef[0] = startTime;
		timdef[1] = endTime;
		timdef[2] = (endTime - startTime) / (double) (nrlst[0] - 1);

		float misval = -999.0f;

		float[] data = new float[nrlst[0]];

		odsDll.getmat(fname, ftype, pardep, loc_index, timdef, misval, lang, nrlst, data, error, option);

		return data;
	}

	public float[] getGridTimeSeriesValues(String parameterFullName, double time, int zLayer) throws Exception {
		String parameter = odsName(parameterFullName);
		getGridDimensions(parameter); // make sure all variables are inifialised

		// Now we need to set the parameter that we are interested in
		pardep[0] = parameterIds.get(parameter);

		int[] loc_index = new int[9];
		int[] noCells = new int[1];
		int[] gisType = new int[1];

		// The values in the file are shaped as matrices
		loc_index[0] = 0;
		loc_index[1] = gridDimensions.xSteps - 1;
		loc_index[2] = 1;
		loc_index[3] = 0;
		loc_index[4] = gridDimensions.ySteps - 1;
		loc_index[5] = 1;
		loc_index[6] = zLayer - 1;
		loc_index[7] = zLayer - 1;
		loc_index[8] = 1;
		nrlst[0] = gridDimensions.xSteps * gridDimensions.ySteps;

		//if (false) {
		if (lgrid == null) {
			loc_index[6] = 0;
			loc_index[7] = 0;
			lgrid = new int[nrlst[0]]; // TODO: actually we require the noCells property - size of the administration array
			odsDll.getgrd(fnameGrd, ftypeGrd, loc_index, lgrid, noCells, gisType, error);
			gridDimensions.noCells = noCells[0];
			//System.out.println("Delft3D: got grid");
		}
		//}

		// We may need to restore the layer information
		loc_index[6] = zLayer - 1;
		loc_index[7] = zLayer - 1;

		if (ndim[4] != 0) {
			// Linear array of data in file
			loc_index[0] = 0;
			loc_index[1] = gridDimensions.noCells - 1;
			loc_index[2] = 1;
			loc_index[3] = 0;
			loc_index[4] = 0;
			loc_index[5] = 1;
			loc_index[6] = zLayer - 1;
			loc_index[7] = zLayer - 1;
			loc_index[8] = 1;
			nrlst[0] = gridDimensions.noCells;
		}

		double[] timdef = new double[3];
		timdef[0] = time;
		timdef[1] = time;
		timdef[2] = 1.0;

		float misval = -999.0f;


		float[] data = new float[nrlst[0]];
		float[] values = new float[gridDimensions.xSteps * gridDimensions.ySteps];

		odsDll.getmat(fname, ftype, pardep, loc_index, timdef, misval, lang, nrlst, data, error, option);

		int k = 0;
		boolean onlyZero = true;
		for (int j = 0; j < gridDimensions.ySteps; j++) {
			for (int i = 0; i < gridDimensions.xSteps; i++) {
				values[k] = misval;
				if (lgrid[k] > 0) {
					values[k] = data[lgrid[k] - 1];
					onlyZero = (onlyZero && (values[k] == 0.0));
					if (values[k] == 0.0) {
						// SH: AM, what should be done here?
					}
				}
				k++;
			}
		}

		// Some output parameters are not updated until the end of the timestep.
		// This is the case of BLOOM output like chlorophyll. Supplying missing
		// values solves the consmetic problem this causes.
		if (onlyZero) {
			for (int i = 0; i < gridDimensions.ySteps * gridDimensions.xSteps; i++) {
				values[i] = misval;
			}
		}

		return values;
	}

	public float[] getGridTimeSeriesValues(String parameter) throws Exception {
		return getGridTimeSeriesValues(parameter, 0, 0);
	}

	// -- private methods
	private String odsName(String parameterFullName) {
		String parameter = parameterFullName;
		if (parameter.startsWith("plo:") && fname[0].endsWith(".plo")) {
			parameter = parameterFullName.substring(4);
		}
		return parameter;
	}

	private int getFileType(String[] fname) throws IOException {
		if (fname[0].endsWith("dat") && fname[1].endsWith("def") && fname[0].contains("trih-")) {
			return odsDll.ODS_TRISULA_HIS_NEFIS;
		} else if (fname[0].endsWith("dat") && fname[1].endsWith("def") && fname[0].contains("trim-")) {
			return odsDll.ODS_TRISULA_MAP_NEFIS;
		} else if (fname[0].endsWith("lga") && fname[1].endsWith("cco") && fname[0].contains("com-")) {
			// WAQ grid definition
			return odsDll.ODS_DELWAQ_GRID_UNF;
		} else if (fname[0].endsWith("map")) {
			// WAQ map data
			return odsDll.ODS_DELWAQ_MAP_BIN;
		} else if (fname[0].endsWith("plo")) {
			// WAQ map data
			return odsDll.ODS_DELPAR_PLO_BIN;
		} else if (fname[0].endsWith("his")) {
			// WAQ point data
			return odsDll.ODS_DELWAQ_HIS_BIN;
		} else if (fname[0].endsWith("bal")) {
			// WAQ balance data
			return odsDll.ODS_DELWAQ_HIS_BIN;
		} else {
			throw new IOException("Unsupported input files type found: \n" + fname[0] + "\n" + fname[1] + "\n" + fname[2]);
		}
	}

	private static File autoDetectSecondFile(File f1) {
		if (f1.getName().endsWith(".dat")) {
			String path = f1.getAbsolutePath();
			return new File(path.substring(0, path.length() - 3) + "def");
		}
		if (f1.getName().endsWith(".lga")) {
			String path = f1.getAbsolutePath();
			return new File(path.substring(0, path.length() - 3) + "cco");
		}

		return null;  //To change body of created methods use File | Settings | File Templates.
	}

	public float[] getParameterValues(String parameter,
									  double startTime, double endTime,
									  int mStart, int mEnd, int nStart, int nEnd,
									  int zLayer) throws Exception {
		getTimes(odsName(parameter)); // make sure all variables are inifialised

		float[] data;

			// Now we need to set the parameter that we are interested in
			pardep[0] = parameterIds.get(odsName(parameter));

			int[] loc_index = new int[9];

			loc_index[0] = mStart;
			loc_index[1] = mEnd;
			loc_index[2] = 1; // Not used for named locations
			loc_index[3] = nStart;
			loc_index[4] = nEnd;
			loc_index[5] = 1;
			loc_index[6] = zLayer - 1;
			loc_index[7] = zLayer - 1;
			loc_index[8] = 1;

			double[] timdef = new double[3];
			timdef[0] = startTime;
			timdef[1] = endTime;
			timdef[2] = (endTime - startTime) / (double) (nrlst[0] - 1);

			float misval = -999.0f;

			int numM = mEnd - mStart + 1;
			int numN = nEnd - nStart + 1;
			data = new float[nrlst[0] * numM * numN];

			odsDll.getmat(fname, ftype, pardep, loc_index, timdef, misval, lang, nrlst, data, error, option);


		return data;
	}
}
