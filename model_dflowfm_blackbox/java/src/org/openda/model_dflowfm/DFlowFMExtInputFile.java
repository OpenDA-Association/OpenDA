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
package org.openda.model_dflowfm;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

import org.ini4j.Options;
import org.ini4j.InvalidFileFormatException;

public class DFlowFMExtInputFile {

	/**
	 * Class for reading settings from the ext input file
	 * *
	 * The file is loosely based on the INI format. There are no sections, only a block of key/value combinations.
	 * <p/>
	 * QUANTITY
	 * : waterlevelbnd, velocitybnd, dischargebnd, tangentialvelocitybnd, normalvelocitybnd  filetype=9         method=2,3
	 * : salinitybnd                                                                         filetype=9         method=2,3
	 * : lowergatelevel, damlevel, pump                                                      filetype=9         method=2,3
	 * : frictioncoefficient, horizontaleddyviscositycoefficient, advectiontype, ibotlevtype filetype=4,10      method=4
	 * : initialwaterlevel, initialsalinity                                                  filetype=4,10      method=4
	 * : windx, windy, windxy, rainfall_mmperday, atmosphericpressure                        filetype=1,2,4,7,8 method=1,2,3
	 * : shiptxy, movingstationtxy                                                           filetype=1         method=1
	 * <p/>
	 * kx = Vectormax = Nr of variables specified on the same time/space frame. Eg. Wind magnitude,direction: kx = 2
	 * FILETYPE=1  : uniform              kx = 1 value               1 dim array      uni
	 * FILETYPE=2  : unimagdir            kx = 2 values              1 dim array,     uni mag/dir transf to u,v, in index 1,2
	 * FILETYPE=3  : svwp                 kx = 3 fields  u,v,p       3 dim array      nointerpolation
	 * FILETYPE=4  : arcinfo              kx = 1 field               2 dim array      bilin/direct
	 * FILETYPE=5  : spiderweb            kx = 3 fields              3 dim array      bilin/spw
	 * FILETYPE=6  : curvi                kx = ?                                      bilin/findnm
	 * FILETYPE=7  : triangulation        kx = 1 field               1 dim array      triangulation
	 * FILETYPE=8  : triangulation_magdir kx = 2 fields consisting of Filetype=2      triangulation in (wind) stations
	 * <p/>
	 * FILETYPE=9  : polyline             kx = 1 For polyline points i= 1 through N specify boundary signals, either as
	 * timeseries or Fourier components or tidal constituents
	 * Timeseries are in files *_000i.tim, two columns: time (min)  values
	 * Fourier components and or tidal constituents are in files *_000i.cmp, three columns
	 * period (min) or constituent name (e.g. M2), amplitude and phase (deg)
	 * If no file is specified for a node, its value will be interpolated from surrounding nodes
	 * If only one signal file is specified, the boundary gets a uniform signal
	 * For a dischargebnd, only one signal file must be specified
	 * <p/>
	 * FILETYPE=10 : inside_polygon       kx = 1 field                                uniform value inside polygon for INITIAL fields
	 * <p/>
	 * METHOD  =0  : provider just updates, another provider that pointers to this one does the actual interpolation
	 * =1  : intp space and time (getval) keep  2 meteofields in memory
	 * =2  : first intp space (update), next intp. time (getval) keep 2 flowfields in memory
	 * =3  : save weightfactors, intp space and time (getval),   keep 2 pointer- and weight sets in memory
	 * =4  : only spatial interpolation
	 * <p/>
	 * OPERAND =O  : Override at all points
	 * =+  : Add to previously specified value
	 * =*  : Multiply with previously specified value
	 * =A  : Apply only if no value specified previously (For Initial fields, similar to Quickin preserving best data specified first)
	 * <p/>
	 * VALUE   =   : Offset value for this provider
	 * <p/>
	 * FACTOR  =   : Conversion factor for this provider
	 */
	private File inputFile = null;
	private static final String HEADER_CHAR = "*";
	private Options ini = new Options();

	public DFlowFMExtInputFile(File myFile) {
		// main constructor
		inputFile = myFile;
		ParseInputFile();

//		Set<String> keyNames = ini.keySet();
//		System.out.println(keyNames);//		System.out.println("QUANTITY = " + ini.getAll("QUANTITY"));
//		System.out.println("FILENAME = " + ini.getAll("FILENAME"));
//		System.out.println("FILETYPE = " + ini.getAll("FILETYPE"));
//		System.out.println("METHOD = "   + ini.getAll("METHOD"));
//		System.out.println("OPERAND = "  + ini.getAll("OPERAND"));
	}

	public DFlowFMExtInputFile(File workingDir, String fileName) {
		this(new File(workingDir, fileName));
	}

	private void ParseInputFile() {

		/*  Parse .ext input file as an ini file without sections
		 *  Remove all keys originating from comment lines 
		 */
		try {
			ini.load(inputFile);
			Set<String> keyNames = ini.keySet();
			// Remove keys starting with HEADER_CHAR
			Iterator<String> iter = keyNames.iterator();
			while (iter.hasNext()) {
				String key = iter.next();
				if (key.startsWith(HEADER_CHAR)) iter.remove();
			}
		} catch (InvalidFileFormatException e) {
			throw new RuntimeException("Invalid Formatting in '" + inputFile.getAbsolutePath() + "'.", e);
		} catch (IOException e) {
			throw new RuntimeException("IOexception'" + inputFile.getAbsolutePath() + "'.", e);
		}
	}

	public String get(String key, int index) {
		String value = (String) ini.get(key, index);
		if (value == null) {
			String error = String.format("The option '%s' #%s not specified in'" + inputFile.getAbsolutePath() + "'.", key, index);
			throw new RuntimeException(error);
		} else if (value.equals("VALUE") || value.equals("FACTOR")) {
			throw new UnsupportedOperationException("Support for the 'VALUE' or 'FACTOR' key is not implemented.");
		}
		return value;
	}

	public int count() {

		return ini.length("QUANTITY");
	}

}

