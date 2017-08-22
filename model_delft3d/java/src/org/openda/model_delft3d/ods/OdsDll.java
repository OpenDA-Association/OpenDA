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
package org.openda.model_delft3d.ods;
import com.sun.jna.Native;
import org.openda.blackbox.config.BBUtils;

import java.io.File;
import java.io.IOException;

/**
 * wrapper for native (c) ODS DLL
 */
public class OdsDll {

	private static final int PARLEN = 20;
	private static final int ODS_FILNAMLEN = 256;
	private static final int IEOK = 0;

	public final static int ODS_TRISULA_HIS_BIN = 1000;
	public final static int ODS_TRISULA_HIS_NEFIS = 1001;
	public final static int ODS_TRISULA_MAP_BIN = 1002;
	public final static int ODS_TRISULA_MAP_NEFIS = 1003;
	public final static int ODS_TRISULA_DRO_NEFIS = 1004;
	public final static int ODS_DELWAQ_HIS_BIN = 4;
	public final static int ODS_DELWAQ_BAL_BIN = 16;
	public final static int ODS_DELWAQ_HIS_NEFIS = 104;
	public final static int ODS_DELWAQ_MAP_BIN = 3;
	public final static int ODS_DELWAQ_MAP_NEFIS = 103;
	public final static int ODS_DELWAQ_GRID_UNF = 14;
	public final static int ODS_DELWAQ_TELEMAC = 207;
	public final static int ODS_DELPAR_HIS_NEFIS = 207;
	public final static int ODS_DELPAR_MAP_NEFIS = 203;
	public final static int ODS_DELPAR_PLO_NEFIS = 205;
	public final static int ODS_DELPAR_PLO_BIN = 15;
	public final static int ODS_DELPAR_TRK_NEFIS = 206;
	public final static int ODS_ANY_TEKAL_ASCII_1D = 3101;
	public final static int ODS_ANY_TEKAL_ASCII_1DE = 3111;
	public final static int ODS_ANY_TEKAL_ASCII_2D = 3111;

	private static boolean dllYetToBeInitialized = true;
	private OdsWinIfortDll odsWinIfortDll = null;

	public OdsDll(String nativeDllPath) {

		if (dllYetToBeInitialized) {

			String actualNativeDllPath = "ods";
			if (nativeDllPath != null) {
				File nativeDllFileOrDir = new File(nativeDllPath);
				if (!nativeDllFileOrDir.exists()) {
					throw new RuntimeException("Invalid native DLL path: " + nativeDllFileOrDir.getAbsolutePath());
				}
				if (nativeDllFileOrDir.isDirectory()) {
					actualNativeDllPath = nativeDllPath + "/ods";
				} else {
					actualNativeDllPath = nativeDllPath;
					for (String ext : new String[]{ ".so", ".dll" }) {
						if (actualNativeDllPath.toLowerCase().endsWith(ext))
							actualNativeDllPath = actualNativeDllPath.substring(
									0, actualNativeDllPath.length() - ext.length());
					}
				}
			}

			// Delft3D-Flow uses ifort for both linux and windows.
			// If in the future another compiler is needed, e.g. gfortran,
			// see org.openda.model_efdc_dll.EfdcDLL for an example of function name mapping.
			if(BBUtils.RUNNING_ON_WINDOWS){
				odsWinIfortDll = (OdsWinIfortDll) Native.loadLibrary(actualNativeDllPath, OdsWinIfortDll.class);

			}else{
				// GfortranFunctionMapper gFortranMapper = new GfortranFunctionMapper();
				// HashMap<String, String> gFortranMap = gFortranMapper.getMap();
				odsWinIfortDll = (OdsWinIfortDll) Native.loadLibrary(
						actualNativeDllPath, OdsWinIfortDll.class); // , gFortranMap);
			}
			dllYetToBeInitialized = true;
		}
	}

	/**
	 * Get error information.
	 */
	public String odserr(int error) {
		byte[] s = new byte[1024];
		odsWinIfortDll.odserr(error, s);
		return new String(s);
	}

	/**
	 * Close all files.
	 */
	public void closal(String[] fname, int[] error) {
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);
		odsWinIfortDll.C_closal(fnameBytes, error);
	}

	/**
	 * get dimensions
	 */
	public void getdim(String[] fname, int[] itype, String dim, int[] pardep,
							  int[] timdep, int[] locdep, int[] ndim, int[] error, StringBuffer option) throws Exception {
		checkFiles(fname);
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);
		byte[] dimType = new byte[4];
		for (int i = 0; i < dim.length(); i ++) {
			dimType[i] = (byte)dim.charAt(i);
		}
		dimType[dim.length()] = '\0';


		String optionAsString = option.toString();
		byte[] optionAsBytes = convertStringsArrayToByteArray(new String[]{optionAsString}, option.length());
		odsWinIfortDll.getdim(fnameBytes, itype, dimType, pardep, timdep, locdep, ndim, error, option.toString());
		checkError(error[0]);
	}

	/**
	 * get parameter information
	 */
	public void getpar(String[] fname, int[] itype, String pardef, int[] maxdef,
							  int[] timdep, int[] locdep, int[] maxlst, int[] lang, String[] parname, String[] parunit,
							  int[] partype, int[] parcode, int[] nrlst, int[] error, StringBuffer option) throws Exception {
		checkFiles(fname);
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);

		// this part is tricky since wldelft_native does not pass strings by reference we will give it array of bytes and then convert output to strings.
		byte[] parnameBytes = new byte[(PARLEN + 1) * parname.length];
		byte[] parunitBytes = new byte[(PARLEN + 1) * parname.length];

		odsWinIfortDll.getpar(fnameBytes, itype, pardef, maxdef, timdep, locdep, maxlst,
				lang, parnameBytes, parunitBytes, partype, parcode, nrlst, error, option.toString());

		// split parameter and unit parnameBytes into strings
		convertByteArrayToStringsArray(parunitBytes, parunit, PARLEN  + 1);
		convertByteArrayToStringsArray(parnameBytes, parname, PARLEN  + 1);

		checkError(error[0]);
	}

	/**
	 * get location information
	 */
	public void getloc(String[] fname, int[] ftype, String locdef, int[] maxdef,
							  int[] pardep, int[] timdep, int[] maxlst, String[] locname, int[] loctype, int[] locid,
							  int[] nrlst, int[] error, StringBuffer option) throws Exception {
		checkFiles(fname);
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);
		byte[] locnameBytes = new byte[(PARLEN + 1) * maxlst[0]];

		odsWinIfortDll.getloc(fnameBytes, ftype, locdef, maxdef, pardep, timdep, maxlst,
				locnameBytes, loctype, locid, nrlst, error, option.toString());
		checkError(error[0]);
		convertByteArrayToStringsArray(locnameBytes, locname, PARLEN + 1);
	}

	/**
	 * get time information
	 */
	public void gettme(String[] fname, int[] ftype, double[] timdef, int[] maxdef, int[] pardep, int[] timdep, int[] maxlst, double[] time, int[] timetype, int[] nrlst, int[] error, StringBuffer option) throws Exception {
		checkFiles(fname);
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);
		odsWinIfortDll.gettme(fnameBytes, ftype, timdef, maxdef, pardep, timdep, maxlst,
				time, timetype, nrlst, error, option.toString());
		checkError(error[0]);
	}

	/**
	 * get a matrix of values
	 */
	public void getmat(String[] fname, int[] ftype, int[] pardep, int[]loc_index, double[] timdef, float misval, int[] lang, int[] nrlst, float[] data, int[] error, StringBuffer option) throws Exception {
		checkFiles(fname);
		float[] misValByRef = new float[1];
		misValByRef[0] = misval;
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);
		odsWinIfortDll.getmat(fnameBytes, ftype, pardep, loc_index, timdef, misValByRef,
				lang, nrlst, data, error, option.toString());
		checkError(error[0]);
	}

	/**
	 * get grid information
	 */
	public void getgrd(String[] fname, int[] ftype, int[] loc_index, int[] lgrid, int[] noCells, int[] gisType, int[] error) throws Exception {
		checkFiles(fname);
		byte[] fnameBytes = convertStringsArrayToByteArray(fname, ODS_FILNAMLEN);
		odsWinIfortDll.getgrd(fnameBytes, ftype, loc_index, lgrid, noCells, gisType, error);
		checkError(error[0]);
	}

	/**
	 * Converts array of strings into array of bytes containing all strings with a fixed length
	 */
	private static byte[] convertStringsArrayToByteArray(String[] strings, int elementLength) {
		byte[] bytes = new byte[elementLength * strings.length];

		int i = 0;
		for(String s : strings) {
			byte[] stringBytes = s.getBytes();
			System.arraycopy(stringBytes, 0, bytes, i * elementLength, Math.min(elementLength, s.length()));
			//bytes[i * elementLength + Math.min(elementLength-1,s.length())] = '\u0000';
			i++;
		}

		return bytes;
	}

	/**
	 * Converts byte array containing several strings into array of strings.
	 */
	private static void convertByteArrayToStringsArray(byte[] bytes, String[] strings, int length) {
		byte[] string = new byte[length];
		for(int i = 0; i < strings.length; i++)
		{
			for(int j = 0; j < length; j++)
			{
				string[j] = bytes[length * i + j];
				if(string[j] == '\u0000') {
					string[j] = ' ';
				}
			}

			strings[i] = new String(string).trim();
		}
	}

	private void checkError(int errorCode) throws Exception {
		if (errorCode != IEOK) {
			String message = odserr(errorCode);
			message = "ODS: " + message.trim();
			throw new Exception(message);
		}
	}

	private static void checkFiles(String[] fname) throws IOException {
		if(!new File(fname[0]).exists()) {
			throw new IOException("File doesn't exist: " + fname[0]);
		}
	}
}

