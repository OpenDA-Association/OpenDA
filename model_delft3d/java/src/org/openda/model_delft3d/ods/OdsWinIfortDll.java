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
package org.openda.model_delft3d.ods;
import com.sun.jna.Library;

/**
 * Jna interface description for ODS native library
 */
public interface OdsWinIfortDll extends Library  {

	void odserr(int error, byte[] s);

	void getdim(byte[] fname, int[] itype, byte[] dimType, int[] pardep,
				int[] timdep, int[] locdep, int[] ndim, int[] error, String option);

	void getpar(byte[] fnameBytes, int[] itype, String pardef, int[] maxdef, int[] timdep, int[] locdep,
				 int[] maxlst, int[] lang, byte[] parnameBytes, byte[] parunitBytes,
				 int[] partype, int[] parcode, int[] nrlst, int[] error, String option);


	void getloc(byte[] fname, int[] ftype, String locdef, int[] maxdef,
                     int[] pardep, int[] timdep, int[] maxlst, byte[] locname, int[] loctype, int[] locid,
                     int[] nrlst, int[] error, String option);

	void getgrd(byte[] fname, int[] ftype, int[] loc_index, int[] lgrid, int[] noCells,
				 int[] gisType, int[] error);

    void gettme(byte[] fname, int[] ftype, double[] timdef, int[] maxdef, int[] pardep, int[] timdep,
				 int[] maxlst, double[] time, int[] timetype, int[] nrlst, int[] error, String option);

    void getmat(byte[] fname, int[] ftype, int[] pardep,
				int[]loc_index, double[] timdef, float[] misval,
				int[] lang, int[] nrlst, float[] data,
				int[] error, String option);

	void C_closal(byte[] fnameBytes, int[] error);
}
