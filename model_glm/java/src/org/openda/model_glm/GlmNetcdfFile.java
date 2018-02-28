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
package org.openda.model_glm;

/**
 *
 *
 *
 * Expected metadata for Netcdf should look something like:
 *
 * dimensions:
 *         STATION = 275 ;
 *         TIME = UNLIMITED ; // (434 currently)
 *         LEN_NAMWL = 80 ;
 *         N = 173 ;
 *         M = 201 ;
 * variables:
 *         double TIME(TIME) ;
 *                 TIME:missing_value = 9.96921e+36f ;
 *                 TIME:units = "minutes since 2006-03-01 00:00:00" ;
 *                 TIME:timezone = "MET" ;
 *         float SEP(TIME, N, M) ;
 *                 SEP:_FillValue = 9.96921e+036f ;
 *                 SEP:missing_value = 9.96921e+036f ;
 *                 SEP:positive = "up" ;
 *                 SEP:horizontal_datum_name = "local mean sealevel" ;
 *                 SEP:coordinates = "YZETA XZETA" ;
 *                 SEP:add_offset = 0.f ;
 *                 SEP:scale_factor = 1.f ;
 *                 SEP:standard_name = "sea_surface_height" ;
 *                 SEP:long_name = "waterlevel" ;
 *                 SEP:units = "m" ;
 *         float ZWL(TIME, STATION) ;
 *                 ZWL:_FillValue = 9.96921e+36f ;
 *                 ZWL:missing_value = 9.96921e+36f ;
 *                 ZWL:standard_name = "water level at wl stations" ;
 *                 ZWL:long_name = "water level at wl stations" ;
 *                 ZWL:units = "m" ;
 *         char NAMWL(STATION, LEN_NAMWL) ;
 *                 NAMWL:_FillValue = "" ;
 *                 NAMWL:missing_value = "" ;
 *                 NAMWL:standard_name = "wl station names" ;
 *                 NAMWL:long_name = "wl station names" ;
 *         int MWL(STATION) ;
 *                 MWL:_FillValue = -2147483647 ;
 *                 MWL:missing_value = -2147483647 ;
 *                 MWL:standard_name = "m-grid index water level stations" ;
 *                 MWL:long_name = "m-grid index water level stations" ;
 *         int NWL(STATION) ;
 *                 NWL:_FillValue = -2147483647 ;
 *                 NWL:missing_value = -2147483647 ;
 *                 NWL:standard_name = "n-grid index water level stations" ;
 *                 NWL:long_name = "n-grid index water level stations" ;
 *         float XZETA(N, M) ;
 *                 XZETA:_FillValue = 9.96921e+36f ;
 *                 XZETA:missing_value = 9.96921e+36f ;
 *                 XZETA:standard_name = "projection_x_coordinate" ;
 *                 XZETA:long_name = "x coordinate according to Dutch grid (RD)" ;
 *                 XZETA:units = "m" ;
 *         float YZETA(N, M) ;
 *                 YZETA:_FillValue = 9.96921e+36f ;
 *                 YZETA:missing_value = 9.96921e+36f ;
 *                 YZETA:standard_name = "projection_y_coordinate" ;
 *                 YZETA:long_name = "y coordinate according to Dutch grid (RD)" ;
 *                 YZETA:units = "m" ;
 *
 * // global attributes:
 *                 :Conventions = "CF-1.0" ;
 *                 :grid_type = "IRREGULAR" ;
 *                 :coordinate_system = "GEO" ;
 *                 :astro = "NO" ;
 *                 :history = "Created by SIDONIA data retrieval on 2009-03-19 15:12:30" ;
 *                 :title = "NetCDF created from SDS-file SDS-test" ;
 *                 :institution = "Rijksinstituut voor Kust en Zee / RIKZ" ;
 *                 :references = "http://www.rikz.nl/" ;
 *                 :source = "SDS-test experiment test" ;
 *                 :Comment = "none." ;
 * }
 *
 */

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.File;
import java.io.IOException;

public class GlmNetcdfFile implements IDataObject{
	File workingDir;
	String configString;
	String fileName = null;


	/**
	 * Settings for this class.
	 * @param workingDir working directory
	 * @param fileName filename pointing to netcdf generated bu getdata.pl
	 * Waqua uses times in minutes with respect to a reference date internally.
	 * @param arguments arguments[0] = referenceDate eg. "01 MAR 2006"
	 */
	public void initialize(File workingDir, String fileName, String[] arguments) {
        this.workingDir = workingDir;
        this.fileName = fileName;

        if (arguments != null && arguments.length > 0) {
            throw new RuntimeException("IoObject SimonaNetcdfFile does not expect any additional arguments");
        }

        // check file
		try{
			File inputFile = new File(workingDir,fileName);
			if(!inputFile.isFile()){
				throw new IOException("Can not find file"+inputFile);
			}
			this.fileName = inputFile.getCanonicalPath();
		}catch (Exception e) {
			System.out.println("Netcdf: trouble opening file "+ this.fileName);
		}

    }

    /**
     * Returns exchange Items from the netcdf file
     * For all waterlevel stations a TimeSeries exchange item is created
     * For the SEP variable a SimonaNetcdfFileExchangeItem is created to pass the netcdf file itself
     * @return IPrevExchangeItem[] list of exchangeItems
     */
    public TimeSeries[] getExchangeItems() {
    	TimeSeries[] result=null;
    	//WaquaNetcdfTimeSeriesFormatter waquaFormatter
    	//     = new WaquaNetcdfTimeSeriesFormatter();
    	//String timeSeriesIds[] = waquaFormatter.getTimeSeriesIds(this.fileName);
        //String mapsIds[] = waquaFormatter.getMapIds(this.fileName);
    	//result = new IPrevExchangeItem[timeSeriesIds.length + mapsIds.length]; MVL disabled maps
    	//result = new TimeSeries[timeSeriesIds.length];
        int i = 0;
    	//for(String id: timeSeriesIds) {
    	//	result[i++] = waquaFormatter.readTimeSeriesFromFile(this.fileName, id);
    	//}
        //for (String id: mapsIds) {
        //    result[i++] = waquaFormatter.readMapsFromFile(this.fileName, id);
        //}
		return result;
    }

    public void finish() {
        // no action needed
    }

	
	public void initialize(File workingDir, String[] arguments) {
		// TODO Auto-generated method stub
		
	}

	
	public String[] getExchangeItemIDs() {
		// TODO Auto-generated method stub
		return null;
	}

	
	public String[] getExchangeItemIDs(Role role) {
		// TODO Auto-generated method stub
		return null;
	}

	
	public TimeSeries getDataObjectExchangeItem(String exchangeItemID) {
		// TODO Auto-generated method stub
		return null;
	}
}
