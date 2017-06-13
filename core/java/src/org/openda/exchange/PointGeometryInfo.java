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
package org.openda.exchange;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IQuantityInfo;
import org.openda.utils.Array;

/**
 * This class exists solely for convenience of creation of the ArrayGeometry for a single point.
 * The coordinates are assumed to be lat lon in degrees. If this is not the case metadata will be
 * wrong, but no conversion will be performed in this class, although it is uncertain what will
 * happen elsewhere.
 * In addition shorter convenient getters are available too.
 * 
 * @author verlaanm
 *
 */
public class PointGeometryInfo extends ArrayGeometryInfo {
	private String location="";

	/**
	 * Constructor for a point.
	 * @param longitude
	 * @param latitude
	 * @param height
	 */
	public PointGeometryInfo(double longitude, double latitude, double height){
		IArray latitudeArray = new Array(new double[]{latitude});
		int[] latitudeValueIndices=null;
		IQuantityInfo latitudeQuantityInfo = new QuantityInfo("latitude", "degrees_north");
		IArray longitudeArray = new Array(new double[]{longitude});
		int[] longitudeValueIndices=null;
		IQuantityInfo longitudeQuantityInfo = new QuantityInfo("longitude", "degrees_east");
		IArray heightArray = new Array(new double[]{height});
		int[] heightValueIndices=null; 
		IQuantityInfo heightQuantityInfo = new QuantityInfo("height", "m");
		this.setInfo(latitudeArray, latitudeValueIndices, latitudeQuantityInfo, 
			  longitudeArray, longitudeValueIndices, longitudeQuantityInfo,
			  heightArray, heightValueIndices, heightQuantityInfo);
	}
	
	public double getLatitude(){
		return this.getLatitudeArray().getValueAsDouble(0);
	}

	public double getLongitude(){
		return this.getLongitudeArray().getValueAsDouble(0);
	}
	
	public double getHeight(){
		return this.getHeightArray().getValueAsDouble(0);
	}
	
	public String getLocation(){
		return this.location;
	}
	
	public void setLocation(String location){
		this.location=location;
	}
}
