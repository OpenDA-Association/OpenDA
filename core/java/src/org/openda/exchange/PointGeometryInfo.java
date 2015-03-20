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
}
