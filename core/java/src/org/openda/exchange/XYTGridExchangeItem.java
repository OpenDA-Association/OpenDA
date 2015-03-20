package org.openda.exchange;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayGeometryInfo;
import org.openda.interfaces.IArrayTimeInfo;

/**
 * Convenience class for 2D spatial data on a structured grid, that is possibly time-dependent.
 * All data is stored in and manipulated by the ArrayExchangeItem. This class just simplifies the
 * interface for this special case.
 * @author verlaanm
 *
 */
public class XYTGridExchangeItem extends ArrayExchangeItem{

	public XYTGridExchangeItem(String id, Role role){
		super(id, role);
	}
	
	public IArray getXGrid(){
		return ((IArrayGeometryInfo) this.getGeometryInfo()).getLongitudeArray();
	}
	
	public IArray getYGrid(){
		return ((IArrayGeometryInfo) this.getGeometryInfo()).getLatitudeArray();
	}

	public void setXYGrid(IArray x, IArray y){
		int[] latitudeValueIndices;
		int[] longitudeValueIndices;
		if(x.getDimensions().length==1){
			latitudeValueIndices = new int[]{1};
			longitudeValueIndices = new int[]{2};
		}else{ //2d-grid
			latitudeValueIndices = new int[]{2,1};
			longitudeValueIndices = new int[]{2,1};			
		}
		QuantityInfo latitudeQuantityInfo = new QuantityInfo("latitude", "degree_north");
		QuantityInfo longitudeQuantityInfo = new QuantityInfo("longitude", "degree_east");
		IArrayGeometryInfo geometry = new ArrayGeometryInfo(y, latitudeValueIndices, latitudeQuantityInfo, 
				x, longitudeValueIndices, longitudeQuantityInfo, null, null, null);
		this.setGeometryInfo(geometry);
	}
	
	public IArray getTimesArray(){
		return ((IArrayTimeInfo) this.getTimeInfo()).getTimeArray();
	}
	
	public void setTimes(IArray times){
		this.setTimeInfo(new TimeInfo(times));
	}
	
	public String getQuantityId(){
		return this.getQuantityInfo().getQuantity();
	}
	
	public String getUnitsId(){
		return this.getQuantityInfo().getUnit();		
	}
	
	public void setQuantityAndUnit(String quantityId, String unitId){
		this.setQuantityInfo(new QuantityInfo(quantityId, unitId));
	}
}
