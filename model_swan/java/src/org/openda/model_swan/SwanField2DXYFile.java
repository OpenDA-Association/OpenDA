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

package org.openda.model_swan;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.XYTGridExchangeItem;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.model_swan.SwanParameters;
import org.openda.utils.Array;
import org.openda.utils.IMyObservable;
import org.openda.utils.IMyObserver;
import org.openda.exchange.timeseries.TimeUtils;

import java.io.*;
import java.text.ParseException;
import java.util.HashMap;

/**
 * Reading and writing vector components (XY) input/output files of SWAN, each in 2D field data format.
 * The data is made available as a DataObject for use in BlackBox wrappers for SWAN.
 * 
 * Require 3 arguments:
 * 1. name of wind or current file
 * 2. name of main swan input file
 * 3. "WIND" or "CURRENT"
 */
public class SwanField2DXYFile implements IDataObject,IMyObserver {

	//constants
	private static final double eps=1e-6;
	
	HashMap<String,ArrayExchangeItem> exchangeItems = new HashMap<String,ArrayExchangeItem>();

	//
	// meta data from main swan file
	//
	private String fileType;
	// geometry
	private int mXInp;
	private int mYInp;
	private double xInp;
	private double yInp;
	private double dXInp;
	private double dYInp;
	//time
	private double tStart;
	private double tStop;
	private double dt;
	private int nTimeLevel;
	// values
	private double valuesScaling;
	//formatting
	File swnFile=null;
	private String windFileName;
	private int nhedf;
	private int nhedt;
	private int nhedvec;
	private int idla;
	private boolean isField2DXYSeries;
	//private ArrayList<String> listWindFiles = new ArrayList<String>();
	private boolean changed=false;

	public void initialize(File workingDir, String[] arguments) {
		String[] knownFileTypes = {"WIND", "CURRENT"};
		// check arguments
		if (arguments.length != 2) {
			throw new RuntimeException("Expected two arguments: 1. SWAN input *.swn file name, 2. the 2D-field-XY type: " +
					knownFileTypes[0]+", "+knownFileTypes[1]);
		}
		fileType = arguments[1]; //Wind and current use same format in SWAN
		if (fileType.toUpperCase().contentEquals(knownFileTypes[0])) {
			// ok
		}else if(fileType.toUpperCase().contentEquals(knownFileTypes[1])) {
			throw new RuntimeException("Field2DXY file type "+fileType+" is not yet implemented");
		}else{
			throw new RuntimeException("Field2DXY file is not of known types: "+fileType+". Known types are: " +
					knownFileTypes[0]+", "+knownFileTypes[1]);
		}

		// read metadata from swan-file
		swnFile = new File(workingDir,arguments[0]);
		if (!(swnFile.exists())) {
			throw new RuntimeException("Input file does not exist: " + swnFile.getAbsolutePath());
		}
		getMetaDataFromSwanFile(swnFile);

		//read values
		File windFile = new File(swnFile.getParentFile(),windFileName);
		Array xValues = new Array(new int[]{nTimeLevel,mYInp,mXInp}); 
		Array yValues = new Array(new int[]{nTimeLevel,mYInp,mXInp}); 
		if(isField2DXYSeries){
			SwanGridFileIo.readVectorNonStationarySeries(xValues, yValues, windFile, idla, mXInp, mYInp, nTimeLevel, nhedf, nhedt, nhedvec);
		}else{
			SwanGridFileIo.readVectorNonStationary(xValues, yValues, windFile, idla, mXInp, mYInp, nTimeLevel, nhedf, nhedt, nhedvec);
		}
		xValues.scale(valuesScaling);
		yValues.scale(valuesScaling);

		// create exchangeItems
		XYTGridExchangeItem windX = new XYTGridExchangeItem("wind.x", Role.InOut);
		IArray xGrid = Array.Range1(this.xInp,this.mXInp,this.dXInp);
		IArray yGrid = Array.Range1(this.yInp,this.mYInp,this.dYInp);
		windX.setXYGrid(xGrid, yGrid);
		IArray timesGrid = Array.Range1(this.tStart,this.nTimeLevel,this.dt);
		windX.setTimes(timesGrid);
		IQuantityInfo qInfo = new QuantityInfo("x_wind", "m/s");
		windX.setQuantityInfo(qInfo);
		windX.setArray(xValues);
		windX.addObserver(this);
		this.exchangeItems.put("wind.x", windX);
		
		XYTGridExchangeItem windY = new XYTGridExchangeItem("wind.y", Role.InOut);
		xGrid = Array.Range1(this.xInp,this.mXInp,this.dXInp);
		yGrid = Array.Range1(this.yInp,this.mYInp,this.dYInp);
		windY.setXYGrid(xGrid, yGrid);
		timesGrid = Array.Range1(this.tStart,this.nTimeLevel,this.dt);
		windY.setTimes(timesGrid);
		qInfo = new QuantityInfo("y_wind", "m/s");
		windY.setQuantityInfo(qInfo);
		windY.setArray(yValues);
		windY.addObserver(this);
		this.exchangeItems.put("wind.y", windY);
	}


	public void getMetaDataFromSwanFile(File swnFile){
		SwanParameters swanParams = new SwanParameters(swnFile);
		// values
		valuesScaling = swanParams.getFacWind();
		//geometry info
		mXInp = swanParams.getMXInpWind(); //TODO Current?
		mYInp = swanParams.getMYInpWind();
		xInp = swanParams.getXInpWind();
		yInp = swanParams.getYInpWind();
		dXInp = swanParams.getDXInpWind();
		dYInp = swanParams.getDYInpWind();
		double angle = swanParams.getAngleInpWind();
		if(Math.abs(angle)>eps){
			throw new RuntimeException("Only Alpha is 0 is supported for swan input grids");
		}
		// time info
		String tStart1 = swanParams.getTStartWind();
		String dtField2DXY = swanParams.getDtWind();
		String dtUnitField2DXY = swanParams.getDtUnitWind();
		String tStop1 = swanParams.getTStopWind();
		try {
			tStart = TimeUtils.date2Mjd(tStart1, "yyyyMMdd.HHmm");
			tStop  = TimeUtils.date2Mjd(tStop1, "yyyyMMdd.HHmm");
		} catch (ParseException e) {
			throw new RuntimeException("Could not parse timeframe for wind in swan input file:"+tStart1+","+tStop1);
		}
		try{
			dt = Double.parseDouble(dtField2DXY);
		}catch (Exception e) {
			throw new RuntimeException("Could not parse timestep for wind in swan input file:"+dtField2DXY);
		}
		if (dtUnitField2DXY.equalsIgnoreCase("SEC")) {
			dt = dt/24./60./60.;
		} else if (dtUnitField2DXY.equalsIgnoreCase("MIN")) {
			dt = dt/24./60.;
		} else if (dtUnitField2DXY.equalsIgnoreCase("HR")) {
			dt = dt/24.;
		} else if (dtUnitField2DXY.equalsIgnoreCase("DAY")) {
			//dt = dt;
		} else {
			throw new RuntimeException("Unit of time interval is not available in *.swn.");
		}
		this.nTimeLevel = (int) (Math.round((tStop-tStart)/dt) +1);
		
		// file formatting
		idla = swanParams.getIDLAWind();
	    nhedf = swanParams.getNHEDFWind();
	    nhedt = swanParams.getNHEDTWind();
		nhedvec = swanParams.getNHEDVECWind();
		isField2DXYSeries = swanParams.isWindSeries();
		windFileName = swanParams.getFileNameWind();
	}

	
	public String[] getExchangeItemIDs() {
		int nKeys = this.exchangeItems.keySet().size();
		String result[] = new String[nKeys];
		int i=0;
		for(String key : this.exchangeItems.keySet()){
			result[i] = key;
			i++;
		}
		return result;
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	public void finish() {
		// TODO write if data was changed
		if(this.changed){
			XYTGridExchangeItem windX = (XYTGridExchangeItem)this.exchangeItems.get("wind.x");
			IArray xValues=windX.getValues();
			XYTGridExchangeItem windY = (XYTGridExchangeItem)this.exchangeItems.get("wind.y");
			IArray yValues=windY.getValues();
			File windFile = new File(this.swnFile.getParentFile(),windFileName);
			String vectorHeaders[] = new String[]{"u","v"};
			String fileHeaders[] =null;
			if(nhedf>0){
				fileHeaders = new String[nhedf];
				for(int iline=0;iline<nhedf;iline++){
					fileHeaders[iline]="";
				}
			}
			double times[]=windX.getTimes();
			String timeHeaders[] =null;
			if(nhedt>0){
				timeHeaders = new String[nhedt*times.length];
				for(int itime=0;itime<times.length;itime++){
					for(int iline=0;iline<nhedt;iline++){
						if(iline!=0){
							timeHeaders[iline+itime*nhedt]="";
						}else{
							timeHeaders[iline+itime*nhedt]=""+times[itime]
									+" "+TimeUtils.mjdToString(times[itime]);							
						}
					}
				}
			}
			if(isField2DXYSeries){
				String includeNames[]=new String[times.length];
				for(int itime=0;itime<times.length;itime++){
					includeNames[itime]="wind_time_"+TimeUtils.mjdToString(times[itime])+".GRD";
				}
				SwanGridFileIo.writeVectorNonStationarySeries(xValues, yValues, windFile, 
						includeNames, idla, fileHeaders, timeHeaders, vectorHeaders);
			}else{
				SwanGridFileIo.writeVectorNonStationary(xValues, yValues, windFile, idla, 
						fileHeaders, timeHeaders, vectorHeaders);				
			}
		}
	}


	@Override
	public void update(IMyObservable observable, Object arg) {
		this.changed=true;
	}

}
