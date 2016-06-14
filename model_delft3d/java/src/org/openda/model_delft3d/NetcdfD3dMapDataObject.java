/* OpenDA v2.3
* Copyright (c) 2016 OpenDA Association
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
package org.openda.model_delft3d;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.ITimeInfo;
import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriteable;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * Data object voor D3D map file
 */
public class NetcdfD3dMapDataObject implements IDataObject {

	NetcdfFile netcdfFile = null;
	D3DBinRestartFile binRestartFile = null;

	private LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	private LinkedHashMap<String, Variable> timeDependentVars = new LinkedHashMap<>();
	private Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();

	double[] timesInNetcdfFile = null;

	private String[] keyVariables = {"S1","R1","V1","U1"};

	int timeDimensionIndex = -1;
	int lstsciDimensionIndex = -1;
	int kmaxOutRestrDimensionIndex = -1;
	private File netcdfFilePath;
	private File workingDir;
	private String[] arguments;
	private File restartFilePath;

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 1 && arguments.length != 3) {
			throw new RuntimeException("NetcdfD3dHisDataObject expects one or three argument: netcdfFileName [bin-restart-file + targetTime] (relative to working dir)");
		}

		File netcdfFilePath = new File(workingDir, arguments[0]);
		this.netcdfFilePath = netcdfFilePath;
		this.workingDir = workingDir;
		this.arguments = arguments;
		try {
			netcdfFile = new NetcdfFile(netcdfFilePath.getAbsolutePath());
//			netcdfFileWrite = new NetcdfFileWriteable();
		} catch (IOException e) {
			throw new RuntimeException("NetcdfD3dMapDataObject could not open netcdf file " + netcdfFilePath.getAbsolutePath());
		}
		readNetCdfVariables();

		if (arguments.length == 3) {
//			File restartFilePath = new File(workingDir, arguments[1]); // compuse name for prefix + target
			File restartFilePath = new File(this.workingDir, "tri-rst." + this.arguments[0].substring(5,this.arguments[0].length()-3) + "." +
					this.arguments[2].substring(0,8) + "." + this.arguments[2].substring(8,12) + "00");
			this.restartFilePath = restartFilePath;

			if (restartFilePath.exists()) {

				File restartFileIn = new File(this.workingDir, "tri-rst." + this.arguments[1] + ".000000");
				// Place copy of file
				try {
					BBUtils.copyFile(this.restartFilePath, restartFileIn);
				} catch (IOException e) {
					throw new RuntimeException("NetcdfD3dMapDataObject could not copy " +
							restartFilePath.getAbsolutePath() + " to " + restartFileIn);
				}

				int mMax = netcdfFile.findDimension("M").getLength();
				int nMax = netcdfFile.findDimension("N").getLength();
				int nLay = netcdfFile.findDimension("K_LYR").getLength();
				int nSubstances = netcdfFile.findDimension("LSTSCI").getLength();
				binRestartFile = new D3DBinRestartFile(restartFileIn, mMax, nMax, nLay, nSubstances);
				binRestartFile.open();
			}
		}
	}

	public void finish() {
		try {
			netcdfFile.close();
			if (binRestartFile != null) {
				binRestartFile.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		if (role == IExchangeItem.Role.Output) {
			// all items
			return getExchangeItemIDs();
		} else {
			// todo: filter onvariables that can be written
			return getExchangeItemIDs();
		}
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	private void readNetCdfVariables() {

		for (Variable variable : this.netcdfFile.getVariables()) {

			if (Arrays.asList(keyVariables).contains(variable.getName())) {

				Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
				if (timeVariable == null) {
					throw new RuntimeException("NetcdfD3dHisDataObject: no time axis for " + variable.getName() + ", file: " + netcdfFile.getLocation());
				}
				timeDimensionIndex = variable.findDimensionIndex(timeVariable.getName());
				timeDependentVars.put(variable.getName(), variable);

				// if this is the first time dependent variable: read the times
				if (timesInNetcdfFile == null) {
					ucar.ma2.Array timesArray;
					try {
						timesArray = timeVariable.read();
					} catch (IOException e) {
						throw new RuntimeException("NetcdfD3dMapDataObject could not read time variable " + timeVariable.getName() +
								"from netcdf file " + netcdfFile.getLocation());
					}
					timesInNetcdfFile = (double[]) timesArray.get1DJavaArray(double.class);
				}

				// get the number of spatial dimensions
				// one of the dimensions was for time
				List<Dimension> dimensions = variable.getDimensions();
				int nonTimeDimensions = dimensions.size() - 1;
				if (nonTimeDimensions != 4 && variable.getName().equalsIgnoreCase("R1")) {
					throw new RuntimeException("NetcdfD3dHisDataObject: #dims for R1 should be four, file: " + netcdfFile.getLocation());
				}

				int layerCount = 0;
				for (int i = 0; i < dimensions.size(); i++) {
					Dimension dimension = dimensions.get(i);
					if (variable.getName().equalsIgnoreCase("R1")){
						if (dimension.getName().equalsIgnoreCase("LSTSCI")) {
							lstsciDimensionIndex = i;
							if (dimension.getLength() != 1) {
								throw new RuntimeException("NetcdfD3dHisDataObject: #R1 != 1 is not supported (temp. or salt), file: "
										+ netcdfFile.getLocation());
							}
						}
					}
					if (!variable.getName().equalsIgnoreCase("S1")) {
						if (dimension.getName().equalsIgnoreCase("KMAXOUT_RESTR")) {
							kmaxOutRestrDimensionIndex = i;
							if (dimension.getLength() < 0) {
								throw new RuntimeException("NetcdfD3dHisDataObject: could not read number of layers, file: "
										+ netcdfFile.getLocation());
							}
							layerCount = dimension.getLength();
						}
					}

				}

				if (lstsciDimensionIndex == 0 || kmaxOutRestrDimensionIndex == 0 ) {
					throw new RuntimeException("NetcdfD3dMapDataObject: dims not available, file: "
							+ netcdfFile.getLocation());
				}

				ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);
				// for memory reasons, for the time being we only keep the last time step in the exchange time
				// so adjust the time info
				double[] times = timeInfo.getTimes();
				timeInfo = new TimeInfo(new double[]{times[times.length-1]});
				IExchangeItem exchangeItem = new NetcdfD3dMapExchangeItem(variable.getName(), this, timeInfo);
				this.exchangeItems.put(exchangeItem.getId(), exchangeItem);

			}
		}
	}

	public double[] getExchangeItemValues(String varName) {

		// find variable
		Variable variable = this.netcdfFile.findVariable(varName);

		int[] origin = createOrigin(variable);
		int[] sizeArray = variable.getShape();

		//select whole 2D or 3D for last time step. Due to huge sizes if we read all time steps I decided for now to keep only the necessary
		ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);
		int LastTimeIndex = timeInfo.getTimes().length;
		origin[timeDimensionIndex] = LastTimeIndex-1;
		sizeArray[timeDimensionIndex] = 1;

//		origin[timeDimensionIndex] = 0;
		if (variable.getName().equalsIgnoreCase("R1")) {
			origin[lstsciDimensionIndex] = 0; //Only a single constituent possible for now, needs to be changed
		}
		if (!variable.getName().equalsIgnoreCase("S1")) {
			origin[kmaxOutRestrDimensionIndex] = 0;
//			sizeArray[kmaxOutRestrDimensionIndex] = 1;
		}
		return NetcdfUtils.readSelectedData(variable, origin, sizeArray,-1);
	}

	public static int[] createOrigin(Variable var) {
		int dimensionCount = var.getDimensions().size();
		int[] origin = new int[dimensionCount];
		Arrays.fill(origin, 0);
		return origin;
	}

	public void setExchangeItemValues(String varName, double[] values) {
		if (binRestartFile != null) {
//			if (varName.equals("R1")) {
				binRestartFile.write(varName, values);
//			}
			return;
		}

		// find variable
		Variable variable = this.netcdfFile.findVariable(varName);

		int[] origin = createOrigin(variable);
		int[] sizeArray = variable.getShape();

		//select whole 2D or 3D for last time step. Due to huge sizes if we read all time steps I decided for now to keep only the necessary
		ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);
		int LastTimeIndex = timeInfo.getTimes().length;
		origin[timeDimensionIndex] = LastTimeIndex-1;
		sizeArray[timeDimensionIndex] = 1;

//		origin[timeDimensionIndex] = 0;
		if (variable.getName().equalsIgnoreCase("R1")) {
			origin[lstsciDimensionIndex] = 0; //Only a single constituent possible for now, needs to be changed
		}
		if (!variable.getName().equalsIgnoreCase("S1")) {
			origin[kmaxOutRestrDimensionIndex] = 0;
//			sizeArray[kmaxOutRestrDimensionIndex] = 1;
		}
		NetcdfFileWriteable netcdfWriteFile = null;
		try {
			netcdfWriteFile = NetcdfFileWriteable.openExisting(netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
		}
		NetcdfUtils.writeSelectedData(netcdfWriteFile,variable, origin, sizeArray, values);
		try {
			netcdfWriteFile.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
