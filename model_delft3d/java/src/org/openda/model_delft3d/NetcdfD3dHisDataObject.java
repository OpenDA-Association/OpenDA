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
package org.openda.model_delft3d;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.*;
import ucar.ma2.Array;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * Data object voor D3D map file
 */
public class NetcdfD3dHisDataObject implements IDataObject {

	NetcdfFile netcdfFile = null;

	private LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	private LinkedHashMap<String, Variable> timeDependentVars = new LinkedHashMap<>();

	ITimeInfo timeInfoForAllTimeDepVars = null;
	double[] timesInNetcdfFile = null;

	int timeDimensionIndex = -1;
	int lstsciDimensionIndex = -1;
	int kmaxOutRestrDimensionIndex = -1;
	int statDimensionIndex = -1;

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 1) {
			throw new RuntimeException("NetcdfD3dHisDataObject expects one argument: netcdfFileName (relative to working dir)");
		}

		File netcdfFilePath = new File(workingDir, arguments[0]);
		try {
			netcdfFile = NetcdfFile.open(netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("NetcdfD3dHisDataObject could not open netcdf file " + netcdfFilePath.getAbsolutePath());
		}

		readNetCdfVariables();
	}

	public void finish() {
		try {
			netcdfFile.close();
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

		//in most netcdfFiles the time and spatial coordinate variables are shared between multiple data variables.
		//Therefore cache timeInfo objects so that time coordinate variables are never read more than once.
		Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();

		// Loading of variable containing stations names
		Variable nameVariable = this.netcdfFile.findVariable("NAMST");
		String[] stationNames;

		Array stationNamesChar = null;
		try {

			stationNamesChar = nameVariable.read();

		} catch (IOException e) {
			e.printStackTrace();
		}
		// Extraction of station names
		char[] nameCharArray = (char[]) stationNamesChar.get1DJavaArray(char.class);
		int stringLength=stationNamesChar.getShape()[1];

		for (Variable variable : this.netcdfFile.getVariables()) {

			if (variable.getShortName().equalsIgnoreCase("GRO")) {

				Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
				if (timeVariable == null) {
					throw new RuntimeException("NetcdfD3dHisDataObject: no time axis for GRO, file: " + netcdfFile.getLocation());
				}
				timeDimensionIndex = variable.findDimensionIndex(timeVariable.getShortName());
				timeDependentVars.put(variable.getShortName(), variable);
				ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);

				// if this is the first time dependent variable: read the times
				if (timesInNetcdfFile == null) {
					ucar.ma2.Array timesArray;
					try {
						timesArray = timeVariable.read();
					} catch (IOException e) {
						throw new RuntimeException("NetcdfD3dHisDataObject could not read time variable " + timeVariable.getShortName() +
								"from netcdf file " + netcdfFile.getLocation());
					}
					timesInNetcdfFile = (double[]) timesArray.get1DJavaArray(double.class);
				}
				timeInfoForAllTimeDepVars = new TimeInfo(timesInNetcdfFile);

				// get the number of spatial dimensions
				// one of the dimensions was for time
				List<Dimension> dimensions = variable.getDimensions();
				int nonTimeDimensions = dimensions.size() - 1;
				if (nonTimeDimensions != 3) {
					throw new RuntimeException("NetcdfD3dHisDataObject: #dims for GRO should be three, file: " + netcdfFile.getLocation());
				}

				int statCount = 0;
				int layerCount = 0;
				for (int i = 0; i < dimensions.size(); i++) {
					Dimension dimension = dimensions.get(i);
					if (dimension.getShortName().equalsIgnoreCase("LSTSCI")) {
						lstsciDimensionIndex = i;
						if (dimension.getLength() != 1) {
							throw new RuntimeException("NetcdfD3dHisDataObject: #GRO != 1 is not supported (temp. or salt), file: "
									+ netcdfFile.getLocation());
						}
					}
					if (dimension.getShortName().equalsIgnoreCase("KMAXOUT_RESTR")) {
						kmaxOutRestrDimensionIndex = i;
						if (dimension.getLength() < 0) {
							throw new RuntimeException("NetcdfD3dHisDataObject: could not read number of layers, file: "
									+ netcdfFile.getLocation());
						}
						layerCount = dimension.getLength();
					}
					if (dimension.getShortName().equalsIgnoreCase("NOSTAT")) {
						statDimensionIndex = i;
						if (dimension.getLength() < 1) {
							throw new RuntimeException("NetcdfD3dHisDataObject: no station found, file: "
									+ netcdfFile.getLocation());
						}
						statCount = dimension.getLength();
					}
				}

				if (lstsciDimensionIndex == 0 || kmaxOutRestrDimensionIndex == 0 || statDimensionIndex == 0) {
					throw new RuntimeException("NetcdfD3dHisDataObject: dims not available, file: "
							+ netcdfFile.getLocation());

				}

				stationNames = new String[statCount];
				for (int stat = 0; stat < statCount ; stat++) {
					for (int layer = 0; layer < layerCount ; layer++) {

						// Building station names (strings) from list of chars
						String statName = String.valueOf(java.util.Arrays.copyOfRange(nameCharArray,(stat*stringLength),((stat+1)*stringLength-1))).replaceAll("\\s","");
						IExchangeItem exchangeItem = new NetcdfD3dHisExchangeItem(
								variable.getShortName(), statName, stat, layer, this, timeInfo);
						this.exchangeItems.put(exchangeItem.getId(), exchangeItem);
					}
				}
			}
		}
	}

	public double[] getExchangeItemValues(String varName, int stationIndex, int layerIndex) {

		Variable variable = this.netcdfFile.findVariable(varName);

		int[] origin = createOrigin(variable);
		int[] sizeArray = variable.getShape();

		//select only the given station and layer
		origin[timeDimensionIndex] = 0;
		origin[lstsciDimensionIndex] = 0; //Only a single constituent possible for now, if several, arguments need to be changed
		origin[kmaxOutRestrDimensionIndex] = layerIndex;
		origin[statDimensionIndex] = stationIndex;

		//only one station and layer
		sizeArray[kmaxOutRestrDimensionIndex] = 1;
		sizeArray[statDimensionIndex] = 1;
		return NetcdfUtils.readSelectedData(variable, origin, sizeArray, -1);

	}

	//TB: this static method is to be called everytime the Map file is written, so that the History file is also updated accordingly
	public static void hisFileWriter(String varName, double[] values,File workingDir,String runID,int mMax,int nMax,int nLay) {

		//Names in the history files don't match the ones in the map file
		if (varName.equalsIgnoreCase("R1")) {
			varName = "GRO";
		}else if (varName.equalsIgnoreCase("U1")){
			varName = "ZCURU";
		}else if (varName.equalsIgnoreCase("V1")){
			varName = "ZCURV";
		}else if (varName.equalsIgnoreCase("S1")){
			varName = "ZWL";
			nLay = 1;
		}

		File netcdfFilePath = new File(workingDir, "trih-" + runID + ".nc");

		NetcdfFile netcdfHisFile;
		try {
			netcdfHisFile = NetcdfFile.open(netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("NetcdfD3dHisDataObject could not open netcdf file " + netcdfFilePath.getAbsolutePath());
		}

		Variable stationIndices = netcdfHisFile.findVariable("MNSTAT");

		Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(stationIndices, netcdfHisFile);
		int timeDimensionIndex = stationIndices.findDimensionIndex(timeVariable.getShortName());

		//int LastTimeIndex = timeInfo.getTimes().length;
		Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();
		ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(stationIndices, netcdfHisFile,timeInfoCache);
		int LastTimeIndex = timeInfo.getTimes().length;

		double[] stationsMN = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(stationIndices,timeDimensionIndex,LastTimeIndex-1,-1);
		int nStation = stationsMN.length/2;

		double[][][] Domain3D = from1dTo3dArray(values,mMax,nMax,nLay);

		// Extracting the stations from the 3D domain
		double[] stationValues = new double[nStation*nLay];
		for (int nstat = 0; nstat < nStation; nstat++){

			int mStat = (int) stationsMN[nstat*2];
			int nStat = (int) stationsMN[nstat*2+1];

			double[] stationArray = getStationArray(Domain3D,mStat,nStat,nLay);

			int k=0;
			for (int i = 0;i < stationArray.length;i++){
				stationValues[k*nStation+nstat] = stationArray[i];
				k++;
			}

		}

		// Writing StationValues into the netcdf his file:
		Variable variable = netcdfHisFile.findVariable(varName);
		int[] sizeArray = variable.getShape();
		int[] origin = createOrigin(variable);

		origin[timeDimensionIndex] = LastTimeIndex-1;
		sizeArray[timeDimensionIndex] = 1;

		int kmaxOutRestrDimensionIndex = -1;
		int lstsciDimensionIndex = -1;

		List<Dimension> dimensions = variable.getDimensions();
		for (int i = 0; i < dimensions.size(); i++) {
			Dimension dimension = dimensions.get(i);
			if (varName.equalsIgnoreCase("GRO")){
				if (dimension.getShortName().equalsIgnoreCase("LSTSCI")) {
					lstsciDimensionIndex = i;
					if (dimension.getLength() != 1) {
						throw new RuntimeException("NetcdfD3dHisDataObject: #R1 != 1 is not supported (temp. or salinity), file: "
								+ netcdfHisFile.getLocation());
					}
					origin[lstsciDimensionIndex] = 0; //Only a single constituent possible for now, needs to be changed
				}
			}
			if (!varName.equalsIgnoreCase("ZWL")) {
				if (dimension.getShortName().equalsIgnoreCase("KMAXOUT_RESTR")) {
					kmaxOutRestrDimensionIndex = i;
					if (dimension.getLength() < 0) {
						throw new RuntimeException("NetcdfD3dHisDataObject: could not read number of layers, file: "
								+ netcdfHisFile.getLocation());
					}
					origin[kmaxOutRestrDimensionIndex] = 0;
				}
			}

		}

		if (lstsciDimensionIndex == 0 || kmaxOutRestrDimensionIndex == 0 ) {
			throw new RuntimeException("NetcdfD3dHisDataObject: dims not available, file: "
					+ netcdfHisFile.getLocation());
		}


		NetcdfFileWriter netcdfFileWriter = null;
		try {
			netcdfFileWriter = NetcdfFileWriter.openExisting(netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
		}

		NetcdfUtils.writeSelectedData(netcdfFileWriter,variable, origin, sizeArray, stationValues);

		try {
			netcdfFileWriter.close();
			netcdfHisFile.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}


	public static int[] createOrigin(Variable var) {
		int dimensionCount = var.getDimensions().size();
		int[] origin = new int[dimensionCount];
		Arrays.fill(origin, 0);
		return origin;
	}

	private static double[][][] from1dTo3dArray(double[] oneDArray, int mMax, int nMax, int nLay){

		double Domain3D[][][] = new double[mMax][nMax][nLay];

		int k=0;
		for (int lay = 0; lay < nLay; lay++) {
			for (int m = 0; m < mMax; m++) {
				for (int n = 0; n < nMax; n++) {

					Domain3D[m][n][lay] = oneDArray[k];
					k++;

				}
			}
		}
		return Domain3D;
	}

	private static double[] getStationArray(double[][][] Domain3D,int mStat,int nStat,int nLay){

		double[] StationArray = new double[nLay];

		int k=0;
		for (int layer = 0; layer < nLay; layer++){

			StationArray[k] = Domain3D[mStat-1][nStat-1][layer];
			k++;
		}

		return StationArray;
	}

}
