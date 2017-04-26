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
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.*;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static java.lang.Math.abs;

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
	private String[] fictiveVariables = {"R1","V1","U1"};

	int timeDimensionIndex = -1;
	int lstsciDimensionIndex = -1;
	int kmaxOutRestrDimensionIndex = -1;
	private File netcdfFilePath;
	private int mMax;
	private int nMax;
	private int nLay;
	private File workingDir;
	private String runID;

	private double[] xCoords = null;
	private double[] yCoords = null;
	private double[] zCoords = null;

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 1 && arguments.length != 3) {
			throw new RuntimeException("NetcdfD3dMapDataObject expects one or three argument: netcdfFileName [bin-restart-file + targetTime] (relative to working dir)");
		}

		File netcdfFilePath = new File(workingDir, arguments[0]);
		this.netcdfFilePath = netcdfFilePath;
		this.workingDir = workingDir;
		this.runID = arguments[0].substring(5,arguments[0].length()-3);

		try {
			netcdfFile = NetcdfFile.open(netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("NetcdfD3dMapDataObject could not open netcdf file " + netcdfFilePath.getAbsolutePath());
		}

		if (netcdfFile.findDimension("SIG_LYR") != null) {
			throw new RuntimeException("NetcdfD3dMapDataObject: Sigma layers detected, only z-layers currently supported for Delft3D-FLOW!");
		}

		this.mMax = netcdfFile.findDimension("M").getLength();
		this.nMax = netcdfFile.findDimension("N").getLength();
		this.nLay = netcdfFile.findDimension("K_LYR").getLength();
		int nSubstances = netcdfFile.findDimension("LSTSCI").getLength();

		readNetCdfVariables();

		if (arguments.length == 3) {
			File restartFilePath = new File(workingDir, "tri-rst." + arguments[0].substring(5,arguments[0].length()-3) + "." +
					arguments[2].substring(0,8) + "." + arguments[2].substring(8,12) + "00");

			if (restartFilePath.exists()) {

				// TODO: instead of copying an existing restart file and modifying it, better to create one fully from the MapFile.
				// Avoids having to create a restart file at small time step even if not needed, also less dependent from user input.
				//File restartFileIn = new File(workingDir, "tri-rst." + arguments[1]);
				File restartFileIn = new File(workingDir, arguments[1]);
				// Place copy of file
				try {
					BBUtils.copyFile(restartFilePath, restartFileIn);
				} catch (IOException e) {
					throw new RuntimeException("NetcdfD3dMapDataObject could not copy " +
							restartFilePath.getAbsolutePath() + " to " + restartFileIn);
				}

				binRestartFile = new D3DBinRestartFile(restartFileIn, mMax, nMax, nLay, nSubstances);
				binRestartFile.open();
			}
		}
	}

	public void finish() {

		// Here the intelligence to go back to the right waterlevel (from the fictive one) is needed, before writing the states
		for (Variable variable : this.netcdfFile.getVariables()){
			String varName = variable.getShortName();
			if (Arrays.asList(fictiveVariables).contains(varName)) {
				ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, this.timeInfoCache);
				int LastTimeIndex = timeInfo.getTimes().length;
				double[] values = getExchangeItemValues(varName);
				// This is the critical method applying the intelligence:
				values = back2RealDomain(values,LastTimeIndex);
				// Might not be the most efficient as we write a second time into the files:
				writeExchangeItemValues(varName, values);
			}
		}
		try {
			netcdfFile.close();
			if (binRestartFile != null) {
				binRestartFile.close();
			}
		} catch (IOException e){
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
			// todo: filter on variables that can be written
			return getExchangeItemIDs();
		}
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	private void readNetCdfVariables() {

		for (Variable variable : this.netcdfFile.getVariables()) {

			if (Arrays.asList(keyVariables).contains(variable.getShortName())) {

				Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
				if (timeVariable == null) {
					throw new RuntimeException("NetcdfD3dMapDataObject: no time axis for " + variable.getShortName() + ", file: " + netcdfFile.getLocation());
				}
				this.timeDimensionIndex = variable.findDimensionIndex(timeVariable.getShortName());
				timeDependentVars.put(variable.getShortName(), variable);

				// if this is the first time dependent variable: read the times
				if (timesInNetcdfFile == null) {
					ucar.ma2.Array timesArray;
					try {
						timesArray = timeVariable.read();
					} catch (IOException e) {
						throw new RuntimeException("NetcdfD3dMapDataObject could not read time variable " + timeVariable.getShortName() +
								"from netcdf file " + netcdfFile.getLocation());
					}
					timesInNetcdfFile = (double[]) timesArray.get1DJavaArray(double.class);
				}

				// get the number of spatial dimensions
				// one of the dimensions was for time
				List<Dimension> dimensions = variable.getDimensions();
				int nonTimeDimensions = dimensions.size() - 1;
				if (nonTimeDimensions != 4 && variable.getShortName().equalsIgnoreCase("R1")) {
					throw new RuntimeException("NetcdfD3dMapDataObject: #dims for R1 should be four, file: " + netcdfFile.getLocation());
				}

				int layerCount = 0;
				for (int i = 0; i < dimensions.size(); i++) {
					Dimension dimension = dimensions.get(i);
					if (variable.getShortName().equalsIgnoreCase("R1")){
						if (dimension.getShortName().equalsIgnoreCase("LSTSCI")) {
							lstsciDimensionIndex = i;
							if (dimension.getLength() != 1) {
								throw new RuntimeException("NetcdfD3dMapDataObject: #R1 != 1 is not supported (temp. or salinity), file: "
										+ netcdfFile.getLocation());
							}
						}
					}
					if (!variable.getShortName().equalsIgnoreCase("S1")) {
						if (dimension.getShortName().equalsIgnoreCase("KMAXOUT_RESTR")) {
							kmaxOutRestrDimensionIndex = i;
							if (dimension.getLength() < 0) {
								throw new RuntimeException("NetcdfD3dMapDataObject: could not read number of layers, file: "
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

				// Extracting geometry information only once
				if (this.xCoords == null | this.yCoords == null | this.zCoords == null) {
					Variable variableX = this.netcdfFile.findVariable("XZ");
					Variable variableY = this.netcdfFile.findVariable("YZ");
					Variable variableZ = this.netcdfFile.findVariable("ZK_LYR");

					int[] originXY = createOrigin(variableX);
					int[] sizeArrayXY = variableX.getShape();
					int[] originZ = createOrigin(variableZ);
					int[] sizeArrayZ = variableZ.getShape();

					this.xCoords = NetcdfUtils.readSelectedData(variableX, originXY, sizeArrayXY, -1);
					this.yCoords = NetcdfUtils.readSelectedData(variableY, originXY, sizeArrayXY, -1);
					this.zCoords = NetcdfUtils.readSelectedData(variableZ, originZ, sizeArrayZ, -1);
				}

				IGeometryInfo geometryInfo;
				if (variable.getName().equalsIgnoreCase("S1")) {
					geometryInfo = new NetcdfD3dMapExchangeItemGeometryInfo(this.xCoords, this.yCoords, null);
				} else {
					geometryInfo = new NetcdfD3dMapExchangeItemGeometryInfo(this.xCoords, this.yCoords, this.zCoords);
				}
				IExchangeItem exchangeItem = new NetcdfD3dMapExchangeItem(variable.getName(), this, timeInfo, geometryInfo);
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
		if (variable.getShortName().equalsIgnoreCase("R1")) {
			origin[lstsciDimensionIndex] = 0; //Only a single constituent possible for now, needs to be changed
		}
		if (!variable.getShortName().equalsIgnoreCase("S1")) {
			origin[kmaxOutRestrDimensionIndex] = 0;
//			sizeArray[kmaxOutRestrDimensionIndex] = 1;
		}

		double[] domainData =  NetcdfUtils.readSelectedData(variable, origin, sizeArray,-1);

		//This calls the intelligence to expand the real to a fictive (full) domain
		if (Arrays.asList(fictiveVariables).contains(variable.getShortName())){
			domainData = expandDomain(domainData,LastTimeIndex);
		}
		return domainData;
	}

	public double[] expandDomain(double[] initialDomain,int LastTimeIndex){
		//This method implements the intelligence to expand the temperature and velocities variables of the real
		//domain to a fictive one, which fill the empty upper cells of the computational grid

		//Variable KFU = this.netcdfFile.findVariable("KFU");
		//Variable KFV = this.netcdfFile.findVariable("KFV");
		Variable KCS = this.netcdfFile.findVariable("KCS");

		//double[] KFU1Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(KFU,timeDimensionIndex,LastTimeIndex-1,-1);
		//double[] KFV1Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(KFV,timeDimensionIndex,LastTimeIndex-1,-1);
		//double[] KCS1Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(KCS,timeDimensionIndex,LastTimeIndex-1,-1);
		double[] KCS1Darray = NetcdfUtils.readSelectedData(KCS, createOrigin(KCS), KCS.getShape(), -1);

		// Here I convert the 1D array into a 3D one, which helps in the thinking for expanding the upper cells
		double[][][] Domain3D = from1dTo3dArray(initialDomain);

		//For debug
		//printNcField(variable.getName(), mMax, nMax, 20, Domain3D);

		// Filling the empty upper layers by the first filled one
		int k = 0;
		for (int m = 0; m < mMax; m++) {
			for (int n = 0; n < nMax; n++) {

				//		if (KFU1Darray[k] == 1 | KFV1Darray[k] == 1) {
				if (KCS1Darray[k] == 1) {

					// Down loop to reach the first filled cell
					int layCount = nLay - 1;
					while (Domain3D[m][n][layCount] == -999.0 & layCount > 0) {
						layCount--;
					}

					// Up loop to fill back the empty domain
					int UpLayCount = layCount;
					while (UpLayCount != nLay) {
						Domain3D[m][n][UpLayCount] = Domain3D[m][n][layCount];
						UpLayCount++;
					}

				}
				k++;
			}
		}

		//For debug
		//printNcField(variable.getShortName(), mMax, nMax, 25, Domain3D);

		// Back to a 1D array
		double[] expandedDomain = from3dTo1dArray(Domain3D,initialDomain.length);

		return expandedDomain;
	}

	// For debug
//	private void printNcField(String varName, int mMax, int nMax, int nLay, double[][][] doubles) {
//		System.out.println("Var: " + varName);
//		for (int lay=0; lay < nLay; lay++) {
//			System.out.println("LAYER: " + lay);
//			for (int n=0; n < nMax; n++) {
//				System.out.print(n);
//				for (int m=0; m < mMax; m++) {
//					int index = n + nMax * m + mMax * nMax * lay;
//					System.out.print(String.format(",%f", Double.isNaN(doubles[m][n][lay]) ? 0 : doubles[m][n][lay] ));
//				}
//				System.out.println("");
//			}
//		}
//	}

	public void setExchangeItemValues(String varName, double[] values) {

		writeExchangeItemValues(varName, values);

	}

	public void writeExchangeItemValues(String varName, double[] values) {

		// Writing to the binary restart file, after the algorithm operations
		if (binRestartFile != null) {
			binRestartFile.write(varName, values);
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

		if (variable.getShortName().equalsIgnoreCase("R1")) {
			origin[lstsciDimensionIndex] = 0; //Only a single constituent possible for now, needs to be changed
		}
		if (!variable.getShortName().equalsIgnoreCase("S1")) {
			origin[kmaxOutRestrDimensionIndex] = 0;
		}

		NetcdfFileWriter netcdfFileWriter= null;

		try {
			netcdfFileWriter = NetcdfFileWriter.openExisting(this.netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
		}

		NetcdfUtils.writeSelectedData(netcdfFileWriter,variable, origin, sizeArray, values);

		try {
			netcdfFileWriter.close();
			//if (binRestartFile != null) {
				//binRestartFile.close();
			//}
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Writing the history file with the new values contained in the map file
		NetcdfD3dHisDataObject.hisFileWriter(varName,values,workingDir,runID,mMax,nMax,nLay);

	}

	public double[] back2RealDomain(double[] expandedDomain, int LastTimeIndex) {
		//This method implements the intelligence to set temperature and flow velocities domain values back to the right (new) waterlevel,
		//which has been computed, from the fictive (full) domain

		//Variable KFU = this.netcdfFile.findVariable("KFU");
		//Variable KFV = this.netcdfFile.findVariable("KFV");
		Variable KCS = this.netcdfFile.findVariable("KCS");
		Variable ZK = this.netcdfFile.findVariable("ZK_LYR");
		Variable S1 = this.netcdfFile.findVariable("S1");

		//double[] KFU1Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(KFU,timeDimensionIndex,LastTimeIndex-1,-1);
		//double[] KFV1Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(KFV,timeDimensionIndex,LastTimeIndex-1,-1);
		double[] KCS1Darray = NetcdfUtils.readSelectedData(KCS, createOrigin(KCS), KCS.getShape(), -1);
		//double[] KCS1Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(KCS,timeDimensionIndex,LastTimeIndex-1,-1);
		double[] ZK1Darray = NetcdfUtils.readSelectedData(ZK, createOrigin(ZK), ZK.getShape(), -1);
		double[] S11Darray = NetcdfUtils.readDataForVariableFor2DGridForSingleTime(S1, timeDimensionIndex, LastTimeIndex - 1, -1);

		// Here I convert the 1D array into a 3D one, which helps in the thinking for expanding the upper cells
		double[][][] Domain3D = from1dTo3dArray(expandedDomain);

		// Emptying the upper cells that are above the computed waterlevel
		int k = 0;
		for (int m = 0; m < mMax; m++) {
			for (int n = 0; n < nMax; n++) {

				//if (KFU1Darray[k] == 1 | KFV1Darray[k] == 1) {
				if (KCS1Darray[k] == 1) {

					double[] distance = new double[ZK1Darray.length];
					for (int i = 0; i < ZK1Darray.length; i++) {
						distance[i] = abs(S11Darray[k] - ZK1Darray[i]); //TODO: I have one concern here, check if the netCDF file we read has to have the new waterlevels! (TB)
					}

					// TB: This could probably be simplified with a function giving the mean
					// and then a binarySearch over the array to find the index of the min value
					// E.g.: on matlab I would write: layIndex = find(distance==minDistance)
					int layIndex = 0;
					for (int i = 0; i < distance.length; i++) {
						layIndex = (distance[i] < distance[layIndex]) ? i : layIndex;
					}

					for (int i = layIndex + 1; i < nLay; i++) {
						Domain3D[m][n][i] = -999.0;
					}

				}
				k++;
			}
		}

		// Back to a 1D array
		double[] realDomain = from3dTo1dArray(Domain3D, expandedDomain.length);

		return realDomain;
	}

	private double[][][] from1dTo3dArray(double[] oneDArray){

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

	private double[] from3dTo1dArray(double[][][] Domain3D,int domainLength){

		double[] Domain1D = new double[domainLength];

		int k=0;
		for (int lay = 0; lay < nLay; lay++) {
			for (int m = 0; m < mMax; m++) {
				for (int n = 0; n < nMax; n++) {

					Domain1D[k] = Domain3D[m][n][lay];
					k++;

				}
			}
		}
		return Domain1D;
	}

	public static int[] createOrigin(Variable var) {
		int dimensionCount = var.getDimensions().size();
		int[] origin = new int[dimensionCount];
		Arrays.fill(origin, 0);
		return origin;
	}

}
