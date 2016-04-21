package org.openda.model_delft3d;

import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.ITimeInfo;
import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;
import ucar.util.prefs.ui.Field;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * Data object voor D3D map file
 */
public class NetcdfD3dMapDataObject implements IDataObject {

	NetcdfFile netcdfFile = null;

	private LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	private LinkedHashMap<String, Variable> timeDependentVars = new LinkedHashMap<>();
	private Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();

	double[] timesInNetcdfFile = null;

	private String[] keyVariables = {"S1","R1","V1","U1"};

	int timeDimensionIndex = -1;
	int lstsciDimensionIndex = -1;
	int kmaxOutRestrDimensionIndex = -1;

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 1) {
			throw new RuntimeException("NetcdfD3dHisDataObject expects one argument: netcdfFileName (relative to working dir)");
		}

		File netcdfFilePath = new File(workingDir, arguments[0]);
		try {
			netcdfFile = new NetcdfFile(netcdfFilePath.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("NetcdfD3dMapDataObject could not open netcdf file " + netcdfFilePath.getAbsolutePath());
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

		for (Variable variable : this.netcdfFile.getVariables()) {

			if (Arrays.asList(keyVariables).contains(variable.getName())) {

				Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
				if (timeVariable == null) {
					throw new RuntimeException("NetcdfD3dHisDataObject: no time axis for " + variable.getName() + ", file: " + netcdfFile.getLocation());
				}
				timeDimensionIndex = variable.findDimensionIndex(timeVariable.getName());
				timeDependentVars.put(variable.getName(), variable);
				ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);

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
					throw new RuntimeException("NetcdfD3dHisDataObject: dims not available, file: "
							+ netcdfFile.getLocation());

				}

				IExchangeItem exchangeItem = new NetcdfD3dMapExchangeItem(variable.getName(), this, timeInfo);
				this.exchangeItems.put(exchangeItem.getId(), exchangeItem);

			}
		}
	}

	public Array getExchangeItemValues(String varName) {

		// find variable. gevalues for sle
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
		return readDataForSelectedDomain(variable, origin, sizeArray);
	}

	public static int[] createOrigin(Variable var) {
		int dimensionCount = var.getDimensions().size();
		int[] origin = new int[dimensionCount];
		Arrays.fill(origin, 0);
		return origin;
	}

	public static Array readDataForSelectedDomain(Variable variable, int[] origin, int[] sizeArray) {
		Array array;
		try {
			array = variable.read(origin, sizeArray);
		} catch (IOException e) {
			throw new RuntimeException("Error while reading data from netcdf variable '" + variable.getName()
					+ "'. Message was: " + e.getMessage(), e);
		} catch (InvalidRangeException e) {
			throw new RuntimeException("Error while reading data from netcdf variable '" + variable.getName()
					+ "'. Message was: " + e.getMessage(), e);
		}
		if (array == null || array.getSize() == 0) {
			return null;
		}
		return array;
	}
}
