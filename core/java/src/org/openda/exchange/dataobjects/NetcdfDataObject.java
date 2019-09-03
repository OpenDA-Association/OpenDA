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

package org.openda.exchange.dataobjects;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.*;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;
import org.openda.utils.generalJavaUtils.StringUtilities;
import org.openda.utils.geometry.GeometryUtils;
import ucar.ma2.ArrayDouble;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * DataObject for data that is stored in a NetCDF file in a format that adheres to the NetCDF CF conventions (see http://cfconventions.org/ ).
 *
 * If this NecdfDataObject is insufficient for reading/writing a particular new NetCDF file, then there are two possibilities:
 * 1. If the file is in a format that is compliant with the NetCDF CF conventions (see http://cfconventions.org/ ),
 *    then this class can be changed to include code for reading/writing the new CF compliant format.
 * 2. If the file is in a format that is not compliant with the NetCDF CF conventions,
 *    then kindly ask the data provider to update their NetCDF files, so that their format adheres to the NetCDF CF conventions.
 *    If the data provider cannot update their format, then create some entirely separate code for reading/writing those files.
 *    This class is only meant for CF compliant NetCDF formats. DO NOT EXTEND THIS CLASS, since that would make the code unnecessarily complicated.
 *
 * @author Arno Kockx
 */
//DO NOT EXTEND THIS CLASS, read the comments above.
public class NetcdfDataObject implements IComposableDataObject, IComposableEnsembleDataObject {

	public static final String ALLOW_TIME_INDEPENDENT_ITEMS = "allowTimeIndependentItems";

	public enum GridStartCorner {NORTH_WEST, SOUTH_WEST, UNKNOWN}

	//TODO Stef: remove this code. Variable names should not be hardcoded. AK
	protected String stationIdVarName = NetcdfUtils.STATION_ID_VARIABLE_NAME; // can be overruled by subclasses
	protected String CrossSectionIdVarName = NetcdfUtils.CROSS_SECTION_ID_VARIABLE_NAME; // can be overruled by subclasses
	
	protected String stationDimensionVarName = NetcdfUtils.STATION_DIMENSION_VARIABLE_NAME; // can be overruled by subclasses
	protected String CrossSectionDimensionVarName = NetcdfUtils.CROSS_SECTION_DIMENSION_VARIABLE_NAME; // can be overruled by subclasses
	

	private File file = null;
	private NetcdfFileWriter netcdfFileWriter = null;
	protected List<IExchangeItem> exchangeItems = new ArrayList<IExchangeItem>();
	/**
	 * For each exchangeItemId contains a map with one exchangeItem per ensembleMemberIndex.
	 */
	private Map<String, Map<Integer, IExchangeItem>> ensembleExchangeItems = new LinkedHashMap<>();

	private Map<ITimeInfo, Dimension> timeInfoTimeDimensionMap = new LinkedHashMap<ITimeInfo, Dimension>();
    private List<String> uniqueStationIds = new ArrayList<>();
	private List<Integer> uniqueEnsembleMemberIndices = new ArrayList<>();
	private Map<IGeometryInfo, GridVariableProperties> geometryInfoGridVariablePropertiesMap = new LinkedHashMap<IGeometryInfo, GridVariableProperties>();

	/**
	 * Corner at which the grid values in the model start.
	 * If the grid values in the netcdf start at a different corner than the grid values
	 * in the model, then the read data is flipped accordingly.
	 * Default is UNKNOWN, i.e. grid values are read in the same order as in the netcdf file.
	 *
	 * This is currently only used when lazyReading is true.
	 */
	private GridStartCorner internalGridStartCorner = GridStartCorner.UNKNOWN;

	/**
	 * Default is false for backwards compatibility.
	 */
	private boolean lazyReading = false;
	/**
	 * Default is true for backwards compatibility.
	 */
	private boolean lazyWriting = true;

	private Boolean allowTimeIndependentItems = false;

	public NetcdfDataObject() {
	}

	public void setInternalGridStartCorner(GridStartCorner internalGridStartCorner) {
		this.internalGridStartCorner = internalGridStartCorner;
	}

	/**
	 * @param workingDir path to work directory
	 * @param arguments required argument 1: the pathname of the data file relative to the given workingDir.
	 * @param arguments optional argument 2: lazyReading true/false.
	 * @param arguments optional argument 3: lazyWriting true/false.
	 * @param arguments optional argument >=2: allowTimeIndependentItems=true/false.
	 */
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		this.file = new File(workingDir, fileName);
		Set<String> requiredExchangeItemIds = new HashSet<>();
		for (int i = 1; i < arguments.length; i++) {
			String argument = arguments[i];
			if (checkLazyReadingOrLazyWritingArguments(i, argument)) continue;
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case ALLOW_TIME_INDEPENDENT_ITEMS:
					this.allowTimeIndependentItems = Boolean.valueOf(value);
					continue;
				case "requiredExchangeItemId":
					requiredExchangeItemIds.add(value);
					continue;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only " + ALLOW_TIME_INDEPENDENT_ITEMS + " as key=value pair");
			}
		}

		if (this.file.exists()) {
			//open existing netcdf file. Always use NetcdfFileWriteable in case data needs to be written later.
			try {
				this.netcdfFileWriter = NetcdfFileWriter.openExisting(this.file.getAbsolutePath());
			} catch (IOException e) {
				throw new RuntimeException("Error while opening existing netcdf file '" + this.file.getAbsolutePath()
						+ "'. Message was: " + e.getMessage(), e);
			}

			if (this.lazyReading) {//if lazyReading is true, then reading from netcdf file will happen later in exchangeItem.getValues methods.
				//create exchangeItems that can read lazily.
				try {
					createExchangeItems(requiredExchangeItemIds);
				} catch (IOException e) {
					throw new RuntimeException("Error while creating exchange items for netcdf file '" + this.file.getAbsolutePath()
							+ "'. Message was: " + e.getMessage(), e);
				}

			} else {//if lazyReading is false.
				//read data from netcdf file.
				try {
					readNetcdfFile();
				} catch (IOException e) {
					throw new RuntimeException("Error while reading netcdf file '" + this.file.getAbsolutePath()
							+ "'. Message was: " + e.getMessage(), e);
				}
			}

		} else {//if file does not exist.
			//no data to read, but this dataObject can still be used for writing data to file.
			//create file here, exchange items need to be added using method addExchangeItem.
			//create new netcdf file.
			try {
				//set fill to true, otherwise missing values will not be written for scalar time series variables that do not have data for all stations.
				this.netcdfFileWriter = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, this.file.getAbsolutePath());
				this.netcdfFileWriter.setFill(true);
			} catch (IOException e) {
				throw new RuntimeException("Error while creating handle for new netcdf file '" + this.file.getAbsolutePath()
						+ "'. Message was: " + e.getMessage(), e);
			}
			//always create large file, in case much data needs to be written.
			this.netcdfFileWriter.setLargeFile(true);
			NetcdfUtils.addGlobalAttributes(this.netcdfFileWriter);
		}
	}

	private boolean checkLazyReadingOrLazyWritingArguments(int i, String argument) {
		int index = argument.indexOf('=');
		if (index == -1) {
			if (i == 1) {
				this.lazyReading = Boolean.valueOf(argument);
				return true;
			}
			if (i == 2) {
				this.lazyWriting = Boolean.valueOf(argument);
				return true;
			}
			throw new IllegalArgumentException("Not a key value pair: " + argument);
		}
		return false;
	}

	/**
	 * Create exchange items that represent the data in the netcdf file
	 * and know how to read/write this data from the netcdf file.
	 *
	 * Currently this method can only create exchangeItems for scalar time series variables
	 * that are in the format of the Delft-FEWS scalar time series netcdf export.
	 * For this copied and adapted code from class nl.wldelft.fews.system.plugin.dataImport.NetcdfTimeSeriesTSParser
	 *
	 * @throws IOException
	 * @param requiredExchangeItemIds
	 */
	//TODO move this method to a new util class that has as its only purpose to read scalar time series in a specific format,
	//then here loop over different util classes for different formats. AK
	private void createExchangeItems(Set<String> requiredExchangeItemIds) throws IOException {
		this.exchangeItems.clear();

		//get locationIds.
		//TODO MVL
		NetcdfFile netcdfFile = this.netcdfFileWriter.getNetcdfFile();
		Map<Integer, String> stationIndexIdMap = NetcdfUtils.readAndStoreStationIdsMap(netcdfFile, stationIdVarName);
		if (!stationIndexIdMap.isEmpty()) {//if stations found.
			Results.putMessage(this.getClass().getSimpleName() + ": station_id variable found in netcdf file " + this.file.getAbsolutePath());
		}
		Map<Integer, String> crossSectionIndexIdMap = NetcdfUtils.readAndStoreStationIdsMap(netcdfFile, CrossSectionIdVarName);
		if (!crossSectionIndexIdMap.isEmpty()) {//if stations found.
			Results.putMessage(this.getClass().getSimpleName() + ": cross_section_name variable found in netcdf file " + this.file.getAbsolutePath());
		}


		//in most netcdfFiles the time and spatial coordinate variables are shared between multiple data variables.
		//Therefore cache timeInfo objects so that time coordinate variables
		//are never read more than once.
		Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();
		for (Variable variable : netcdfFile.getVariables()) {
			//skip coordinate variables.
			//a time variable is also a coordinate variable (i.e. has only itself as dimension).
			//do not create exchangeItems for coordinate variables.
			//the coordinate values will be stored in the metadata objects contained in the exchangeItems.
			if (variable.isCoordinateVariable()) {
				continue;
			}

			if (!variable.getDataType().isNumeric()) {
				continue;
			}

			//skip variables that do not depend on time.
			Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, netcdfFile);
			if (timeVariable == null) {
				continue;
			}
			ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, netcdfFile, timeInfoCache);


			//skip variables that are not two or three dimensional.
			int dimensionCount = variable.getDimensions().size();

			// Test and correct for ensembleIndex named "realization".
			//TODO Edwin: realization dimension can be called anything. Instead search for a dimension for which there is a coordinate variable that has standard_name "realization". AK
			int realizationDimensionIndex = variable.findDimensionIndex("realization");
			if (realizationDimensionIndex != -1) {
				dimensionCount -= 1;
			}

			if (dimensionCount != 2 && dimensionCount != 3) {
				continue;
			}
			//TODO MVL
			//Look for index with dimension that matches names/ids
			int stationDimensionIndex = variable.findDimensionIndex(stationDimensionVarName);
			String nameSource="stationDimensionVarName";
			if(stationDimensionIndex<0){
				stationDimensionIndex = variable.findDimensionIndex(CrossSectionDimensionVarName);
				nameSource="CrossSectionDimensionVarName";
			}
			if (dimensionCount == 2 && stationDimensionIndex != -1) {
				//if variable has data for scalar time series for a list of separate stations.
				int stationCount = variable.getDimension(stationDimensionIndex).getLength();
				String parameterId = variable.getShortName();

				//create an exchangeItem that can read/write lazily, for each location in this variable.
				for (int stationIndex = 0; stationIndex < stationCount; stationIndex++) {
					String stationId;
					if(nameSource.equals("stationDimensionVarName")){
						stationId=stationIndexIdMap.get(stationIndex);
					}else{
						stationId=crossSectionIndexIdMap.get(stationIndex);
					}
					if (realizationDimensionIndex == -1) {
						String exchangeItemId = stationId + '.' + parameterId;
						if (!requiredExchangeItemIds.isEmpty() && !requiredExchangeItemIds.contains(exchangeItemId)) continue;
						IExchangeItem exchangeItem = new NetcdfScalarTimeSeriesExchangeItem(stationDimensionIndex, stationIndex,
								stationId, parameterId, realizationDimensionIndex, -1, Role.InOut, timeInfo, this);
						this.exchangeItems.add(exchangeItem);
					} else {
						//TODO Edwin: this code assumes that the realization indices are always the numbers 0, 1, 2, 3, etc. in sorted order. It should read the actual ensemble member indices from the file. AK
						for (int realizationIndex = 0; realizationIndex < variable.getDimension(realizationDimensionIndex).getLength(); realizationIndex++) {
							IExchangeItem exchangeItem = new NetcdfScalarTimeSeriesExchangeItem(stationDimensionIndex, stationIndex,
									stationId, parameterId, realizationDimensionIndex, realizationIndex, Role.InOut, timeInfo, this);
							addEnsembleExchangeItem(exchangeItem, realizationIndex);
						}
					}
				}

				continue;
			}

			int timeDimensionIndex = variable.findDimensionIndex(timeVariable.getShortName());
			IGeometryInfo geometryInfo = NetcdfUtils.createGeometryInfo(variable,netcdfFile);
			if (dimensionCount == 3 && geometryInfo != null) {
				//if variable has data for a 2D grid time series.
				String parameterId = variable.getShortName();

				//create an exchangeItem that can read/write lazily.
				IQuantityInfo quantityInfo = new QuantityInfo(parameterId, variable.getUnitsString());
				int dimensionIndexToFlipForReadData = NetcdfUtils.getDimensionIndexToFlipFor2DGrid(variable, netcdfFile,
						this.internalGridStartCorner);
				if (realizationDimensionIndex == -1) {
					IExchangeItem exchangeItem = new NetcdfGridTimeSeriesExchangeItem(parameterId, Role.InOut,
							timeInfo, quantityInfo, geometryInfo, this, timeDimensionIndex, dimensionIndexToFlipForReadData);
					this.exchangeItems.add(exchangeItem);
				} else {
					//TODO Edwin: this code assumes that the realization indices are always the numbers 0, 1, 2, 3, etc. in sorted order. It should read the actual ensemble member indices from the file. AK
					for (int realizationIndex = 0; realizationIndex < variable.getDimension(realizationDimensionIndex).getLength(); realizationIndex++) {
						IExchangeItem exchangeItem = new NetcdfGridTimeSeriesExchangeItem(parameterId, realizationDimensionIndex, realizationIndex, Role.InOut,
								timeInfo, quantityInfo, geometryInfo, this, timeDimensionIndex, dimensionIndexToFlipForReadData);
						addEnsembleExchangeItem(exchangeItem, realizationIndex);
					}
				}
			}
		}
	}

	/**
	 * Read data from netcdf file.
	 *
	 * @throws IOException
	 */
	//TODO remove, always read data lazily. AK
	private void readNetcdfFile() throws IOException  {
		this.exchangeItems.clear();
		NetcdfFile netcdfFile = this.netcdfFileWriter.getNetcdfFile();

		Results.putMessage(this.getClass().getSimpleName() + ": reading data from file " + this.file.getAbsolutePath());

		//in most netcdfFiles the time and spatial coordinate variables are shared between multiple data variables.
		//Therefore cache timeInfo objects so that time coordinate variables
		//are never read more than once.
		Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();
		for (Variable variable : netcdfFile.getVariables()) {
			//do not create exchangeItems for coordinate variables.
			//the coordinate values will be stored in the metadata objects contained in the exchangeItems.
			if (variable.isCoordinateVariable()) {
				continue;
			}

			if (!variable.getDataType().isNumeric()) {
				continue;
			}

			//create exchangeItem.
			IExchangeItem exchangeItem;
			//Note: never use standard name or long name as exchangeItemId, since these are not always unique within a netcdf file.
			String exchangeItemId = variable.getShortName();
			if (variable.getDimensions().isEmpty()) {//if scalar.
				exchangeItem = new DoubleExchangeItem(exchangeItemId, Role.InOut, variable.readScalarDouble());

			} else {//if array.
				ArrayExchangeItem arrayBasedExchangeItem = new ArrayExchangeItem(exchangeItemId, IPrevExchangeItem.Role.InOut);
				arrayBasedExchangeItem.setQuantityInfo(new QuantityInfo(exchangeItemId, variable.getUnitsString()));
				IArrayTimeInfo newTimeInfo = NetcdfUtils.createTimeInfo(variable, netcdfFile, timeInfoCache);
				//skip variables that do not depend on time.
				if (newTimeInfo == null && !allowTimeIndependentItems) continue;

				arrayBasedExchangeItem.setTimeInfo(newTimeInfo);
				//TODO cache variables with spatial coordinates. AK
				arrayBasedExchangeItem.setGeometryInfo(NetcdfUtils.createGeometryInfo(variable, netcdfFile));
				IArray array = (IArray) NetcdfUtils.readData(variable);
				arrayBasedExchangeItem.setArray(array);
				exchangeItem = arrayBasedExchangeItem;
			}

			this.exchangeItems.add(exchangeItem);
		}
	}

    public String[] getExchangeItemIDs() {
        //ignore ensemble exchange items.
        String[] exchangeItemIDs = new String[this.exchangeItems.size()];
        for (int n = 0; n < this.exchangeItems.size(); n++) {
            exchangeItemIDs[n] = this.exchangeItems.get(n).getId();
        }

        return exchangeItemIDs;
    }

	public String[] getExchangeItemIDs(Role role) {
		//ignore ensemble exchange items.
		ArrayList<String> exchangeItemIDs = new ArrayList<>();
		for (IExchangeItem exchangeItem : this.exchangeItems) {
			if (exchangeItem.getRole().equals(role)) {
				exchangeItemIDs.add(exchangeItem.getId());
			}
		}

		return exchangeItemIDs.toArray(new String[exchangeItemIDs.size()]);
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		for (IExchangeItem exchangeItem : this.exchangeItems) {
			if (exchangeItem.getId().equals(exchangeItemID)) {//if non-ensemble exchange item.
				return exchangeItem;
			}
		}

		if (ensembleExchangeItems.keySet().contains(exchangeItemID)) {//if ensemble exchange item.
			throw new IllegalStateException(getClass().getSimpleName() + ".getDataObjectExchangeItem: exchange item with id '"
					+ exchangeItemID + "' is an ensemble exchange item. Call method getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) instead.");
		}

		//if not found.
		return null;
	}

	/**
	 * Get the ensemble member indices of the ensemble exchange items.
	 * The ensemble member indices must be the same for all ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return int[0] if there are no ensemble members.
	 *
	 * @return array of ensemble member indices.
	 */
	public int[] getEnsembleMemberIndices() {
		if (ensembleExchangeItems.isEmpty()) return new int[0];

		//ignore non-ensemble exchange items.
		Set<Integer> uniqueIndices = new HashSet<>();
		for (Map<Integer, IExchangeItem> ensembleMemberIndexExchangeItemMap : ensembleExchangeItems.values()) {
			uniqueIndices.addAll(ensembleMemberIndexExchangeItemMap.keySet());
		}

		//return sorted indices.
		int[] indices = BBUtils.unbox(uniqueIndices.toArray(new Integer[uniqueIndices.size()]));
		Arrays.sort(indices);
		return indices;
	}

	/**
	 * Get the identifiers of the ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return String[0] if there are no matching ensemble items.
	 *
	 * @return array of ensemble exchange item identifiers.
	 */
	public String[] getEnsembleExchangeItemIds() {
		if (ensembleExchangeItems.isEmpty()) return new String[0];

		//ignore non-ensemble exchange items.
		Set<String> ensembleExchangeItemIds = ensembleExchangeItems.keySet();
		return ensembleExchangeItemIds.toArray(new String[ensembleExchangeItemIds.size()]);
	}

	/**
	 * Get the ensemble exchange item specified by the given exchangeItemId and ensembleMemberIndex.
	 * If the given ensembleMemberIndex does not exist, then this method should throw an IllegalStateException.
	 * If there are no ensemble members available for the given exchangeItem, then it should throw an
	 * IllegalStateException stating that the equivalent method without the argument "int ensembleMemberIndex" must be called instead.
	 * Returns null if no ensemble exchange item with the given exchangeItemId is found.
	 *
	 * @param exchangeItemId ensemble exchange item identifier.
	 * @param ensembleMemberIndex ensemble member index.
	 * @return the requested ensemble exchange item.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) {
		Map<Integer, IExchangeItem> ensembleMemberIndexExchangeItemMap = ensembleExchangeItems.get(exchangeItemId);
		if (ensembleMemberIndexExchangeItemMap != null) {//if ensemble exchange item.
			return ensembleMemberIndexExchangeItemMap.get(ensembleMemberIndex);
		}

		if (Arrays.asList(getExchangeItemIDs()).contains(exchangeItemId)) {//if non-ensemble exchange item.
			throw new IllegalStateException(getClass().getSimpleName() + ".getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex): exchange item with id '"
					+ exchangeItemId + "' is a non-ensemble exchange item. Call method getDataObjectExchangeItem(String exchangeItemID) instead.");
		}

		//if not found.
		return null;
	}

	/**
	 * Add a <b>copy</b> of exchangeItem to this dataObject. Since a dataObject owns its exchangeItems, it is
	 * possible that the actual work is postponed until the finish method is called, so modification of an
	 * exchangeItem after adding it, but before calling finish, may alter the outcome.
	 * Throws an UnsupportedOperationException when the dataObject does not support the addition of items.
	 * A RuntimeException is thrown if the type of data can not be handled. Note that in general, it is also
	 * possible that the type of echchangeItem can be handled, but with degraded meta data.
	 *
	 * @param item  exchangeItem to be duplicated
	 */
	public void addExchangeItem(IExchangeItem item) {
        if (!this.netcdfFileWriter.isDefineMode()) {
            throw new RuntimeException(getClass().getSimpleName() + ": cannot add new exchangeItems to an existing netcdf file.");
        }
		if (!GeometryUtils.isScalar(item.getGeometryInfo())) {
			throw new RuntimeException(getClass().getSimpleName() + ".addExchangeItem(IExchangeItem item) only implemented for scalar exchange items.");
		}

		//create a new internal exchangeItem in this dataObject that matches the given external exchangeItem.
		//This internal exchangeItem can then be used later to copy data from the matching external exchangeItem.
		//here assume that stationDimensionIndex is 1.
		IExchangeItem newItem = new NetcdfScalarTimeSeriesExchangeItem(1, -1, NetcdfUtils.getStationId(item), NetcdfUtils.getVariableName(item), -1, -1, Role.Output, item.getTimeInfo(), this);
		//store new item.
        this.exchangeItems.add(newItem);
    }

	/**
	 * Add a <b>copy</b> of exchangeItem to this dataObject. Since a dataObject owns its exchangeItems, it is
	 * possible that the actual work is postponed until the finish method is called, so modification of an
	 * exchangeItem after adding it, but before calling finish, may alter the outcome.
	 * Throws an UnsupportedOperationException when the dataObject does not support the addition of items.
	 * A RuntimeException is thrown if the type of data can not be handled. Note that in general, it is also
	 * possible that the type of echchangeItem can be handled, but with degraded meta data.
	 *
	 * @param item to be duplicated.
	 * @param ensembleMemberIndex ensemble member index of the exchangeItem.
	 */
	public void addExchangeItem(IExchangeItem item, int ensembleMemberIndex) {
		if (!this.netcdfFileWriter.isDefineMode()) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot add new exchangeItems to an existing netcdf file.");
		}
		if (!GeometryUtils.isScalar(item.getGeometryInfo())) {
			throw new RuntimeException(getClass().getSimpleName() + ".addExchangeItem(IExchangeItem item, int ensembleMemberIndex) only implemented for scalar exchange items.");
		}

		//create a new internal ensemble exchangeItem in this dataObject that matches the given external ensemble exchangeItem.
		//This internal ensemble exchangeItem can then be used later to copy data from the matching external ensemble exchangeItem.
		//here assume that realizationDimensionIndex is 1.
		//here assume that stationDimensionIndex is 2.
		IExchangeItem newItem = new NetcdfScalarTimeSeriesExchangeItem(2, -1, NetcdfUtils.getStationId(item), NetcdfUtils.getVariableName(item), 1, -1, Role.Output, item.getTimeInfo(), this);

		//store new item.
		addEnsembleExchangeItem(newItem, ensembleMemberIndex);
	}

	private void addEnsembleExchangeItem(IExchangeItem item, int ensembleMemberIndex) {
		String id = item.getId();
		Map<Integer, IExchangeItem> ensemble = ensembleExchangeItems.get(id);
		if (ensemble == null) {
			ensemble = new LinkedHashMap<>();
			ensembleExchangeItems.put(id, ensemble);
		}
		IExchangeItem previous = ensemble.put(ensembleMemberIndex, item);
		if (previous != null) {
			throw new IllegalStateException(getClass().getSimpleName() + ": exchange item with id '" + id + "' and ensemble member index " + ensembleMemberIndex + " already present in data object.");
		}
	}

	public void finish() {
		makeSureFileHasBeenCreated();

		//TODO remove. This is only used for SWAN state files (see SwanStateNetcdfFileTest.testSwanNetcdfStateFile_1). AK
		if (this.lazyWriting) {
			//write data to netcdf file.
			Results.putMessage(this.getClass().getSimpleName() + ": writing data to file " + this.file.getAbsolutePath());
			writeData(this.netcdfFileWriter, this.exchangeItems);
		} else {//if lazyWriting is false, then the variable data has already been written to netcdf file in exchangeItem.setValues methods,
			//and the metadata variable values have already been written to netcdf file in method createFile.
			//do nothing.
		}

		try {
			this.netcdfFileWriter.close();
		} catch (IOException e) {
			throw new RuntimeException("Error while closing netcdf file '" + this.file.getAbsolutePath() + "'. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * If the netcdf file has not yet been created, then creates it. Otherwise does nothing.
	 */
	public void makeSureFileHasBeenCreated() {
		if (this.netcdfFileWriter.isDefineMode()) {
			createFile();
		}
	}

	/**
	 * Actually creates the new netcdf file. This can be done only once and has to be done before any data can be written
	 * and has to be done after all new exchange items have been added.
	 * This method also creates the metadata variables just before the file is created and writes the metadata variable values right after the file has been created.
	 */
	private void createFile() {
		//create metadata for all exchange items at the last possible moment, so that information of all exchange items is available while creating the metadata.
		//This is needed for e.g. scalar time series, since in that case the file should contain a single station dimension and variable that contains all stations.
		//This cannot be done in method addExchangeItem, since at that moment it is not yet clear how many stations and how many ensemble indices there will be in total.
		if (!exchangeItems.isEmpty() || !ensembleExchangeItems.isEmpty()) {
			IExchangeItem firstItem = !exchangeItems.isEmpty() ? exchangeItems.get(0) : ensembleExchangeItems.values().iterator().next().values().iterator().next();
			boolean scalars = GeometryUtils.isScalar(firstItem.getGeometryInfo());
			validateExchangeItems(scalars, exchangeItems, ensembleExchangeItems);
			if (scalars) {
				uniqueStationIds = NetcdfUtils.getStationIds(exchangeItems, ensembleExchangeItems);
				//here assume that all ensemble exchange items have the same ensemble member indices.
				uniqueEnsembleMemberIndices = Arrays.asList(BBUtils.box(getEnsembleMemberIndices()));
				//set station indices and realization indices in exchangeItems as soon as they are known.
				setStationAndRealizationIndicesForWriting(exchangeItems, ensembleExchangeItems);
				NetcdfUtils.createMetadataAndDataVariablesForScalars(netcdfFileWriter, exchangeItems, ensembleExchangeItems, timeInfoTimeDimensionMap, stationIdVarName,
						stationDimensionVarName, uniqueStationIds.size(), uniqueEnsembleMemberIndices.size());
			} else {//if grids.
				NetcdfUtils.createMetadataAndDataVariablesForGrids(this.netcdfFileWriter, this.exchangeItems, this.timeInfoTimeDimensionMap, this.geometryInfoGridVariablePropertiesMap);
			}
		}

		try {
			this.netcdfFileWriter.create();
		} catch (IOException e) {
			throw new RuntimeException("Error while creating new netcdf file '" + this.netcdfFileWriter.getNetcdfFile().getLocation() + "'. Message was: " + e.getMessage(), e);
		}

		//write metadata variable values for all exchange items, i.e. times and spatial coordinates.
		//This is only needed if a new file has been created. If an existing file has been opened, then the file should already contain these values.
		try {
			NetcdfUtils.writeMetadata(netcdfFileWriter, timeInfoTimeDimensionMap, geometryInfoGridVariablePropertiesMap, stationIdVarName, uniqueStationIds, uniqueEnsembleMemberIndices);
		} catch (Exception e) {
			throw new RuntimeException("Error while writing metadata values to netcdf file '" + this.file.getAbsolutePath() + "'. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * Validates that either all exchange items are scalars or all exchange items are grids, depending on the value of the given boolean.
	 *
	 * @param scalar all exchange items are scalar
	 * @param exchangeItems list of exchange items
	 */
	private void validateExchangeItems(boolean scalar, List<IExchangeItem> exchangeItems, Map<String, Map<Integer, IExchangeItem>> ensembleExchangeItems) {
		if (exchangeItems == null) throw new IllegalArgumentException("exchangeItems == null");
		if (ensembleExchangeItems == null) throw new IllegalArgumentException("ensembleExchangeItems == null");

		for (IExchangeItem item : exchangeItems) {
			if (GeometryUtils.isScalar(item.getGeometryInfo()) != scalar) {
				throw new RuntimeException(getClass().getSimpleName() + ": Not all exchange items are of the same geometryInfo type. "
						+ getClass().getSimpleName() + " can only write a NetCDF file for exchange items that are all of the same geometryInfo type.");
			}
		}

		for (Map<Integer, IExchangeItem> ensemble : ensembleExchangeItems.values()) {
			for (IExchangeItem item : ensemble.values()) {
				if (GeometryUtils.isScalar(item.getGeometryInfo()) != scalar) {
					throw new RuntimeException(getClass().getSimpleName() + ": Not all exchange items are of the same geometryInfo type. "
							+ getClass().getSimpleName() + " can only write a NetCDF file for exchange items that are all of the same geometryInfo type.");
				}
			}
		}
	}

	private void setStationAndRealizationIndicesForWriting(List<IExchangeItem> exchangeItems, Map<String, Map<Integer, IExchangeItem>> ensembleExchangeItems) {
		for (IExchangeItem item : exchangeItems) {
			((NetcdfScalarTimeSeriesExchangeItem) item).setLocationIndex(uniqueStationIds.indexOf(NetcdfUtils.getStationId(item)));
		}

		for (Map<Integer, IExchangeItem> ensemble : ensembleExchangeItems.values()) {
			for (Map.Entry<Integer, IExchangeItem> entry : ensemble.entrySet()) {
				Integer ensembleMemberIndex = entry.getKey();
				IExchangeItem item = entry.getValue();
				((NetcdfScalarTimeSeriesExchangeItem) item).setLocationIndex(uniqueStationIds.indexOf(NetcdfUtils.getStationId(item)));
				((NetcdfScalarTimeSeriesExchangeItem) item).setRealizationIndex(uniqueEnsembleMemberIndices.indexOf(ensembleMemberIndex));
			}
		}
	}

	/**
	 * Writes all data for the given exchangeItems to the given netcdfFile.
	 * Only executed when this.lazyWriting = true
     *
	 * @param netcdfFileWriter
	 * @param exchangeItems to write.
	 */
	//TODO remove. This is only used for SWAN state files (see SwanStateNetcdfFileTest.testSwanNetcdfStateFile_1). AK
	private void writeData(NetcdfFileWriter netcdfFileWriter, List<IExchangeItem> exchangeItems) {
		for (IExchangeItem exchangeItem : exchangeItems){
			if (exchangeItem.getGeometryInfo() == null){
				//TODO Julius: please remove this hack for SWAN state files. AK
                //TODO: replace this SWAN specific implementation with the above generic ones.
				String exchangeItemId = exchangeItem.getId();
				//TODO: add netcdf writers for various data type / exchangeitems.
				//For SWAN state file, only wave_spectrum is modified and rewritten.
				if (exchangeItemId.equalsIgnoreCase("wave_spectrum")){
					double[] dblValues = exchangeItem.getValuesAsDoubles();
					int my=31;
					int mx=61;
					int wave_frequency=25;
					int wave_direction=18;
					// get dimensions:
					// TODO: in the future, dimensions should be available in the exchangeItem as part of meta data
					// This will avoid having to read the netcdf file for obtaining the dimensions.
					List<Dimension> dimensions = netcdfFileWriter.getNetcdfFile().getDimensions();
					int nDim = dimensions.size();
					for (Dimension dimension : dimensions) {
						if ("my".equalsIgnoreCase(dimension.getShortName())) {
							my = dimension.getLength();
						} else if ("mx".equalsIgnoreCase(dimension.getShortName())) {
							mx = dimension.getLength();
						} else if ("wave_frequency".equalsIgnoreCase(dimension.getShortName())) {
							wave_frequency = dimension.getLength();
						} else if ("wave_direction".equalsIgnoreCase(dimension.getShortName())) {
							wave_direction = dimension.getLength();
						} else {
							continue;
						}
					}
					ArrayDouble.D4 values = new ArrayDouble.D4(my,mx,wave_frequency,wave_direction);
					int iVal = 0;
					for (int iy=0; iy<my; iy++){
						for (int ix=0; ix<mx; ix++){
							for (int iwf=0; iwf<wave_frequency; iwf++){
								for (int iwd=0; iwd<wave_direction; iwd++){
									values.set(iy,ix,iwf,iwd,dblValues[iVal]);
									iVal++;
								}
							}
						}
					}
					try {
						Variable myVar = netcdfFileWriter.findVariable("wave_spectrum");
						netcdfFileWriter.write(myVar,values);
					} catch (IOException | InvalidRangeException e) {
						e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
					}
				}
			}
		}
	}

	public double[] readDataForExchangeItemForSingleLocation(IExchangeItem item, int stationDimensionIndex, int stationIndex) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
		return NetcdfUtils.readDataForVariableForSingleLocation(variable, stationDimensionIndex, stationIndex);
	}

	public double[] readDataForExchangeItemForSingleLocationSingleRealization(IExchangeItem item, int stationDimensionIndex, int stationIndex, int realizationDimensionIndex, int realizationIndex) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
		return NetcdfUtils.readDataForVariableForSingleLocationAndRealization(variable, stationDimensionIndex, stationIndex, realizationDimensionIndex, realizationIndex);
	}

	public double[] readDataForExchangeItemFor2DGridForSingleTime(IExchangeItem item, int timeDimensionIndex, int timeIndex,
			int dimensionIndexToFlip) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
		return NetcdfUtils.readDataForVariableFor2DGridForSingleTime(variable, timeDimensionIndex, timeIndex,
				dimensionIndexToFlip);
	}

	public double[] readDataForExchangeItemFor2DGridForSingleTimeAndRealization(
			IExchangeItem item, int realizationDimensionIndex, int realizationIndex, int timeDimensionIndex, int timeIndex, int dimensionIndexToFlip) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
		return NetcdfUtils.readDataForVariableFor2DGridForSingleTimeAndRealization(variable, realizationDimensionIndex, realizationIndex, timeDimensionIndex, timeIndex,
				dimensionIndexToFlip);
	}

    public void writeDataForExchangeItemForSingleTime(IExchangeItem item, int timeDimensionIndex, int timeIndex, double[] values) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
        NetcdfUtils.writeDataForVariableForSingleTime(this.netcdfFileWriter, variable, timeDimensionIndex, timeIndex, values);
    }

    public void writeDataForExchangeItemForSingleTimeSingleLocation(IExchangeItem item, int timeIndex, int stationDimensionIndex, int stationIndex, double[] values) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
        NetcdfUtils.writeDataForVariableForSingleTimeSingleLocation(this.netcdfFileWriter, variable, timeIndex, stationDimensionIndex, stationIndex, values);
    }

	public void writeDataForExchangeItemForSingleTimeSingleLocationSingleRealization(IExchangeItem item, int timeIndex, int realizationDimensionIndex, int realizationIndex,
			int stationDimensionIndex, int stationIndex, double[] values) {
		Variable variable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
		NetcdfUtils.writeDataForVariableForSingleTimeSingleLocationSingleRealization(this.netcdfFileWriter, variable, timeIndex, realizationDimensionIndex, realizationIndex, stationDimensionIndex, stationIndex, values);
	}
}
