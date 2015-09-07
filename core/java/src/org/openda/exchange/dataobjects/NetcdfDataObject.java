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

import java.io.File;
import java.io.IOException;
import java.util.*;

import org.openda.exchange.*;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.IComposableDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IEnsembleDataObject;
import org.openda.utils.Results;

import ucar.ma2.ArrayDouble;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFileWriteable;
import ucar.nc2.Variable;

/**
 * DataObject for data that is stored in a netcdf file.
 *
 * Note: writing to netcdf file is not yet implemented.
 *
 * @author Arno Kockx
 */
public class NetcdfDataObject implements IComposableDataObject, IEnsembleDataObject {

	/**
	 * Get the ensemble member indices of the ensemble exchange items.
	 * The ensemble member indices must be the same for all ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return int[0] if there are no ensemble members.
	 *
	 * @return array of ensemble member indices.
	 */
	public int[] getEnsembleMemberIndices() {
		if (this.ensembleExchangeItems.isEmpty()) {
			return new int[0];
		} else {
			// Since Java has no range method.
			// TODO Upgrade as soon as explicit ensemble indices are stored in the NetCDF file.
			Set keys = this.ensembleExchangeItems.keySet();
			int[] result = new int[keys.size()];
			for(int i = 0; i < keys.size(); i++) {
				result[i] = i;
			}
			return result;
		}
	}

	/**
	 * Get the identifiers of the ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return String[0] if there are no matching ensemble items.
	 *
	 * @return array of ensemble exchange item identifiers.
	 */
	public String[] getEnsembleExchangeItemIds() {
		if (this.ensembleExchangeItems.isEmpty()) {
			return new String[0];
		} else {
			Set keys = this.ensembleExchangeItems.keySet();
			List<IExchangeItem> someItems = this.ensembleExchangeItems.get(keys.iterator().next());
			String[] exchangeItemIDs = new String[someItems.size()];
			for (int n = 0; n < someItems.size(); n++) {
				exchangeItemIDs[n] = someItems.get(n).getId();
			}
			return exchangeItemIDs;
		}
	}

	/**
	 * Get the ensemble exchange item specified by the given exchangeItemId and ensembleMemberIndex.
	 * If the given ensembleMemberIndex does not exist, then this method should throw an IllegalStateException.
	 * If there are no ensemble members available for the given exchangeItem, then it should throw an
	 * IllegalStateException stating that the equivalent method without the argument "int ensembleMemberIndex" must be called instead.
	 * Returns null if no ensemble exchange item with the given exchangeItemId is found.
	 *
	 * @param exchangeItemId      ensemble exchange item identifier.
	 * @param ensembleMemberIndex ensemble member index.
	 * @return the requested ensemble exchange item.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) {
		if (!this.ensembleExchangeItems.isEmpty()) {
			List<IExchangeItem> items = this.ensembleExchangeItems.get(ensembleMemberIndex);
			for (int n = 0; n < items.size(); n++) {
				IExchangeItem exchangeItem = items.get(n);
				if (exchangeItem.getId().equals(exchangeItemId)) {
					return exchangeItem;
				}
			}
		}
		return null;
	}

	public enum GridStartCorner {NORTH_WEST, SOUTH_WEST, UNKNOWN}

	private File file = null;
	private NetcdfFileWriteable netcdfFile = null;
	private List<IExchangeItem> exchangeItems = new ArrayList<IExchangeItem>();
	private Map<Integer, List<IExchangeItem>> ensembleExchangeItems = new LinkedHashMap<>();

	private Map<ITimeInfo, Dimension> timeInfoTimeDimensionMap = new LinkedHashMap<ITimeInfo, Dimension>();
    private List<String> stationIdList = new ArrayList<String>();
	private Map<IGeometryInfo, GridVariableProperties> geometryInfoGridVariablePropertiesMap =
			new LinkedHashMap<IGeometryInfo, GridVariableProperties>();
	private int[] uniqueTimeVariableCount = new int[]{0};
	private int[] uniqueFaceDimensionCount = new int[]{0};
	private int[] uniqueLatLonDimensionCount = new int[]{0};

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

	public NetcdfDataObject() {
	}

	public void setInternalGridStartCorner(GridStartCorner internalGridStartCorner) {
		this.internalGridStartCorner = internalGridStartCorner;
	}

	/**
	 * @param workingDir
	 * @param arguments required argument 1: the pathname of the data file relative to the given workingDir.
	 * @param arguments optional argument 2: lazyReading true/false.
	 * @param arguments optional argument 3: lazyWriting true/false.
	 */
	
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		this.file = new File(workingDir, fileName);

		//TODO combine these two arguments into one argument "directAccess" true/false. AK
		if (arguments.length > 1) {
			this.lazyReading = Boolean.valueOf(arguments[1]);
		}
		if (arguments.length > 2) {
			this.lazyWriting = Boolean.valueOf(arguments[2]);
		}

		if (this.file.exists()) {
			//open existing netcdf file. Always use NetcdfFileWriteable in case data needs to be written later.
			try {
				//this.netcdfFile = NetcdfFileWriteable.openExisting(this.file.getAbsolutePath(), true); for speeding up based on mail Arno changed to
				this.netcdfFile = NetcdfFileWriteable.openExisting(this.file.getAbsolutePath(), false);
			} catch (IOException e) {
				throw new RuntimeException("Error while opening existing netcdf file '" + this.file.getAbsolutePath()
						+ "'. Message was: " + e.getMessage(), e);
			}

			if (this.lazyReading) {//if lazyReading is true, then reading from netcdf file will happen later in exchangeItem.getValues methods.
				//create exchangeItems that can read lazily.
				try {
					createExchangeItems();
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
				//this.netcdfFile = NetcdfFileWriteable.createNew(this.file.getAbsolutePath(), true); for speeding up based on mail Arno changed to
				this.netcdfFile = NetcdfFileWriteable.createNew(this.file.getAbsolutePath(), false);
			} catch (IOException e) {
				throw new RuntimeException("Error while creating handle for new netcdf file '" + this.file.getAbsolutePath()
						+ "'. Message was: " + e.getMessage(), e);
			}
			//always create large file, in case much data needs to be written.
			this.netcdfFile.setLargeFile(true);
			addGlobalAttributes(this.netcdfFile);
		}
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
	 */
	//TODO move this method to a new class that has as its only purpose to read scalar time series in a specific format,
	//then here loop over different classes for different formats. AK
	private void createExchangeItems() throws IOException {
		this.exchangeItems.clear();

		//get locationIds.
		Map<Integer, String> stationIndexIdMap = new LinkedHashMap<Integer, String>();
		NetcdfUtils.readAndStoreStationIdsMap(this.netcdfFile, stationIndexIdMap);
		if (!stationIndexIdMap.isEmpty()) {//if stations found.
			Results.putMessage(this.getClass().getSimpleName() + ": station_id variable found in netcdf file " + this.file.getAbsolutePath());
		}

		//in most netcdfFiles the time and spatial coordinate variables are shared between multiple data variables.
		//Therefore cache timeInfo objects so that time coordinate variables
		//are never read more than once.
		Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();
		for (Variable variable : this.netcdfFile.getVariables()) {
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
			Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
			if (timeVariable == null) {
				continue;
			}
			ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);


			//skip variables that are not two or three dimensional.
			int dimensionCount = variable.getDimensions().size();

			// Test for ensembleIndex named "realization".
			int realizationDimensionIndex = variable.findDimensionIndex("realization");
			if (realizationDimensionIndex != -1) {
				dimensionCount -= 1;
			}

			if (dimensionCount != 2 && dimensionCount != 3) {
				continue;
			}

			int stationDimensionIndex = variable.findDimensionIndex(NetcdfUtils.STATION_DIMENSION_NAME);
			if (dimensionCount == 2 && stationDimensionIndex != -1) {
				//if variable has data for scalar time series for a list of separate stations.
				int stationCount = variable.getDimension(stationDimensionIndex).getLength();
				String parameterId = variable.getName();

				//create an exchangeItem that can read/write lazily, for each location in this variable.
				for (int n = 0; n < stationCount; n++) {
					int stationIndex = n;
					String stationId = stationIndexIdMap.get(n);
					if (realizationDimensionIndex == -1) {
						IExchangeItem exchangeItem = new NetcdfScalarTimeSeriesExchangeItem(stationDimensionIndex, stationIndex,
								stationId, parameterId, realizationDimensionIndex, -1, Role.InOut, timeInfo, this);
						this.exchangeItems.add(exchangeItem);
					} else {
						for (int realizationIndex = 0; realizationIndex < variable.getDimension(realizationDimensionIndex).getLength(); realizationIndex++) {
							IExchangeItem exchangeItem = new NetcdfScalarTimeSeriesExchangeItem(stationDimensionIndex, stationIndex,
									stationId, parameterId, realizationDimensionIndex, realizationIndex, Role.InOut, timeInfo, this);
							if (!this.ensembleExchangeItems.containsKey(realizationIndex)) {
								this.ensembleExchangeItems.put(realizationIndex, new ArrayList<IExchangeItem>());
							}
							List<IExchangeItem> oldValue = this.ensembleExchangeItems.get(realizationIndex);
							oldValue.add(exchangeItem);
							this.ensembleExchangeItems.put(realizationIndex, oldValue);
						}
					}
				}

				continue;
			}

			// TODO Support realization
			int timeDimensionIndex = variable.findDimensionIndex(timeVariable.getName());
			IGeometryInfo geometryInfo = NetcdfUtils.createGeometryInfo(variable, netcdfFile);
			if (dimensionCount == 3 && geometryInfo != null) {
				//if variable has data for a 2D grid time series.
				String parameterId = variable.getName();

				//create an exchangeItem that can read/write lazily.
				IQuantityInfo quantityInfo = new QuantityInfo(parameterId, variable.getUnitsString());
				int dimensionIndexToFlipForReadData = NetcdfUtils.getDimensionIndexToFlipFor2DGrid(variable, this.netcdfFile,
						this.internalGridStartCorner);
				IExchangeItem exchangeItem = new NetcdfGridTimeSeriesExchangeItem(parameterId, Role.InOut,
						timeInfo, quantityInfo, geometryInfo, this, timeDimensionIndex, dimensionIndexToFlipForReadData);
				this.exchangeItems.add(exchangeItem);

				continue;
			}
		}
	}

	/**
	 * Read data from netcdf file.
	 *
	 * @param file
	 * @throws IOException
	 */
	private void readNetcdfFile() throws IOException  {
		this.exchangeItems.clear();

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
			String exchangeItemId = createExchangeItemId(variable);
			if (variable.getDimensions().isEmpty()) {//if scalar.
				exchangeItem = new DoubleExchangeItem(exchangeItemId, Role.InOut, variable.readScalarDouble());

			} else {//if array.
				ArrayExchangeItem arrayBasedExchangeItem = new ArrayExchangeItem(exchangeItemId, IPrevExchangeItem.Role.InOut);
				arrayBasedExchangeItem.setQuantityInfo(new QuantityInfo(exchangeItemId, variable.getUnitsString()));
				IArrayTimeInfo newTimeInfo = NetcdfUtils.createTimeInfo(variable, netcdfFile, timeInfoCache);
				if (newTimeInfo != null) {
					arrayBasedExchangeItem.setTimeInfo(newTimeInfo);
				} else {
					continue;
				}
				//TODO cache variables with spatial coordinates. AK
				arrayBasedExchangeItem.setGeometryInfo(NetcdfUtils.createGeometryInfo(variable, netcdfFile));
				arrayBasedExchangeItem.setArray((IArray) NetcdfUtils.readData(variable));
				exchangeItem = arrayBasedExchangeItem;
			}

			this.exchangeItems.add(exchangeItem);
		}
	}

    
    public String[] getExchangeItemIDs() {
        String[] exchangeItemIDs = new String[this.exchangeItems.size()];

        for (int n = 0; n < this.exchangeItems.size(); n++) {
            exchangeItemIDs[n] = this.exchangeItems.get(n).getId();
        }

        return exchangeItemIDs;
    }

    
    public String[] getExchangeItemIDs(Role role) {
        ArrayList<String> exchangeItemIDs = new ArrayList<String>();

        for (int n = 0; n < this.exchangeItems.size(); n++) {
            IExchangeItem exchangeItem = this.exchangeItems.get(n);
            if (exchangeItem.getRole().equals(role)) {
                exchangeItemIDs.add(exchangeItem.getId());
            }
        }

        return exchangeItemIDs.toArray(new String[exchangeItemIDs.size()]);
    }

    
    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        for (int n = 0; n < this.exchangeItems.size(); n++) {
            IExchangeItem exchangeItem = this.exchangeItems.get(n);
            if (exchangeItem.getId().equals(exchangeItemID)) {
                return exchangeItem;
            }
        }

        return null;
    }

	public void addExchangeItem(IExchangeItem item) {
        IExchangeItem newItem = storeExchangeItem(item);

		//create metadata for the given exchangeItem.
		NetcdfUtils.createMetadata(this.netcdfFile, newItem, this.timeInfoTimeDimensionMap, this.uniqueTimeVariableCount,
				this.geometryInfoGridVariablePropertiesMap, this.uniqueFaceDimensionCount, this.uniqueLatLonDimensionCount);
	}

    public void addScalarExchangeItems(IExchangeItem[] items) {
        IExchangeItem[] newItems = new IExchangeItem[items.length];
        for (int i=0; i<items.length; i++){
            newItems[i] = storeExchangeItem(items[i]);
        }

        //create metadata for the given exchangeItem.
        NetcdfUtils.createScalarMetadata(this.netcdfFile, newItems, this.timeInfoTimeDimensionMap, this.uniqueTimeVariableCount,
				this.stationIdList);
    }

    private IExchangeItem storeExchangeItem(IExchangeItem item) {
        if (!this.netcdfFile.isDefineMode()) {
            throw new RuntimeException(getClass().getSimpleName() + ": cannot add new exchangeItems to an existing netcdf file.");
        }

        IExchangeItem newItem;
        if (this.lazyWriting) {
            //copy exchange item.
            ArrayExchangeItem itemCopy = new ArrayExchangeItem(item.getId(), item.getRole());
            itemCopy.copyValuesFromItem((IExchangeItem) item);
            newItem = itemCopy;
        } else {
            //add exchange item.
            newItem = (IExchangeItem) item;
        }

        //store new item.
        this.exchangeItems.add(newItem);
        return newItem;
    }

    
	public void finish() {
		makeSureFileHasBeenCreated();

		if (this.lazyWriting) {
			//write data to netcdf file.
			Results.putMessage(this.getClass().getSimpleName() + ": writing data to file " + this.file.getAbsolutePath());
			writeData(this.netcdfFile, this.exchangeItems);
		} else {//if lazyWriting is false, then the variable data has already been written to netcdf file in exchangeItem.setValues methods,
			//and the metadata variable values have already been written to netcdf file in method createFile.
			//do nothing.
		}

		try {
			this.netcdfFile.close();
		} catch (IOException e) {
			throw new RuntimeException("Error while closing netcdf file '" + this.file.getAbsolutePath()
					+ "'. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * If the netcdf file has not yet been created, then creates it. Otherwise does nothing.
	 */
	private void makeSureFileHasBeenCreated() {
		if (this.netcdfFile.isDefineMode()) {
			createFile();
		}
	}

	/**
	 * Actually creates the new netcdf file. This can be done only once and has to be done before any data can be written
	 * and has to be done after all variables and attributes have been created.
	 * This method also writes the metadata variable values right after the file has been created.
	 */
	private void createFile() {
		try {
			this.netcdfFile.create();
		} catch (IOException e) {
			throw new RuntimeException("Error while creating new netcdf file '" + this.netcdfFile.getLocation()
					+ "'. Message was: " + e.getMessage(), e);
		}

		if (!this.lazyWriting) {//if lazyWriting is false, then here write the metadata variable values, i.e. times and spatial coordinates.
			try {
				NetcdfUtils.writeMetadata(this.netcdfFile, this.timeInfoTimeDimensionMap, this.geometryInfoGridVariablePropertiesMap,
                        this.stationIdList);
			} catch (Exception e) {
				throw new RuntimeException("Error while writing metadata values to netcdf file '" + this.file.getAbsolutePath()
						+ "'. Message was: " + e.getMessage(), e);
			}
		}
	}

	private static void addGlobalAttributes(NetcdfFileWriteable netcdfFile) {
		netcdfFile.addGlobalAttribute("title", "Netcdf data");
		netcdfFile.addGlobalAttribute("institution", "Deltares");
		netcdfFile.addGlobalAttribute("source", "written by OpenDA");
		netcdfFile.addGlobalAttribute("history", "Created at " + new Date(System.currentTimeMillis()));
		netcdfFile.addGlobalAttribute("references", "http://www.openda.org");
		netcdfFile.addGlobalAttribute("Conventions", "CF-1.6");
	}

	/**
	 * Writes all data for the given exchangeItems to the given netcdfFile.
	 * Only executed when this.lazyWriting = true
     *
	 * @param netcdfFile
	 * @param exchangeItems to write.
	 */
	private void writeData(NetcdfFileWriteable netcdfFile, List<IExchangeItem> exchangeItems) {
		int iItem = 0;
		for (IExchangeItem exchangeItem : exchangeItems){
            IGeometryInfo geometryInfo = exchangeItem.getGeometryInfo();
            if (geometryInfo != null){
                int latLength = ((ArrayGeometryInfo) geometryInfo).getLatitudeArray().length();
                int lonLength = ((ArrayGeometryInfo) geometryInfo).getLongitudeArray().length();
                if (latLength == lonLength && latLength == 1) {
                    // scalar time series
                    if (iItem==0){
                        try {
                            NetcdfUtils.writeMetadata(this.netcdfFile, this.timeInfoTimeDimensionMap, this.geometryInfoGridVariablePropertiesMap,
                                    this.stationIdList);
                        } catch (Exception e) {
                            throw new RuntimeException("Error while writing metadata values to netcdf file '" + this.file.getAbsolutePath()
                                    + "'. Message was: " + e.getMessage(), e);
                        }
                    }
                    int nTime = exchangeItem.getTimeInfo().getTimes().length;
                    double[] values = ((IArray) exchangeItem.getValues()).getValuesAsDoubles(); // for the time being only for TimeSeries which is of type IArray
                    for (int iTime=0; iTime<nTime; iTime++){
                        writeDataForExchangeItemForSingleTimeSingleLocation(exchangeItem,iTime,iItem,new double[]{values[iTime]});
                    }
                    iItem++;
                } else {
                    // 2D/gridded time series
                    throw new UnsupportedOperationException("method writeData not yet implemented for 2D data."+this.getClass().getName());
                }
            } else {
                //TODO: replace this SWAN specific implementation with the above generic ones.
				String exchangeItemId = exchangeItem.getId();
				//TODO: add netcdf writers for various data type / exchangeitems.
				//For SWAN state file, only wave_spectrum is modified and rewritten.
				if (exchangeItemId.equalsIgnoreCase("sea_surface_wave_directional_variance_spectral_density")){
					double[] dblValues = exchangeItem.getValuesAsDoubles();
					int my=31;
					int mx=61;
					int wave_frequency=25;
					int wave_direction=18;
					// get dimensions:
					// TODO: in the future, dimensions should be available in the exchangeItem as part of meta data
					// This will avoid having to read the netcdf file for obtaining the dimensions.
					List<Dimension> dimensions = netcdfFile.getDimensions();
					int nDim = dimensions.size();
					for (int iDim=0; iDim<nDim; iDim++){
						Dimension dimension = dimensions.get(iDim);
						if (dimension.getName().equalsIgnoreCase("my")){
							my = dimension.getLength();
						} else if (dimension.getName().equalsIgnoreCase("mx")){
							mx = dimension.getLength();
						} else if (dimension.getName().equalsIgnoreCase("wave_frequency")){
							wave_frequency = dimension.getLength();
						} else if (dimension.getName().equalsIgnoreCase("wave_direction")){
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
						String varName = "wave_spectrum";
						netcdfFile.write(varName,values);
					} catch (IOException e) {
						e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
					} catch (InvalidRangeException e) {
						e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
					}
				}
			}
		}
	}

	private Variable getVariableForExchangeItem(IExchangeItem item) {
		Variable variable = this.netcdfFile.findVariable(item.getQuantityInfo().getQuantity());
		if (variable == null) {
			throw new IllegalStateException(getClass().getSimpleName() + ": Cannot find variable for exchangeItem with id '"
					+ item.getId() + "' in netcdf file " + this.file.getAbsolutePath());
		}
		return variable;
	}

	public double[] readDataForExchangeItemForSingleLocation(IExchangeItem item, int stationDimensionIndex, int stationIndex) {
		Variable variable = getVariableForExchangeItem(item);
		return NetcdfUtils.readDataForVariableForSingleLocation(variable, stationDimensionIndex, stationIndex);
	}

	public double[] readDataForExchangeItemForSingleLocationAndRealization(IExchangeItem item, int stationDimensionIndex, int stationIndex, int realizationDimensionIndex, int realizationIndex) {
		Variable variable = getVariableForExchangeItem(item);
		return NetcdfUtils.readDataForVariableForSingleLocationAndRealization(variable, stationDimensionIndex, stationIndex, realizationDimensionIndex, realizationIndex);
	}

	public double[] readDataForExchangeItemFor2DGridForSingleTime(IExchangeItem item, int timeDimensionIndex, int timeIndex,
			int dimensionIndexToFlip) {
		Variable variable = getVariableForExchangeItem(item);
		return NetcdfUtils.readDataForVariableFor2DGridForSingleTime(variable, timeDimensionIndex, timeIndex,
				dimensionIndexToFlip);
	}

    public void writeDataForExchangeItemForSingleTime(IExchangeItem item, int timeDimensionIndex, int timeIndex, double[] values) {
        Variable variable = getVariableForExchangeItem(item);
        makeSureFileHasBeenCreated();
        NetcdfUtils.writeDataForVariableForSingleTime(this.netcdfFile, variable, timeDimensionIndex, timeIndex, values);
    }

    public void writeDataForExchangeItemForSingleTimeSingleLocation(IExchangeItem item, int timeIndex, int stationIndex, double[] values) {
        Variable variable = getVariableForExchangeItem(item);
        makeSureFileHasBeenCreated();
        NetcdfUtils.writeDataForVariableForSingleTimeSingleLocation(this.netcdfFile, variable, timeIndex, stationIndex, values);
    }

    /**
     * If the given variable has a standard_name attribute, then returns that.
     * Otherwise, if the given variable has a long_name attribute, then returns that.
     * Otherwise returns the variable name.
     *
     * @param variable
     * @return String exchangeItemId.
     */
    //TODO never use standard name or long name as exchangeItemId, since these are not always unique within a file. AK
    private static String createExchangeItemId(Variable variable) {
        String exchangeItemId;

        Attribute standardNameAttribute = variable.findAttributeIgnoreCase(NetcdfUtils.STANDARD_NAME_ATTRIBUTE_NAME);
        if (standardNameAttribute != null && standardNameAttribute.getStringValue() != null
                && !standardNameAttribute.getStringValue().trim().isEmpty()) {
            exchangeItemId = standardNameAttribute.getStringValue();

        } else {//if standard name not defined.
            Attribute longNameAttribute = variable.findAttributeIgnoreCase(NetcdfUtils.LONG_NAME_ATTRIBUTE_NAME);
            if (longNameAttribute != null && longNameAttribute.getStringValue() != null
                    && !longNameAttribute.getStringValue().trim().isEmpty()) {
                exchangeItemId = longNameAttribute.getStringValue();

            } else {//if long name not defined.
                exchangeItemId = variable.getName();
            }
        }

        return exchangeItemId;
    }
}
