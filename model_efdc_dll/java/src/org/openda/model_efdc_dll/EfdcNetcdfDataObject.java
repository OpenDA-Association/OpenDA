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

package org.openda.model_efdc_dll;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.dataobjects.GridVariableProperties;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.IComposableDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.ITimeInfo;
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
//TODO after OpenDA release 2.1 make this into a subclass of NetcdfDataObject with the only difference
//that this has lazyReading = true and lazyWriting = false by default. AK
public class EfdcNetcdfDataObject implements IComposableDataObject {

	private File file = null;
	private NetcdfFileWriteable netcdfFile = null;
	private List<IExchangeItem> exchangeItems = new ArrayList<IExchangeItem>();

	private Map<ITimeInfo, Dimension> timeInfoTimeDimensionMap = new LinkedHashMap<ITimeInfo, Dimension>();
	private Map<IGeometryInfo, GridVariableProperties> geometryInfoGridVariablePropertiesMap =
			new LinkedHashMap<IGeometryInfo, GridVariableProperties>();
	private int[] uniqueTimeVariableCount = new int[]{0};
	private int[] uniqueFaceDimensionCount = new int[]{0};
	private int[] uniqueLatLonDimensionCount = new int[]{0};

	/**
	 * Default is false for backwards compatibility.
	 */
	private boolean lazyReading = false;
	/**
	 * Default is true for backwards compatibility.
	 */
	private boolean lazyWriting = true;

    public EfdcNetcdfDataObject() {
	}

	/**
	 * For this dataObject lazyReading is always true and lazyWriting is always false.
	 *
	 * @param workingDir 
	 * @param arguments required argument: the pathname of the data file relative to the given workingDir.
	 */
	
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		this.file = new File(workingDir, fileName);

		this.lazyReading = true;
		this.lazyWriting = false;

		if (this.file.exists()) {
			//open existing netcdf file. Always use NetcdfFileWriteable in case data needs to be written later.
			try {
				this.netcdfFile = NetcdfFileWriteable.openExisting(this.file.getAbsolutePath(), true);
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
				this.netcdfFile = NetcdfFileWriteable.createNew(this.file.getAbsolutePath(), true);
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
		if (stationIndexIdMap.isEmpty()) {//if no stations found.
			Results.putMessage(this.getClass().getSimpleName() + ": no stations found in netcdf file " + this.file.getAbsolutePath()
					+ ". The station_id variable should be called '" + NetcdfUtils.STATION_ID_VARIABLE_NAME + "'.");
			return;
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

			//skip variables that are not two dimensional.
			if (variable.getDimensions().size() != 2) {
				continue;
			}

			//skip variables that do not depend on time.
			Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
			if (timeVariable == null) {
				continue;
			}
			ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);

			//if variable is a scalar time series.
			int stationDimensionIndex = variable.findDimensionIndex(NetcdfUtils.STATION_DIMENSION_NAME);
			if (stationDimensionIndex == -1) {
				Results.putMessage(this.getClass().getSimpleName() + ": no station dimension found for variable "
						+ variable.getName() + " in netcdf file " + this.file.getAbsolutePath()
						+ ". The stations dimension should be called '" + NetcdfUtils.STATION_DIMENSION_NAME + "'.");
				continue;
			}
			int stationCount = variable.getDimension(stationDimensionIndex).getLength();
			String parameterId = variable.getName();
			//create an exchangeItem that can read/write lazily, for each location in this variable.
			for (int n = 0; n < stationCount; n++) {
				int stationIndex = n;
				String stationId = stationIndexIdMap.get(n);
				IExchangeItem exchangeItem = new EfdcNetcdfScalarTimeSeriesExchangeItem(stationDimensionIndex, stationIndex,
						stationId, parameterId, Role.InOut, timeInfo, this);
				this.exchangeItems.add(exchangeItem);
			}
		}
	}

	/**
	 * Read data from netcdf file.
	 *
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
				arrayBasedExchangeItem.setTimeInfo(NetcdfUtils.createTimeInfo(variable, netcdfFile, timeInfoCache));
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

		//create metadata for the given exchangeItem.
		NetcdfUtils.createMetadata(this.netcdfFile, newItem, this.timeInfoTimeDimensionMap, this.uniqueTimeVariableCount,
				this.geometryInfoGridVariablePropertiesMap, this.uniqueFaceDimensionCount, this.uniqueLatLonDimensionCount);
	}

	/**
	 * Make sure that any data written to this.netcdfFile object is moved from the cache to the actual netcdf file.
	 */
	public void flush() {
		makeSureFileHasBeenCreated();

		try {
			this.netcdfFile.flush();
		} catch (IOException e) {
			throw new RuntimeException("Error while flushing data to netcdf file '" + this.file.getAbsolutePath()
					+ "'. Message was: " + e.getMessage(), e);
		}
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
                        new ArrayList<String>());
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
	 *
	 * @param netcdfFile
	 * @param exchangeItems to write.
	 */
	private static void writeData(NetcdfFileWriteable netcdfFile, List<IExchangeItem> exchangeItems) {
		for (IExchangeItem exchangeItem : exchangeItems){
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

	public void writeDataForExchangeItemForSingleTime(IExchangeItem item, int timeDimensionIndex, int timeIndex, double[] values) {
		Variable variable = getVariableForExchangeItem(item);
		makeSureFileHasBeenCreated();
		NetcdfUtils.writeDataForVariableForSingleTime(this.netcdfFile, variable, timeDimensionIndex, timeIndex, values);
	}

	/**
     * If the given variable has a standard_name attribute, then returns that.
     * Otherwise, if the given variable has a long_name attribute, then returns that.
     * Otherwise returns the variable name.
     *
     * @param variable
     * @return String exchangeItemId.
     */
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
