package org.openda.exchange.dataobjects;

import org.openda.exchange.GenericNetcdfArrayExchangeItem;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Array;
import org.openda.utils.DimensionIndex;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GenericNetcdfDataObject implements IDataObject {
	HashMap<String, IExchangeItem> exchangeItems = new HashMap<>();
	private GenericNetcdfConfig ncConfig;
	private NetcdfFile netcdfFile;
	private File ncFilePath;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		ncFilePath = new File(workingDir, arguments[0]);
		File xmlFilePath = new File(workingDir, arguments[1]);
		GenericNetcdfConfigReader ncConfigReader = new GenericNetcdfConfigReader(xmlFilePath);
		this.ncConfig = ncConfigReader.getGenericNetcdfConfig();
		// now create a handle to netCDF
		try {
			this.netcdfFile = NetcdfFile.open(ncFilePath.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("GenericNetcdfDataObject could not open netcdf file " + ncFilePath.getAbsolutePath());
		}

		// create array exchangeItems
		if (this.ncConfig.arrayConfigs.size()>0){
			GenericNetcdfArrayConfig arrayConfig;
			for (String keyset: this.ncConfig.arrayConfigs.keySet()){
				arrayConfig = ncConfig.arrayConfigs.get(keyset);
				IExchangeItem arrayExchangeItem = new GenericNetcdfArrayExchangeItem(arrayConfig.getExchangeitemId(),this);
				this.exchangeItems.put(arrayExchangeItem.getId(),arrayExchangeItem);
			}
		}

		// create time series exchangeItems
		if (this.ncConfig.tsConfigs.size()>0){
			GenericNetcdfTSConfig tsConfig;
			for (String keyset: this.ncConfig.tsConfigs.keySet()){
				tsConfig = ncConfig.tsConfigs.get(keyset);
				String template = tsConfig.getTemplate();
				if (template != null){
					// create exchangeItemId's
					String exchangeItemId;
					String metaQuantity = "";
					if (template.toLowerCase().contains("${meta:quantity}")){
						metaQuantity = tsConfig.getMetaQuantity();
					}
					String[] metaLocVarValuesXML = tsConfig.getMetaLocationVariableValues();
					String[] metaLocVarValuesNetCDF = null;
					try {
						Map<Integer, String> temp = NetcdfUtils.readAndStoreStationIdsMap(netcdfFile, tsConfig.getMetaLocationVariableName());
						metaLocVarValuesNetCDF = new String[temp.size()];
						for (int i=0; i<metaLocVarValuesNetCDF.length; i++){
							metaLocVarValuesNetCDF[i] = temp.get(i);
						}
					} catch (IOException e) {
						e.printStackTrace();
					}
					if (metaLocVarValuesXML[0]==null){
						// when not specified, the values (i.e. names of stations/locations) will be read from netCDF and for each
						// location an exchangeItem will be created.
						metaLocVarValuesXML = metaLocVarValuesNetCDF;
					}
					// get times:
					String timeVarName = tsConfig.getTimesVariableName();
					Variable timeVariable = netcdfFile.findVariable(timeVarName);
					Map<Variable, IArrayTimeInfo> timeInfoMap = new HashMap<>();
					IArrayTimeInfo timeInfoCache = null;
					timeInfoMap.put(timeVariable,timeInfoCache);
					IArrayTimeInfo timeInfo = NetcdfUtils.createTimeInfo(timeVariable, netcdfFile, timeInfoMap);

					for (int iMetaLocVarValues=0; iMetaLocVarValues<metaLocVarValuesXML.length; iMetaLocVarValues++) {
						// get values:
						Variable valuesVariable = netcdfFile.findVariable(tsConfig.getValuesVariableName());
						int[] origin = createOrigin(valuesVariable);
						int[] sizeArray = valuesVariable.getShape();

						Variable metaLocationVariable = netcdfFile.findVariable(tsConfig.getMetaLocationVariableName());
						List<Dimension> metaLocDimensions = metaLocationVariable.getDimensions();
						int dimensionIndex = -1;
						String metaDimensionName;
						for (int i = 0; i < metaLocDimensions.size(); i++) {
							metaDimensionName = metaLocDimensions.get(i).getShortName();
							if (valuesVariable.findDimensionIndex(metaDimensionName) != -1) {
								dimensionIndex = valuesVariable.findDimensionIndex(metaDimensionName);
							}
						}
						if (dimensionIndex == -1) {
							throw new RuntimeException("GenericNetcdfDataObject, TimeSeriesExchangeItem: no shared dimension is available between <meta><location><variable> and <values><variable>." +
								"The specified <meta><location><variable> cannot be used to select the <values><variable>");
						}

						origin[dimensionIndex] = Arrays.asList(metaLocVarValuesNetCDF).indexOf(metaLocVarValuesXML[iMetaLocVarValues]);
						sizeArray[dimensionIndex] = 1;

						HashMap<String, DimensionIndex> valuesDimension = tsConfig.getValuesVariableDimensions();
						int dimIndex;
						for (String valuesDimensionName : valuesDimension.keySet()) {
							dimIndex = valuesVariable.findDimensionIndex(valuesDimensionName);
							if (dimIndex == -1) {
								throw new RuntimeException("GenericNetcdfDataObject, TimeSeries, variable " + tsConfig.getValuesVariableName() + " doesn't have the following dimension-variable: " + valuesDimensionName);
							}
							DimensionIndex dimIndexDI = valuesDimension.get(valuesDimensionName);
							int startIndex = dimIndexDI.getStart();
							int endIndex = dimIndexDI.getEnd();
							if (startIndex == -1) {
								origin[dimIndex] = sizeArray[dimIndex] - 1;
								sizeArray[dimIndex] = 1;
							} else {
								origin[dimIndex] = startIndex;
								sizeArray[dimIndex] = endIndex - startIndex;
							}
						}
						String metaLayerVariableName = tsConfig.getMetaLayerVariableName();
						double[] values = null;
						if (metaLayerVariableName==null) {

							values = NetcdfUtils.readSelectedData(valuesVariable, origin, sizeArray, -1);

							if (values.length != timeInfo.getTimes().length) {
								throw new RuntimeException("GenericNetcdfDataObject, TimeSeriesExchangeItem: the length of values and times are not identical. " +
									"Please check the configuration of the GenericNetcdfDataObject. " +
									"Include all <meta> selection variables in the template, e.g. ${meta:location}, ${meta:layer}, or ${meta:quantity}.");
							}

							exchangeItemId = template.replace("${meta:location}", metaLocVarValuesXML[iMetaLocVarValues]);
							exchangeItemId = exchangeItemId.replace("${meta:quantity}", metaQuantity);

							IExchangeItem tsExchangeItem = new GenericNetcdfTSExchangeItem(exchangeItemId, timeInfo, values);
							this.exchangeItems.put(tsExchangeItem.getId(), tsExchangeItem);
						} else {
							Variable metaLayerVariable = netcdfFile.findVariable(metaLayerVariableName);
							List<Dimension> metaLayDimensions = metaLayerVariable.getDimensions();
							dimensionIndex = -1;
							for (int i = 0; i < metaLayDimensions.size(); i++) {
								metaDimensionName = metaLayDimensions.get(i).getShortName();
								if (valuesVariable.findDimensionIndex(metaDimensionName) != -1) {
									dimensionIndex = valuesVariable.findDimensionIndex(metaDimensionName);
								}
							}
							if (dimensionIndex == -1) {
								throw new RuntimeException("GenericNetcdfDataObject, TimeSeriesExchangeItem: no shared dimension is available between <meta><layer><variable> and <values><variable>." +
									"The specified <meta><layer><variable> cannot be used to select the <values><variable>");
							}

							int layerArraySize = sizeArray[dimensionIndex];
							sizeArray[dimensionIndex] = 1;
							for (int iLayer=0; iLayer<layerArraySize; iLayer++){
								origin[dimensionIndex] = iLayer;
								values = NetcdfUtils.readSelectedData(valuesVariable, origin, sizeArray, -1);
								exchangeItemId = template.replace("${meta:location}", metaLocVarValuesXML[iMetaLocVarValues]);
								exchangeItemId = exchangeItemId.replace("${meta:quantity}", metaQuantity);
								exchangeItemId = exchangeItemId.replace("${meta:layer}", Integer.toString(iLayer));
								IExchangeItem tsExchangeItem = new GenericNetcdfTSExchangeItem(exchangeItemId, timeInfo, values);
								this.exchangeItems.put(tsExchangeItem.getId(), tsExchangeItem);
							}
						}
					}
				} else {
					// get id:
					String id = tsConfig.getId();

					// get times:
					String timeVarName = tsConfig.getTimesVariableName();
					Variable timeVariable = netcdfFile.findVariable(timeVarName);
					Map<Variable, IArrayTimeInfo> timeInfoMap = new HashMap<>();
					IArrayTimeInfo timeInfoCache = null;
					timeInfoMap.put(timeVariable,timeInfoCache);
					IArrayTimeInfo timeInfo = NetcdfUtils.createTimeInfo(timeVariable, netcdfFile, timeInfoMap);

					// get values:
					Variable valuesVariable = netcdfFile.findVariable(tsConfig.getValuesVariableName());
					int[] origin = createOrigin(valuesVariable);
					int[] sizeArray = valuesVariable.getShape();
					HashMap<String, DimensionIndex> valuesDimension = tsConfig.getValuesVariableDimensions();
					int dimIndex;
					for (String valuesDimensionName : valuesDimension.keySet()) {
						dimIndex = valuesVariable.findDimensionIndex(valuesDimensionName);
						if (dimIndex == -1) {
							throw new RuntimeException("GenericNetcdfDataObject, TimeSeries, variable " + tsConfig.getValuesVariableName() + " doesn't have the following dimension-variable: " + valuesDimensionName);
						}
						DimensionIndex dimIndexDI = valuesDimension.get(valuesDimensionName);
						int startIndex = dimIndexDI.getStart();
						int endIndex = dimIndexDI.getEnd();
						if (startIndex == -1) {
							origin[dimIndex] = sizeArray[dimIndex] - 1;
							sizeArray[dimIndex] = 1;
						} else {
							origin[dimIndex] = startIndex;
							sizeArray[dimIndex] = endIndex - startIndex;
						}
					}
					double[] values = NetcdfUtils.readSelectedData(valuesVariable, origin, sizeArray, -1);
					IExchangeItem tsExchangeItem = new GenericNetcdfTSExchangeItem(id, timeInfo, values);
					this.exchangeItems.put(tsExchangeItem.getId(), tsExchangeItem);
				}

			}
		}
	}

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[0]);
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		for (String id : this.exchangeItems.keySet()){
			IExchangeItem exchangeItem = this.exchangeItems.get(id);
			double[] values = exchangeItem.getValuesAsDoubles();
			writeArrayExchangeItemValues(id,values);
		}
		try {
			netcdfFile.close();
		} catch (IOException e){
			e.printStackTrace();
		}

	}

	public static int[] createOrigin(Variable var) {
		int dimensionCount = var.getDimensions().size();
		int[] origin = new int[dimensionCount];
		Arrays.fill(origin, 0);
		return origin;
	}

	public void writeArrayExchangeItemValues(String id, double[] values) {
		GenericNetcdfArrayConfig arrayConfig = ncConfig.arrayConfigs.get(id);
		String variableName = arrayConfig.getNetcdfVariableName();
		Variable variable = netcdfFile.findVariable(variableName);
		int[] origin = createOrigin(variable);
		int[] sizeArray = variable.getShape();
		// loop over dimension variables and their selection indices, and then redefine origin and sizeArray
		for (int iDimVar=0; iDimVar<arrayConfig.getNetcdfDimensionVariables().length; iDimVar++){
			String dimensionVariable = arrayConfig.getNetcdfDimensionVariables()[iDimVar];
			int dimIndex = variable.findDimensionIndex(dimensionVariable);
			int startIndex = arrayConfig.getDimensionIndex(dimensionVariable).getStart();
			int endIndex = arrayConfig.getDimensionIndex(dimensionVariable).getEnd();
			if (startIndex==-1) {
				origin[dimIndex] = sizeArray[dimIndex]-1;
				sizeArray[dimIndex] = 1;
			}
			else {
				origin[dimIndex] = startIndex;
				sizeArray[dimIndex] = endIndex - startIndex;
			}
		}
		// write values to netCDF:
		NetcdfFileWriter netcdfFileWriter= null;
		try {
			netcdfFileWriter = NetcdfFileWriter.openExisting(this.ncFilePath.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
		}
		NetcdfUtils.writeSelectedData(netcdfFileWriter,variable, origin, sizeArray, values);

		try {
			netcdfFileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void setArrayExchangeItemValues(String id, double[] values) {
		writeArrayExchangeItemValues(id,values);
	}

	public double[] getArrayExchangeItemValues(String id) {
		GenericNetcdfArrayConfig arrayConfig = ncConfig.arrayConfigs.get(id);
		String variableName = arrayConfig.getNetcdfVariableName();
		// start work-around:
		// to make modified values directly accessible without closing the handle to netCDF file, it turns out
		// that we need to re-open the handle to netCDF everytime we want to read / get the values of an exchange item. This is
		// the consequence of the choice not to load the values in the initialization, but to read them directly from netCDF file
		// whenever the values are asked for.
		NetcdfFile netcdfFile = null;
		try {
			netcdfFile = NetcdfFile.open(ncFilePath.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
		}
		// stop work-around
		Variable variable = netcdfFile.findVariable(variableName);
		int[] origin = createOrigin(variable);
		int[] sizeArray = variable.getShape();
		// loop over dimension variables and their selection indices, and then redefine origin and sizeArray
		for (int iDimVar=0; iDimVar<arrayConfig.getNetcdfDimensionVariables().length; iDimVar++){
			String dimensionVariable = arrayConfig.getNetcdfDimensionVariables()[iDimVar];
			int dimIndex = variable.findDimensionIndex(dimensionVariable);
			if (dimIndex==-1){
				throw new RuntimeException("GenericNetcdfDataObject, variable " + variableName + " doesn't have the following dimension-variable: " + dimensionVariable);
			}
			int startIndex = arrayConfig.getDimensionIndex(dimensionVariable).getStart();
			int endIndex = arrayConfig.getDimensionIndex(dimensionVariable).getEnd();
			if (startIndex==-1) {
				origin[dimIndex] = sizeArray[dimIndex]-1;
				sizeArray[dimIndex] = 1;
			}
			else {
				origin[dimIndex] = startIndex;
				sizeArray[dimIndex] = endIndex - startIndex;
			}
		}
//		return NetcdfUtils.readSelectedData(variable, origin, sizeArray,-1);
		double[] values = NetcdfUtils.readSelectedData(variable, origin, sizeArray,-1);
		//TODO: the following only works for netCDF where _FillValue is defined. Otherwise, no missing value will be identified.
		// It may not be a problem. But if it is, then we need to add an option in the configuration to let user specify missingValue.
		double missingValue = NetcdfUtils.getMissingValueDouble(variable);
		for (int i=0; i<values.length; i++){
			if (values[i]==missingValue){
				values[i] = Double.NaN;
			}
		}
		return values;
	}
}
