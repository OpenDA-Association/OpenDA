package org.openda.exchange.dataobjects;

import org.openda.utils.DimensionIndex;

import java.util.HashMap;

public class GenericNetcdfArrayConfig {
	private final String exchangeitemId;
	private final String netcdf_variable_name;
	private final HashMap<String, DimensionIndex> netcdf_dimension_var;

	public GenericNetcdfArrayConfig(String id, String variable_name, HashMap<String, DimensionIndex> dimensions) {
		exchangeitemId = id;
		netcdf_variable_name = variable_name;
		netcdf_dimension_var = dimensions;
	}

	public String getExchangeitemId(){
		return exchangeitemId;
	}

	public String getNetcdfVariableName(){
		return netcdf_variable_name;
	}

	public String[] getNetcdfDimensionVariables(){
		return netcdf_dimension_var.keySet().toArray(new String[0]);
	}

	public DimensionIndex getDimensionIndex(String dimension_variable_id){
		return netcdf_dimension_var.get(dimension_variable_id);
	}
}
