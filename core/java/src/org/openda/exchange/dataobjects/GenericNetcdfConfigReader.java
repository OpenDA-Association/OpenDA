package org.openda.exchange.dataobjects;

import org.openda.core.io.castorgenerated.*;
import org.openda.utils.DimensionIndex;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.HashMap;

public class GenericNetcdfConfigReader {

	private GenericNetcdfConfig genericNetcdfConfig;

	public GenericNetcdfConfigReader(File genericNetcdfConfigFile) {
		TNC_dataObjectConfig tncDataObjectConfig = (TNC_dataObjectConfig) CastorUtils.parse(genericNetcdfConfigFile, TNC_dataObjectConfig.class);
		genericNetcdfConfig = parseGenericNetcdfConfig(genericNetcdfConfigFile, tncDataObjectConfig);
	}

	public GenericNetcdfConfig getGenericNetcdfConfig() {
		return genericNetcdfConfig;
	}

	public static GenericNetcdfConfig parseGenericNetcdfConfig(File genericNetcdfConfigFile, TNC_dataObjectConfig tncDataObjectConfig) {

		GenericNetcdfConfig netcdfConfig = new GenericNetcdfConfig();

		org.openda.core.io.castorgenerated.TNC_arrayExchangeItem[] arrayExchangeItemsXML = tncDataObjectConfig.getArrayExchangeItem();
		for (org.openda.core.io.castorgenerated.TNC_arrayExchangeItem arrayExchangeItemXML: arrayExchangeItemsXML) {
			String id = arrayExchangeItemXML.getId().getContent();
			String variable_name = arrayExchangeItemXML.getValues().getSelection().getVariable()[0].getName(); // only 1 variable is possible for array exchange item
			int nDimension = arrayExchangeItemXML.getValues().getSelection().getVariable()[0].getDimensionCount();
			HashMap<String, DimensionIndex> dimensions = new HashMap<>();
			if (nDimension>0){
				dimensions = new HashMap<>();
				for (int i=0; i<nDimension; i++){
					String dimension_name = arrayExchangeItemXML.getValues().getSelection().getVariable()[0].getDimension(i).getName();
					String dimension_index_string = arrayExchangeItemXML.getValues().getSelection().getVariable()[0].getDimension(i).getIndex();
					DimensionIndex dimension_index = new DimensionIndex(dimension_index_string,0); //TODO: check if it is okay to assume that dimensionBase is always 0
					dimensions.put(dimension_name,dimension_index);
				}
			}
			GenericNetcdfArrayConfig arrayConfig = new GenericNetcdfArrayConfig(id,variable_name,dimensions);
			netcdfConfig.arrayConfigs.put(id,arrayConfig);
		}

		TNC_timeSeriesExchangeItem[] timeseriesExchangeItemsXML = tncDataObjectConfig.getTimeSeriesExchangeItem();
		for (TNC_timeSeriesExchangeItem timeseriesExchangeItemXML: timeseriesExchangeItemsXML) {
			TNC_id tnc_id = timeseriesExchangeItemXML.getId();
			String template = tnc_id.getTemplate();
			String id = null;
			if (template == null) {
				id = timeseriesExchangeItemXML.getId().getContent();
			}
			GenericNetcdfTSConfig tsConfig = new GenericNetcdfTSConfig(id,template,timeseriesExchangeItemXML);
			if (id==null){
				netcdfConfig.tsConfigs.put(template,tsConfig);
			} else {
				netcdfConfig.tsConfigs.put(id, tsConfig);
			}
		}

		return netcdfConfig;
	}
}
