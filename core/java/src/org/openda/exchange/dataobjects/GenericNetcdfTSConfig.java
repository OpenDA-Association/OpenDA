package org.openda.exchange.dataobjects;

import org.openda.core.io.castorgenerated.*;
import org.openda.utils.DimensionIndex;

import java.util.HashMap;

public class GenericNetcdfTSConfig {
	private final String id;
	private final String template;
	private final String metaLocationVariableName;
	private final String[] metaLocationVariableValues;
	private final String metaLayerVariableName;
	private final String metaQuantity;
	private final String timesVariableName;
	private final String valuesVariableName;
	private final HashMap<String, DimensionIndex> valuesVariableDimensions;

	public GenericNetcdfTSConfig(String id, String template, TNC_timeSeriesExchangeItem timeseriesExchangeItemXML) {
		this.id = id;
		this.template = template;
		if (timeseriesExchangeItemXML.getMeta() != null){
			if (timeseriesExchangeItemXML.getMeta().getLocation() == null){
				throw new RuntimeException("GenericNetcdfTSConfig: <location> is missing in <meta>. Please check the configuration file.");
			}
			TNC_variable[] metaLocationVariablesXML = timeseriesExchangeItemXML.getMeta().getLocation().getSelection().getVariable();
			this.metaLocationVariableName = metaLocationVariablesXML[0].getName();
			this.metaLocationVariableValues = new String[metaLocationVariablesXML.length];
			for (int i=0; i<metaLocationVariablesXML.length; i++){
				this.metaLocationVariableValues[i] = metaLocationVariablesXML[i].getValue();
			}
			if (timeseriesExchangeItemXML.getMeta().getLayer() != null) {
				TNC_variable[] metaLayerVariableXML = timeseriesExchangeItemXML.getMeta().getLayer().getSelection().getVariable();
				this.metaLayerVariableName = metaLayerVariableXML[0].getName();
			} else {
				this.metaLayerVariableName = null;
			}
			this.metaQuantity = timeseriesExchangeItemXML.getMeta().getQuantity();
			//NOTE: we only support one variable name for Meta-Location and Meta-Layer
		} else {
			this.metaLocationVariableName = null;
			this.metaLocationVariableValues = null;
			this.metaLayerVariableName = null;
			this.metaQuantity = null;
		}

		this.timesVariableName =  timeseriesExchangeItemXML.getTimes().getSelection().getVariable()[0].getName();
		this.valuesVariableName = timeseriesExchangeItemXML.getValues().getSelection().getVariable()[0].getName();
		//NOTE: we only support only one values and times variable per exchange item configuration

		int nDimension = timeseriesExchangeItemXML.getValues().getSelection().getVariable()[0].getDimensionCount();
		this.valuesVariableDimensions = new HashMap<>();
		if (nDimension>0){
			for (int i=0; i<nDimension; i++){
				String dimension_name = timeseriesExchangeItemXML.getValues().getSelection().getVariable()[0].getDimension(i).getName();
				String dimension_index_string = timeseriesExchangeItemXML.getValues().getSelection().getVariable()[0].getDimension(i).getIndex();
				DimensionIndex dimension_index = new DimensionIndex(dimension_index_string,0); //TODO: check if it is okay to assume that dimensionBase is always 0
				this.valuesVariableDimensions.put(dimension_name,dimension_index);
			}
		}
	}

	public String getId(){
		return this.id;
	}

	public String getTemplate(){
		return this.template;
	}

	public String getMetaLocationVariableName(){
		return this.metaLocationVariableName;
	}

	public String[] getMetaLocationVariableValues(){
		return this.metaLocationVariableValues;
	}

	public String getMetaLayerVariableName(){
		return this.metaLayerVariableName;
	}

	public String getMetaQuantity(){
		return this.metaQuantity;
	}

	public String getTimesVariableName(){
		return this.timesVariableName;
	}

	public String getValuesVariableName(){
		return this.valuesVariableName;
	}

	public HashMap<String, DimensionIndex> getValuesVariableDimensions(){
		return this.valuesVariableDimensions;
	}
}
