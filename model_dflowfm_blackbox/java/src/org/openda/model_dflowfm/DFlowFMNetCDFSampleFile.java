package org.openda.model_dflowfm;

import org.openda.exchange.AbstractDataObject;
import org.openda.utils.generalJavaUtils.StringUtilities;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

public class DFlowFMNetCDFSampleFile extends AbstractDataObject {

	enum DataFormat {TimeConstant, TimeIndependent}

	private static final String ID_PREFIX = "idPrefix";
	private static final String NETCDF_VARIABLE = "netcdfVariable";
	private static final String DATA_FORMAT = "dataFormat";

	private Set<String> variablesForExchangeItems = new HashSet<>();
	private DataFormat dataFormat;
	private String idPrefix;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 4) {
			throw new RuntimeException(String.format("Incorrect number of arguments. Please specify [%s, %s, %s] as key=value pairs", ID_PREFIX, NETCDF_VARIABLE, NETCDF_VARIABLE));
		}
		for (int i = 0; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			if (keyValue == null || keyValue.length != 2) throw new RuntimeException(String.format("Invalid key=value pair: %s", argument));
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case ID_PREFIX:
					idPrefix = value;
					continue;
				case NETCDF_VARIABLE:
					variablesForExchangeItems.add(value);
					continue;
				case DATA_FORMAT:
					dataFormat = DataFormat.valueOf(value);
					continue;
				default:
					throw new RuntimeException(String.format("Unknown key %s. Please only specify [%s, %s, %s] as key=value pairs", key, ID_PREFIX, NETCDF_VARIABLE, NETCDF_VARIABLE));
			}
		}
		if (idPrefix == null || variablesForExchangeItems.isEmpty() || dataFormat == null)
			throw new RuntimeException(String.format("Arguments missing. Please specify [%s, %s, %s] as key=value pairs", ID_PREFIX, NETCDF_VARIABLE, NETCDF_VARIABLE));


	}

	@Override
	public void finish() {

	}
}
