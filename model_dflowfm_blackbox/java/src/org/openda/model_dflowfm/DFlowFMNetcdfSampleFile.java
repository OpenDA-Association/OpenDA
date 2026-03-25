package org.openda.model_dflowfm;

import org.openda.exchange.AbstractDataObject;
import org.openda.utils.generalJavaUtils.StringUtilities;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DFlowFMNetcdfSampleFile extends AbstractDataObject {

	enum DataFormat {TimeIndependent(1), TimeConstant(2);

		private final int variableDimensions;

		DataFormat(int variableDimensions) {
			this.variableDimensions = variableDimensions;
		}

	}

	public static final String AREA_NUMBER = "area_number";
	private static final String ID_PREFIX = "idPrefix";
	private static final String NETCDF_VARIABLE = "netcdfVariable";
	private static final String DATA_FORMAT = "dataFormat";

	private Set<String> variablesForExchangeItems = new HashSet<>();
	private DataFormat dataFormat;
	private String idPrefix;
	private File file = null;
	private NetcdfFile netcdfFile = null;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 4)
			throw new RuntimeException(String.format("Incorrect number of arguments. Please specify [%s, %s, %s] as key=value pairs", ID_PREFIX, NETCDF_VARIABLE, DATA_FORMAT));
		String fileName = arguments[0];
		this.file = new File(workingDir, fileName);
		for (int i = 1; i < arguments.length; i++) {
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
					throw new RuntimeException(String.format("Unknown key %s. Please only specify [%s, %s, %s] as key=value pairs", key, ID_PREFIX, NETCDF_VARIABLE, DATA_FORMAT));
			}
		}
		if (idPrefix == null || variablesForExchangeItems.isEmpty() || dataFormat == null)
			throw new RuntimeException(String.format("Arguments missing. Please specify [%s, %s, %s] as key=value pairs", ID_PREFIX, NETCDF_VARIABLE, DATA_FORMAT));

		try {
			this.netcdfFile = NetcdfFile.open(this.file.getAbsolutePath());
			List<Variable> variables = netcdfFile.getVariables();
			int[] areaNumbers = null;
			for (Variable variable : variables) {
				String varName = variable.getShortName();
				int[] shape = variable.getShape();
				if (varName.equals(AREA_NUMBER)) {
					areaNumbers = (int[]) variable.read().get1DJavaArray(int.class);
					continue;
				}
				if (!variablesForExchangeItems.contains(varName)) continue;
				if (dataFormat.variableDimensions != shape.length) throw new RuntimeException(String.format("Variable %s has %d dimensions, but expected %d dimensions for data format %s", variable.getShortName(), shape.length, dataFormat.variableDimensions, dataFormat.name()));

			}
			if (areaNumbers == null) throw new RuntimeException(String.format("Variable %s not found in netCDF file", AREA_NUMBER));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void finish() {

	}
}
