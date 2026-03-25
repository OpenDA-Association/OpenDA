package org.openda.model_dflowfm;

import org.openda.exchange.AbstractDataObject;
import org.openda.utils.generalJavaUtils.StringUtilities;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import java.io.File;
import java.util.*;

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
			Variable areaNumberVar = netcdfFile.findVariable(AREA_NUMBER);
			if (areaNumberVar == null) throw new RuntimeException(String.format("Variable %s not found in netCDF file", AREA_NUMBER));
			int[] areaNumbers = (int[]) areaNumberVar.read().get1DJavaArray(int.class);
			Map<Integer, List<Integer>> areaNumberIndexListMap = new LinkedHashMap<>();
			for (int i = 0; i < areaNumbers.length; i++) {
				int areaNumber = areaNumbers[i];
				List<Integer> indexList = areaNumberIndexListMap.computeIfAbsent(areaNumber, k -> new ArrayList<>());
				indexList.add(i);
			}
			areaNumberIndexListMap.forEach((index, list) -> System.out.printf("Area number %d has %d indices%n", index, list.size()));
			for (Variable variable : variables) {
				String varName = variable.getShortName();
				int[] shape = variable.getShape();
				if (!variablesForExchangeItems.contains(varName)) continue;
				if (dataFormat.variableDimensions != shape.length) throw new RuntimeException(String.format("Variable %s has length %d dimensions, but expected %d dimensions for data format %s", variable.getShortName(), shape.length, dataFormat.variableDimensions, dataFormat.name()));
				double[] values = (double[]) variable.read().get1DJavaArray(Double.class);
				if (values.length != areaNumbers.length) throw new RuntimeException(String.format("Variable %s has length %d, but expected length %d equal to the number of areas in variable %s", variable.getShortName(), values.length, areaNumbers.length, AREA_NUMBER));
				Set<Map.Entry<Integer, List<Integer>>> indicesPerAreaNumber = areaNumberIndexListMap.entrySet();
				for (Map.Entry<Integer, List<Integer>> entry : indicesPerAreaNumber) {
					List<Integer> indices = entry.getValue();
					double[] eiValues = new double[indices.size()];
					for (int i = 0; i < indices.size(); i++) {
						eiValues[i] = values[indices.get(i)];
					}
					String id = String.format("%s_%s_%d", idPrefix, varName, entry.getKey());
					exchangeItems.put(id, new DFlowFMNetcdfSampleFileExchangeItem(id, indices, eiValues));
				}
			}

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void finish() {

	}
}
