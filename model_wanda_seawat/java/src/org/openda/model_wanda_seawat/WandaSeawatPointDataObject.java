package org.openda.model_wanda_seawat;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.io.AsciiFileUtils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;

public class WandaSeawatPointDataObject extends AbstractDataObject {
	private static final int FIRST_VALUE_OFFSET = 1;
	private String header;
	private List<Double> heights;
	private List<Double> radii;
	private TreeMap<String, File> fileNamesToFiles;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (!workingDir.exists() || !workingDir.isDirectory()) {
			return;
		}

		if (arguments.length == 0 || arguments[0] == null) {
			return;
		}

		findFiles(workingDir, arguments[0]);

		Map<String, List<Double>> keyToValues = new HashMap<>();
		for (Map.Entry<String, File> entry : fileNamesToFiles.entrySet()) {
			List<String> lines = AsciiFileUtils.readLines(entry.getValue());

			if (lines.size() < 2) {
				return;
			}

			header = lines.get(0);
			radii = readHeader(lines);
			heights = new ArrayList<>();
			for (String line : lines.subList(1, lines.size())) {
				String[] values = line.trim().split("\\s+");

				if (values.length < 1) {
					continue;
				}

				double height = Double.parseDouble(values[0]);
				heights.add(height);

				readRow(keyToValues, entry, values, height);
			}
		}
	}

	private void readRow(Map<String, List<Double>> keyToValues, Map.Entry<String, File> entry, String[] values, double height) {
		for (int valueIndex = FIRST_VALUE_OFFSET; valueIndex < values.length; valueIndex++) {
			String value = values[valueIndex];
			String key = String.format("depth%s_radius%s", height, radii.get(valueIndex - FIRST_VALUE_OFFSET));
			List<Double> valueList = keyToValues.computeIfAbsent(key, v -> new ArrayList<>());
			valueList.add(Double.parseDouble(value));

			if (!Objects.equals(fileNamesToFiles.lastKey(), entry.getKey())) {
				continue;
			}

			DoublesExchangeItem doublesExchangeItem = new DoublesExchangeItem(key, IExchangeItem.Role.InOut, valueList.stream().mapToDouble(Double::doubleValue).toArray());
			exchangeItems.putIfAbsent(key, doublesExchangeItem);
		}
	}

	private void findFiles(File workingDir, String filePrefix) {
		fileNamesToFiles = new TreeMap<>();
		
		if (workingDir == null) {
			return;
		}
		
		File[] fileList = workingDir.listFiles();
		
		if (fileList == null) {
			return;
		}

		for (File file : fileList) {
			if (!file.getName().startsWith(filePrefix)) {
				continue;
			}

			fileNamesToFiles.putIfAbsent(file.getName(), file);
		}
	}

	private List<Double> readHeader(List<String> lines) {
		if (lines.isEmpty()) {
			return new ArrayList<>();
		}

		List<Double> columns = new ArrayList<>();
		String[] columnTitles = lines.get(0).split("\\s+");
		for (int columnTitleIndex = 1; columnTitleIndex < columnTitles.length; columnTitleIndex++) {
			String columnTitle = columnTitles[columnTitleIndex];
			columns.add(Double.parseDouble(columnTitle));
		}
		return columns;
	}

	@Override
	public void finish() {
		DecimalFormat decimalFormat = new DecimalFormat(".00000000E00", new DecimalFormatSymbols(Locale.UK));

		int fileCount = 0;
		for (Map.Entry<String, File> entry : fileNamesToFiles.entrySet()) {
			try (BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(entry.getValue()))) {
				bufferedWriter.write(header + "\n");

				for (Double height : heights) {
					bufferedWriter.write(String.format("%10s", String.format(Locale.UK, "%5.4f", height)));

					for (Double radius : radii) {
						String key = String.format("depth%s_radius%s", height, radius);
						DoublesExchangeItem doublesExchangeItem = (DoublesExchangeItem) exchangeItems.get(key);
						String formattedNumber = "0" + decimalFormat.format(doublesExchangeItem.getValuesAsDoubles()[fileCount]);
						if (!formattedNumber.contains("E-")) {
							formattedNumber = formattedNumber.replace("E", "E+");
						}
						bufferedWriter.write(String.format("%24s", formattedNumber));
					}

					bufferedWriter.write("\n");
				}
				fileCount++;
			} catch (IOException exception) {
				throw new RuntimeException(exception);
			}
		}
	}
}
