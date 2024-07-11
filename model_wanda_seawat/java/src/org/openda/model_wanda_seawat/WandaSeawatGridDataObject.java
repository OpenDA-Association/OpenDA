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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TreeMap;

public class WandaSeawatGridDataObject extends AbstractDataObject {
	private String header;
	private List<Double> heights;
	private File file;

	@Override
	public void finish() {
		DoublesExchangeItem data = (DoublesExchangeItem) exchangeItems.get("data");
		double[] values = data.getValuesAsDoubles();

		if (values.length % heights.size() != 0) {
			return;
		}

		int columns = values.length / heights.size();
		DecimalFormat decimalFormat = new DecimalFormat(".00000000E00", new DecimalFormatSymbols(Locale.UK));
		
		try (BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(file))) {
			bufferedWriter.write(header + "\n");

			int row = 0;

			for (Double height : heights) {
				bufferedWriter.write(String.format("%10s", String.format(Locale.UK, "%5.4f", height)));

				for (int column = 0; column < columns; column++) {
					String formattedNumber = "0" + decimalFormat.format(values[row * columns + column]);
					if (!formattedNumber.contains("E-")) {
						formattedNumber = formattedNumber.replace("E", "E+");
					}
					bufferedWriter.write(String.format("%24s", formattedNumber));
				}

				bufferedWriter.write("\n");
				row++;
			}

		} catch (IOException exception) {
			throw new RuntimeException(exception);
		}
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (workingDir == null || !workingDir.exists() || !workingDir.isDirectory()) {
			return;
		}

		if (arguments.length == 0 || arguments[0] == null) {
			return;
		}

		String filePrefix = arguments[0];

		TreeMap<String, File> fileNamesToFiles = new TreeMap<>();

		for (File listedFile : workingDir.listFiles()) {
			if (!listedFile.getName().startsWith(filePrefix)) {
				continue;
			}

			fileNamesToFiles.putIfAbsent(listedFile.getName(), listedFile);
		}

		file = fileNamesToFiles.firstEntry().getValue();
		List<String> lines = AsciiFileUtils.readLines(file);

		if (lines.size() < 2) {
			return;
		}

		header = lines.get(0);
		heights = new ArrayList<>();
		List<Double> data = new ArrayList<>();
		for (String line : lines.subList(1, lines.size())) {
			String[] values = line.trim().split("\\s+");

			if (values.length < 1) {
				continue;
			}

			heights.add(Double.parseDouble(values[0]));

			for (int valueIndex = 1; valueIndex < values.length; valueIndex++) {
				String value = values[valueIndex];
				data.add(Double.parseDouble(value));
			}
		}
		double[] dataArray = new double[data.size()];
		for (int datum = 0; datum < data.size(); datum++) {
			dataArray[datum] = data.get(datum);
		}
		DoublesExchangeItem dataExchangeItem = new DoublesExchangeItem("dataExchangeItem", IExchangeItem.Role.InOut, dataArray);
		
		exchangeItems.put("data", dataExchangeItem);
	}
}
