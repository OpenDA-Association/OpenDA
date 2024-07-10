package org.openda.model_wanda_seawat;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class WandaSeawatPointDataObject extends AbstractDataObject {
	private static final int FIRST_VALUE_OFFSET = 2;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		File file = new File(workingDir, arguments[0]);
		List<String> lines = AsciiFileUtils.readLines(file);

		if (lines.size() < 2) {
			return;
		}

		List<Double> columns = readHeader(lines);
		for (String line : lines.subList(1, lines.size())) {
			String[] values = line.split("\\s+");

			if (values.length < 1) {
				continue;
			}

			double row = Double.parseDouble(values[1]);

			for (int valueIndex = FIRST_VALUE_OFFSET; valueIndex < values.length; valueIndex++) {
				String value = values[valueIndex];
				String key = String.format("depth%s_radius%s", row, columns.get(valueIndex - FIRST_VALUE_OFFSET));
				DoubleExchangeItem doubleExchangeItem = new DoubleExchangeItem(key, Double.parseDouble(value));
				exchangeItems.putIfAbsent(key, doubleExchangeItem);
			}
		}
	}

	private List<Double> readHeader(List<String> lines) {
		if (lines.isEmpty()) {
			return new ArrayList<>();
		}

		List<Double> columns = new ArrayList<>();
		String[] headers = lines.get(0).split("\\s+");
		for (int headerIndex = 1; headerIndex < headers.length; headerIndex++) {
			String header = headers[headerIndex];
			columns.add(Double.parseDouble(header));
		}
		return columns;
	}

	@Override
	public void finish() {
		// Do nothing
	}
}
