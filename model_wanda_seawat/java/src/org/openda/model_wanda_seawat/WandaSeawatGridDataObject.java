package org.openda.model_wanda_seawat;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.util.*;

public class WandaSeawatGridDataObject extends AbstractDataObject {
	@Override
	public void finish() {
		// Do nothing
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		File file = new File(workingDir, arguments[0]);
		List<String> lines = AsciiFileUtils.readLines(file);

		if (lines.size() < 2) {
			return;
		}

		List<Double> radii = readHeader(lines);
		List<Double> heights = new ArrayList<>();
		List<Double> data = new ArrayList<>();
		for (String line : lines.subList(1, lines.size())) {
			String[] values = line.split("\\s+");
			
			if (values.length < 1) {
				continue;
			}
			
			heights.add(Double.parseDouble(values[1]));
			
			for (int valueIndex = 2; valueIndex < values.length; valueIndex++) {
				String value = values[valueIndex];
				data.add(Double.parseDouble(value));
			}
		}
		double[] dataArray = new double[data.size()];
		for (int datum = 0; datum < data.size(); datum++) {
			dataArray[datum] = data.get(datum);
		}
		DoublesExchangeItem dataExchangeItem = new DoublesExchangeItem("dataExchangeItem", IExchangeItem.Role.InOut, dataArray);
		DoublesExchangeItem radiiExchangeItem = new DoublesExchangeItem("radii", IExchangeItem.Role.InOut, radii.stream().mapToDouble(Double::doubleValue).toArray());
		DoublesExchangeItem heightsExchangeItem = new DoublesExchangeItem("heights", IExchangeItem.Role.InOut, heights.stream().mapToDouble(Double::doubleValue).toArray());
		
		exchangeItems.put("data", dataExchangeItem);
		exchangeItems.put("radii", radiiExchangeItem);
		exchangeItems.put("heights", heightsExchangeItem);
	}

	private List<Double> readHeader(List<String> lines) {
		List<Double> columns = new ArrayList<>();
		String[] headers = lines.get(0).split("\\s+");
		for (int headerIndex = 1; headerIndex < headers.length; headerIndex++) {
			String header = headers[headerIndex];
			columns.add(Double.parseDouble(header));
		}
		return columns;
	}
}
