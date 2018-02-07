package org.openda.model_metaswap;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.io.CsvReader;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class SwapResultFile implements IDataObject {

	public static final String S01MM_COLUMN_NAME = "S01(mm)";
	public static final String DPRZTB_COLUMN_NAME = "dprztb(m)";
	LinkedHashMap<String, SwapResultExchangeItem> exchangeItems = new LinkedHashMap<>();

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		throw new RuntimeException("Finish (write) method not implemented because data object is read only");
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String argument = arguments[0];
		File sourceFile = new File(workingDir, argument);
		if (!sourceFile.exists()) throw new RuntimeException("Swap result file " + sourceFile + " not found.");
		String nameWithoutExt = null; // TODO FileUtils.getNameWithoutExt(sourceFile);
		String[] split = nameWithoutExt.split("_");
		String locationSuffix = split[split.length - 1];
		try {
			CsvReader csvReader = new CsvReader(sourceFile);
			String[] headerSplit = csvReader.readCSVLineTrimElements();
			int s01Index = -1; // TODO StringArrayUtils.indexOf(headerSplit, S01MM_COLUMN_NAME);
			int dprztbIndex = -1; // TODO StringArrayUtils.indexOf(headerSplit, DPRZTB_COLUMN_NAME);
			int yearIndex = -1; // TODO StringArrayUtils.indexOf(headerSplit, "yr");
			int dayIndex = -1; // TODO StringArrayUtils.indexOf(headerSplit, "td(d)");
			List<Double> doubleValuesList = new ArrayList<>();
			List<Double> doubleTimesList = new ArrayList<>();
			String[] lineElements = csvReader.readCSVLineTrimElements();
			while (lineElements != null && lineElements.length != 0) {
				Double s01 = Double.valueOf(lineElements[s01Index]);
				Double dprztb = Double.valueOf(lineElements[dprztbIndex]);
				doubleValuesList.add(s01 / dprztb);
				int year = Integer.valueOf(lineElements[yearIndex]);
				double day = Double.valueOf(lineElements[dayIndex]);
				double mjd = -1d; // TimeUtils.date2Mjd(TODO DateUtils.getDate(year, 1, 1)) + day - 1;
				doubleTimesList.add(mjd);
				lineElements = csvReader.readCSVLineTrimElements();
			}
			double[] values = null; // TODO DoubleArrayUtils.unbox(doubleValuesList.toArray(new Double[doubleValuesList.size()]));
			double[] times = null; // TODO DoubleArrayUtils.unbox(doubleTimesList.toArray(new Double[doubleTimesList.size()]));
			String id = locationSuffix + ".soilmoisture";
			SwapResultExchangeItem swapResultExchangeItem = new SwapResultExchangeItem(id, times, values);
			exchangeItems.put(id, swapResultExchangeItem);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}
