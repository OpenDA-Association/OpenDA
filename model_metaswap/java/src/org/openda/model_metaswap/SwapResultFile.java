package org.openda.model_metaswap;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.SortUtils;
import org.openda.utils.io.CsvReader;

import java.io.File;
import java.io.IOException;
import java.util.*;

public class SwapResultFile implements IDataObject {

	private static final String S01MM_COLUMN_NAME = "S01(mm)";
	private static final String DPRZTB_COLUMN_NAME = "dprztb(m)";
	private LinkedHashMap<String, SwapResultExchangeItem> exchangeItems = new LinkedHashMap<>();
	private static final Calendar CALENDAR = new GregorianCalendar(TimeZone.getTimeZone("GMT"));

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		// No action needed, data object is read only
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String argument = arguments[0];
		File sourceFile = new File(workingDir, argument);
		if (!sourceFile.exists()) throw new RuntimeException("Swap result file " + sourceFile + " not found.");
		String nameWithoutExt = getFileNameWithoutExt(sourceFile.getName());
		String[] split = nameWithoutExt.split("_");
		String locationSuffix = split[split.length - 1];
		try {
			CsvReader csvReader = new CsvReader(sourceFile);
			String[] headerSplit = csvReader.readCSVLineTrimElements();
			int s01Index = SortUtils.indexOfString(headerSplit, S01MM_COLUMN_NAME);
			int dprztbIndex = SortUtils.indexOfString(headerSplit, DPRZTB_COLUMN_NAME);
			int yearIndex = SortUtils.indexOfString(headerSplit, "yr");
			int dayIndex = SortUtils.indexOfString(headerSplit, "td(d)");
			List<Double> doubleValuesList = new ArrayList<>();
			List<Double> doubleTimesList = new ArrayList<>();
			String[] lineElements = csvReader.readCSVLineTrimElements();
			while (lineElements != null && lineElements.length != 0) {
				Double s01 = Double.valueOf(lineElements[s01Index]);
				Double dprztb = Double.valueOf(lineElements[dprztbIndex]);
				// added divide by 1000 to convert mm to m
				doubleValuesList.add(s01 / dprztb / 1000);
				int year = Integer.valueOf(lineElements[yearIndex]);
				CALENDAR.clear();
				CALENDAR.set(year, Calendar.JANUARY, 1);
				double day = Double.valueOf(lineElements[dayIndex]);
				double mjd = TimeUtils.date2Mjd(CALENDAR.getTime()) + day - 1;
				doubleTimesList.add(mjd);
				lineElements = csvReader.readCSVLineTrimElements();
			}
			double[] values = unboxDoubleArray(doubleValuesList.toArray(new Double[doubleValuesList.size()]));
			double[] times = unboxDoubleArray(doubleTimesList.toArray(new Double[doubleTimesList.size()]));
			String id = locationSuffix + ".soilmoisture";
			SwapResultExchangeItem swapResultExchangeItem = new SwapResultExchangeItem(id, times, values);
			exchangeItems.put(id, swapResultExchangeItem);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}


	private static double[] unboxDoubleArray(Double[] array) {
		if (array == null)
			throw new IllegalArgumentException("array == null");

		double[] res = new double[array.length];
		for (int i = 0; i < array.length; i++) {
			res[i] = array[i];
		}

		return res;
	}

	private static String getFileNameWithoutExt(String path) {
		int end = path.length();
		for (int i = path.length() - 1; i >= 0; i--) {
			char ch = path.charAt(i);
			if (ch == '/') return path.substring(i + 1, end);
			if (ch == '\\') return path.substring(i + 1, end);
			if (ch == ':') return path.substring(i + 1, end);
			if (ch == '.' && end == path.length()) end = i;
		}

		return path.substring(0, end);
	}
}
