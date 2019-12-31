package org.openda.model_metaswap;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
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
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
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
			int s01Index = indexOfString(headerSplit, S01MM_COLUMN_NAME);
			int dprztbIndex = indexOfString(headerSplit, DPRZTB_COLUMN_NAME);
			int yearIndex = indexOfString(headerSplit, "yr");
			int dayIndex = indexOfString(headerSplit, "td(d)");
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

	private static int indexOfString(String[] array, String string) {
		return indexOfString(array, 0, array.length, string);
	}

	private static int indexOfString(String[] array, int pos, int length, String string) {
		if (string == null) {
			for (int i = pos, n = pos + length; i < n; i++) {
				if (array[i] == null) return i;
			}
		} else {
			for (int i = pos, n = pos + length; i < n; i++) {
				if (stringEquals(array[i], string)) return i;
			}
		}
		return -1;
	}

	private static boolean stringEquals(String s1, String s2) {
		if (s1 == s2) return true;
		if (s1 == null || s2 == null) return false;
		int length = s1.length();
		if (length != s2.length()) return false;
		if (s1.hashCode() != s2.hashCode()) return false;
		return s1.regionMatches(0, s2, 0, length);  // 3 times faster than equals, no cast required
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
