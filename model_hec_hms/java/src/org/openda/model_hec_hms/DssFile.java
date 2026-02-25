package org.openda.model_hec_hms;

import hec.heclib.dss.DSSPathname;
import hec.heclib.util.HecTime;
import hec.heclib.util.Heclib;
import hec.hecmath.DSS;
import hec.hecmath.DSSFile;
import hec.hecmath.HecMath;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;
import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;

public class DssFile extends AbstractDataObject {
	private HecTime hecTimeObject;

	static {
		if (System.getProperty("os.name").startsWith("Linux")) System.loadLibrary("libjavaHeclib.so");
		Heclib.zset("MLEVEL", "", 0); // set log level
	}

	@Override
	public void finish() {
		// Not needed yet
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {

		File inputFile = new File(workingDir, arguments[0]);

		DSSFile dssFile = DSS.open(inputFile.getPath());
		try {
			Map<String, ArrayList<double[]>> idTimesMap = new LinkedHashMap<>();
			Map<String, ArrayList<double[]>> idValuesMap = new LinkedHashMap<>();
			List<String> pathNames = new ArrayList<String>(dssFile.getCatalogedPathnames());
			// TODO filrst order pathnames on part D
			for (String pathname : pathNames) {

				try {
					//System.out.println(pathname);
					if (dssFile.getDataManager().recordType(pathname) == 106) continue;
					HecMath hecMath = dssFile.read(pathname);
					DataContainer container = hecMath.getData();

					if (!(container instanceof TimeSeriesContainer)) continue;

					TimeSeriesContainer timeSeriesContainer = (TimeSeriesContainer) container;
					DSSPathname dssPathName = new DSSPathname();
					dssPathName.setPathname(pathname);
					StringBuilder idBuilder = new StringBuilder();
					String aPart = dssPathName.getAPart();
					idBuilder.append(aPart);
					idBuilder.append('/');
					String bPart = dssPathName.getBPart();
					idBuilder.append(bPart);
					idBuilder.append('/');
					String cPart = dssPathName.getCPart();
					idBuilder.append(cPart);
					idBuilder.append('/');
					String fPart = dssPathName.getFPart();
					idBuilder.append(fPart);

					String exchangeItemId = idBuilder.toString();
					ArrayList<double[]> valuesList = idValuesMap.getOrDefault(exchangeItemId, new ArrayList<>());
					valuesList.add(timeSeriesContainer.values);
					idValuesMap.putIfAbsent(exchangeItemId, valuesList);
					double[] times = new double[timeSeriesContainer.numberValues];
					for (int i = 0; i < timeSeriesContainer.numberValues; i++) {
						times[i] = hecTimeToMJD(timeSeriesContainer.times[i]);
					}
					ArrayList<double[]> timesList = idTimesMap.getOrDefault(exchangeItemId, new ArrayList<>());
					timesList.add(times);
					idTimesMap.putIfAbsent(exchangeItemId, timesList);
				} catch (Exception e) {
					System.out.println("Error reading " + pathname + ": " + e.getMessage());
				}
			}
			for (Map.Entry<String, ArrayList<double[]>> entry : idValuesMap.entrySet()) {
				String exchangeItemId = entry.getKey();
				ArrayList<double[]> valuesList = entry.getValue();
				int totalValuesLength = 0;
				for (double[] arr : valuesList) totalValuesLength += arr.length;
				double[] combinedValues = new double[totalValuesLength];
				int valPos = 0;
				for (double[] arr : valuesList) {
					System.arraycopy(arr, 0, combinedValues, valPos, arr.length);
					valPos += arr.length;
				}
				ArrayList<double[]> timesList = idTimesMap.get(exchangeItemId);
				double[] combinedTimes = new double[totalValuesLength];
				int timePos = 0;
				for (double[] arr : timesList) {
					System.arraycopy(arr, 0, combinedTimes, timePos, arr.length);
					timePos += arr.length;
				}
				if (!isAscending(combinedTimes)) throw new IllegalStateException("Times for exchange item " + exchangeItemId + " are not in ascending order");
				TimeInfo timeInfo = new TimeInfo(combinedTimes);
				DoublesExchangeItem exchangeItem = new DoublesExchangeItem(exchangeItemId, IExchangeItem.Role.Output, combinedValues);
				exchangeItem.setTimeInfo(timeInfo);
				exchangeItems.put(exchangeItemId, exchangeItem);
				System.out.println(exchangeItemId);
			}
		} finally {
			dssFile.close();
			try {
				Files.deleteIfExists(new File(inputFile.getPath().replace(".dss", ".dsc")).toPath());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	private double hecTimeToMJD(int hecTime) {
		if (hecTimeObject == null) hecTimeObject = new HecTime();
		hecTimeObject.set(hecTime);
		return TimeUtils.date2Mjd(new Date(hecTimeObject.getTimeInMillis()));
	}

	public static boolean isAscending(double[] array) {
		if (array.length <= 1) return true;

		double lastValue = array[0];
		for (int i = 1; i < array.length; i++) {
			double v = array[i];
			if (v <= lastValue) return false;
			lastValue = v;
		}

		return true;
	}
}
