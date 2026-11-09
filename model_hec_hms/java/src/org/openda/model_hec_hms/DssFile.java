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
import java.nio.file.Path;
import java.util.*;

public class DssFile extends AbstractDataObject {
	private HecTime hecTimeObject;

	static {
		if (System.getProperty("os.name").startsWith("Linux")) System.loadLibrary("javaHeclib");
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
			Map<String, List<TimeSeriesContainer>> idContainersMap = getContainersPerExchangeItemIdMap(dssFile);
			mergeContainersIntoExchangeItems(idContainersMap);
		} finally {
			dssFile.close();
			File dscFile = new File(inputFile.getPath().replace(".dss", ".dsc"));
			Path dscPath = dscFile.toPath();
			try {
				Files.deleteIfExists(dscPath);
			} catch (IOException e) {
				System.out.println("Error deleting " + dscFile.getAbsolutePath() + ": " + e.getMessage());
			}
		}
	}

	private void mergeContainersIntoExchangeItems(Map<String, List<TimeSeriesContainer>> idContainersMap) {
		for (Map.Entry<String, List<TimeSeriesContainer>> entry : idContainersMap.entrySet()) {
			String exchangeItemId = entry.getKey();
			List<TimeSeriesContainer> valuesList = entry.getValue();
			valuesList.sort(Comparator.comparingLong(c -> c.startHecTime.getTimeInMillis()));
			int totalValuesLength = 0;
			for (TimeSeriesContainer arr : valuesList) totalValuesLength += arr.times.length;
			double[] combinedValues = new double[totalValuesLength];
			double[] combinedTimes = new double[totalValuesLength];
			int valPos = 0;
			for (TimeSeriesContainer container : valuesList) {
				System.arraycopy(container.values, 0, combinedValues, valPos, container.values.length);
				double[] times = new double[container.numberValues];
				for (int i = 0; i < container.numberValues; i++) {
					times[i] = hecTimeToMJD(container.times[i]);
				}
				System.arraycopy(times, 0, combinedTimes, valPos, times.length);
				valPos += container.values.length;
			}
			if (!isAscending(combinedTimes)) throw new IllegalStateException("Programming error: Times for exchange item " + exchangeItemId + " are not in ascending order");
			TimeInfo timeInfo = new TimeInfo(combinedTimes);
			DoublesExchangeItem exchangeItem = new DoublesExchangeItem(exchangeItemId, IExchangeItem.Role.Output, combinedValues);
			exchangeItem.setTimeInfo(timeInfo);
			exchangeItems.put(exchangeItemId, exchangeItem);
		}
	}

	private Map<String, List<TimeSeriesContainer>> getContainersPerExchangeItemIdMap(DSSFile dssFile) {
		List<String> pathNames = new ArrayList<String>(dssFile.getCatalogedPathnames());
		Map<String, List<TimeSeriesContainer>> idContainersMap = new LinkedHashMap<>();
		for (String pathname : pathNames) {
			try {
				if (dssFile.getDataManager().recordType(pathname) == 106) continue;
				HecMath hecMath = dssFile.read(pathname);
				DataContainer container = hecMath.getData();

				if (!(container instanceof TimeSeriesContainer)) continue;
				TimeSeriesContainer timeSeriesContainer = (TimeSeriesContainer) container;
				String exchangeItemId = getExchangeItemId(pathname);
				List<TimeSeriesContainer> valuesList = idContainersMap.getOrDefault(exchangeItemId, new ArrayList<>());
				valuesList.add(timeSeriesContainer);
				idContainersMap.putIfAbsent(exchangeItemId, valuesList);
			} catch (Exception e) {
				System.out.println("Error reading " + pathname + ": " + e.getMessage());
			}
		}
		return idContainersMap;
	}

	private String getExchangeItemId(String pathname) {
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

		return idBuilder.toString();
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
			if (v < lastValue) return false;
			lastValue = v;
		}

		return true;
	}
}
