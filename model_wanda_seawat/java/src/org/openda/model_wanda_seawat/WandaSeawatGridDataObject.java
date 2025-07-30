package org.openda.model_wanda_seawat;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.io.AsciiFileUtils;
import org.openda.utils.io.FileSupport;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.*;

public class WandaSeawatGridDataObject extends AbstractDataObject {
	private String header;
	private List<Double> heights;
	private File file;
	private String filePrefix;

	@Override
	public void finish() {
		DoublesExchangeItem data = (DoublesExchangeItem) exchangeItems.get(filePrefix + "Grid");
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

		String subDirString = arguments[0];
		filePrefix = arguments[1];
		File fileDirectory = new File(workingDir, subDirString);

		if (!fileDirectory.exists() || !fileDirectory.isDirectory()) throw new RuntimeException(fileDirectory + " does not exist or is not a directory");

		String fileNamePattern = String.format("'%s'yyyyMMddHHmmss'.ASC'", filePrefix);
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(fileNamePattern);
		simpleDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		ArrayList<Date> dates = FileSupport.getDatesFromFileNames(fileNamePattern, simpleDateFormat, fileDirectory);
		Date mostRecentDate = dates.get(dates.size() - 1);
		String formattedFileName = simpleDateFormat.format(mostRecentDate);

		file = new File(fileDirectory, formattedFileName);
		System.out.println("Reading grid data from " + file.getAbsolutePath());
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
		String id = filePrefix + "Grid";
		System.out.println("Reading data from " + file.getAbsolutePath() + " into exchange item with id " + id + " and length " + dataArray.length);
		System.out.println("Setting values into exchange item: " + Arrays.toString(dataArray));
		DoublesExchangeItem dataExchangeItem = new DoublesExchangeItem(id, IExchangeItem.Role.InOut, dataArray);
		System.out.println("Getting values from exchange item: " + Arrays.toString(dataExchangeItem.getValuesAsDoubles()));
		TimeInfo timeInfo = new TimeInfo(new double[]{TimeUtils.date2Mjd(mostRecentDate)});
		dataExchangeItem.setTimeInfo(timeInfo);
		exchangeItems.put(id, dataExchangeItem);
	}

	private static TreeMap<String, File> getFileNamesToFiles(File workingDir, String filePrefix) {
		TreeMap<String, File> fileNamesToFiles = new TreeMap<>();

		File[] fileList = workingDir.listFiles();

		if (fileList == null) {
			return fileNamesToFiles;
		}

		for (File listedFile : fileList) {
			if (!listedFile.getName().startsWith(filePrefix)) {
				continue;
			}

			fileNamesToFiles.putIfAbsent(listedFile.getName(), listedFile);
		}
		return fileNamesToFiles;
	}
}
