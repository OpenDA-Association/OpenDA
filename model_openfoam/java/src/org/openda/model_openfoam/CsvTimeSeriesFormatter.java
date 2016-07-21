package org.openda.model_openfoam;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.exchange.timeseries.TimeUtils;

import java.io.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.joda.time.DateTime;

/*
#= SensorID = SensorID_1
#= Location = Koude_Gang
#= Position = (10;5.0;3.0)
#= Source   = observed
#= Quantity = temperature
#= Unit     = celcius
#= Status   = use
 */


public class CsvTimeSeriesFormatter extends TimeSeriesFormatter {

    private static final Logger logger = LoggerFactory.getLogger(CsvTimeSeriesFormatter.class);

	public static final String PROPERTY_ANALTIME = "analTime";
	public static final String PROPERTY_TIMEZONE = "timeZone";
	private Map<String,Object> headerInformation;
	{
		headerInformation = new LinkedHashMap<>();
		headerInformation.put("sensorid", null);
		headerInformation.put("location", null);
		headerInformation.put("position", null);
		headerInformation.put("source", null);
		headerInformation.put("quantity", null);
		headerInformation.put("unit", null);
		headerInformation.put("status", null);
	}

	private static final String header =
		"# -----------------------------------------------------\n" +
		"#= SensorID = %1$s\n" +
		"#= Location = %2$s\n" +
		"#= Position = %3$s\n" +
		"#= Source   = %4$s\n" +
		"#= Quantity = %5$s\n" +
		"#= Unit     = %6$s\n" +
		"#= Status   = %7$s\n" +
		"# -----------------------------------------------------\n";
	private int writePrecision = 4;
	private String delimiter;
	private String datePattern;

	public CsvTimeSeriesFormatter() {
		this(",","yyyy-MM-dd'T'HH:mm:ss'Z'");
	}

	public CsvTimeSeriesFormatter(String delimiter) {
		this(delimiter, "yyyy-MM-dd'T'HH:mm:ss'Z'");
	}

	public CsvTimeSeriesFormatter(String delimiter, String datePattern) {
		super();
		this.delimiter = delimiter;
		this.datePattern = datePattern;
	}
	
	public void write(OutputStream out, TimeSeries series) {
		write(new PrintWriter(out), series);
	}
	
	public void write(PrintWriter printer, TimeSeries series) {

		Locale locale  = new Locale("en", "US");
		DecimalFormat formatter = (DecimalFormat)NumberFormat.getNumberInstance(locale);
		formatter.applyPattern("0.0000######");

		printer.print(String.format(header, series.getProperty("sensorid"), series.getLocation(), series.getProperty("position"), series.getSource(), series.getQuantityId(), series.getUnitId(),  series.getProperty("use") ));

		final double times[] = series.getTimesRef();
		final double values[] = series.getValuesRef();
		//TODO: detect order on read and use this order
		printer.println("datetime;value");
		String dateString;
		for (int i = 0; i < times.length; i++) {
			dateString = TimeUtils.mjdToString( times[i],this.datePattern);
			printer.println( dateString + this.delimiter + formatter.format(values[i]) );
		}
		printer.flush();
	}
	

	public TimeSeries read(InputStream in) {

		Scanner scanner = new Scanner(in);
		scanner.useLocale(Locale.US);

		String description = "";
		double x = 0.0; // Position : (4.745356,52.966001)
		double y = 0.0;
		double height = Double.NaN;
		String analTime = ""; // Analyse time: most recent
		String timeZone = ""; // Timezone : GMT
		Role role     = Role.InOut;

		// Parse header meta data block
		while ( scanner.findInLine("(?m)^#") != null  ) {
			if (scanner.hasNext("=")) {
				String line = scanner.nextLine();
				String[] items = line.split("=",3);
				String key = items[1].trim().toLowerCase();
				String item = items[2].trim();
				if (headerInformation.containsKey(key)) {
					headerInformation.put(key, item);
				} else {
					throw new RuntimeException("Cannot parse key and value from line " + line);
				}
			} else {
				scanner.nextLine();
			}
		}

		// read line with column names
		String line;
		scanner.nextLine();
		//String[] labelStrings = line.split(delimiter);
		// read line with column names
		// extract labels
		//TODO: parse from line
		int timeColumnIndex = 0;
		int valueColumnIndex = 1;


		// read content
		Vector<Double> timeVector = new Vector<>(); // for temporary storage of values,
		Vector<Double> valueVector = new Vector<>(); // since we don't
		while ( scanner.hasNextLine()) {
			line = scanner.nextLine();
			logger.debug("Scan line: " + line);
			if (line.isEmpty()) continue;
			String[] valueStrings = line.split(delimiter);
			logger.debug("Parsing date from string: " +  valueStrings[timeColumnIndex]);
			Date date = new DateTime( valueStrings[timeColumnIndex] ).toDate();
			Double time = TimeUtils.date2Mjd(date);
			timeVector.add(time);
			valueVector.add(Double.parseDouble(valueStrings[valueColumnIndex]) );
		}

		// create and fill TimeSeries object
		double[] times = new double[timeVector.size()];
		double[] values = new double[valueVector.size()];
		if (times.length != values.length) { throw new RuntimeException("Value vector length doesn't match time vector length."); }

		for (int i = 0; i < times.length; i++) {
			times[i] = timeVector.get(i);
			values[i] = valueVector.get(i);
		}
		TimeSeries result = new TimeSeries(times, values, x, y,
				(String) this.headerInformation.get("source"),
				(String) this.headerInformation.get("quantity"),
				(String) this.headerInformation.get("unit"),
				(String) this.headerInformation.get("location"),
				role);
		result.setProperty("sensorid", (String) this.headerInformation.get("sensorid") );
		result.setProperty("position", (String) this.headerInformation.get("position") );
		result.setProperty("use", (String) this.headerInformation.get("status") );
		result.setHeight(height);
		result.setProperty(PROPERTY_ANALTIME, analTime);
		result.setProperty(PROPERTY_TIMEZONE, timeZone);
		result.setDescription(description);
		return result;
	}
	
	
	
	
}
