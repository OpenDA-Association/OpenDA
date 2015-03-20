package org.openda.exchange.timesseries;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Calendar;
import java.util.Vector;

/**
 * Read and write TimeSeries in the NOOS format. This format was created for the
 * exchange of observations and forecasts at individual locations between the
 * coutries around the North Sea. See also http://www.noos.cc This format is
 * also the standard format for the Matroos database.
 * 
 * Here is an example of this simple format:
 * 
 * #------------------------------------------------------ 
 * # Timeseries retrieved from the MATROOS series database 
 * # Created at Mon Mar 17 10:37:58CET 2008 
 * #------------------------------------------------------ 
 * # Location :den helder 
 * # Position : (4.745356,52.966001) 
 * # Source : observed 
 * # Unit : waterlevel_astro 
 * # Analyse time: most recent 
 * # Timezone : GMT
 * #------------------------------------------------------ 200801010000 0.7300
 * 200801010010 0.7300 200801010020 0.7200E
 * 
 * The rules are: - lines with a '#' contain comments and metadata - lines with '#
 * <property> : <value>' contain the metadata. If the line does not fit this
 * format it is a comment. - data lines contain one <dateTime> <value> pair. -
 * the <dateTime> format is 'yyyymmddHHMM'
 * 
 * Note that the property 'quantity' for the TimeSeries-object is labeled 'Unit' in the files.
 * 
 * @author verlaanm
 * 
 */
public class NoosTimeSeriesFormatter extends TimeSeriesFormatter {

	@Override
	public TimeSeries read(InputStream in) {
		TimeSeries result = null;
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));

		String location = ""; // Location : den helder
		double x = 0.0; // Position : (4.745356,52.966001)
		double y = 0.0;
		String source = "observed"; // Source : observed
		String quantity = ""; // Unit : waterlevel_astro !!! Note different
								// label
		String analTime = ""; // Analyse time: most recent
		String timeZone = "GMT"; // Timezone : GMT
		double times[] = null;
		double values[] = null;
		Vector<Double> timeVector = new Vector<Double>(); // for temporary
															// storage of
															// values, since we
															// don't
		Vector<Double> valueVector = new Vector<Double>(); // know the size
															// beforehand

		boolean eof = false;
		String line;
		while (!eof) {
			try {
				line = buff.readLine();
			} catch (Exception e) {
				throw new RuntimeException("");
			}
			if (line == null)
				eof = true;
			else { // now parse this line
				//System.out.println("line = '" + line + "'");
				if (line.startsWith("#")) { // comment or metadata
					int separatorIndex = line.indexOf(": ");
					if (separatorIndex >= 0) {
						// # Location : den helder
						// # Position : (4.745356,52.966001)
						// # Source : observed
						// # Unit : waterlevel_astro
						// # Analyse time: most recent
						// # Timezone : GMT
						String property = line.substring(1, separatorIndex);
						String value = line.substring(separatorIndex + 2);
						property = property.trim();
						value = value.trim();
						if (property.equalsIgnoreCase("Location")) {
							location = value;
						}
						if (property.equalsIgnoreCase("Position")) {
							int posComma = value.indexOf(",");
							try {
								x = Double.parseDouble(value.substring(1,posComma));
							} catch (Exception e) {
								x = Double.NaN;
							}
							try {
								y = Double.parseDouble(value.substring(posComma + 1, value.length() - 1));
							} catch (Exception e) {
								y = Double.NaN;
							}
						}
						if (property.equalsIgnoreCase("Source")) {
							source = value;
						}
						if (property.equalsIgnoreCase("Unit")) { 
							//Note Unit label in file refers to quantity here
							
   							
							quantity = value;
						}
						if (property.toLowerCase().startsWith("anal")) {
							analTime = value;
						}
						if (property.equalsIgnoreCase("TimeZone")) {
							timeZone = value;
						}
					} else {
						// Comment : do nothing
					}
				} else { // datetime value pair
					if(line.length()>5){
						String columns[] = line.split("(\\s)+");
						double time;
						try {
							time = TimeUtils.date2Mjd(columns[0]);
						} catch (Exception e) {
							throw new RuntimeException("Trouble parsing time :"
									+ columns[0]);
						}
						double value;
						try {
							value = Double.parseDouble(columns[1]);
						} catch (Exception e) {
							throw new RuntimeException("Trouble parsing value :"
									+ columns[1]);
						}
						//System.out.println("> data, time=" + time + ", value="+ value);
						timeVector.add(time);
						valueVector.add(value);
					}
				}
			}

		}
		// create and fill TimeSeries object
		times = new double[timeVector.size()];
		values = new double[valueVector.size()];
		for (int i = 0; i < times.length; i++) {
			times[i] = timeVector.get(i);
			values[i] = valueVector.get(i);
		}
		result = new TimeSeries(times, values);
		result.setLocation(location);
		result.setPosition(x, y);
		result.setQuantity(quantity);
		result.setSource(source);
		result.setProperty("analTime", analTime);
		result.setProperty("timeZone", timeZone);
		return result;
	}

	@Override
	public void write(OutputStream out, TimeSeries series) {
		PrintWriter printer = new PrintWriter(out);
		printer.println("# ----------------------------------------------------");
		printer.println("# Printed at "+Calendar.getInstance().getTime().toString());
		printer.println("# ----------------------------------------------------");
		
		// # Location : den helder
		printer.println("# Location : "+series.getLocation());
		// # Position : (4.745356,52.966001)
		double position[] = series.getPosition();
		printer.println("# Position : ("+position[0]+","+position[1]+")");
		// # Source : observed
		printer.println("# Source : "+series.getLocation());
		// # Unit : waterlevel_astro
		printer.println("# Unit : "+series.getQuantityId());
		// # Analyse time: most recent
		printer.println("# Analyse time: "+series.getProperty("analTime"));
		// # Timezone : GMT
		printer.println("# Timezone : "+series.getProperty("timeZone"));
		
		//now the data
		double times[]  = series.getTimesRef();
		double values[] = series.getValuesRef();
		String time;
		for(int i=0;i<times.length;i++){
			time = TimeUtils.mjdToString(times[i]);
			printer.println(time+"   "+values[i]);
		}
		
		
		printer.flush();
	}

}
