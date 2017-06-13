/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.exchange.timeseries;

import java.io.*;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.Vector;

/**
 * Read and write TimeSeries in the NOOS format. This format was created for the
 * exchange of observations and forecasts at individual locations between the
 * countries around the North Sea. See also http://www.noos.cc This format is
 * also the standard format for the Matroos database.
 *
 * Here is an example of this simple format:
 *
 * #------------------------------------------------------ # Timeseries
 * retrieved from the MATROOS series database # Created at Mon Mar 17
 * 10:37:58CET 2008 #------------------------------------------------------ #
 * Location :den helder # Position : (4.745356,52.966001) # Source : observed #
 * Unit : waterlevel_astro # Analyse time: most recent # Timezone : GMT
 * #------------------------------------------------------ 200801010000 0.7300
 * 200801010010 0.7300 200801010020 0.7200E
 *
 * The rules are: - lines with a '#' contain comments and metadata - lines with
 * '# <property> : <value>' contain the metadata. If the line does not fit this
 * format it is a comment. - data lines contain one <dateTime> <value> pair. -
 * the <dateTime> format is 'yyyymmddHHMM'
 *
 * Note that the property 'quantity' for the TimeSeries-object is labeled 'Unit'
 * in the files.
 *
 * @author verlaanm
 *
 */
public final class NoosTimeSeriesFormatter extends TimeSeriesFormatter {

   public static final String PROPERTY_ANALTIME = "analTime";
   public static final String PROPERTY_TIMEZONE = "timeZone";

   
   public TimeSeries read(InputStream in) {
      return read(new BufferedReader(new InputStreamReader(in)));
   }

   public TimeSeries read(BufferedReader buff) {
      TimeSeries result = null;
      String description = "";
      boolean collectDescription = false;
      String location = ""; // Location : den helder
      double x = 0.0; // Position : (4.745356,52.966001)
      double y = 0.0;
      double height = Double.NaN;
      String source = ""; // Source : observed
      String quantity = ""; // Unit : waterlevel_astro !!! Note different label
      String analTime = ""; // Analyse time: most recent
      TimeZone timeZone = TimeZone.getTimeZone("GMT"); // Timezone : GMT
      double times[] = null;
      double values[] = null;
      double analTimes[] = null;
      Vector<Double> timeVector = new Vector<Double>(); // for temporary storage of values,
      Vector<Double> valueVector = new Vector<Double>(); // since we don't
      Vector<Double> analVector = new Vector<Double>(); // know the size beforehand

      boolean eof = false;
      String line;
      while (!eof) {
         try {
            line = buff.readLine();
         }
         catch (Exception e) {
            throw new RuntimeException("");
         }
         if (line == null) eof = true;
         else { // now parse this line
                // System.out.println("line = '" + line + "'");
            if (line.startsWith("#")) { // comment or metadata
               int separatorIndex = line.indexOf(":");
               //check for multiple : as in the time of Created at ...
               int lastSeparatorIndex = line.lastIndexOf(":");
               if(lastSeparatorIndex>separatorIndex) separatorIndex=-1; //treat as comment not as property
               
               if (separatorIndex >= 0) {
                  // # Location : den helder
                  // # Position : (4.745356,52.966001)
                  // # Height   : 10.0
                  // # Source : observed
                  // # Unit : waterlevel_astro
                  // # Analyse time: most recent
                  // # Timezone : GMT
                  String property = line.substring(1, separatorIndex);
                  String value = line.substring(separatorIndex + 1);
                  property = property.trim();
                  value = value.trim();
                  if (property.equalsIgnoreCase("Location")) {
                     location = value;
                  }
		  if (property.equalsIgnoreCase("Height")){
			  try {
				  height = Double.parseDouble(value);
			  }
			  catch (Exception e) {
				  height = Double.NaN;
			  }
		  }
                  if (property.equalsIgnoreCase("Position")) {
                     int posComma = value.indexOf(",");
                     try {
                        x = Double.parseDouble(value.substring(1, posComma));
                     }
                     catch (Exception e) {
                        x = Double.NaN;
                     }
                     try {
                        y = Double.parseDouble(value.substring(posComma + 1, value.length() - 1));
                     }
                     catch (Exception e) {
                        y = Double.NaN;
                     }
                  }
                  if (property.equalsIgnoreCase("Source")) {
                     source = value;
                  }
                  if (property.equalsIgnoreCase("Unit")) {
                     // Note Unit label in file refers to quantity here

                     quantity = value;
                  }
                  if (property.toLowerCase().startsWith("anal")) {
                     analTime = value;
                  }
                  if (property.equalsIgnoreCase("TimeZone")) {
                     timeZone = TimeZone.getTimeZone(value);
                  }
               }
               else {
                  // Comment : collect description when applicable
                  if (line.startsWith("#-----------------------")) {
                     collectDescription = !collectDescription;
                  }
                  else if (collectDescription && line.length() > 2) {
                     if (description != null && description.length() > 0) description += "\n";
                     description += line;
                  }
               }

            }
            else { // datetime value pair
               if (line.length() > 5) {
                  String columns[] = line.split("(\\s)+");
                  double time;
                  try {
                     time = TimeUtils.date2Mjd(columns[0], timeZone);
                  }
                  catch (Exception e) {
                     throw new RuntimeException("Trouble parsing time :" + columns[0]);
                  }
                  timeVector.add(time);

                  double value;
                  try {
                     value = Double.parseDouble(columns[1]);
                  }
                  catch (Exception e) {
                     throw new RuntimeException("Trouble parsing value :" + columns[1]);
                  }
                  valueVector.add(value);

                  if (columns.length > 2) {
                     double anal;
                     try {
                        anal = TimeUtils.date2Mjd(columns[2]);
                     }
                     catch (Exception e) {
                        throw new RuntimeException("Trouble parsing anal time :" + columns[2]);
                     }
                     analVector.add(anal);
                  }
               }
            }
         }

      }
      // create and fill TimeSeries object
      times = new double[timeVector.size()];
      values = new double[valueVector.size()];
      if (times.length != values.length) { throw new RuntimeException("Value vector length doesn't match time vector length."); }

      boolean hasAnalTimes = !(analVector.isEmpty());
      if (hasAnalTimes) {
         if (analVector.size() == timeVector.size()) {
            analTimes = new double[analVector.size()];
         }
         else {
            throw new RuntimeException("Analysis time vector length doesn't match time vector length.");
         }
      }

      for (int i = 0; i < times.length; i++) {
         times[i] = timeVector.get(i);
         values[i] = valueVector.get(i);
         if (hasAnalTimes) {
            analTimes[i] = analVector.get(i);
         }
      }
      result = new TimeSeries(times, values);
      if (hasAnalTimes) {
         result.setExtraValues("analTimes", analTimes);
      }
      result.setLocation(location);
      result.setPosition(x, y);
      result.setHeight(height);
      result.setQuantity(quantity);
      result.setSource(source);
      result.setProperty(PROPERTY_ANALTIME, analTime);
      result.setProperty(PROPERTY_TIMEZONE, timeZone.getID());
      result.setDescription(description);
      return result;
   }

   
   public void write(OutputStream out, TimeSeries series) {
      write(new PrintWriter(out), series);
   }

   public void write(PrintWriter printer, TimeSeries series) {
      final String printedAt = "# Printed at ";

      printer.println("#------------------------------------------------------");
      String description = series.getDescription();
      String now = Calendar.getInstance().getTime().toString();
      if (description != null && description.length() > 0) {
         description.replaceFirst(printedAt + ".*$", printedAt + now);
         printer.println(description);
      }
      if (description != null && !description.contains(printedAt)) {
         printer.println(printedAt + now);
      }
      printer.println("#------------------------------------------------------");

      // # Location : den helder
      printer.println("# Location : " + series.getLocation());
      // # Position : (4.745356,52.966001)
      double position[] = series.getPosition();
      printer.println("# Position : (" + position[0] + "," + position[1] + ")");
      // # Source : observed
      printer.println("# Source : " + series.getSource());
      // # Unit : waterlevel_astro
      printer.println("# Unit : " + series.getQuantityId());
      // # Analyse time: most recent
      printer.println("# Analyse time: " + series.getProperty(PROPERTY_ANALTIME));
      // # Timezone : GMT
      String timeZoneId = series.getProperty(PROPERTY_TIMEZONE);
      printer.println("# Timezone : " + timeZoneId);
      printer.println("#------------------------------------------------------");

      // now the data
      final DecimalFormatSymbols symbols = new DecimalFormatSymbols(java.util.Locale.US);
      final DecimalFormat four0 = new DecimalFormat("0.0000", symbols);
      final DecimalFormat anal = new DecimalFormat("0", symbols);
      final double times[] = series.getTimesRef();
      final double values[] = series.getValuesRef();
      final boolean hasAnalTimes = series.hasExtraValues("analTimes");
      TimeZone timeZone = timeZoneId != null ? TimeZone.getTimeZone(timeZoneId) : TimeZone.getTimeZone("GMT");
      for (int i = 0; i < times.length; i++) {
         String line = TimeUtils.mjdToString(times[i], "yyyyMMddHHmm", timeZone) + "   " + four0.format(values[i]);
         if (hasAnalTimes) line += "   " + TimeUtils.mjdToString(series.getExtraValuesRef("analTimes")[i]);
         printer.println(line);
      }
      printer.flush();
   }
}
