/* MOD_V2.0
 * Copyright (c) 2012 OpenDA Association
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
package org.openda.exchange.ioobjects;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesSet;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Glob;

import java.io.*;

public final class NoosTimeSeriesIoObject implements IoObjectInterface {
   public static final String PROPERTY_PATHNAME = "pathName";
   private TimeSeriesSet      timeSeriesSet     = new TimeSeriesSet();

   /**
    * Initialize the IoObject
    *
    * @param workingDir
    *           Working directory
    * @param fileName
    *           The name of the file containing the data (relative to the working dir) This fileName may be null, empty or
    *           contain wildcard characters.
    * @param arguments
    *           Additional arguments (may be null zero-length)
    */
   public void initialize(File workingDir, String fileName, String[] arguments) {
      this.timeSeriesSet = new TimeSeriesSet();

      // Check whether the fileName is null or contains * or ?
      // If so, use the multi-file version
      // It it doesn't, call readNoosTimeSeries directly
      if (fileName == null || fileName.isEmpty() || fileName.matches(".*[*?].*")) {
         // Determine which filenames to read
         String[] fileNames;
         if (fileName == null || fileName.isEmpty()) {
            fileNames = workingDir.list();
         }
         else {
            final String regexp = Glob.createRegex(fileName);
            fileNames = workingDir.list(new FilenameFilter() {
               public boolean accept(File dir, String name) {
                  return name.matches(regexp);
               }
            });
         }

         // Then, read each file
         for (String name : fileNames) {
            readNoosTimeSeries(workingDir, name);
         }
      }
      else {
         // Read the file indicated by fileName directly
         readNoosTimeSeries(workingDir, fileName);
      }
   }

   /**
    * Initialize the IoObject without arguments
    *
    * @param workingDir
    *           Working directory
    * @param fileName
    *           The name of the file containing the data (relative to the working dir) This fileName may be null, empty or
    *           contain wildcard characters.
    */
   public void initialize(File workingDir, String fileName) {
      final String args[] = {};
      this.initialize(workingDir, fileName, args);
   }

   /**
    * Read a single Noos timeseries from a file.
    *
    * @param workingDir
    *           Working directory
    * @param fileName
    *           The name of the file containing the data (relative to the working dir) This fileName may NOT contain wildcard
    *           characters.
    */
   public void readNoosTimeSeries(File workingDir, String fileName) {
      File noosFile = new File(workingDir, fileName);
      FileInputStream noosFileInputStream;
      try {
         noosFileInputStream = new FileInputStream(noosFile);
      }
      catch (FileNotFoundException e) {
         throw new RuntimeException("Cannot read noos file: " + fileName + " " + e.getMessage());
      }
      NoosTimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
      TimeSeries series = noosFormatter.read(noosFileInputStream);
      series.setProperty(PROPERTY_PATHNAME, noosFile.getAbsolutePath());
      this.timeSeriesSet.add(series);
   }

   /**
    * Ask which elements can be accessed
    *
    * @return The list of element identifiers can be accessed
    */
   public IPrevExchangeItem[] getExchangeItems() {
      return this.timeSeriesSet.toArray(new IPrevExchangeItem[this.timeSeriesSet.size()]);
   }

   /**
    * Clear this IoObject without writing
    */
   public void clear() {
      this.timeSeriesSet.clear();
      this.timeSeriesSet = new TimeSeriesSet();
   }

   /**
    * Write all time series in this IoObject that were read from file (with property NoosTimeSeriesIoObject.PROPERTY_PATHNAME
    * set). Ignores all other time series, including those obtained from an URL.
    */
   public void finish() {
      if (this.timeSeriesSet == null) return;
      for (TimeSeries series : this.timeSeriesSet)
         if (series.hasProperty(PROPERTY_PATHNAME)) writeNoosTimeSeries(series);
   }

   /**
    * Write the specified time series to the path name it was read from (using a property).
    *
    * @param series
    *           The time series to write (path name will be determined from its property).
    */
   public void writeNoosTimeSeries(TimeSeries series) {
      if (!series.hasProperty(PROPERTY_PATHNAME))
         throw new RuntimeException("Cannot write a time series without " + PROPERTY_PATHNAME + " property");
      File noosFile = new File(series.getProperty(PROPERTY_PATHNAME));
      writeNoosTimeSeries(series, noosFile);
   }

   /**
    * Write the specified time series to the specified file
    *
    * @param series
    *           The time series to write
    * @param noosFile
    *           The file to write to
    */
   public static void writeNoosTimeSeries(TimeSeries series, File noosFile) {
      if (!noosFile.exists()) {
         try {
            noosFile.createNewFile();
         }
         catch (IOException e) {
            throw new RuntimeException("Cannot create noos file " + e.getMessage());
         }
      }

      FileOutputStream noosFileOutputStream;
      try {
         noosFileOutputStream = new FileOutputStream(noosFile);
      }
      catch (FileNotFoundException e) {
         throw new RuntimeException("Cannot find output noos file " + e.getMessage());
      }

      NoosTimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
      noosFormatter.write(noosFileOutputStream, series);
	   try {
		   noosFileOutputStream.close();
	   } catch (IOException e) {
		   throw new RuntimeException("Cannot close output noos file " + e.getMessage());
	   }
   }

   /**
    * @return Reference to the time series set
    */
   public TimeSeriesSet getTimeSeriesSet() {
      return this.timeSeriesSet;
   }

   /**
    * @param set
    *           The TimeSeriesSet to set in this IoObject
    */
   public void setTimeSeriesSet(TimeSeriesSet set) {
      this.timeSeriesSet = set;
   }
}
