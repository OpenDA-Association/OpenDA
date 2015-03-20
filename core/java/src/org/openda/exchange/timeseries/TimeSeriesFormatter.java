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
package org.openda.exchange.timeseries;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * This interface describes methods for reading and writing TimeSeries. This allows one to specify the formats for the
 * TimeSeries eg. within another object or at runtime.
 *
 * This is abstract class can not be used directly. It should be 'extended' and read and write should be implemented for that
 * specific case.
 *
 * Some examples: TimeSeries series = formatter.read("myfile.txt"); formatter.write(series); // write to screen
 * formatter.write("outputFile.txt",series);
 *
 * @author verlaanm
 *
 */

public abstract class TimeSeriesFormatter {

   /**
    * Abstract method for writing a TimeSeries in some format. You have to create/use a subclass that implement this method to
    * make writing work.
    *
    * @param out
    *           OutputStream to write to
    * @param series
    *           TimeSeries with the data
    */
   public abstract void write(OutputStream out, TimeSeries series);

   /**
    * Abstract method for read a TimeSeries in some format. You have to create/use that implement this method to make writing
    * work.
    *
    * @param in
    *           InputStream to read from
    * @return series TimeSeries with the data
    */
   public abstract TimeSeries read(InputStream in);

   /*
    * ============================================================================================== general utilities (more
    * convenient reading from file or url etc...
    * ==============================================================================================
    */

   /**
    * Write TimeSeries to a file. This may be more convenient than constructing the OutputStream yourself
    *
    * @param filename
    *           where to write
    * @param series
    *           TimeSeries with the data
    * @param overwriteExistingFiles
    *           true if an existing file with the same name should be deleted before writing.
    */
   public void writeFile(String filename, TimeSeries series, boolean overwriteExistingFiles) {
      File outFile = new File(filename);
      if (outFile.exists()) {
         if (overwriteExistingFiles) {
            outFile.delete();
         }
         else {
            outFile = null; // disable writing to this file
         }
      }
      if (outFile != null) {
         try {
            FileOutputStream out = new FileOutputStream(outFile);
            write(out, series);
            out.close();
         }
         catch (Exception e) {
            throw new RuntimeException("Problem writing to file " + filename + " : " + e.getMessage());
         }
      }
   }

   /**
    * Write TimeSeries to standard output. This may be more convenient than constructing the OutputStream yourself
    *
    * @param series
    *           TimeSeries with the data
    */
   public void writeToStandardOut(TimeSeries series) {
      try {
         OutputStream out = System.out;
         write(out, series);
      }
      catch (Exception e) {
         throw new RuntimeException("Problem writing to standard output : " + e.getMessage());
      }
   }

   /**
    * Write TimeSeries to a string. This may be more convenient than constructing the OutputStream yourself
    *
    * @param series
    *           TimeSeries with the data
    * @param initialSize
    *           size of the buffer. Will be extended if needed.
    *
    * @return The output stream.
    */
   public String writeString(TimeSeries series) {
      ByteArrayOutputStream out = new ByteArrayOutputStream(10000);
      write(out, series);
      return out.toString();
   }

   /**
    * Read timeseries from file. This may be more convenient than doing this all yourself.
    *
    * @param filename
    *           where to read from
    * @return TimeSeries with data from file. Returns null in case of trouble.
    */
   public TimeSeries readFile(String filename) {
      TimeSeries result = null;
      File inFile = new File(filename);
      if (inFile.exists()) {
         try {
            FileInputStream in = new FileInputStream(inFile);
            result = read(in);
            in.close();
         }
         catch (Exception e) {
            throw new RuntimeException("Problem reading from file " + filename + " : " + e.getMessage());
         }
      }
      return result;
   }

   /**
    * Read timeseries from a string. This may be more convenient than doing this all yourself.
    *
    * @param data
    *           Where to read from
    * @return TimeSeries with data from the string. Returns null in case of trouble.
    */
   public TimeSeries readString(String data) {
      TimeSeries result = null;
      // StringBufferInputStream in = new StringBufferInputStream(buffer);
      try {
         InputStream in = new ByteArrayInputStream(data.getBytes("UTF-8"));
         result = read(in);
      }
      catch (Exception e) {
         throw new RuntimeException(
                  "Trouble reading from string :" + data.substring(0, Math.min(50, data.length())));
      }
      return result;
   }

}
