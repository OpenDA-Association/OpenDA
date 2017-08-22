/* OpenDA v2.4.1 
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
package org.openda.model_dflowfm;

import java.util.ArrayList;
import java.io.*;
import java.lang.StringBuilder;


/**
 */
public class DFlowFMXyzUtils {

	// Manipulate a line by putting it in uppercase, removing spaces, tabs, etc
	public static String prepareLine(String line) {
		// replace tabs by spaces
		//line=line.replaceAll("\t"," ");
		// remove leading whitespace
		line = line.replaceFirst("^\\s+", "");
		// replace double spaces by single spaces
		//line=line.replaceAll("  *"," ");
		return line;
	}

	// Read content of the whole file and store it in a string array
	public static String[] readWholeFile(File myFile) {

		ArrayList<String> content = new ArrayList<String>();

		try {
			BufferedReader input = new BufferedReader(new FileReader(myFile));
			try {
				String line;
				while ((line = input.readLine()) != null) {
					content.add(line);
					// System.out.println("READ " + line);
				}
			} finally {
				input.close();
			}
		} catch (IOException ex) {
			throw new RuntimeException("Cannot read file '" +
					myFile.getAbsolutePath() + "'");
		}

		String[] sContent = new String[content.size()];
		return content.toArray(sContent);
	}

	// Write the whole forcings file as stored in given string array
	public static void writeWholeFile(File myFile, String[] sContent) {


		try {
			BufferedWriter output = new BufferedWriter(new FileWriter(myFile));
			for (int i = 0; i < sContent.length; i++) {
				try {
					//System.out.println("WRITE " + sContent[i]);
					output.write(sContent[i]);
					output.newLine();
				} catch (IOException e) {
					throw new RuntimeException("Cannot write line in file: " + myFile);
				}
			}
			//System.out.println("Written to " + myFile);
			output.close();
		} catch (IOException ex) {
			throw new RuntimeException("Cannot write file " + myFile);
		}
	}


	// Write value to the frictioncoefficient file
	public static void writeValueToFile(File file, int lineNum, double value) {

		// Read content of whole file
		String[] sContent = readWholeFile(file);
		//System.out.println("LINE" + sContent[lineNum] );
		// Prepare the line for easy handling
		String line = prepareLine(sContent[lineNum]);
		//System.out.println("LINE" + sContent[lineNum] );
		// Chop line in parts
		String[] lineParts = line.split("\\s+", 4);

		// value must be written to third column

		if (lineParts.length < 3) {
			throw new RuntimeException("DFlowFMXyzFile: number of columns incorrect for line number: " + lineNum + "\n");
		}

		//System.out.println("XY" + lineParts[0] + " " + lineParts[1]);

		lineParts[2] = Double.toString(value);

		// Glue line back together
		sContent[lineNum] = "";
		for (int i = 0; i < lineParts.length; i++) {
			sContent[lineNum] = sContent[lineNum] + lineParts[i] + " ";
		}
		//System.out.println("WRITE " + lineNum + ":" + sContent[lineNum]);

		// write content of whole file
		writeWholeFile(file, sContent);
	}

	// Read the value from the forcings file
	public static double readValueFromFile(File myFile, int lineNum) {
		Double value; //Return value

		// Read content of whole file
		String[] sContent = readWholeFile(myFile);

		// Prepare the line for easy handling
		String line = prepareLine(sContent[lineNum]);
		//System.out.println("READ " + lineNum + ":" + line);

		// Chop line in parts
		String[] lineParts = line.split("\\s+", 4);

		if (lineParts.length < 3) {
			throw new RuntimeException("DFlowFMXyzFile: number of columns incorrect for line number: " + lineNum + "\n");
		}

		value = Double.parseDouble(lineParts[2]);
		//System.out.println(lineNum + ":" + sContent[lineNum]);

		return value;
	}
}
