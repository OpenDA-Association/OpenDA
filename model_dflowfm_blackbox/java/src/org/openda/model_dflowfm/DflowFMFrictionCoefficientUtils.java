/* OpenDA v2.4.3 
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

/**
 * @deprecated
 * This class is replaced by the generic DFlowFMXynUtils class
 */
public class DflowFMFrictionCoefficientUtils {

    // Manipulate a line by putting it in uppercase, removing spaces, tabs, etc
    public static String prepareLine(String line){
		// replace tabs by spaces
        line=line.replaceAll("\t"," ");
		// remove leading whitespace
		line=line.replaceFirst("^\\s+","");
		// replace double spaces by single spaces
        line=line.replaceAll("  *"," ");
		return line;
    }

    // Read content of the whole file and store it in a string array
    public static String[] readWholeFile(File frictioncoefFile) {

        ArrayList<String> content= new ArrayList<String>();

        try {
            BufferedReader input =  new BufferedReader(new FileReader(frictioncoefFile));
            try {
                String line;
                while (( line = input.readLine()) != null){
                    content.add(line);
                }
            }
            finally {
               input.close();
            }
        }
        catch (IOException ex){
            throw new RuntimeException("Cannot read file '" +
            		                   frictioncoefFile.getAbsolutePath() + "'");
        }

        String[] sContent=new String[content.size()];
        return content.toArray(sContent);
  }

    // Write the whole forcings file as stored in given string array
    public static void writeWholeFile(File frictioncoefFile, String[] sContent) {


        try {
            BufferedWriter output =  new BufferedWriter(new FileWriter(frictioncoefFile));
            for (int i=0; i<sContent.length ; i++ ){
                try {
                    output.write(sContent[i]);
                    output.newLine();
                } catch (IOException e) {
                    throw new RuntimeException("Cannot write line in file: " + frictioncoefFile);
                }
            }
            output.close();
        }
        catch (IOException ex){
            throw new RuntimeException("Cannot write file " + frictioncoefFile);
        }
    }


    // Write value to the frictioncoefficient file
    public static void writeValueToFile(File frictioncoefFile, int lineNum, double value){

        // Read content of whole file
        String[] sContent= readWholeFile(frictioncoefFile);

        // Prepare the line for easy handling
        String line=prepareLine(sContent[lineNum]);

        // Chop line in parts
        String [] lineParts=line.split(" ");

		// value must be written to third column

		if (lineParts.length !=3) {
			throw new RuntimeException("DflowfmFrictionCoefficientFile: number of columns incorrect for line number: "+lineNum+"\n");
		}

        lineParts[2] = Double.toString(value);
        
        // Glue line back together
        sContent[lineNum]="";
        for (int i=0; i<lineParts.length; i++){
            sContent[lineNum]=sContent[lineNum]+lineParts[i]+" ";
        }

        // write content of whole file
        writeWholeFile(frictioncoefFile, sContent);
    }

    // Read the value from the forcings file
   public static double readValueFromFile(File frictioncoefFile, int lineNum){
	   Double value; //Return value

       // Read content of whole file
       String[] sContent= readWholeFile(frictioncoefFile);

       // Prepare the line for easy handling
       String line=prepareLine(sContent[lineNum]);

       // Chop line in parts
       String [] lineParts=line.split(" ");

	   if (lineParts.length !=3) {
		   throw new RuntimeException("DflowfmFrictionCoefficientFile: number of columns incorrect for line number: "+lineNum+"\n");
	   }

	   value = Double.parseDouble(lineParts[2]);

       return value;
    }
}
