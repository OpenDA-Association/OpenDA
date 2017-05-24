/* MOD_V2.0
 * Copyright (c) 2016 OpenDA Association
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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.io.*;

/**
 * Started with a copy from model_delft3d
 * To change this template use File | Settings | File Templates.
 */
public class DFlowFMRoughnessUtils {

    // Manipulate the line from roughness file by putting it in uppercase,
    // removing spaces, tabs, etc
    public static String prepareRoughLine(String line){
        // replace tabs by spaces
        line=line.replaceAll("\t"," ");
        // replace double spaces by single spaces
        line=line.replaceAll("  *"," ");
       return line;
    }

    // Read content of the whole roughness file and store it in a string array
    public static String[] readWholeFile(File roughFile) {

        ArrayList<String> content= new ArrayList<String>();

        try {
            BufferedReader input =  new BufferedReader(new FileReader(roughFile));
            try {
                String line = null;
                while (( line = input.readLine()) != null){
                    content.add(line);
                }
            }
            finally {
               input.close();
            }
        }
        catch (IOException ex){
            throw new RuntimeException("Cannot read roughness file '" +
            		                   roughFile.getAbsolutePath() + "'");
        }

        String[] sContent=new String[content.size()];
        return content.toArray(sContent);
  }

    // Write the whole roughness file as stored is given string array
    public static void writeWholeFile(File roughFile, String[] sContent) {
        try {
            BufferedWriter output;
			output = new BufferedWriter(new FileWriter(roughFile));
            for (int i=0; i<sContent.length ; i++ ){
                try {
                    output.write(sContent[i]);
                    output.newLine();
                } catch (IOException e) {
                    throw new RuntimeException("Cannot write line in roughness file");
                }
            }
            output.close();
        }
        catch (IOException ex){
            throw new RuntimeException("Cannot write roughness file");
        }
    }

    // Write the value to the roughness file
    public static void writeValueToFile(File roughFile, int lineNum, String code, double value){
        // Read content of whole file
        String[] sContent= readWholeFile(roughFile);

        // Prepare the line for easy handling
        String line=prepareRoughLine(sContent[lineNum]);

        // Chop line in parts
        String [] lineParts=line.split(" ");

        // Find the sub-string that holds the parameter value [A,B,C,D,E]
        HashMap<String, Integer> fieldNumber = new LinkedHashMap<String, Integer>();
        fieldNumber.put("A", 2);
        fieldNumber.put("B", 3);
        fieldNumber.put("C", 4);
        fieldNumber.put("D", 5);
        fieldNumber.put("E", 6);

        if(!fieldNumber.containsKey(code)){
        	throw new RuntimeException("D3dRoughParamsFile: Do not recognize parameter code: "+code);
        }
        int iField=fieldNumber.get(code);
        if(iField>=lineParts.length){
        	throw new RuntimeException("D3dRoughParamsFile: Line in roughness file does not contain enough columns.\n"+
        			"Looking for col="+iField+" in line:\n"+line);
        }

        lineParts[iField] = Double.toString(value);

        // Glue line back together
        sContent[lineNum]="";
        for (int i=0; i<lineParts.length; i++){
            sContent[lineNum]=sContent[lineNum]+lineParts[i]+" ";
        }

        // write content of whole file
        writeWholeFile(roughFile, sContent);
    }

    // Read the value from the roughness file
   public static double readValueFromFile(File roughFile, int lineNum, String code){
        Double value=0.0; //Return value

        // Read content of whole file
        String[] sContent= readWholeFile(roughFile);

        // Prepare the line for easy handling
        String line=prepareRoughLine(sContent[lineNum]);

        // Chop line in parts
        String [] lineParts=line.split(" ");

        // Find the sub-string that holds the parameter value [A,B,C,D,E]
        HashMap<String, Integer> fieldNumber = new LinkedHashMap<String, Integer>();
        fieldNumber.put("A", 2);
        fieldNumber.put("B", 3);
        fieldNumber.put("C", 4);
        fieldNumber.put("D", 5);
        fieldNumber.put("E", 6);

        if(!fieldNumber.containsKey(code)){
        	throw new RuntimeException("D3dRoughParamsFile: Do not recognize parameter code: "+code);
        }
        int i=fieldNumber.get(code);
        if(i>=lineParts.length){
        	throw new RuntimeException("D3dRoughParamsFile: Line in roughness file does not contain enough columns.\n"+
        			"Looking for col="+i+" in line:\n"+line);
        }
        value=Double.parseDouble(lineParts[i]);

        return value;
    }
}
