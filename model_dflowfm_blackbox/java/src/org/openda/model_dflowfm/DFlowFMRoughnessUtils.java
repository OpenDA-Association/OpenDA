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
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.io.*;

/**
 * Started with a copy from model_delft3d
 * To change this template use File | Settings | File Templates.
 */
public class DFlowFMRoughnessUtils {

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

    public static void updateContent(String[] sContent, DFlowFMRougnessFileExchangeItem[] exchangeItems){
    	for(DFlowFMRougnessFileExchangeItem item: exchangeItems){
    		int iColumn=item.columnNum;
    		int iLine=item.lineNum;
    		String line=sContent[iLine];
			String [] lineParts=line.split("\\s+");
			lineParts[iColumn]=Double.toString(item.getValue());
			line=stringJoin(lineParts);
			//put modified line back
			sContent[iLine]=line;
    	}
    }
    
    /**
     * Joins some strings. Can be replaced with String.join() when we move to java 1.8
     * @param parts
     * @return parts[0] <space> part[1] ... etc
     */
    private static String stringJoin(String[] parts){
    	return Arrays.toString(parts).replace(", ", " ").replaceAll("[\\[\\]]", "");
    }
}
