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

package org.openda.model_hspf;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

/**
 * @author Arno Kockx
 */
public class HspfUtils {

    /**
     * Read content of the given file and store it in a list with Strings.
     *
     * @param inputFile
     * @return ArrayList<String>
     */
    public static ArrayList<String> readFile(File inputFile) {
        ArrayList<String> content = new ArrayList<String>();

        try {
            BufferedReader reader =  new BufferedReader(new FileReader(inputFile));
            try {
                String line = reader.readLine();
                while (line != null) {
                    content.add(line);
                    line = reader.readLine();
                }
            } finally {
               reader.close();
            }
        } catch (IOException e){
            throw new RuntimeException("HspfUtils: Problem while reading file '" + inputFile.getAbsolutePath() + "'.", e);
        }

        return content;
    }

    /**
     * Write the given list with Strings to the given file.
     *
     * @param outputFile
     * @param content
     */
    public static void writeFile(File outputFile, ArrayList<String> content) {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(outputFile));
            try {
                for (int n = 0; n < content.size(); n++) {
                    writer.write(content.get(n));
                    writer.newLine();
                }
            } finally {
                writer.close();
            }
        } catch (IOException e){
            throw new RuntimeException("HspfUtils: Problem while writing file '" + outputFile.getAbsolutePath() + "'.", e);
        }
    }
}
