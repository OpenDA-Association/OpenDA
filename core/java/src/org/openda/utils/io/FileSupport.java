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
package org.openda.utils.io;

import java.io.*;
import java.util.ArrayList;

/**
 * Utility class for manipulating files
 */
public class FileSupport {

	

    public static boolean FileContains(File file,String toBeFound){
        return FileContains(file,toBeFound, false);
    }
    
    /**
     * Does this file contain a certain string, eg. "Ended OK."
     * Throws exceptions on IO troubles
     * @param file File to be checkec
     * @param toBeFound regex specification of the string to be found
     * @return True if string was found in file, false otherwise
     */
    public static boolean FileContains(File file,String toBeFound, boolean regex){
        boolean found = false;
         try {
            if (!file.exists()) {
                throw new FileNotFoundException("File does not exist: " + file);
            }
            if (!file.isFile()) {
                throw new IllegalArgumentException("Should be a file: " + file);
            }
            FileReader fileReader = new FileReader(file);
            BufferedReader buff = new BufferedReader(fileReader);
            // read header
            String line;
            boolean done = false;
            while (!done) {
                line = buff.readLine();
                if (line == null)
                    done = true;
                else{
                    //System.out.println(line);
                	if(! regex){
                        found = (line.indexOf(toBeFound)>=0);
                	}else{
                		found=line.matches(toBeFound);
                	}
                    if(found){
                        System.out.println("   Found: "+line);
                    	done=true;
                    }
                }
            }
            buff.close();
        } catch (IOException e) {
            System.out.println("Error -- " + e.toString());
            throw new RuntimeException("Cannot read from file '" + file + "' Message was: " + e.getMessage(), e);
        }
        return found;
    }

    /**
     * Read content of the entire file and store it in a string array
     * @param file  Input file to be read
     * @return String[] Array of Strings containing content of <code>file</code>
     */
    public static String[] readContentOfFile(File file) {

        ArrayList<String> content= new ArrayList();

        try {
            BufferedReader input =  new BufferedReader(new FileReader(file));
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
            System.out.println("Error -- " + ex.toString());
            throw new RuntimeException("Cannot read file '" + file.getAbsolutePath() + "' Message was: " + ex.getMessage(), ex);
        }

        String[] sContent=new String[content.size()];
        return content.toArray(sContent);
    }

    /**
     * Write the entire file as stored is given string array
     * @param file  output file
     * @param sContent Array of Strings containing content to be written to <code>file</code>
     */
    public static void writeContentOfFile(File file, String[] sContent) {
        try {
            BufferedWriter output =  new BufferedWriter(new FileWriter(file));
            for (int i=0; i<sContent.length ; i++ ){
                try {
                    output.write(sContent[i]);
                    output.newLine();
                } catch (IOException e) {
                    System.out.println("Error -- " + e.toString());
                    throw new RuntimeException("Cannot writeValuesFile line in file: '" + file.getAbsolutePath()
                    		+ "' Message was: " + e.getMessage(), e);
                }
            }
            output.close();
        }
        catch (IOException ex){
            System.out.println("Error -- " + ex.toString());
            throw new RuntimeException("Cannot writeValuesFile file: '" + file.getAbsolutePath() + "' Message was: " + ex.getMessage(), ex);
        }
    }

    public static void recursive_list(String directoryName, ArrayList<File> files) {
        File directory = new File(directoryName);

        // get all the files from a directory
        File[] fList = directory.listFiles();
        if (fList !=null) {
            for (File file : fList) {
                if (file.isFile()) {
                    files.add(file);
                } else if (file.isDirectory()) {
                    recursive_list(file.getAbsolutePath(), files);
                }
            }
        }
    }

}
