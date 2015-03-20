package org.openda.utils.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

/**
 * Some utlities to simplify input and output to/from files in java for small commom datasets,
 * such as:
 * - some lines given as String
 * @author verlaanm
 *
 */
public class AsciiFileUtils {

	/**
	 * Read an ascii file into an ArrayList of Strings. The ArrayList avoids the difficulties associated with
	 * the fact that the number of lines is unknown beforehand.
	 * @param inFile file to read 
	 * @return each line is put in a separate String
	 * @throws IOException problems reading data from the file
	 * @throws FileNotFoundException file can not be opened 
	 */
	public static ArrayList<String> readLines(File inFile) throws IOException, FileNotFoundException{
		ArrayList<String> lines = new ArrayList<String>();
		BufferedReader input;
		input = new BufferedReader(new FileReader(inFile));
		String line = input.readLine();
		while(line!=null){
			lines.add(line);
			line=input.readLine();
		}
		input.close();
		return lines;
	}
	
	/**
	 * Write a number of lines of text, given as Strings to an ascii file
	 * @param outFile file to write to
	 * @param lines content to write
	 * @throws IOException problems with file IO
	 */
	public static void writeLines(File outFile, ArrayList<String> lines) throws IOException{
		PrintWriter output = new PrintWriter(new FileWriter(outFile));
		for(String line : lines){
			output.println(line);
		}
		output.close();
	}
	
	/**
	 * Write a number of lines of text, given as Strings to an ascii file
	 * @param outFile file to write to
	 * @param lines content to write
	 * @throws IOException problems with file IO
	 */
	public static void writeLines(File outFile, String[] lines) throws IOException{
		PrintWriter output = new PrintWriter(new FileWriter(outFile));
		for(String line : lines){
			output.println(line);
		}
		output.close();
	}

}
