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
package org.openda.tools;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class HeaderModifier {

	String suffix = "java";
	
	String license= "/* MOD_V2.0 \n"+
	"* Copyright (c) 2012 OpenDA Association \n"+
	"* All rights reserved.\n"+
	"* \n"+
	"* This file is part of OpenDA. \n"+
	"* \n"+
	"* OpenDA is free software: you can redistribute it and/or modify \n"+
	"* it under the terms of the GNU Lesser General Public License as \n"+
	"* published by the Free Software Foundation, either version 3 of \n"+
	"* the License, or (at your option) any later version. \n"+
	"* \n"+
	"* OpenDA is distributed in the hope that it will be useful, \n"+
	"* but WITHOUT ANY WARRANTY; without even the implied warranty of \n"+
	"* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \n"+
	"* GNU Lesser General Public License for more details. \n"+
	"* \n"+
	"* You should have received a copy of the GNU Lesser General Public License\n"+
	"* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.\n"+
	"*/";

	/**
	 * Create a headerModifier which looks for java extensions other than 'java'. The reason
	 * is that we do not want to destroy our source while testing.
	 * @param suffix (I used .jav for testing)
	 */
	public HeaderModifier(String suffix){
		if(suffix.startsWith(".")){
			this.suffix = suffix.substring(1); //skip the dot
		}else{
			this.suffix = suffix;
		}
	}


	public void modifyOneHeader(File source_in, File source1_out){
		PrintWriter outFile=null;
		try {
			if (!source_in.exists()) {
				throw new FileNotFoundException ("File does not exist: " + source_in);
			}
			if (!source_in.isFile()) {
				throw new IllegalArgumentException("Should be a file: " + source_in);
			}
			System.out.println("Processing file "+source_in.getAbsolutePath());
			FileReader fileReader = new FileReader(source_in);
			BufferedReader buff = new BufferedReader(fileReader);
			// read first line
			String line = buff.readLine();
			System.out.println("   line 1: "+line);
			// remember if the first line contains the package
			String packageLine="";
			if(line.indexOf("package")>=0){ //TODO This is not a very thorough check
				packageLine=line;
				line = buff.readLine();
			} 
			//skip empty lines
			while((line!=null) && (line.trim().length()==0)){
				line=buff.readLine();
			}
			//read first non-empty line --> Question: contains header?
			boolean needsModification =false;
			boolean containsHeader=false;
			if(line!=null){
				needsModification=true;
				if(line.indexOf("/* MOD_V2.0")>=0){
					needsModification=false;
					containsHeader=true;
				}else if(line.indexOf("/*")>=0){
					needsModification=true;
					containsHeader=true;
				}
			}
			// open output file
			if(needsModification){
				System.out.println("   start modifying source");
				try {
					FileWriter fw = new FileWriter (source1_out);
					outFile = new PrintWriter (fw);
				}catch (IOException e) {
					throw new RuntimeException("HeaderModifier: could not open " +
							" for writing (" + e.getMessage() + ")");
				}
			}
			// skip header lines
			int lineNo=2;
			if(containsHeader){
				System.out.println("   header found --> skipping");
				boolean done = false;
				while (!done) {
					line = buff.readLine();
					if (line == null)
						done = true;
					else{
						System.out.println(""+String.valueOf(lineNo)+":"+line);
						lineNo++;
						boolean found = (line.indexOf("*/")>=0);
						if(found){done=true;}
					}
				}
				line="";
			}
			if(needsModification){
				//write new header
				outFile.println(this.license);
				if(packageLine.length()>0){
					outFile.println(packageLine);
				}
				if(line.length()>0){
					outFile.println(line);
				}
				//copy remainder of file
				boolean done = false;
				while (!done) {
					line = buff.readLine();
					if (line == null)
						done = true;
					else{
						System.out.println(line);
						outFile.println(line);
					}
				}
				outFile.close();
			}

			buff.close();

		} catch (IOException e) {
			System.out.println("Error -- " + e.toString());
			throw new RuntimeException("Cannot read from file "+source_in);
		}

	}

	public void modifyAllHeaders(File tree, boolean replaceFile){
		if(tree.isFile()){
			if(tree.getName().endsWith(this.suffix)){
				System.out.println("+ "+tree.getAbsolutePath());
				//now modify
				String outFileName = tree.getAbsolutePath()+".mod";
				File outFile = new File(outFileName);
				if(outFile.exists()){
					outFile.delete();
				}
				modifyOneHeader(tree, outFile);
				if(replaceFile){
					if(outFile.exists()){
						tree.delete();
						outFile.renameTo(tree);
					}
				}
			}else{
				System.out.println("- "+tree.getAbsolutePath());
			}
		}else if(tree.isDirectory()){
			if(tree.getAbsolutePath().indexOf(".svn")<0){
				File fileList[] = tree.listFiles();
				for(File aFile : fileList){
					modifyAllHeaders(aFile, replaceFile);
				}

			}else{
				System.out.println("- "+tree.getAbsolutePath());
			}
		}
	}

	/**
	 * Main routine for running as a separate program
	 * There are 2 optional arguments.
	 * @param args 1) directory name 2) suffix
	 */
	public static void main(String[] args) {
		String dirName=".";
		if(args.length>0){
			dirName = args[0];
		}
		String suffix="java";
		if(args.length>1){
			suffix = args[1];
		}
		HeaderModifier mod = new HeaderModifier(suffix);

		File tree = new File(dirName);
		if(tree.exists()){
			mod.modifyAllHeaders(tree,true);
		}else{
			System.out.println("File or Dir does not exist "+tree.getAbsolutePath());
		}
	}


}
