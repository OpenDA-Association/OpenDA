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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Locale;
import java.util.Scanner;
import java.util.Vector;

import javax.management.RuntimeErrorException;

public class DFlowFMCmpInputFile {
	
	
	/**
	 * Class for reading settings from the *.cmp input file
	 * *
	 *  * COLUMNN=3
	 *  * COLUMN1=Period (min) or Astronomical Componentname
	 *  * COLUMN2=Amplitude (ISO)
	 *  * COLUMN3=Phase (deg)
	 *
	 *  M2    1.0000000    0.0000000
	 *  S2    1.0000000    0.0000000
	 *
	 *  or
	 *
	 *  0.0  1.0  0.0
	 */

	private File workingDir = null;
	private String fileName = null;
	private String[] ACname = null;
	private double Period = 0.0;
	private double[] Phase = null;
	private double[] Amplitude = null;
	
	public DFlowFMCmpInputFile(File workingDir, String filename) {
		/* Just save the initialization input */
		this.workingDir = workingDir;
		this.fileName   = filename;
		ReadInputFile();
	}

	public void ReadInputFile() {
		
		/* read the file */
		File inputFile = new File(workingDir, fileName);
		Scanner scanner = null;
		// temporary storage
		Vector<String> ACVector = new Vector<String>();
		Vector<Double> AVector =  new Vector<Double>();
		Vector<Double> PVector =  new Vector<Double>();
        try {
            /*Read header*/
            scanner = new Scanner(new BufferedReader(new FileReader(inputFile)) );
            scanner.useLocale(Locale.US);
			while (scanner.hasNext()) {
				if (scanner.hasNext("\\*")) {
					scanner.nextLine();
				} else {
					// read period or Astro Component Name
					if (scanner.hasNextDouble()) {
					// read period
						Period = scanner.nextDouble();
						ACVector.add("period");
					} else {
					// read Astro Component
						ACVector.add(scanner.next());
					}
					// read amplitude
                	AVector.add(scanner.nextDouble());
					// read phase
					PVector.add(scanner.nextDouble());
				}
	        }
		}
        catch (IOException ex){
            throw new RuntimeException("Problem reading file '" +
            		                   inputFile.getAbsolutePath() + "'");
        }
        finally {
			if (scanner != null) {
				scanner.close();
			}
		}

		this.ACname = new String[ACVector.size()];
		this.Amplitude = new double[AVector.size()];
		this.Phase = new double[PVector.size()];
		for (int i = 0; i < ACname.length; i++) {
			ACname[i] = ACVector.get(i);
			Amplitude[i] = AVector.get(i);
			Phase[i] = PVector.get(i);
		}
	}

	public String getFilename() {
		return this.fileName;
	}

	public String[] getACname() {
		return this.ACname;
	}
	public double getPeriod() {
		if (ACname.length > 1) {
			throw new RuntimeException("Period is undefined, file "+fileName+"contains Astro Components.");

		}
		return this.Period;
	}

	public double getAmplitude(String Id) {
		for (int i=0 ; i < ACname.length ; i++ ) {
			if (ACname[i].matches(Id)) {
				return this.Amplitude[i];
			}
		}
		return Double.parseDouble(null);
	}

	public double getPhase(String Id) {
		for (int i=0 ; i < ACname.length ; i++ ) {
			if (ACname[i].matches(Id)) {
				return this.Phase[i];
			}
		}
		return Double.parseDouble(null);
	}

	public void setAmplitude(String Id,double value) {
		for (int i=0 ; i < ACname.length ; i++ ) {
			if (ACname[i].matches(Id)) {
				this.Amplitude[i]=value;
			}
		}
	}

	public void setPhase(String Id,double value) {
		for (int i=0 ; i < ACname.length ; i++ ) {
			if (ACname[i].matches(Id)) {
				this.Phase[i]=value;
			}
		}
	}
	
	public void WriteInputFile() {
		File cmpFile = new File(workingDir, fileName);
		try{
			Writer writer = new OutputStreamWriter(new FileOutputStream(cmpFile));
			writer.write("* COLUMNN=3\n");
			writer.write("* COLUMN2=Amplitude (ISO)\n");
			writer.write("* COLUMN3=Phase (deg)\n");
			for (int i=0 ; i < ACname.length ; i++ ) {
				String line=this.ACname[i]+" "+this.Amplitude[i]+" "+this.Phase[i]+"\n";
				writer.write(line);
			}
			writer.close();
		}catch(Exception e){
			throw new RuntimeException("Could not write changes to .cmp file with name : "+fileName);
		}
	}
}



