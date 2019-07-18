/* MOD_V2.0
* Copyright (c) 2013 OpenDA Association
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
import java.io.FileReader;
import java.io.IOException;
import java.util.InputMismatchException;
import java.util.Locale;
import java.util.Scanner;
import java.util.Vector;

public class DFlowFMPliInputFile {


	/**
	 * Class for reading settings from the *.pli input file
	 * *
	 * 	estuary_01
	 *	2      2
	 *	-2.50000000000000000e+02	0.00000000000000000e+02	
	 *	-2.50000000000000000e+02	5.00000000000000000e+02	
	 *
	 */
	private File workingDir = null;
	private String fileName = null;
	private int locationsCount = 0;
	private String locationId = null;
	private double[] x = null;
	private double[] y = null;

	public DFlowFMPliInputFile(File workingDir, String[] arguments) {
		/* Just save the initialization input */
		this.workingDir = workingDir;
		this.fileName   = arguments[0];
		ReadInputFile();
	}

	public DFlowFMPliInputFile(File workingDir, String fileName) {
		this(workingDir, new String[]{fileName} );
	}

	public void ReadInputFile() {

		Vector<Double> xVector =  new Vector<Double>(); // temporary storage
		Vector<Double> yVector =  new Vector<Double>(); // temporary storage
		/* read the file */
		File inputFile = new File(workingDir, fileName);
		Scanner scanner = null;
		int lineNr = 0;
        try {
            /*Read header*/
            scanner = new Scanner(new BufferedReader(new FileReader(inputFile)));
			scanner.useLocale(Locale.US);
			lineNr++;
            locationId = scanner.nextLine();
            lineNr++;
            this.locationsCount = scanner.nextInt();
			int dimension = scanner.nextInt();
			scanner.nextLine();
			lineNr++;
			/* Read coordinates */
            for (int i = 0; i < locationsCount; i++ ) {
            	if (dimension == 2) {
                	Double x = scanner.nextDouble();
                    Double y = scanner.nextDouble();
                    scanner.nextLine();
                    lineNr++;
                    xVector.add(x);
                    yVector.add(y);
            	} else if (dimension == 3) {
                	throw new UnsupportedOperationException("Three dimensions in pli files are not supported." );
            	}
            }

        }
        catch (IOException ex){
            throw new RuntimeException("Cannot read file '" +
            		                   inputFile.getAbsolutePath() + "'");
        }
        catch (InputMismatchException ex) {
			throw new RuntimeException("Error parsing '" + scanner.next() + "' in file '" +
				inputFile.getAbsolutePath() + "' at line: " + lineNr  ) ;
		}
        finally {
			if (scanner != null) {
				scanner.close();
			}
		}
		this.x = new double[xVector.size()];
		this.y = new double[yVector.size()];
		for (int i = 0; i < x.length; i++) {
			x[i] = xVector.get(i);
			y[i] = yVector.get(i);
		}
	}

	public int getLocationsCount() {
		return this.locationsCount;
	}

	public String getLocationId() {
		return this.locationId;
	}

	public String getFilename() {
		return this.fileName;
	}

	public double getX(int location) {
		return this.x[location];
	}

	public double getY(int location) {
		return this.y[location];
	}
}

