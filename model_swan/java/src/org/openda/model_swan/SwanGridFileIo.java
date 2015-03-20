package org.openda.model_swan;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

import org.openda.interfaces.IArray;
import org.openda.utils.Array;
import org.openda.utils.io.AsciiFileUtils;

/**
 * Gridded swan files are all formatted in the same manner. See SWAN User Manual for the details.
 * Here only part of the functionality is implemented. Limitations are:
 * - only free formatting is supported
 * - exception values are not supported
 * 
 *  Formatting:
 *  
 *  - blocks:
 * 		[idla] prescribes the order in which the values of bottom levels and other Fields
 * 		should be given in the File.
 * 		=1: SWAN reads the map from left to right starting in the upper-left-hand
 * 		    corner of the map (it is assumed that the x-axis of the grid is pointing
 * 		to the right and the y-axis upwards). A new line in the map should
 * 		start on a new line in the file. The lay-out is as follows:
 * 		1,myc+1 2,myc+1 ... mxc+1, myc+1
 * 		1,myc 2,myc ... mxc+1, myc
 * 		... ... ... ...
 * 		1,1 2,1 ... mxc+1, 1
 * 		=2: as [idla]=1 but a new line in the map need not start on a new line in
 * 		the File.
 * 		=3: SWAN reads the map from left to right starting in the lower-left-hand
 * 		corner of the map. A new line in the map should start on a new line in
 * 		the File. The lay-out is as follows:
 * 		1,1 2,1 ... mxc+1, 1
 * 		1,2 2,2 ... mxc+1, 2
 * 		... ... ... ...
 * 		1,myc+1 2,myc+1 ... mxc+1, myc+1
 * 		=4: as [idla]=3 but a new line in the map need not start on a new line
 * 		in the file.
 * 		=5: SWAN reads the map from top to bottom starting in the lower-left-hand
 * 		corner of the map. A new column in the map should start on a new line in
 * 		the file. The lay-out is as follows:
 * 		1,1 1,2 ... 1, myc+1
 * 		2,1 2,2 ... 2, myc+1
 * 		... ... ... ...
 * 		mxc+1,1 mxc+1,2 ... mxc+1, myc+1
 * 		=6: as [idla]=5 but a new column in the map need not start on a new line
 * 		in the File.
 * 		Default: [idla]=1.
 * 
 * 	[nhedf] is the number of header lines at the start of the File. The text in the header
 * 		lines is reproduced in the print File created by SWAN (see Section 3.3). The
 * 		File may start with more header lines than [nhedf] because the start of the
 * 		File is often also the start of a time step and possibly also of a vector
 * 		variable (each having header lines, see below, [nhedt] and [nhedvec]).
 * 		Default: [nhedf]=0.
 * 	[nhedt] only if variable is time dependent: number of header lines in the File at the
 * 		start of each time level. A time step may start with more header lines than
 * 		[nhedt] because the variable may be a vector variable which has its own header
 * 		lines (see below [nhedvec]).
 * 		Default: [nhedt]=0.
 * 	[nhedvec] for each vector variable: number of header lines in the File at the start of
 * 		Description of commands 41
 * 		each component (e.g., x- or y-component).
 * 		Default: [nhedvec]=0.
 * 
 * The data-ordering in memory is like values[y][x] or values[time][y][x], where the first index runs slowest.
 * Indices start counting at 0 and the sizes are one larger than in SWAN mx=mxc+1 and my=myc+1.
 * Where data is stored in OpenDA arrays dimensions are [mt,my,mx] consistent with the java ordering.
 * 
 * 
 * @author verlaanm
 *
 */
public class SwanGridFileIo {

	/**
	 * Read one data block from a swan formatted ascii file.
	 * @param input input file. The filepointer is assumed to be at the correct position within the file,
	 *  ie all data before the values block have been read.
	 * @param idla ordering of the values - see above
	 * @param mx number of values in m or x direction. Note mx = mxc+1 compared to SWAN
	 * @param my number of values in n or y direction. Note my = myc+1 compared to SWAN
	 * @return values
	 */
	public static double[] readBlock(BufferedReader input, int idla, int mx, int my) throws IOException{
		int mnMax=mx*my;
		double values[] = new double[mnMax];
		String line = input.readLine();
		String lineFields[];
		int nData = 0;
		while ((line != null) & (nData<mnMax)){
			lineFields = line.trim().split("[ \t]+");
			for (String lineField : lineFields) {
				if(nData>=mnMax){
					throw new RuntimeException("Too many values were found in grid data: \n" + 
							mnMax+" numbers are expected.\n"+
							"last line read:"+line);
				}
				double value = Double.parseDouble(lineField);
				/*
				 * In OpenDA arrays the order of the data is with the first dimension running slowest,
				 * ie [x(1,1), x(1,2), x(1.3), x(2,1), x(2,2), x(2,3)]
				 * To match CF standards we need coordinates (y,x) or (lat,lon)
				 * 
				 * In SWAN the ordering in gridfiles depends on the value of idla
				 */
				int index = swan_to_cf_order(nData,idla,mx,my);
				values[index]=value;
				nData++;
			}
			if(nData<mnMax){
				line = input.readLine();
			}
		}
		if (nData<mnMax){
			throw new RuntimeException("Number of data values is not complete: \n" + 
					nData+"numbers were read, but "+mnMax+" were expected.\n"+
					"last line read:"+line);
		}

		return values;
	}

	/**
	 * Write one data block as SWAN formatted ascii.
	 * @param output The filepointer is assumed to be at the correct position within the file,
	 *  ie all data before the values block have been read.
	 * @param values content with the y index running slowest as in values[my][mx]
	 * @param idla ordering of the values - see above
	 * @param mx number of values in m or x direction. Note mx = mxc+1 compared to SWAN
	 * @param my number of values in n or y direction. Note my = myc+1 compared to SWAN
	 * @throws IOException there can be trouble when writing
	 */
	public static void writeBlock(PrintWriter output, double[] values, int idla, int mx, int my) throws IOException{
		int mnMax=mx*my;
		int nFieldsPerLine=-1;
		if(idla<=4){
			nFieldsPerLine=mx;
		}else{
			nFieldsPerLine=my;
		}
		String line="";
		int fieldsOnThisLine=0;
		for(int nData=0;nData<mnMax;nData++){
			int index = swan_to_cf_order(nData,idla,mx,my);
			double value = values[index];
			line+=" "+value;
			fieldsOnThisLine++;
			if(fieldsOnThisLine>=nFieldsPerLine){
				output.println(line);
				line="";
				fieldsOnThisLine=0;
			}
		}

	}

	/**
	 * Read data from a SWAN grid file with nonstationary scalar data.
	 * To use the indirect SERIES approach with a separate include file for each time use readScalarNonStationarySeries 
	 * @param values This array is used to return the values. The array should be initialized to size [mt,my,mx]
	 * @param inFile input file
	 * @param idla ordering of the values - see above
	 * @param mx number of values in m or x direction. Note mx = mxc+1 compared to SWAN
	 * @param my number of values in n or y direction. Note my = myc+1 compared to SWAN
	 * @param mt number of timesteps
	 * @param nhedf number of header lines for the file
	 * @param nhedt number of header lines at the start of each time
	 */
	public static void readScalarNonStationary(Array values, File inFile, int idla, int mx, int my, int mt,
			int nhedf, int nhedt){
		//check array size
		int valuesDims[] = values.getDimensions();
		if(valuesDims.length!=3){
			throw new RuntimeException("Values array should be 3D length(dims)="+valuesDims.length);
		}
		if(valuesDims[0]!=mt){
			throw new RuntimeException("First dimension of values array ("+valuesDims[0]+
					") should correspond to number of times "+mt);
		}
		if(valuesDims[1]!=my){
			throw new RuntimeException("Second dimension of values array ("+valuesDims[1]+
					") should correspond to lenght of y "+my);
		}
		if(valuesDims[2]!=mx){
			throw new RuntimeException("First dimension of values array ("+valuesDims[2]+
					") should correspond to length of x "+mx);
		}
		double sliceValues[];
		BufferedReader input;
		try {
			input = new BufferedReader(new FileReader(inFile));
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Problems opening file:"+inFile.getAbsolutePath());
		}
		// skip file header lines
		for(int iline=0;iline<nhedf;iline++){
			try {
				String line=input.readLine();
			} catch (IOException e) {
				throw new RuntimeException("Problem reading file header line from file:"+inFile.getAbsolutePath());
			}
		}

		for(int itime=0;itime<mt;itime++){
			// skip time header lines
			for(int iline=0;iline<nhedt;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading time header line from file:"+inFile.getAbsolutePath());
				}
			}
			try {
				sliceValues = SwanGridFileIo.readBlock(input, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problem reading data block in file:"+inFile.getAbsolutePath());
			}
			values.setSlice(sliceValues, 0, itime);

		}
		try {
			input.close();
		} catch (IOException e) {
			throw new RuntimeException("Problems opening file:"+inFile.getAbsolutePath());
		}
	}

	/**
	 * Read data from a SWAN grid file with nonstationary vector data.
	 * To use the indirect SERIES approach with a separate include file for each time use readVectorNonStationarySeries 
	 * @param xValues This array is used to return the x-values. The array should be initialized to size [mt,my,mx]
	 * @param yValues This array is used to return the y-values. The array should be initialized to size [mt,my,mx]
	 * @param inFile input file
	 * @param idla ordering of the values - see above
	 * @param mx number of values in m or x direction. Note mx = mxc+1 compared to SWAN
	 * @param my number of values in n or y direction. Note my = myc+1 compared to SWAN
	 * @param mt number of timesteps
	 * @param nhedf number of header lines for the file
	 * @param nhedt number of header lines at the start of each time
	 * @param nhedvec number of header lines at the start of each vector component
	 */
	public static void readVectorNonStationary(Array xValues, Array yValues, File inFile, int idla, int mx, int my, int mt,
			int nhedf, int nhedt, int nhedvec ){
		//check xValues array size
		int xValuesDims[] = xValues.getDimensions();
		if(xValuesDims.length!=3){
			throw new RuntimeException("xValues array should be 3D length(dims)="+xValuesDims.length);
		}
		if(xValuesDims[0]!=mt){
			throw new RuntimeException("First dimension of xValues array ("+xValuesDims[0]+
					") should correspond to number of times "+mt);
		}
		if(xValuesDims[1]!=my){
			throw new RuntimeException("Second dimension of xValues array ("+xValuesDims[1]+
					") should correspond to lenght of y "+my);
		}
		if(xValuesDims[2]!=mx){
			throw new RuntimeException("First dimension of xValues array ("+xValuesDims[2]+
					") should correspond to length of x "+mx);
		}
		//check yValues array size
		int yValuesDims[] = xValues.getDimensions();
		if(yValuesDims.length!=3){
			throw new RuntimeException("yValues array should be 3D length(dims)="+yValuesDims.length);
		}
		if(yValuesDims[0]!=mt){
			throw new RuntimeException("First dimension of yValues array ("+yValuesDims[0]+
					") should correspond to number of times "+mt);
		}
		if(yValuesDims[1]!=my){
			throw new RuntimeException("Second dimension of yValues array ("+yValuesDims[1]+
					") should correspond to lenght of y "+my);
		}
		if(yValuesDims[2]!=mx){
			throw new RuntimeException("First dimension of yValues array ("+yValuesDims[2]+
					") should correspond to length of x "+mx);
		}

		double sliceValues[];
		BufferedReader input;
		try {
			input = new BufferedReader(new FileReader(inFile));
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Problems opening file:"+inFile.getAbsolutePath());
		}
		// skip file header lines
		for(int iline=0;iline<nhedf;iline++){
			try {
				String line=input.readLine();
			} catch (IOException e) {
				throw new RuntimeException("Problem reading file header line from file:"+inFile.getAbsolutePath());
			}
		}

		for(int itime=0;itime<mt;itime++){
			// skip time header lines
			for(int iline=0;iline<nhedt;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading time header line from file:"+inFile.getAbsolutePath());
				}
			}
			// skip vector header lines
			for(int iline=0;iline<nhedvec;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading vector header line from file:"+inFile.getAbsolutePath());
				}
			}
			try {
				sliceValues = SwanGridFileIo.readBlock(input, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problem reading data block in file:"+inFile.getAbsolutePath());
			}
			xValues.setSlice(sliceValues, 0, itime);
			// skip vector header lines
			for(int iline=0;iline<nhedvec;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading vector header line from file:"+inFile.getAbsolutePath());
				}
			}
			try {
				sliceValues = SwanGridFileIo.readBlock(input, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problem reading data block in file:"+inFile.getAbsolutePath());
			}
			yValues.setSlice(sliceValues, 0, itime);
		}
		try {
			input.close();
		} catch (IOException e) {
			throw new RuntimeException("Problems opening file:"+inFile.getAbsolutePath());
		}

	}

	/**
	 * Read data from a SWAN grid file with nonstationary scalar data using the indirect SERIES approach 
	 * with a separate include file for each time.  Use readScalarNonStationary when all data is in one file 
	 * @param values This array is used to return the values. The array should be initialized to size [mt,my,mx]
	 * @param inFile input file
	 * @param idla ordering of the values - see above
	 * @param mx number of values in m or x direction. Note mx = mxc+1 compared to SWAN
	 * @param my number of values in n or y direction. Note my = myc+1 compared to SWAN
	 * @param mt number of timesteps
	 * @param nhedf number of header lines for the file
	 * @param nhedt number of header lines at the start of each time
	 */
	public static void readScalarNonStationarySeries(Array values, File inFile, int idla, int mx, int my, int mt,
			int nhedf, int nhedt){
		//check array size
		int valuesDims[] = values.getDimensions();
		if(valuesDims.length!=3){
			throw new RuntimeException("Values array should be 3D length(dims)="+valuesDims.length);
		}
		if(valuesDims[0]!=mt){
			throw new RuntimeException("First dimension of values array ("+valuesDims[0]+
					") should correspond to number of times "+mt);
		}
		if(valuesDims[1]!=my){
			throw new RuntimeException("Second dimension of values array ("+valuesDims[1]+
					") should correspond to lenght of y "+my);
		}
		if(valuesDims[2]!=mx){
			throw new RuntimeException("First dimension of values array ("+valuesDims[2]+
					") should correspond to length of x "+mx);
		}

		// cache filenames
		ArrayList<String> seriesFileNames = null;
		try {
			seriesFileNames = AsciiFileUtils.readLines(inFile);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Could not find swan series file with name "+inFile.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("Problems reading from swan series file: "+inFile.getAbsolutePath());
		}

		// check sizes
		if(seriesFileNames.size()<mt+nhedf){
			throw new RuntimeException("The length of the SWAN series file is smaller than number of times plus"+
					"number of header lines:"+"nhedf="+nhedf+" lines="+seriesFileNames.size()+" noTimes="+mt);
		}

		double sliceValues[];
		BufferedReader input;
		for(int itime=0;itime<mt;itime++){
			File timeFile = new File(inFile.getParentFile(),seriesFileNames.get(itime+nhedf)); // skip file header
			if(!timeFile.exists()){
				throw new RuntimeException("SWAN grid input file does not exist:"+timeFile.getAbsolutePath());				
			}
			try {
				input = new BufferedReader(new FileReader(timeFile));
			} catch (FileNotFoundException e) {
				throw new RuntimeException("Problems opening file:"+timeFile.getAbsolutePath());
			}

			// skip time header lines
			for(int iline=0;iline<nhedt;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading time header line from file:"+timeFile.getAbsolutePath());
				}
			}
			try {
				sliceValues = SwanGridFileIo.readBlock(input, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problem reading data block in file:"+timeFile.getAbsolutePath());
			}
			values.setSlice(sliceValues, 0, itime);

			try {
				input.close();
			} catch (IOException e) {
				throw new RuntimeException("Problems opening file:"+timeFile.getAbsolutePath());
			}

		}

	}

	/**
	 * Read data from a SWAN grid file with nonstationary vector data using the indirect SERIES approach 
	 * with a separate include file for each time.  Use readVectorNonStationary when all data is in one file 
	 * @param xValues This array is used to return the x-values. The array should be initialized to size [mt,my,mx]
	 * @param yValues This array is used to return the y-values. The array should be initialized to size [mt,my,mx]
	 * @param inFile input file
	 * @param idla ordering of the values - see above
	 * @param mx number of values in m or x direction. Note mx = mxc+1 compared to SWAN
	 * @param my number of values in n or y direction. Note my = myc+1 compared to SWAN
	 * @param mt number of timesteps
	 * @param nhedf number of header lines for the file
	 * @param nhedt number of header lines at the start of each time
	 * @param nhedvec number of header lines at the start of each vector component
	 */
	public static void readVectorNonStationarySeries(Array xValues, Array yValues, File inFile, int idla, int mx, int my, int mt,
			int nhedf, int nhedt, int nhedvec ){
		//check xValues array size
		int xValuesDims[] = xValues.getDimensions();
		if(xValuesDims.length!=3){
			throw new RuntimeException("xValues array should be 3D length(dims)="+xValuesDims.length);
		}
		if(xValuesDims[0]!=mt){
			throw new RuntimeException("First dimension of xValues array ("+xValuesDims[0]+
					") should correspond to number of times "+mt);
		}
		if(xValuesDims[1]!=my){
			throw new RuntimeException("Second dimension of xValues array ("+xValuesDims[1]+
					") should correspond to lenght of y "+my);
		}
		if(xValuesDims[2]!=mx){
			throw new RuntimeException("First dimension of xValues array ("+xValuesDims[2]+
					") should correspond to length of x "+mx);
		}
		//check yValues array size
		int yValuesDims[] = xValues.getDimensions();
		if(yValuesDims.length!=3){
			throw new RuntimeException("yValues array should be 3D length(dims)="+yValuesDims.length);
		}
		if(yValuesDims[0]!=mt){
			throw new RuntimeException("First dimension of yValues array ("+yValuesDims[0]+
					") should correspond to number of times "+mt);
		}
		if(yValuesDims[1]!=my){
			throw new RuntimeException("Second dimension of yValues array ("+yValuesDims[1]+
					") should correspond to lenght of y "+my);
		}
		if(yValuesDims[2]!=mx){
			throw new RuntimeException("First dimension of yValues array ("+yValuesDims[2]+
					") should correspond to length of x "+mx);
		}

		// cache filenames
		ArrayList<String> seriesFileNames = null;
		try {
			seriesFileNames = AsciiFileUtils.readLines(inFile);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Could not find swan series file with name "+inFile.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("Problems reading from swan series file: "+inFile.getAbsolutePath());
		}

		// check sizes
		if(seriesFileNames.size()<mt+nhedf){
			throw new RuntimeException("The length of the SWAN series file is smaller than number of times plus"+
					"number of header lines:"+"nhedf="+nhedf+" lines="+seriesFileNames.size()+" noTimes="+mt);
		}

		double sliceValues[];
		BufferedReader input;
		for(int itime=0;itime<mt;itime++){
			File timeFile = new File(inFile.getParentFile(),seriesFileNames.get(itime+nhedf)); // skip file header
			if(!timeFile.exists()){
				throw new RuntimeException("SWAN grid input file does not exist:"+timeFile.getAbsolutePath());				
			}
			try {
				input = new BufferedReader(new FileReader(timeFile));
			} catch (FileNotFoundException e) {
				throw new RuntimeException("Problems opening file:"+timeFile.getAbsolutePath());
			}

			// skip time header lines
			for(int iline=0;iline<nhedt;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading time header line from file:"+timeFile.getAbsolutePath());
				}
			}
			// skip vector header lines
			for(int iline=0;iline<nhedvec;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading vector header line from file:"+inFile.getAbsolutePath());
				}
			}
			try {
				sliceValues = SwanGridFileIo.readBlock(input, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problem reading data block in file:"+inFile.getAbsolutePath());
			}
			xValues.setSlice(sliceValues, 0, itime);
			// skip vector header lines
			for(int iline=0;iline<nhedvec;iline++){
				try {
					String line=input.readLine();
				} catch (IOException e) {
					throw new RuntimeException("Problem reading vector header line from file:"+inFile.getAbsolutePath());
				}
			}
			try {
				sliceValues = SwanGridFileIo.readBlock(input, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problem reading data block in file:"+inFile.getAbsolutePath());
			}
			yValues.setSlice(sliceValues, 0, itime);

			try {
				input.close();
			} catch (IOException e) {
				throw new RuntimeException("Problems opening file:"+timeFile.getAbsolutePath());
			}
		}
	}

	/**
	 * Write a time dependent nostationary array in SWAN grid format
	 * @param values dimensions are taken from the array, ie values[mt][my][mx]
	 * @param outFile
	 * @param idla ordering of the values - see above
	 * @param fileHeaders lines to write at the top of the file
	 * @param timeHeaders lines to add per timestep, the length of this array should be a multiple of the number of timesteps.
	 */
	public static void writeScalarNonStationary(Array values, File outFile, int idla, String[] fileHeaders, String[] timeHeaders){
		//input checks
		int[] valuesDims=values.getDimensions();
		int mt=valuesDims[0];
		int my=valuesDims[1];
		int mx=valuesDims[2];
		if(valuesDims.length!=3){
			throw new RuntimeException("Values array should be 3D length(dims)="+valuesDims.length);
		}
		int nhedf=0;
		if(fileHeaders!=null){
			nhedf=fileHeaders.length;
		}
		int nhedt=0;
		if(timeHeaders!=null){
			nhedt=timeHeaders.length/mt;
			if(timeHeaders.length%mt!=0){
				throw new RuntimeException("Number of time header lines ("+timeHeaders.length+") is not a multiple of the number of times ("+mt+")");
			}
		}
		//open file
		PrintWriter output;
		try {
			output = new PrintWriter(new FileWriter(outFile));
		} catch (IOException e) {
			throw new RuntimeException("Problems opening file for writing:"+outFile.getAbsolutePath());
		}
		//write file header
		for(int iline=0;iline<nhedf;iline++){
			output.println(fileHeaders[iline]);
		}
		//time loop
		for(int itime=0;itime<mt;itime++){
			//time header
			for(int iline=0;iline<nhedt;iline++){
				output.println(timeHeaders[iline+itime*nhedt]);
			}
			//datablock
			double slice[] = values.getSliceAsDoubles(0, itime,itime);
			try {
				writeBlock(output, slice, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problems writing data block to SWAN grid file:"+outFile.getAbsolutePath()
						+"\n itime="+itime+" message="+e.getMessage());
			}
		}
		//close file
		output.close();
	}

	/**
	 * Write a time dependent nonstationary vector data in SWAN grid format
	 * @param xValues values dimensions are taken from the array, ie values[mt][my][mx]
	 * @param yValues The dimensions for yValues and xValues should be equal
	 * @param outFile
	 * @param idla ordering of the values - see above
	 * @param fileHeaders lines to write at the top of the file
	 * @param timeHeaders lines to add per timestep, the length of this array should be a multiple of the number of timesteps.
	 * @param vectorHeaders lines to add for each vector component. If eg the length of this array is 4 then the first 
	 * two lines are used for the x-component and the remaining 2 for the y-component.
	 */
	public static void writeVectorNonStationary(IArray xValues, IArray yValues, File outFile, int idla, String[] fileHeaders, String[] timeHeaders, String[] vectorHeaders){
		//input checks
		int[] xValuesDims=xValues.getDimensions();
		int[] yValuesDims=yValues.getDimensions();
		int mt=xValuesDims[0];
		int my=xValuesDims[1];
		int mx=xValuesDims[2];
		if(xValuesDims.length!=3){
			throw new RuntimeException("xValues array should be 3D length(dims)="+xValuesDims.length);
		}
		if(yValuesDims.length!=3){
			throw new RuntimeException("yValues array should be 3D length(dims)="+yValuesDims.length);
		}
		if(xValuesDims[0]!=yValuesDims[0]){
			throw new RuntimeException("xValues and yValues should have same first dimensions (time) "
					+xValuesDims[0]+"<>"+yValuesDims[0]);
		}
		if(xValuesDims[1]!=yValuesDims[1]){
			throw new RuntimeException("xValues and yValues should have same second dimensions (y) "
					+xValuesDims[1]+"<>"+yValuesDims[1]);
		}
		if(xValuesDims[2]!=yValuesDims[2]){
			throw new RuntimeException("xValues and yValues should have same third dimensions (x) "
					+xValuesDims[2]+"<>"+yValuesDims[2]);
		}
		// get lengths from array
		int nhedf=0;
		if(fileHeaders!=null){
			nhedf=fileHeaders.length;
		}
		int nhedt=0;
		if(timeHeaders!=null){
			nhedt=timeHeaders.length/mt;
			if(timeHeaders.length%mt!=0){
				throw new RuntimeException("Number of time header lines ("+timeHeaders.length
						+") is not a multiple of the number of times ("+mt+")");
			}
		}
		int nhedvec=0;
		if(vectorHeaders!=null){
			nhedvec=vectorHeaders.length/2;
			if(vectorHeaders.length%2!=0){
				throw new RuntimeException("Number of vector header lines ("+timeHeaders.length
						+") is not a multiple of the number of vector components (2)");
			}
		}

		//open file
		PrintWriter output;
		try {
			output = new PrintWriter(new FileWriter(outFile));
		} catch (IOException e) {
			throw new RuntimeException("Problems opening file for writing:"+outFile.getAbsolutePath());
		}
		//write file header
		for(int iline=0;iline<nhedf;iline++){
			output.println(fileHeaders[iline]);
		}
		//time loop
		double slice[];
		for(int itime=0;itime<mt;itime++){
			//time header
			for(int iline=0;iline<nhedt;iline++){
				output.println(timeHeaders[iline+itime*nhedt]);
			}
			//x vector header
			for(int iline=0;iline<nhedvec;iline++){
				output.println(vectorHeaders[iline]);
			}
			//datablock
			slice = xValues.getSliceAsDoubles(0, itime,itime);
			try {
				writeBlock(output, slice, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problems writing data block to SWAN grid file:"+outFile.getAbsolutePath()
						+"\n itime="+itime+" message="+e.getMessage());
			}
			//y vector header
			for(int iline=0;iline<nhedvec;iline++){
				output.println(vectorHeaders[iline+nhedvec]);
			}
			//datablock
			slice = yValues.getSliceAsDoubles(0, itime,itime);
			try {
				writeBlock(output, slice, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problems writing data block to SWAN grid file:"+outFile.getAbsolutePath()
						+"\n itime="+itime+" message="+e.getMessage());
			}
		}
		//close file
		output.close();
	}

	/**
	 * Write a time dependent nonstationary scalar data in SWAN grid format. Uses indirect TIMESERIES format,
	 * use writeScalarNonStationary if all data should be written to one output file.
	 * @param values dimensions are taken from the array, ie values[mt][my][mx]
	 * @param outFile
	 * @param includeNames files names to use for separate include files
	 * @param idla ordering of the values - see above
	 * @param fileHeaders lines to write at the top of the file
	 * @param timeHeaders lines to add per timestep, the length of this array should be a multiple of the number of timesteps.
	 */
	public static void writeScalarNonStationarySeries(Array values, File outFile, String includeNames[], 
			int idla, String[] fileHeaders, String[] timeHeaders){
		//input checks
		int[] valuesDims=values.getDimensions();
		int mt=valuesDims[0];
		int my=valuesDims[1];
		int mx=valuesDims[2];
		if(valuesDims.length!=3){
			throw new RuntimeException("Values array should be 3D length(dims)="+valuesDims.length);
		}
		if(includeNames.length!=mt){
			throw new RuntimeException("One filename should be given for each include file (ie timestep"
					+"#times="+mt+" #includeFiles="+includeNames.length);
		}
		int nhedf=0;
		if(fileHeaders!=null){
			nhedf=fileHeaders.length;
		}
		int nhedt=0;
		if(timeHeaders!=null){
			nhedt=timeHeaders.length/mt;
			if(timeHeaders.length%mt!=0){
				throw new RuntimeException("Number of time header lines ("+timeHeaders.length+") is not a multiple of the number of times ("+mt+")");
			}
		}
		//write series file
		String lines[]=new String[nhedf+mt];
		for(int iline=0;iline<nhedf;iline++){
			lines[iline]=fileHeaders[iline];
		}
		for(int iline=0;iline<mt;iline++){
			lines[iline+nhedf]=includeNames[iline];
		}
		try {
			AsciiFileUtils.writeLines(outFile, lines);
		} catch (IOException e1) {
			throw new RuntimeException("Problems writing to SWAN series file:"+outFile.getAbsolutePath());
		}
		File workDir=outFile.getParentFile();
		//write include files
		//time loop
		for(int itime=0;itime<mt;itime++){
			PrintWriter output;
			try {
				outFile = new File(workDir,includeNames[itime]);
				output = new PrintWriter(new FileWriter(outFile));
			} catch (IOException e) {
				throw new RuntimeException("Problems opening file for writing:"+outFile.getAbsolutePath());
			}
			//time header
			for(int iline=0;iline<nhedt;iline++){
				output.println(timeHeaders[iline+itime*nhedt]);
			}
			//datablock
			double slice[] = values.getSliceAsDoubles(0, itime,itime);
			try {
				writeBlock(output, slice, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problems writing data block to SWAN grid file:"+outFile.getAbsolutePath()
						+"\n itime="+itime+" message="+e.getMessage());
			}
			//close file
			output.close();
		}
	}

	/**
	 * Write a time dependent nonstationary vector data in SWAN grid format. Uses indirect TIMESERIES format,
	 * use writeScalarNonStationary if all data should be written to one output file.
	 * @param xValues values dimensions are taken from the array, ie values[mt][my][mx]
	 * @param yValues The dimensions for yValues and xValues should be equal
	 * @param outFile
	 * @param includeNames files names to use for separate include files
	 * @param idla ordering of the values - see above
	 * @param fileHeaders lines to write at the top of the file
	 * @param timeHeaders lines to add per timestep, the length of this array should be a multiple of the number of timesteps.
	 * @param vectorHeaders lines to add for each vector component. If eg the length of this array is 4 then the first 
	 * two lines are used for the x-component and the remaining 2 for the y-component.
	 */
	public static void writeVectorNonStationarySeries(IArray xValues, IArray yValues, File outFile,  String includeNames[],
			int idla, String[] fileHeaders, String[] timeHeaders, String[] vectorHeaders){
		//input checks
		int[] xValuesDims=xValues.getDimensions();
		int[] yValuesDims=yValues.getDimensions();
		int mt=xValuesDims[0];
		int my=xValuesDims[1];
		int mx=xValuesDims[2];
		if(xValuesDims.length!=3){
			throw new RuntimeException("xValues array should be 3D length(dims)="+xValuesDims.length);
		}
		if(yValuesDims.length!=3){
			throw new RuntimeException("yValues array should be 3D length(dims)="+yValuesDims.length);
		}
		if(xValuesDims[0]!=yValuesDims[0]){
			throw new RuntimeException("xValues and yValues should have same first dimensions (time) "
					+xValuesDims[0]+"<>"+yValuesDims[0]);
		}
		if(xValuesDims[1]!=yValuesDims[1]){
			throw new RuntimeException("xValues and yValues should have same second dimensions (y) "
					+xValuesDims[1]+"<>"+yValuesDims[1]);
		}
		if(xValuesDims[2]!=yValuesDims[2]){
			throw new RuntimeException("xValues and yValues should have same third dimensions (x) "
					+xValuesDims[2]+"<>"+yValuesDims[2]);
		}
		if(includeNames.length!=mt){
			throw new RuntimeException("One filename should be given for each include file (ie timestep"
					+"#times="+mt+" #includeFiles="+includeNames.length);
		}
		int nhedf=0;
		if(fileHeaders!=null){
			nhedf=fileHeaders.length;
		}
		int nhedt=0;
		if(timeHeaders!=null){
			nhedt=timeHeaders.length/mt;
			if(timeHeaders.length%mt!=0){
				throw new RuntimeException("Number of time header lines ("+timeHeaders.length+") is not a multiple of the number of times ("+mt+")");
			}
		}
		int nhedvec=0;
		if(vectorHeaders!=null){
			nhedvec=vectorHeaders.length/2;
			if(vectorHeaders.length%2!=0){
				throw new RuntimeException("Number of vector header lines ("+timeHeaders.length
						+") is not a multiple of the number of vector components (2)");
			}
		}

		//write series file
		String lines[]=new String[nhedf+mt];
		for(int iline=0;iline<nhedf;iline++){
			lines[iline]=fileHeaders[iline];
		}
		for(int iline=0;iline<mt;iline++){
			lines[iline+nhedf]=includeNames[iline];
		}
		try {
			AsciiFileUtils.writeLines(outFile, lines);
		} catch (IOException e1) {
			throw new RuntimeException("Problems writing to SWAN series file:"+outFile.getAbsolutePath());
		}
		File workDir=outFile.getParentFile();
		//write include files
		//time loop
		double slice[];
		for(int itime=0;itime<mt;itime++){
			PrintWriter output;
			try {
				outFile = new File(workDir,includeNames[itime]);
				output = new PrintWriter(new FileWriter(outFile));
			} catch (IOException e) {
				throw new RuntimeException("Problems opening file for writing:"+outFile.getAbsolutePath());
			}
			//time header
			for(int iline=0;iline<nhedt;iline++){
				output.println(timeHeaders[iline+itime*nhedt]);
			}
			//x vector header
			for(int iline=0;iline<nhedvec;iline++){
				output.println(vectorHeaders[iline]);
			}
			//datablock
			slice = xValues.getSliceAsDoubles(0, itime,itime);
			try {
				writeBlock(output, slice, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problems writing data block to SWAN grid file:"+outFile.getAbsolutePath()
						+"\n itime="+itime+" message="+e.getMessage());
			}
			//y vector header
			for(int iline=0;iline<nhedvec;iline++){
				output.println(vectorHeaders[iline+nhedvec]);
			}
			//datablock
			slice = yValues.getSliceAsDoubles(0, itime,itime);
			try {
				writeBlock(output, slice, idla, mx, my);
			} catch (IOException e) {
				throw new RuntimeException("Problems writing data block to SWAN grid file:"+outFile.getAbsolutePath()
						+"\n itime="+itime+" message="+e.getMessage());
			}
			//close file
			output.close();
		}
	}

	/**
	 * In OpenDA arrays the order of the data is with the first dimension running slowest,
	 * ie [x(1,1), x(1,2), x(1.3), x(2,1), x(2,2), x(2,3)]
	 * To match CF standards we need coordinates (y,x) or (lat,lon)
	 * 
	 * In SWAN the ordering in gridfiles depends on the value of idla
	 * 		[idla] prescribes the order in which the values of bottom levels and other Fields
	 * 		should be given in the File.
	 * 		=1: SWAN reads the map from left to right starting in the upper-left-hand
	 * 		    corner of the map (it is assumed that the x-axis of the grid is pointing
	 * 		to the right and the y-axis upwards). A new line in the map should
	 * 		start on a new line in the file. The lay-out is as follows:
	 * 		1,myc+1 2,myc+1 ... mxc+1, myc+1
	 * 		1,myc 2,myc ... mxc+1, myc
	 * 		... ... ... ...
	 * 		1,1 2,1 ... mxc+1, 1
	 * 		=2: as [idla]=1 but a new line in the map need not start on a new line in
	 * 		the File.
	 * 		=3: SWAN reads the map from left to right starting in the lower-left-hand
	 * 		corner of the map. A new line in the map should start on a new line in
	 * 		the File. The lay-out is as follows:
	 * 		1,1 2,1 ... mxc+1, 1
	 * 		1,2 2,2 ... mxc+1, 2
	 * 		... ... ... ...
	 * 		1,myc+1 2,myc+1 ... mxc+1, myc+1
	 * 		=4: as [idla]=3 but a new line in the map need not start on a new line
	 * 		in the file.
	 * 		=5: SWAN reads the map from top to bottom starting in the lower-left-hand
	 * 		corner of the map. A new column in the map should start on a new line in
	 * 		the file. The lay-out is as follows:
	 * 		1,1 1,2 ... 1, myc+1
	 * 		2,1 2,2 ... 2, myc+1
	 * 		... ... ... ...
	 * 		mxc+1,1 mxc+1,2 ... mxc+1, myc+1
	 * 		=6: as [idla]=5 but a new column in the map need not start on a new line
	 * 		in the File.
	 * 		Default: [idla]=1.
	 * 
	 * @param nData index in SWAN grid file
	 * @param idla swan ordering code
	 * @return CF index for v[y][x]
	 */
	private static int swan_to_cf_order(int swanIndex,int idla,int swanMmax,int swanNmax){
		int index=-1;
		if((idla==1) | (idla==2)){
			/* flip y and mirror x,y
			 * 
			 * 		=1: SWAN reads the map from left to right starting in the upper-left-hand
			 * 		    corner of the map (it is assumed that the x-axis of the grid is pointing
			 * 		to the right and the y-axis upwards). A new line in the map should
			 * 		start on a new line in the file. The lay-out is as follows:
			 * 		1,myc+1 2,myc+1 ... mxc+1, myc+1
			 * 		1,myc 2,myc ... mxc+1, myc
			 * 		... ... ... ...
			 * 		1,1 2,1 ... mxc+1, 1
			 *  
			 *  openda needs order 1,1 2,1 3,1 1,2 2,2 ...
			 */
			int m = swanIndex%swanMmax; //note that we start counting at 0 here.
			int n = swanNmax-1-swanIndex/swanMmax;
			index=n*swanMmax+m;
		}else if((idla==3) | (idla==4)){
			/*
			 * 		=3: SWAN reads the map from left to right starting in the lower-left-hand
			 * 		corner of the map. A new line in the map should start on a new line in
			 * 		the File. The lay-out is as follows:
			 * 		1,1 2,1 ... mxc+1, 1
			 * 		1,2 2,2 ... mxc+1, 2
			 * 		... ... ... ...
			 * 		1,myc+1 2,myc+1 ... mxc+1, myc+1
			 */
			//mirror x,y, ie transpose
			int m = swanIndex%swanMmax; //note that we start counting at 0 here.
			int n = swanIndex/swanMmax;
			index=n*swanMmax+m;
		}else if((idla==5) |(idla==6)){
			/*
			 * 		=5: SWAN reads the map from top to bottom starting in the lower-left-hand
			 * 		corner of the map. A new column in the map should start on a new line in
			 * 		the file. The lay-out is as follows:
			 * 		1,1 1,2 ... 1, myc+1
			 * 		2,1 2,2 ... 2, myc+1
			 * 		... ... ... ...
			 * 		mxc+1,1 mxc+1,2 ... mxc+1, myc+1
			 */
			int n = swanIndex%swanNmax; //note that we start counting at 0 here.
			int m = swanIndex/swanNmax;
			index=n*swanMmax+m;
		}
		return index;
	}

}
