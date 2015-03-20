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

package org.openda.model_swan;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.IOException;
import java.util.Arrays;

/**
 * Test of reading and writing wind vector data file of SWAN.
 */
public class SwanGridFileIoTest extends TestCase {

	OpenDaTestSupport testData = null;
	File gridFilesTestDir = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(SwanGridFileIoTest.class, "model_swan");
		gridFilesTestDir = new File(testData.getTestRunDataDir(), "GridFiles");
	}


	public void testSwanGridFileIo_readBlock() throws IOException {
		File windFile = new File(gridFilesTestDir, "idla_test.WND");
		BufferedReader input = new BufferedReader(new FileReader(windFile));
		int idla=1;
		int mx=3;
		int my=4;
		String headerLine = input.readLine();
		System.out.println("reading:"+headerLine);
		double values1[] = SwanGridFileIo.readBlock(input, idla, mx, my);
		System.out.println("values="+new Vector(values1));
		assertEquals("number of values in the block",mx*my, values1.length);
		assertEquals("values[0]",1.1,values1[0]);
		assertEquals("values[1]",2.1,values1[1]);
		assertEquals("values[2]",3.1,values1[2]);
		assertEquals("values[3]",1.2,values1[3]);
		assertEquals("values[4]",2.2,values1[4]);
		assertEquals("values[11]",3.4,values1[11]);

		idla=2;
		mx=3;
		my=4;
		headerLine = input.readLine();
		System.out.println("reading:"+headerLine);
		double values2[] = SwanGridFileIo.readBlock(input, idla, mx, my);
		System.out.println("values="+new Vector(values2));
		assertEquals("number of values in the block",mx*my, values2.length);
		assertEquals("values[0]",1.1,values2[0]);
		assertEquals("values[1]",2.1,values2[1]);
		assertEquals("values[2]",3.1,values2[2]);
		assertEquals("values[3]",1.2,values2[3]);
		assertEquals("values[4]",2.2,values2[4]);
		assertEquals("values[11]",3.4,values2[11]);

		idla=3;
		mx=3;
		my=4;
		headerLine = input.readLine();
		System.out.println("reading:"+headerLine);
		double values3[] = SwanGridFileIo.readBlock(input, idla, mx, my);
		System.out.println("values="+new Vector(values3));
		assertEquals("number of values in the block",mx*my, values3.length);
		assertEquals("values[0]",1.1,values3[0]);
		assertEquals("values[1]",2.1,values3[1]);
		assertEquals("values[2]",3.1,values3[2]);
		assertEquals("values[3]",1.2,values3[3]);
		assertEquals("values[4]",2.2,values3[4]);
		assertEquals("values[11]",3.4,values3[11]);

		idla=4;
		mx=3;
		my=4;
		headerLine = input.readLine();
		System.out.println("reading:"+headerLine);
		double values4[] = SwanGridFileIo.readBlock(input, idla, mx, my);
		System.out.println("values="+new Vector(values4));
		assertEquals("number of values in the block",mx*my, values4.length);
		assertEquals("values[0]",1.1,values4[0]);
		assertEquals("values[1]",2.1,values4[1]);
		assertEquals("values[2]",3.1,values4[2]);
		assertEquals("values[3]",1.2,values4[3]);
		assertEquals("values[4]",2.2,values4[4]);
		assertEquals("values[11]",3.4,values4[11]);

		idla=5;
		mx=3;
		my=4;
		headerLine = input.readLine();
		System.out.println("reading:"+headerLine);
		double values5[] = SwanGridFileIo.readBlock(input, idla, mx, my);
		System.out.println("values="+new Vector(values5));
		assertEquals("number of values in the block",mx*my, values5.length);
		assertEquals("values[0]",1.1,values5[0]);
		assertEquals("values[1]",2.1,values5[1]);
		assertEquals("values[2]",3.1,values5[2]);
		assertEquals("values[3]",1.2,values5[3]);
		assertEquals("values[4]",2.2,values5[4]);
		assertEquals("values[11]",3.4,values5[11]);

		idla=6;
		mx=3;
		my=4;
		headerLine = input.readLine();
		System.out.println("reading:"+headerLine);
		double values6[] = SwanGridFileIo.readBlock(input, idla, mx, my);
		System.out.println("values="+new Vector(values6));
		assertEquals("number of values in the block",mx*my, values6.length);
		assertEquals("values[0]",1.1,values6[0]);
		assertEquals("values[1]",2.1,values6[1]);
		assertEquals("values[2]",3.1,values6[2]);
		assertEquals("values[3]",1.2,values6[3]);
		assertEquals("values[4]",2.2,values6[4]);
		assertEquals("values[11]",3.4,values6[11]);

		input.close();
	}

	public void testSwanGridFileIo_writeBlock() throws IOException {
		File windFile = new File(gridFilesTestDir, "idla_test_out.WND");
		File windRefFile = new File(gridFilesTestDir, "idla_test_ref.WND");
		PrintWriter output = new PrintWriter(new FileWriter(windFile));
		double values[]={1.1,2.1,3.1,1.2,2.2,3.2,1.3,2.3,3.3,1.4,2.4,3.4};

		for(int idla=1;idla<7;idla++){
			int mx=3;
			int my=4;
			String headerLine="idla="+idla;
			output.println(headerLine);
			System.out.println("reading:"+headerLine);
			SwanGridFileIo.writeBlock(output, values, idla, mx, my);
		}
		output.close();
		
		assertTrue(testData.FilesAreIdentical(windFile, windRefFile));
	}

	public void testSwanGridFileIo_readScalarNonSeries() throws IOException {
		File inFile = new File(gridFilesTestDir, "scalar_notseries.GRD");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		int nhedf=1;
		int nhedt=1;
		Array values=new Array(new int[]{mt,my,mx});
		SwanGridFileIo.readScalarNonStationary(values, inFile, idla, mx, my, mt, nhedf, nhedt);
		System.out.println("values="+values.toString());
		double valuesAsDouble[]=values.getValuesAsDoubles();
		assertEquals("number of values in the block",mx*my*mt, valuesAsDouble.length);
		assertEquals("values[0]",1.11,valuesAsDouble[0]);
		assertEquals("values[1]",1.21,valuesAsDouble[1]);
		assertEquals("values[2]",1.31,valuesAsDouble[2]);
		assertEquals("values[3]",1.12,valuesAsDouble[3]);
		assertEquals("values[4]",1.22,valuesAsDouble[4]);
		assertEquals("values[12]",2.11,valuesAsDouble[12]);
		assertEquals("values[13]",2.21,valuesAsDouble[13]);
		assertEquals("values[59]",5.34,valuesAsDouble[59]);

	}

	public void testSwanGridFileIo_readVectorNonSeries() throws IOException {
		File inFile = new File(gridFilesTestDir, "vector_notseries.GRD");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		int nhedf=1;
		int nhedt=1;
		int nhedvec=1;
		Array xValues=new Array(new int[]{mt,my,mx});
		Array yValues=new Array(new int[]{mt,my,mx});
		SwanGridFileIo.readVectorNonStationary(xValues, yValues, inFile, idla, mx, my, mt, nhedf, nhedt,nhedvec);
		System.out.println("xValues="+xValues.toString());
		double u[]=xValues.getValuesAsDoubles();
		assertEquals("number of values in the block",mx*my*mt, u.length);
		assertEquals("u[0]",81.11,u[0]);
		assertEquals("u[1]",81.21,u[1]);
		assertEquals("u[2]",81.31,u[2]);
		assertEquals("u[3]",81.12,u[3]);
		assertEquals("u[4]",81.22,u[4]);
		assertEquals("u[12]",82.11,u[12]);
		assertEquals("u[13]",82.21,u[13]);
		assertEquals("u[59]",85.34,u[59]);
		System.out.println("yValues="+yValues.toString());
		double v[]=yValues.getValuesAsDoubles();
		assertEquals("number of values in the block",mx*my*mt, v.length);
		assertEquals("v[0]",91.11,v[0]);
		assertEquals("v[1]",91.21,v[1]);
		assertEquals("v[2]",91.31,v[2]);
		assertEquals("v[3]",91.12,v[3]);
		assertEquals("v[4]",91.22,v[4]);
		assertEquals("v[12]",92.11,v[12]);
		assertEquals("v[13]",92.21,v[13]);
		assertEquals("v[59]",95.34,v[59]);
	}
	
	public void testSwanGridFileIo_readScalarSeries() throws IOException {
		File inFile = new File(gridFilesTestDir, "scalar.SER");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		int nhedf=0;
		int nhedt=1;
		Array values=new Array(new int[]{mt,my,mx});
		SwanGridFileIo.readScalarNonStationarySeries(values, inFile, idla, mx, my, mt, nhedf, nhedt);
		System.out.println("values="+values.toString());
		double valuesAsDouble[]=values.getValuesAsDoubles();
		assertEquals("number of values in the block",mx*my*mt, valuesAsDouble.length);
		assertEquals("values[0]",1.11,valuesAsDouble[0]);
		assertEquals("values[1]",1.21,valuesAsDouble[1]);
		assertEquals("values[2]",1.31,valuesAsDouble[2]);
		assertEquals("values[3]",1.12,valuesAsDouble[3]);
		assertEquals("values[4]",1.22,valuesAsDouble[4]);
		assertEquals("values[12]",2.11,valuesAsDouble[12]);
		assertEquals("values[13]",2.21,valuesAsDouble[13]);
		assertEquals("values[59]",5.34,valuesAsDouble[59]);
	}

	public void testSwanGridFileIo_readVectorSeries() throws IOException {
		File inFile = new File(gridFilesTestDir, "vector.SER");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		int nhedf=0;
		int nhedt=1;
		int nhedvec=1;
		Array xValues=new Array(new int[]{mt,my,mx});
		Array yValues=new Array(new int[]{mt,my,mx});
		SwanGridFileIo.readVectorNonStationarySeries(xValues, yValues, inFile, idla, mx, my, mt, nhedf, nhedt,nhedvec);
		System.out.println("xValues="+xValues.toString());
		double u[]=xValues.getValuesAsDoubles();
		assertEquals("number of values in the block",mx*my*mt, u.length);
		assertEquals("u[0]",81.11,u[0]);
		assertEquals("u[1]",81.21,u[1]);
		assertEquals("u[2]",81.31,u[2]);
		assertEquals("u[3]",81.12,u[3]);
		assertEquals("u[4]",81.22,u[4]);
		assertEquals("u[12]",82.11,u[12]);
		assertEquals("u[13]",82.21,u[13]);
		assertEquals("u[59]",85.34,u[59]);
		System.out.println("yValues="+yValues.toString());
		double v[]=yValues.getValuesAsDoubles();
		assertEquals("number of values in the block",mx*my*mt, v.length);
		assertEquals("v[0]",91.11,v[0]);
		assertEquals("v[1]",91.21,v[1]);
		assertEquals("v[2]",91.31,v[2]);
		assertEquals("v[3]",91.12,v[3]);
		assertEquals("v[4]",91.22,v[4]);
		assertEquals("v[12]",92.11,v[12]);
		assertEquals("v[13]",92.21,v[13]);
		assertEquals("v[59]",95.34,v[59]);
	}
	
	public void testSwanGridFileIo_writeScalarNonSeries() throws IOException {
		File outFile = new File(gridFilesTestDir, "scalar_notseries_out.GRD");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		int nhedf=1;
		int nhedt=1;
		double data[]=new double[]{
				1.11,1.21,1.31,1.12,1.22,1.32,1.13,1.23,1.33,1.14,1.24,1.34,
				2.11,2.21,2.31,2.12,2.22,2.32,2.13,2.23,2.33,2.14,2.24,2.34,
				3.11,3.21,3.31,3.12,3.22,3.32,3.13,3.23,3.33,3.14,3.24,3.34,
				4.11,4.21,4.31,4.12,4.22,4.32,4.13,4.23,4.33,4.14,4.24,4.34,
				5.11,5.21,5.31,5.12,5.22,5.32,5.13,5.23,5.33,5.14,5.24,5.34	};
		Array values=new Array(data,new int[]{mt,my,mx},true);
		String fileHeaders[]=new String[]{"one header line for the file"};
		String timeHeaders[]=new String[]{"time=1","time=2","time=3","time=4","time=5"};
		SwanGridFileIo.writeScalarNonStationary(values, outFile, idla, fileHeaders, timeHeaders);
		
		File refFile = new File(gridFilesTestDir, "scalar_notseries.GRD");
		assertTrue(testData.FilesAreIdentical(outFile, refFile));
	}

	public void testSwanGridFileIo_writeVectorNonSeries() throws IOException {
		File outFile = new File(gridFilesTestDir, "vector_notseries_out.GRD");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		//int nhedf=1;
		//int nhedt=1;
		//int nhedvec=1;
		double xdata[]=new double[]{
				81.11,81.21,81.31,81.12,81.22,81.32,81.13,81.23,81.33,81.14,81.24,81.34,
				82.11,82.21,82.31,82.12,82.22,82.32,82.13,82.23,82.33,82.14,82.24,82.34,
				83.11,83.21,83.31,83.12,83.22,83.32,83.13,83.23,83.33,83.14,83.24,83.34,
				84.11,84.21,84.31,84.12,84.22,84.32,84.13,84.23,84.33,84.14,84.24,84.34,
				85.11,85.21,85.31,85.12,85.22,85.32,85.13,85.23,85.33,85.14,85.24,85.34	};
		Array xValues=new Array(xdata,new int[]{mt,my,mx},true);
		double ydata[]=new double[]{
				91.11,91.21,91.31,91.12,91.22,91.32,91.13,91.23,91.33,91.14,91.24,91.34,
				92.11,92.21,92.31,92.12,92.22,92.32,92.13,92.23,92.33,92.14,92.24,92.34,
				93.11,93.21,93.31,93.12,93.22,93.32,93.13,93.23,93.33,93.14,93.24,93.34,
				94.11,94.21,94.31,94.12,94.22,94.32,94.13,94.23,94.33,94.14,94.24,94.34,
				95.11,95.21,95.31,95.12,95.22,95.32,95.13,95.23,95.33,95.14,95.24,95.34	};
		Array yValues=new Array(ydata,new int[]{mt,my,mx},true);
		String fileHeaders[]=new String[]{"one header line for the file"};
		String timeHeaders[]=new String[]{"time=1","time=2","time=3","time=4","time=5"};
		String vectorHeaders[]=new String[]{"u","v"};
		SwanGridFileIo.writeVectorNonStationary(xValues, yValues, outFile, idla, fileHeaders, timeHeaders, vectorHeaders);
		
		File refFile = new File(gridFilesTestDir, "vector_notseries.GRD");
		assertTrue(testData.FilesAreIdentical(outFile, refFile));
	}
	
	public void testSwanGridFileIo_writeScalarSeries() throws IOException {
		File outFile = new File(gridFilesTestDir, "scalar_out.SER");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		int nhedf=0;
		int nhedt=1;
		double data[]=new double[]{
				1.11,1.21,1.31,1.12,1.22,1.32,1.13,1.23,1.33,1.14,1.24,1.34,
				2.11,2.21,2.31,2.12,2.22,2.32,2.13,2.23,2.33,2.14,2.24,2.34,
				3.11,3.21,3.31,3.12,3.22,3.32,3.13,3.23,3.33,3.14,3.24,3.34,
				4.11,4.21,4.31,4.12,4.22,4.32,4.13,4.23,4.33,4.14,4.24,4.34,
				5.11,5.21,5.31,5.12,5.22,5.32,5.13,5.23,5.33,5.14,5.24,5.34	};
		Array values=new Array(data,new int[]{mt,my,mx},true);
		String fileHeaders[]=null;
		String timeHeaders[]=new String[]{"time=1","time=2","time=3","time=4","time=5"};
		String includeNames[]=new String[]{"scalar_series_time1_out.GRD","scalar_series_time2_out.GRD",
				"scalar_series_time3_out.GRD","scalar_series_time4_out.GRD","scalar_series_time5_out.GRD"};

		SwanGridFileIo.writeScalarNonStationarySeries(values, outFile, includeNames, idla, fileHeaders, timeHeaders);		

		File refFile = new File(gridFilesTestDir, "scalar_ref.SER");
		assertTrue(testData.FilesAreIdentical(outFile, refFile));
		
		String includeRefNames[]=new String[]{"scalar_series_time1.GRD","scalar_series_time2.GRD",
				"scalar_series_time3.GRD","scalar_series_time4.GRD","scalar_series_time5.GRD"};
		for(int itime=0;itime<mt;itime++){
			outFile = new File(gridFilesTestDir, includeNames[itime]);
			refFile = new File(gridFilesTestDir, includeRefNames[itime]);
			System.out.println("Comparing include files "+includeNames[itime]+" "+includeRefNames[itime]);
			assertTrue(testData.FilesAreIdentical(outFile, refFile));			
		}
	}

	public void testSwanGridFileIo_writeVectorSeries() throws IOException {
		File outFile = new File(gridFilesTestDir, "vector_out.SER");
		int idla=1;
		int mx=3;
		int my=4;
		int mt=5;
		//int nhedf=0;
		//int nhedt=1;
		//int nhedvec=1;
		double xdata[]=new double[]{
				81.11,81.21,81.31,81.12,81.22,81.32,81.13,81.23,81.33,81.14,81.24,81.34,
				82.11,82.21,82.31,82.12,82.22,82.32,82.13,82.23,82.33,82.14,82.24,82.34,
				83.11,83.21,83.31,83.12,83.22,83.32,83.13,83.23,83.33,83.14,83.24,83.34,
				84.11,84.21,84.31,84.12,84.22,84.32,84.13,84.23,84.33,84.14,84.24,84.34,
				85.11,85.21,85.31,85.12,85.22,85.32,85.13,85.23,85.33,85.14,85.24,85.34	};
		Array xValues=new Array(xdata,new int[]{mt,my,mx},true);
		double ydata[]=new double[]{
				91.11,91.21,91.31,91.12,91.22,91.32,91.13,91.23,91.33,91.14,91.24,91.34,
				92.11,92.21,92.31,92.12,92.22,92.32,92.13,92.23,92.33,92.14,92.24,92.34,
				93.11,93.21,93.31,93.12,93.22,93.32,93.13,93.23,93.33,93.14,93.24,93.34,
				94.11,94.21,94.31,94.12,94.22,94.32,94.13,94.23,94.33,94.14,94.24,94.34,
				95.11,95.21,95.31,95.12,95.22,95.32,95.13,95.23,95.33,95.14,95.24,95.34	};
		Array yValues=new Array(ydata,new int[]{mt,my,mx},true);
		String fileHeaders[]=null;
		String timeHeaders[]=new String[]{"time=1","time=2","time=3","time=4","time=5"};
		String vectorHeaders[]=new String[]{"u","v"};
		String includeNames[]=new String[]{"vector_series_time1_out.GRD","vector_series_time2_out.GRD",
				"vector_series_time3_out.GRD","vector_series_time4_out.GRD","vector_series_time5_out.GRD"};
		SwanGridFileIo.writeVectorNonStationarySeries(xValues, yValues, outFile, includeNames, idla, fileHeaders, timeHeaders, vectorHeaders);
		
		File refFile = new File(gridFilesTestDir, "vector_ref.SER");
		assertTrue(testData.FilesAreIdentical(outFile, refFile));
		
		String includeRefNames[]=new String[]{"vector_series_time1.GRD","vector_series_time2.GRD",
				"vector_series_time3.GRD","vector_series_time4.GRD","vector_series_time5.GRD"};
		for(int itime=0;itime<mt;itime++){
			outFile = new File(gridFilesTestDir, includeNames[itime]);
			refFile = new File(gridFilesTestDir, includeRefNames[itime]);
			System.out.println("Comparing include files "+includeNames[itime]+" "+includeRefNames[itime]);
			assertTrue(testData.FilesAreIdentical(outFile, refFile));			
		}

	}

}

