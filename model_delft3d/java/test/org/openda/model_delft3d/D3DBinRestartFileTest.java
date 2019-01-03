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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.*;

/**
 * Created by Theo on 21.04.2016.
 */
public class D3DBinRestartFileTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfD3dMapExchangeItemTest.class,"model_delft3d");
	}

	public void testReadWriteBinaryRestartFile() throws Exception {

		int mMax = 33;
		int nMax = 18;
		int nLay = 25;
		int nSubstances = 1;

		File binFilePath = new File(testData.getTestRunDataDir(), "tri-rst.cadagno_netcdf.20150603.000000");

		// Place undisturbed copy of file
		File binFilePathBase = new File(testData.getTestRunDataDir(), "tri-rest.cadagno_base_20150603.000000");
		BBUtils.copyFile(binFilePath, binFilePathBase);

		// Preparing the D3DBinRestartFile Object
		D3DBinRestartFile binFile = new D3DBinRestartFile(binFilePath, mMax, nMax, nLay, nSubstances);
		binFile.open();

		// Getting some data
		NetcdfD3dMapDataObject netcdfFile = new NetcdfD3dMapDataObject();
		netcdfFile.initialize(testData.getTestRunDataDir(), new String[]{"trim-cadagno_netcdf.nc"});
		String[] exchangeItemIDs = netcdfFile.getExchangeItemIDs();
		IExchangeItem exchangeItem = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[1]);
//		Array dataMapTime = (Array) exchangeItem.getValues();
		double[] dataMapTime = exchangeItem.getValuesAsDoubles();

		// Writing the data to the binary file
		binFile.write(exchangeItemIDs[1], dataMapTime);

		// Closing
		binFile.close();

		// Comparing
//		Files.equal(binFilePath, binFilePathBase);
//		assertBinaryEquals(binFilePath,binFilePathBase);

		if (binFilePath.length() != binFilePathBase.length()) {
			throw new RuntimeException("Binary files not the same length");
		}

		RandomAccessFile newRestartFile = new RandomAccessFile(binFilePath, "r");
		RandomAccessFile orgRestartFile = new RandomAccessFile(binFilePathBase, "r");

		float value1, value2;
		int index=0;
		do {
			//since we're buffered read() isn't expensive
			value1 = newRestartFile.readFloat();
			value2 = orgRestartFile.readFloat();
			if (value1 != value2) {
				throw new RuntimeException("Binary files different: " + value1 + " != " + value2 + ", float-index: "+ index);
			}
			index++;
		} while (value1 >= 0);

		//since we already checked that the file sizes are equal
		//if we're here we reached the end of both files without a mismatch
		System.out.println("Success, files identical");
	}

	public void testReadBinRestart() {
		File binRestartTestDir = new File(testData.getTestRunDataDir(), "binRestart");
		File binFilePath = new File(binRestartTestDir, "restart.bin");

		NetcdfD3dMapDataObject netcdfFile = new NetcdfD3dMapDataObject();
		netcdfFile.initialize(binRestartTestDir, new String[]{"mapfile.nc"});
		IExchangeItem s1 = netcdfFile.getDataObjectExchangeItem("S1");
		assertNotNull("Exch.Item S1 must be there", s1);
		double[] s1FromMap = s1.getValuesAsDoubles();

		int mMax = 33;
		int nMax = 18;
		int nLay = 25;
		int nSubstances = 1;
		D3DBinRestartFile d3DBinRestartFile = new D3DBinRestartFile(binFilePath, mMax, nMax, nLay, nSubstances);
		d3DBinRestartFile.open();

		float[] s1FromBin = d3DBinRestartFile.read("S1");

		// printNcField("S1 from MAP", mMax, nMax, 1, s1FromMap);
		printBinField("S1 from BIN", mMax, nMax, 1, s1FromBin);

//		for (int i=0; i < s1FromBin.length; i++) {
//			if (s1FromBin[i] != s1FromMap[i]) {
//				throw new RuntimeException("Bin and map file differ: " + s1FromBin[i] + " != " + s1FromMap[i] + ", index: "+ i);
//			}
//		}

		float[] u1FromBin = d3DBinRestartFile.read("R1");

		// printNcField("U1 from MAP", mMax, nMax, 1, s1FromMap);
		printBinField("U1 from BIN", mMax, nMax, 20, u1FromBin);

	}

	private void printNcField(String varName, int mMax, int nMax, int nLay, double[] doubles) {
		System.out.println("Var: " + varName);
		for (int lay=0; lay < nLay; lay++) {
			System.out.println("LAYER: " + lay);
			for (int n=0; n < nMax; n++) {
				System.out.print(n);
				for (int m=0; m < mMax; m++) {
					int index = n + nMax * m + mMax * nMax * lay;
					System.out.print(String.format(",%f", Double.isNaN(doubles[index]) ? 0 : doubles[index] ));
				}
				System.out.println("");
			}
		}
	}

	private void printBinField(String varName, int mMax, int nMax, int nLay, float[] floats) {
		System.out.println("Var: " + varName);
		for (int lay=0; lay < nLay; lay++) {
			System.out.println("LAYER: " + lay);
			for (int n=0; n < nMax; n++) {
				System.out.print(n);
				for (int m=0; m < mMax; m++) {
					int index = m + mMax * n + mMax * nMax * lay;
					System.out.print(String.format(",%f", Float.isNaN(floats[index]) ? 0 : floats[index]));
				}
				System.out.println("");
			}
		}
	}
}
