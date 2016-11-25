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


package org.openda.resultwriters;

import junit.framework.TestCase;
import org.openda.costa.CtaOpenDaModel;
import org.openda.costa.CtaTreeVector;
import org.openda.costa.CtaVector;
import org.openda.interfaces.IMatrix;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.*;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Tests for the configuration of resultWriters
 */
public class NetcdfResultWriterNativeTest extends TestCase {

	//File testDir = null;
	private File testRunDataDir;
	OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfResultWriterNativeTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	//private void setTestDir() {
	//    testDir = OpenDaTestSupport.getUnitTestDir("openda/test/org/openda/resultwriters");
	//}

	private void doAlgorithmSteps(int num) {

		Results.putMessage("starting test");

		//--------------------------------------------------------------------------------
		// workaround for creating a ctatreevector: use a state from a model !!!


		File modelConfigFile = new File(testRunDataDir, "oscill_sp.xml");
		File modelClsConfigFile = new File(testRunDataDir, "oscill_sp_class.xml");

		if (num == 1) {
			CtaOpenDaModel oscill = new CtaOpenDaModel(modelClsConfigFile.getAbsolutePath(), modelConfigFile.getAbsolutePath());
			IVector x = oscill.getState();          // this is an instance of ctaTreeVector!


            Results.putValue("ctatv_from_model_1", x, x.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);

			Results.putProgression("Netcdf-export: TREEVECTOR from model ");

			double cost=0.1;
            Results.putValue("cost", cost, 1, "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);

		}
		// --------------------------------------------------


		IVector v = new Vector(3);
		v.setConstant(0);

		double[] vals1 = new double[]{1.0, 2.2, 3.0};
		double[] vals2 = new double[]{4.0, 5.2, 6.0};
		IVector vec1 = new CtaVector(3);
		IVector vec2 = new CtaVector(3);
		vec1.setValues(vals1);
		vec2.setValues(vals2);

		if (num == 2) {
			// export the ctavector
            Results.putValue("ctavector_2", vec1, vec1.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
			Results.putProgression("Netcdf-export: CTAVECTOR ");
		}


		CtaTreeVector v2cta_part1 = new CtaTreeVector("first part", "first", vec1);
		CtaTreeVector v2cta_part2 = new CtaTreeVector("second part", "second", vec2);

		if (num == 3) {
			// export this ctatreevector
            Results.putValue("ctatv_A_3", v2cta_part1, v2cta_part1.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
			Results.putProgression("Netcdf-export: CTATREEVECTOR (from vector)");
		}

		//concatenate for ctatreevector is now implemented:

		CtaTreeVector[] handlelist = {v2cta_part1,v2cta_part2};

		// int [] handlelist = { (int) v2cta_part1, (int) v2cta_part2};
		CtaTreeVector v2cta_tot = new CtaTreeVector("both parts", "total", handlelist);

		if (num == 4) {
			// export this concatenated ctatreevector
            Results.putValue("ctatv_B_4", v2cta_tot, v2cta_tot.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
			Results.putProgression("Netcdf-export: CTATREEVECTOR (concatenated)");
			//------------------------------------------------------
		}

		ITreeVector v2_part1 = new TreeVector("first part", new Vector("[1,2,3]"));
		ITreeVector v2_part2 = new TreeVector("second part", new Vector("[4,5,6]"));


		TreeVector v2 = new TreeVector("all", "Total");
		v2.addChild(v2_part1);
		v2.addChild(v2_part2);

		if (num == 5) {
			// export the concatenated TreeVector
            Results.putValue("simple_tv_5", v2, v2.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
			Results.putProgression("Netcdf-export: SIMPLE TREEVECTOR");
		}

		Vector v3 = new Vector("[5.0,6.1,7.2]");

		if (num == 6) {
			// export the concatenated Vector
            Results.putValue("simplevec_6", v3, v3.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
			Results.putProgression("Netcdf-export: SIMPLE VECTOR");
		}

		IMatrix Mat1 = new Matrix("[1.0,2.0,3.0;4.0,5.0,6.0]");

		if (num == 7) {
			// export the Matrix
            Results.putValue("simplematrix_7", Mat1, Mat1.getNumberOfColumns()*Mat1.getNumberOfRows(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
			Results.putProgression("Netcdf-export: SIMPLE MATRIX");
		}

		// ------------------------------------------------------------------------

		Results.putProgression("*** START doAlgorithmSteps ***");

	}

	public void testAllOutput() {  // first implementation

		//setTestDir();
		Results.reset();
		IResultWriter netcdfWriter = new NetcdfResultWriterNative(testRunDataDir, "de_test_.nc");
		Results.addResultWriter(netcdfWriter);
		doAlgorithmSteps(1);
		doAlgorithmSteps(2);
		doAlgorithmSteps(7);
		doAlgorithmSteps(5);
		doAlgorithmSteps(3);
		for (int i = 0; i < 3; i++) {
			doAlgorithmSteps(4);
		}
		doAlgorithmSteps(6);

		Results.reset();

		File ncFile1 = new File(testRunDataDir, "de_test_ctatv_A_3.nc");
		assertTrue("Output file should exist",ncFile1.exists());
		File ncFile1Dump = new File(testRunDataDir, "de_test_ctatv_A_3.txt");
		File ncFile1Ref = new File(testRunDataDir, "de_test_ctatv_A_3.ref");
		generateAsciiDump(ncFile1.getAbsolutePath(),ncFile1Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile1Dump,ncFile1Ref,1));
		
		File ncFile2 = new File(testRunDataDir, "de_test_ctatv_B_4.nc");
		assertTrue("Output file should exist",ncFile2.exists());
		File ncFile2Dump = new File(testRunDataDir, "de_test_ctatv_B_4.txt");
		File ncFile2Ref = new File(testRunDataDir, "de_test_ctatv_B_4.ref");
		generateAsciiDump(ncFile2.getAbsolutePath(),ncFile2Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile2Dump,ncFile2Ref,1));

		File ncFile3 = new File(testRunDataDir, "de_test_ctatv_from_model_1.nc");
		assertTrue("Output file should exist",ncFile3.exists());
		File ncFile3Dump = new File(testRunDataDir, "de_test_ctatv_from_model_1.txt");
		File ncFile3Ref = new File(testRunDataDir, "de_test_ctatv_from_model_1.ref");
		generateAsciiDump(ncFile3.getAbsolutePath(),ncFile3Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile3Dump,ncFile3Ref,1));

		File ncFile4 = new File(testRunDataDir, "de_test_ctavector_2.nc");
		assertTrue("Output file should exist",ncFile4.exists());
		File ncFile4Dump = new File(testRunDataDir, "de_test_ctavector_2.txt");
		File ncFile4Ref = new File(testRunDataDir, "de_test_ctavector_2.ref");
		generateAsciiDump(ncFile4.getAbsolutePath(),ncFile4Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile4Dump,ncFile4Ref,1));

		File ncFile5 = new File(testRunDataDir, "de_test_simple_tv_5.nc");
		assertTrue("Output file should exist",ncFile5.exists());
		File ncFile5Dump = new File(testRunDataDir, "de_test_simple_tv_5.txt");
		File ncFile5Ref = new File(testRunDataDir, "de_test_simple_tv_5.ref");
		generateAsciiDump(ncFile5.getAbsolutePath(),ncFile5Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile5Dump,ncFile5Ref,1));

		File ncFile6 = new File(testRunDataDir, "de_test_simplematrix_7.nc");
		assertTrue("Output file should exist",ncFile6.exists());
		File ncFile6Dump = new File(testRunDataDir, "de_test_simplematrix_7.txt");
		File ncFile6Ref = new File(testRunDataDir, "de_test_simplematrix_7.ref");
		generateAsciiDump(ncFile6.getAbsolutePath(),ncFile6Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile6Dump,ncFile6Ref,1));

		File ncFile7 = new File(testRunDataDir, "de_test_simplevec_6.nc");
		assertTrue("Output file should exist",ncFile7.exists());
		File ncFile7Dump = new File(testRunDataDir, "de_test_simplevec_6.txt");
		File ncFile7Ref = new File(testRunDataDir, "de_test_simplevec_6.ref");
		generateAsciiDump(ncFile7.getAbsolutePath(),ncFile7Dump.getAbsolutePath());
		assertTrue(testData.FilesAreIdentical(ncFile7Dump,ncFile7Ref,1));

	}

	private void generateAsciiDump(String nameNcFile, String nameTextFile){
		//NCdump filename [-ncml] [-c | -vall] [-v varName;...]
		String command=nameNcFile+" -vall";
		try{
			FileWriter out = new FileWriter(nameTextFile);
			ucar.nc2.NCdumpW.print(command,out);
		}catch (Exception e) {
			throw new RuntimeException("NetcdfResultWriterNative junit test. Problem with ascii dump of netcdf.");
		}
	}
}
