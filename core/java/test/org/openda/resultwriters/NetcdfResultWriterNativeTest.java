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

	public void testAllOutput() throws IOException {  // first implementation

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

		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_ctatv_A_3.nc"),
			new File(testRunDataDir,"de_test_ctatv_A_3.nc"));
		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_ctatv_B_4.nc"),
			new File(testRunDataDir,"de_test_ctatv_B_4.nc"));
		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_ctatv_from_model_1.nc"),
			new File(testRunDataDir,"de_test_ctatv_from_model_1.nc"));
		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_ctavector_2.nc"),
			new File(testRunDataDir,"de_test_ctavector_2.nc"));
		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_simple_tv_5.nc"),
			new File(testRunDataDir,"de_test_simple_tv_5.nc"));
		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_simplematrix_7.nc"),
			new File(testRunDataDir,"de_test_simplematrix_7.nc"));
		OpenDaTestSupport.compareNetcdfFiles(
			new File(testRunDataDir,"de_ref_simplevec_6.nc"),
			new File(testRunDataDir,"de_test_simplevec_6.nc"));
	}
}
