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
package org.openda.utils.io;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.costa.CtaTreeVector;
import org.openda.costa.CtaVector;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Kalman gain storage object.
 */
public class KalmanGainStorageTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(KalmanGainStorageTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadWriteKalmanGainXML() throws Exception {

		File kgStorageDir = new File(testRunDataDir, "kgStorage_201102181800");
		File kgStorageDirCopy = new File(testRunDataDir, "kgStorage_201102181800_copy");
		BBUtils.makeDirectoryClone(kgStorageDir, kgStorageDirCopy);

		double timeAsMJD = 55610.75;  // 2011-02-18T19:00:00GMT 2011-02-18T20:00:00CET

		KalmanGainStorage kgStorageIn = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageIn.readKalmanGain();

		KalmanGainStorage kgStorageOut = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageOut.setKalmanGainStorageXmlFileName("kalmanGainStorage_out.xml");
		kgStorageOut.writeKalmanGain(kgStorageIn.getObservationIds(),
				kgStorageIn.getObservationOffsetInDays(), kgStorageIn.getKalmanGainColumns());

		testData.FilesAreIdentical(
				new File(kgStorageDir, "kalmanGainStorage_out.xml"),
				new File(kgStorageDirCopy, "kalmanGainStorage.xml"));

	}


	public void testWriteReadKalmanGainXML() throws Exception {
		double timeAsMJD = 55615.00;  // 2011-02-23T00:00:00GMT 2011-02-23T01:00:00CET
		int columnSize = KalmanGainStorage.DefaultMaxKeepVectorInXMLSize - 5;
		writeAndReadKalmanGain(timeAsMJD, columnSize);
	}

	public void testWriteReadKalmanGainNetCdf() throws Exception {
		for (int ii=0; ii< 2; ii++){
		   double timeAsMJD = 55615.00 + ii/24.0;  // 2011-02-23T00:00:00GMT 2011-02-23T01:00:00CET
		   int columnSize = KalmanGainStorage.DefaultMaxKeepVectorInXMLSize + 5;
		   writeAndReadKalmanGain(timeAsMJD, columnSize);
		}
	}

	private void writeAndReadKalmanGain(double timeAsMJD, int columnSize) {
		// write the kalman gain
		IVector[] kalmanGainColumnsOut = createKalmanGainColumns(columnSize);
		String[] observationIdsOut = {"loc-A", "loc-B", "loc-C", "loc-D"};
		double[] observationOffsetsInDaysOut = {-0.1d, -3d, 0d, 0d};
		String commentOut = "this is a four column kalman gain";
		KalmanGainStorage kgStorageOut = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageOut.setComment(commentOut);
		kgStorageOut.setKalmanGainStorageXmlFileName("fourColumnStorage.xml");
		kgStorageOut.writeKalmanGain(observationIdsOut, observationOffsetsInDaysOut, kalmanGainColumnsOut);

		// read the kalman gain
		KalmanGainStorage kgStorageIn = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageIn.setKalmanGainStorageXmlFileName("fourColumnStorage.xml");
		kgStorageIn.readKalmanGain();
		String commentIn = kgStorageIn.getComment();
		assertEquals("Comment out/in", commentOut, commentIn);

		String[] observationIdsIn = kgStorageIn.getObservationIds();
		for (int i = 0; i < observationIdsIn.length; i++) {
			assertEquals("observationId out/in", observationIdsOut[i], observationIdsIn[i]);
		}

		double[] observationOffsetsInDaysIn = kgStorageIn.getObservationOffsetInDays();
		for (int i = 0; i < observationOffsetsInDaysIn.length; i++) {
			assertEquals("observationOffsetInDays out/in",
					observationOffsetsInDaysOut[i], observationOffsetsInDaysIn[i]);
		}

		IVector[] kalmanGainColumnsIn = kgStorageIn.getKalmanGainColumns();
		for (int i = 0; i < kalmanGainColumnsIn.length; i++) {
			for (int j = 0; j < kalmanGainColumnsIn[i].getSize(); j++) {
				assertEquals("Value out/in", kalmanGainColumnsOut[i].getValue(j), kalmanGainColumnsIn[i].getValue(j));
			}
		}
	}

	private IVector[] createKalmanGainColumns(int columnSize) {
		// kalman gain column that will be written
		IVector[] kalmanGainColumns = new IVector[4];
		// first kalman gain column
		kalmanGainColumns[0] = new Vector(columnSize);
		kalmanGainColumns[0].setConstant(321.0d);
		// second kalman gain column
		kalmanGainColumns[1] = new Vector(columnSize);
		for (int i = 0; i < kalmanGainColumns[1].getSize(); i++) {
			kalmanGainColumns[1].setValue(i, 101d + 1);
		}
		// third kalman gain column (cta-tree vector)
		kalmanGainColumns[2] = createKgColumnTreeVector("kgColumn3XML", columnSize);
		// third kalman gain column (cta-tree vector)
		kalmanGainColumns[3] = createKgColumnCTATreeVector("kgColumn4Cta", columnSize);
		return kalmanGainColumns;
	}

	private TreeVector createKgColumnTreeVector(String treeVectorId, int totalSize) {
		TreeVector treeVector = new TreeVector(treeVectorId);

		// tree vector part 1
		double[] part1Values = {999d, 888d, 777d, 666d, 555d, 444d, 333d, 222d, 111d};
		treeVector.addChild("part1", part1Values);

		// tree vector part 3
		IVector part3Vector = new Vector(16);
		part3Vector.setConstant(23d);
		treeVector.addChild("part2", part3Vector.getValues());

		// tree vector part 2
		double[] part2Values = new double[totalSize- part1Values.length-part3Vector.getSize()];
		for (int i = 0; i < part2Values.length; i++) {
			part2Values[i] = 1010d + i * 10;
		}
		treeVector.addChild("part3", part2Values);
		return treeVector;
	}

	private CtaTreeVector createKgColumnCTATreeVector(String treeVectorId, int totalSize) {

		// tree vector part 1
		double[] part1Values = {999d, 888d, 777d, 666d, 555d, 444d, 333d, 222d, 111d};
		IVector vec1 = new CtaVector(9);
		vec1.setValues(part1Values);
		CtaTreeVector cta_part1 = new CtaTreeVector("first part", "first", vec1);

		// tree vector part 3
		IVector part3Vector = new CtaVector(16);
		part3Vector.setConstant(23d);
		CtaTreeVector cta_part3 = new CtaTreeVector("third part", "third", part3Vector);

		// tree vector part 2
		double[] part2Values = new double[totalSize- part1Values.length-part3Vector.getSize()];
		for (int i = 0; i < part2Values.length; i++) {
			part2Values[i] = 1010d + i * 10;
		}
		IVector vec2 = new CtaVector(part2Values.length);
		vec2.setValues(part2Values);
		CtaTreeVector cta_part2 = new CtaTreeVector("second part", "second", vec1);

		CtaTreeVector[] subVectors = {cta_part1,  cta_part2, cta_part3};
		return new CtaTreeVector(treeVectorId,treeVectorId,subVectors);

	}

}
