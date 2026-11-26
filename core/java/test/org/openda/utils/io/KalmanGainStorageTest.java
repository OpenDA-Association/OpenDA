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
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.TreeVector;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;

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

	public void testReadWriteKalmanGainXML() {

		File kgStorageDir = new File(testRunDataDir, "kgStorage_201102181800");
		File kgStorageDirCopy = new File(testRunDataDir, "kgStorage_201102181800_copy");
		BBUtils.makeDirectoryClone(kgStorageDir, kgStorageDirCopy);

		double timeAsMJD = 55610.75;  // 2011-02-18T19:00:00GMT 2011-02-18T20:00:00CET

		KalmanGainStorage kgStorageIn = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageIn.readKalmanGain();

		KalmanGainStorage kgStorageOut = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageOut.setKalmanGainStorageFileName("kalmanGainStorage_out.xml");
		kgStorageOut.writeKalmanGain(kgStorageIn.getObservationIds(),
				kgStorageIn.getObservationOffsetInDays(), kgStorageIn.getKalmanGainColumns(), null);

        File outFile = new File(kgStorageDir, "kalmanGainStorage_out.xml");
		File refFile = new File(kgStorageDirCopy, "kalmanGainStorage.xml");
		assert(outFile.exists());
		assert(testData.FilesAreIdentical(outFile, refFile));
	}

	public void testReadWriteKalmanGainNetcdfCF() {

		double timeAsMJD = 54101;

		KalmanGainStorage kgStorageIn = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageIn.setColumnFileType(KalmanGainStorage.StorageType.netcdf_cf);
		kgStorageIn.setKalmanGainStorageFileName("KalmanGainStorage.nc");
		kgStorageIn.readKalmanGain();

		IVector[] kalmanGainColumnsIn = kgStorageIn.getKalmanGainColumns();
		assertEquals(8, kalmanGainColumnsIn.length);

		String[] observationIds = kgStorageIn.getObservationIds();
		assert(checkKalmanGainContents(timeAsMJD, kgStorageIn, kalmanGainColumnsIn, observationIds));

		KalmanGainStorage kgStorageOut = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageOut.setColumnFileType(KalmanGainStorage.StorageType.netcdf_cf);
		kgStorageOut.setKalmanGainStorageFileName("KalmanGainStorage.nc");
		kgStorageOut.writeKalmanGain(observationIds, kgStorageIn.getObservationOffsetInDays(), kalmanGainColumnsIn, kgStorageIn.getHk());
		kgStorageOut.readKalmanGain();

		assert(checkKalmanGainContents(timeAsMJD, kgStorageOut, kgStorageOut.getKalmanGainColumns(), kgStorageOut.getObservationIds()));
	}

	public void testReadWriteKalmanGainNetcdfCFHK() throws ParseException {

		double timeAsMJD = TimeUtils.date2Mjd("202209011200");

		KalmanGainStorage kgStorageIn = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageIn.setColumnFileType(KalmanGainStorage.StorageType.netcdf_cf);
		kgStorageIn.setKalmanGainStorageFileName("kalmanGainStorageHK.nc");
		kgStorageIn.readKalmanGain();

		IVector[] kalmanGainColumnsIn = kgStorageIn.getKalmanGainColumns();
		assertEquals(4, kalmanGainColumnsIn.length);

		String[] observationIds = kgStorageIn.getObservationIds();
		String[] expectedObsIds = {"WESTKPLE.waterlevel", "VLISSGN.waterlevel", "SCHEVNGN.waterlevel", "ROOMPBTN.waterlevel"};
		assertEquals(4, observationIds.length);
		for (int i = 0; i < observationIds.length; i++) {
			assertEquals(expectedObsIds[i], observationIds[i]);
		}
		double[][] hk = kgStorageIn.getHk();
		assertNotNull(hk);
		assertEquals(4, hk.length);
		assertEquals(4, hk[0].length);
		double[][] expectedHK = {{0.19371548763628316, 0.21185091852367516, 0.23657124885992412, 0.21748081376956335} , {0.21185091852367666, 0.23168416850382453, 0.2587187889765871, 0.23784112835038507},{0.2365712488599269, 0.2587187889765884, 0.28890800869895084, 0.26559418838590676}, {0.2174808137695654, 0.23784112835038568, 0.2655941883859062, 0.24416170815767535}};
		for (int i = 0; i < hk.length; i++) {
			double[] hkRow = hk[i];
			for (int j = 0; j < hkRow.length; j++) {
				assertEquals(expectedHK[i][j], hkRow[j]);
			}
		}
	}

	private boolean checkKalmanGainContents(double timeAsMJD, KalmanGainStorage kgStorageIn, IVector[] kalmanGainColumns, String[] observationIds) {
		String[] expectedObservationIds = {"WICK.waterlevel", "VLISSGN.waterlevel", "DENHDR.waterlevel", "NORTHSS.waterlevel", "DOVR.waterlevel", "SHEERNS.waterlevel", "HOEKVHLD.waterlevel", "LOWST.waterlevel"};
		for (int i = 0; i < expectedObservationIds.length; i++) {
			assertEquals(expectedObservationIds[i], observationIds[i]);
		}

		assertEquals(timeAsMJD, kgStorageIn.getTimeStampAsMjd());

		assertEquals(8, kalmanGainColumns.length);
		IVector firstKalmanGainColumn = kalmanGainColumns[0];
		assertEquals(437, firstKalmanGainColumn.getSize());
		assertTrue(firstKalmanGainColumn instanceof TreeVector);
		TreeVector firstTreeVector = (TreeVector) firstKalmanGainColumn;
		String id = firstTreeVector.getId();
		assertEquals("state", id);
		ArrayList<String> subTreeVectorIds = firstTreeVector.getSubTreeVectorIds();
		assertEquals(2, subTreeVectorIds.size());

		assertEquals("state", subTreeVectorIds.get(0));
		TreeVector stateTreeVector = (TreeVector) firstTreeVector.getSubTreeVector("state");
		ArrayList<String> stateSubTreeVectorIds = stateTreeVector.getSubTreeVectorIds();
		assertEquals(1, stateSubTreeVectorIds.size());
		assertEquals("2DNoise", stateSubTreeVectorIds.get(0));

		double[] values = firstKalmanGainColumn.getValues();
		double delta = 0.000001;
		assertEquals(-0.115087, values[0], delta);
		assertEquals(0.101872, values[15], delta);
		assertEquals(0.111053, values[16], delta);
		assertEquals(0.118487, values[17], delta);
		assertEquals(0.146230, values[26], delta);
		assertEquals(-0.078988, values[27], delta);
		assertEquals(-0.076138, values[28], delta);
		assertEquals(0.109289, values[431], delta);

		assertEquals("s1", subTreeVectorIds.get(1));
		TreeVector s1TreeVector = (TreeVector) firstTreeVector.getSubTreeVector("s1");
		ArrayList<String> s1SubTreeVectorIds = s1TreeVector.getSubTreeVectorIds();
		assertTrue(s1SubTreeVectorIds.isEmpty());

		assertEquals(-0.049176, values[432], delta);
		assertEquals(-0.018843, values[433], delta);
		assertEquals(-0.052612, values[434], delta);
		assertEquals(0.015112, values[435], delta);
		assertEquals(-0.038791, values[436], delta);

		double[][] hk = kgStorageIn.getHk();

		assertEquals(8, hk.length);
		assertEquals(8, hk[0].length);
		for (int i = 0; i < hk.length; i++) {
			for (int j = 0; j < hk[0].length; j++) {
				assertEquals(i * 10 + j, hk[i][j], 0.000001);
			}
		}
		return true;
	}

	public void testReadWriteKalmanGainNetcdfCFDoubleState() {

		double timeAsMJD = 60310;

		KalmanGainStorage kgStorageIn = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageIn.setColumnFileType(KalmanGainStorage.StorageType.netcdf_cf);
		kgStorageIn.setKalmanGainStorageFileName("KalmanGainStorage.nc");
		kgStorageIn.readKalmanGain();

		IVector[] kalmanGainColumnsIn = kgStorageIn.getKalmanGainColumns();
		assertEquals(8, kalmanGainColumnsIn.length);

		String[] observationIds = kgStorageIn.getObservationIds();
		assert(checkKalmanGainContentsDoubleState(timeAsMJD, kgStorageIn, kalmanGainColumnsIn, observationIds));

		KalmanGainStorage kgStorageOut = new KalmanGainStorage(testRunDataDir, timeAsMJD);
		kgStorageOut.setColumnFileType(KalmanGainStorage.StorageType.netcdf_cf);
		kgStorageOut.setKalmanGainStorageFileName("KalmanGainStorage.nc");
		kgStorageOut.writeKalmanGain(observationIds, kgStorageIn.getObservationOffsetInDays(), kalmanGainColumnsIn, null);
		kgStorageOut.readKalmanGain();

	    assert(checkKalmanGainContentsDoubleState(timeAsMJD, kgStorageOut, kgStorageOut.getKalmanGainColumns(), kgStorageOut.getObservationIds()));
	}

	private boolean checkKalmanGainContentsDoubleState(double timeAsMJD, KalmanGainStorage kgStorageIn, IVector[] kalmanGainColumns, String[] observationIds) {
		String[] expectedObservationIds = {"WICK.waterlevel", "VLISSGN.waterlevel", "DENHDR.waterlevel", "NORTHSS.waterlevel", "DOVR.waterlevel", "SHEERNS.waterlevel", "HOEKVHLD.waterlevel", "LOWST.waterlevel"};
		for (int i = 0; i < expectedObservationIds.length; i++) {
			assertEquals(expectedObservationIds[i], observationIds[i]);
		}

		assertEquals(timeAsMJD, kgStorageIn.getTimeStampAsMjd());

		assertEquals(8, kalmanGainColumns.length);
		IVector firstKalmanGainColumn = kalmanGainColumns[0];
		assertEquals(35, firstKalmanGainColumn.getSize());
		assertTrue(firstKalmanGainColumn instanceof TreeVector);
		TreeVector firstTreeVector = (TreeVector) firstKalmanGainColumn;
		String id = firstTreeVector.getId();
		assertEquals("state", id);
		ArrayList<String> subTreeVectorIds = firstTreeVector.getSubTreeVectorIds();
		assertEquals(3, subTreeVectorIds.size());

		assertEquals("state", subTreeVectorIds.get(0));
		TreeVector stateTreeVector = (TreeVector) firstTreeVector.getSubTreeVector("state");
		ArrayList<String> stateSubTreeVectorIds = stateTreeVector.getSubTreeVectorIds();
		assertEquals(1, stateSubTreeVectorIds.size());
		assertEquals("NoiseX", stateSubTreeVectorIds.get(0));

		double[] firstStateValues = stateTreeVector.getValues();
		double delta = 0.000001;
		assertEquals(0, firstStateValues[0], delta);
		assertEquals(1, firstStateValues[1], delta);
		assertEquals(2, firstStateValues[2], delta);
		assertEquals(13, firstStateValues[13], delta);
		assertEquals(14, firstStateValues[14], delta);

		ITreeVector secondStateTreeVector = firstTreeVector.getSubTreeVector(1);
		assertEquals("state", secondStateTreeVector.getId());
		ArrayList<String> secondStateSubTreeVectorIds = secondStateTreeVector.getSubTreeVectorIds();
		assertEquals(1, secondStateSubTreeVectorIds.size());
		assertEquals("NoiseY", secondStateSubTreeVectorIds.get(0));

		double[] secondStateValues = secondStateTreeVector.getValues();
		assertEquals(120    , secondStateValues[0], delta);
		assertEquals(120 - 1, secondStateValues[1], delta);
		assertEquals(120 - 2, secondStateValues[2], delta);
		assertEquals(120 - 13, secondStateValues[13], delta);
		assertEquals(120 - 14, secondStateValues[14], delta);

		ITreeVector s1TreeVector = firstTreeVector.getSubTreeVector(2);
		ArrayList<String> s1SubTreeVectorIds = s1TreeVector.getSubTreeVectorIds();
		assertTrue(s1SubTreeVectorIds.isEmpty());

		double[] s1Values = s1TreeVector.getValues();

		assertEquals(-0.049176, s1Values[0], delta);
		assertEquals(-0.018843, s1Values[1], delta);
		assertEquals(-0.052612, s1Values[2], delta);
		assertEquals(0.015112, s1Values[3], delta);
		assertEquals(-0.038791, s1Values[4], delta);
		return true;
	}
}
