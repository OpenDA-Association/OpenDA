package org.openda.model_delft3d;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

import java.nio.ByteBuffer;

/**
 * Created by hummel on 20-Apr-16.
 */
public class D3DBinRestartFile {

	private File binRestartFilePath;
	private RandomAccessFile randomAccessFile;

	D3DBinRestartFile(File binRestartFilePath, int mMax, int nMax, int nLay, int nSubstances) {
		this.binRestartFilePath = binRestartFilePath;
	}

	public void open(){
		try {
			randomAccessFile = new RandomAccessFile(binRestartFilePath, "rw");
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Could not find binary restart file " + binRestartFilePath.getAbsolutePath());
		}
	}

	public void write(String varName, double[] values){
		try {
			long position = 0; // S1: 0
			                   // U1: mMax*nMax (size of S1) [+ record divider]
			                   // V1: mMax*nMax + mMax*nMax*nLay (size of S1 plus size of U1) [+ 2 * record divider]
							   // R1: mMax*nMax + 2*mMax*nMax*nLay (size of S1 plus twice size of U1/V1) [+ 3 * record divider]
			randomAccessFile.seek(position);
			int totalSize = 0; // TODO: mMax*nMax or mMax*nMax*nLay
			byte[] allBytes = new byte[totalSize];
			int index = 0;
			// TODO: loop over all doubles values. in that loop:
			double value = 0d;
			byte[] doubleAsBytes = toByteArray(value);
			for (int i = 0; i < 8; i++) {
				allBytes[index]=doubleAsBytes[i];
				index++;
			}
			randomAccessFile.write(allBytes);
		} catch (IOException e) {
			throw new RuntimeException("Could not jump through binary restart file " + binRestartFilePath.getAbsolutePath());
		}
	}

	public void close(){
		try {
			randomAccessFile.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not close binary restart file " + binRestartFilePath.getAbsolutePath());
		}
	}

	private static byte[] toByteArray(double value) {
		byte[] bytes = new byte[8];
		ByteBuffer.wrap(bytes).putDouble(value);
		return bytes;
	}

	private static double toDouble(byte[] bytes) {
		return ByteBuffer.wrap(bytes).getDouble();
	}
}
