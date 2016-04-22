package org.openda.model_delft3d;

import ucar.ma2.Array;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

import java.nio.ByteBuffer;

/**
 * Created by hummel on 20-Apr-16.
 */
public class D3DBinRestartFile {

	private final int mMax;
	private final int nMax;
	private final int nLay;
	private final int nSubstances;
	private File binRestartFilePath;
	private RandomAccessFile randomAccessFile;

	D3DBinRestartFile(File binRestartFilePath, int mMax, int nMax, int nLay, int nSubstances) {
		this.binRestartFilePath = binRestartFilePath;
		this.mMax = mMax;
		this.nMax = nMax;
		this.nLay = nLay;
		this.nSubstances = nSubstances;
	}

	public void open(){
		try {
			randomAccessFile = new RandomAccessFile(binRestartFilePath, "rw");
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Could not find binary restart file " + binRestartFilePath.getAbsolutePath());
		}
	}

	public void write(String varName, double[] values){

		long position = 0;
		// S1: 0
		// U1: mMax*nMax (size of S1) [+ record divider]
		// V1: mMax*nMax + mMax*nMax*nLay (size of S1 plus size of U1) [+ 2 * record divider]
		// R1: mMax*nMax + 2*mMax*nMax*nLay (size of S1 plus twice size of U1/V1) [+ 3 * record divider]

		int recLenIndicatorSize = 4;
		int recordDividerSize = 4;
		int valueSize = 4; // float
		int totalSize = 0;
		if (varName.equals("S1")){
			position = recLenIndicatorSize; // after first rec length
			totalSize = mMax*nMax*valueSize;
		}else if (varName.equals("U1")){
			position = 2 * recLenIndicatorSize + recordDividerSize + mMax*nMax*valueSize;
			totalSize = mMax*nMax*nLay*valueSize;
		}else if (varName.equals("V1")){
			position = 3 * recLenIndicatorSize + 2 * recordDividerSize + mMax*nMax*nLay*valueSize;
			totalSize = mMax*nMax*nLay*valueSize;
		}else if (varName.equals("R1")){
			position = 4 * recLenIndicatorSize + 3 * recordDividerSize + 2*mMax*nMax*nLay*valueSize;
			totalSize = mMax*nMax*nLay*nSubstances*valueSize;
		}else{
			throw new RuntimeException("Only S1, U1 V1 and R1 are supported for writing into the binary restart file");
		}

		try {
			// There are two solutions here, one per byte and one per block (if that is possible)
//			randomAccessFile.seek(position); // I assumed that the position is in bytes
			byte[] allBytes = new byte[totalSize];
			int index = 0;
			for (int j = 0; j < values.length; j++){
				float value = (float) values[j];
				byte[] floatAsBytes = float2ByteArray(value);
				for (int i = 0; i < valueSize; i++) {
					allBytes[index] = floatAsBytes[i];
//					byte oneByte = floatAsBytes[i];
//					randomAccessFile.seek(position+index); // I assumed that the position is in bytes
//					randomAccessFile.write(oneByte);
					index++;
				}
			}
            randomAccessFile.seek(position); // I assumed that the position is in bytes
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

	private static byte[] float2ByteArray(float value) {
//		byte[] bytes = new byte[4];
//		ByteBuffer.wrap(bytes).putFloat(value);
//		return bytes;
		return ByteBuffer.allocate(4).putFloat(value).array();
	}

	private static double toDouble(byte[] bytes) {
		return ByteBuffer.wrap(bytes).getDouble();
	}

	public double[] read(String varName) {
		throw new RuntimeException("org.openda.model_delft3d.D3DBinRestartFile.read() not implemented yet");
	}
}
