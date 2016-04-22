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

		long [] positionAndSize; // positionAndSize[0]: positionAndSize[1]: size
		int valueSize = 4;
		positionAndSize = getPositionAndSize(varName, valueSize);

		try {
			byte[] allBytes = new byte[(int) positionAndSize[1]];
			int index = 0;
			for (int j = 0; j < values.length; j++){
				float value = (float) values[j];
				byte[] floatAsBytes = float2ByteArray(value);
				for (int i = 0; i < valueSize; i++) {
					allBytes[index] = floatAsBytes[i];
					index++;
				}
			}
            randomAccessFile.seek(positionAndSize[0]); // I assumed that the position is in bytes
			randomAccessFile.write(allBytes);
		} catch (IOException e) {
			throw new RuntimeException("Could not jump through binary restart file " + binRestartFilePath.getAbsolutePath());
		}
	}

	public float[] read(String varName) {

		long [] positionAndSize; // positionAndSize[0]: positionAndSize[1]: size
		int valueSize = 4;
		positionAndSize = getPositionAndSize(varName, valueSize);


		float[] values = new float[(int)(positionAndSize[1]/valueSize)];
		try {
			// randomAccessFile.seek(0); // I assumed that the position is in bytes
			long recLen = randomAccessFile.readShort();
			byte[] allBytes = new byte[(int) positionAndSize[1]];
			randomAccessFile.seek(positionAndSize[0]); // I assumed that the position is in bytes
			randomAccessFile.read(allBytes);
			int byteIndex = 0;
			for (int j = 0; j < values.length; j++){
				byte[] floatAsBytes = new byte[valueSize];
				for (int i = 0; i < valueSize; i++) {
					floatAsBytes[i] = allBytes[byteIndex];
					byteIndex++;
				}
				values[j] = toFloat(floatAsBytes);
			}
		} catch (IOException e) {
			throw new RuntimeException("Could not jump through binary restart file " + binRestartFilePath.getAbsolutePath());
		}
		return values;
	}

	public void close(){
		try {
			randomAccessFile.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not close binary restart file " + binRestartFilePath.getAbsolutePath());
		}
	}

	private long[] getPositionAndSize(String varName, int valueSize) {
		// S1: 0
		// U1: mMax*nMax (size of S1) [+ record divider]
		// V1: mMax*nMax + mMax*nMax*nLay (size of S1 plus size of U1) [+ 2 * record divider]
		// R1: mMax*nMax + 2*mMax*nMax*nLay (size of S1 plus twice size of U1/V1) [+ 3 * record divider]

		long[] positionAndSize = new long[2];

		int recLenIndicatorSize = 4;
		int recordDividerSize = 0;

		if (varName.equals("S1")){
			positionAndSize[0] = recLenIndicatorSize; // after first rec length
			positionAndSize[1] = mMax*nMax*valueSize;
		}else if (varName.equals("U1")){
			positionAndSize[0] = 2 * recLenIndicatorSize + recordDividerSize + mMax*nMax*valueSize;
			positionAndSize[1] = mMax*nMax*nLay*valueSize;
		}else if (varName.equals("V1")){
			positionAndSize[0] = 3 * recLenIndicatorSize + 2 * recordDividerSize + mMax*nMax*nLay*valueSize;
			positionAndSize[1] = mMax*nMax*nLay*valueSize;
		}else if (varName.equals("R1")){
			positionAndSize[0] = 4 * recLenIndicatorSize + 3 * recordDividerSize + 2*mMax*nMax*nLay*valueSize;
			positionAndSize[1] = mMax*nMax*nLay*nSubstances*valueSize;
		}else{
			throw new RuntimeException("Only S1, U1 V1 and R1 are supported for writing into the binary restart file");
		}
		return positionAndSize;
	}

	private static byte[] float2ByteArray(float value) {
//		byte[] bytes = new byte[4];
//		ByteBuffer.wrap(bytes).putFloat(value);
//		return bytes;
		byte[] bytes = ByteBuffer.allocate(4).putFloat(value).array();
		return swapBytes(bytes);
	}

	private static float toFloat(byte[] bytes) {
		byte[] swappedBytes = swapBytes(bytes);
		return ByteBuffer.wrap(swappedBytes).getFloat();
	}

	private static byte[] swapBytes(byte[] bytes) {
		int length = bytes.length;
		byte[] swappedBytes = new byte[length];
		for (int i = 0; i < length; i++) {
			swappedBytes[i] = bytes[length-i-1];
		}
		return swappedBytes;
	}

}
