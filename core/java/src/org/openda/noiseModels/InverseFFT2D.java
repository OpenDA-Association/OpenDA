package org.openda.noiseModels;

import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.TransformType;

public class InverseFFT2D {

	/**
	 * Computes the inverse 2D FFT using row-column decomposition.
	 * Input dimensions must be powers of 2.
	 *
	 * @param F  2D complex frequency-domain array [rows][cols]
	 * @return   2D complex spatial-domain array
	 */
	public static Complex[][] ifft2(Complex[][] F) {
		int M = F.length;
		int N = F[0].length;
		FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
		Complex[][] result = new Complex[M][N];

		// Step 1: inverse FFT along each row
		for (int i = 0; i < M; i++) {
			result[i] = fft.transform(F[i], TransformType.INVERSE);
		}

		// Step 2: inverse FFT along each column
		for (int j = 0; j < N; j++) {
			Complex[] col = new Complex[M];
			for (int i = 0; i < M; i++) col[i] = result[i][j];
			col = fft.transform(col, TransformType.INVERSE);
			for (int i = 0; i < M; i++) result[i][j] = col[i];
		}

		return result;
	}

	/** Extract real part of IFFT2 result */
	public static double[][] ifft2Real(Complex[][] F) {
		Complex[][] spatial = ifft2(F);
		int M = spatial.length, N = spatial[0].length;
		double[][] out = new double[M][N];
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
				out[i][j] = spatial[i][j].getReal();
		return out;
	}
}
