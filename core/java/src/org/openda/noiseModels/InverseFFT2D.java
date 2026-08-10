package org.openda.noiseModels;

import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.TransformType;

public class InverseFFT2D {

	private InverseFFT2D() {}

	/**
	 * Computes the inverse 2D FFT using row-column decomposition.
	 * Input dimensions must be powers of 2.
	 *
	 * @param complexes  2D complex frequency-domain array [rows][cols]
	 * @return   2D complex spatial-domain array
	 */
	public static Complex[][] ifft2(Complex[][] complexes) {
		int m = complexes.length;
		int n = complexes[0].length;
		FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
		Complex[][] result = new Complex[m][n];

		// Step 1: inverse FFT along each row
		for (int i = 0; i < m; i++) {
			result[i] = fft.transform(complexes[i], TransformType.INVERSE);
		}

		// Step 2: inverse FFT along each column
		for (int j = 0; j < n; j++) {
			Complex[] col = new Complex[m];
			for (int i = 0; i < m; i++) col[i] = result[i][j];
			col = fft.transform(col, TransformType.INVERSE);
			for (int i = 0; i < m; i++) result[i][j] = col[i];
		}

		return result;
	}

	/** Extract real part of IFFT2 result */
	public static double[][] ifft2Real(Complex[][] complexes) {
		Complex[][] spatial = ifft2(complexes);
		int m = spatial.length;
		int n = spatial[0].length;
		double[][] out = new double[m][n];
		for (int i = 0; i < m; i++)
			for (int j = 0; j < n; j++)
				out[i][j] = spatial[i][j].getReal();
		return out;
	}
}
