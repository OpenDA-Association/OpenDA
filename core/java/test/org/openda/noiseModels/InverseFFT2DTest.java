package org.openda.noiseModels;

import junit.framework.TestCase;
import org.apache.commons.math3.complex.Complex;

public class InverseFFT2DTest extends TestCase {

	public void setUp() throws Exception {
		super.setUp();
	}

	private static final double EPS = 1e-12;

	public void testIFFT2DOnlyReal() {
		// 2x2 frequency-domain matrix with only DC component:
		// ifft2 should produce a constant field of F00/(M*N) = 4/(2*2) = 1.
		Complex[][] F = new Complex[][]{
			{new Complex(4.0, 0.0), new Complex(0.0, 0.0)},
			{new Complex(0.0, 0.0), new Complex(0.0, 0.0)}
		};

		Complex[][] spatial = InverseFFT2D.ifft2(F);
		double[][] real = InverseFFT2D.ifft2Real(F);

		for (int i = 0; i < spatial.length; i++) {
			for (int j = 0; j < spatial[0].length; j++) {
				assertEquals(1.0, spatial[i][j].getReal(), EPS);
				assertEquals(1.0, real[i][j], EPS);
				assertEquals(0.0, spatial[i][j].getImaginary(), EPS);
			}
		}
	}

	public void testIFFT2DWithDoubles() {
		// A small Hermitian-symmetric spectrum -> purely real spatial result.
		// Indices: (0,1) and (1,0) are conjugate partners in 2x2.
		Complex[][] F = new Complex[][]{
			{new Complex(2.0, 0.0), new Complex(1.0, 0.0)},
			{new Complex(1.0, 0.0), Complex.ZERO}
		};

		Complex[][] spatialComplex = InverseFFT2D.ifft2(F);
		double[][] spatialReal = InverseFFT2D.ifft2Real(F);

		for (int i = 0; i < spatialComplex.length; i++) {
			for (int j = 0; j < spatialComplex[0].length; j++) {
				assertEquals(spatialComplex[i][j].getReal(), spatialReal[i][j], EPS);
			}
		}
	}

	public void testIFFT2DWithComplex4x4() {
		// 4x4 spectrum with a single complex impulse at (u=1, v=2).
		// For a single-bin spectrum F[u0,v0] = A:
		// f[x,y] = (A/(M*N)) * exp(i*2*pi*(u0*x/M + v0*y/N))
		// Here M=N=4, A=(16+8i), so scale is A/16 = (1+0.5i).
		Complex[][] F = new Complex[4][4];
		for (int i = 0; i < 4; i++) {
			for (int j = 0; j < 4; j++) {
				F[i][j] = Complex.ZERO;
			}
		}
		F[1][2] = new Complex(16.0, 8.0);

		Complex[][] spatial = InverseFFT2D.ifft2(F);
		double[][] real = InverseFFT2D.ifft2Real(F);

		// Compare against closed-form expected values.
		for (int x = 0; x < 4; x++) {
			for (int y = 0; y < 4; y++) {
				double angle = 2.0 * Math.PI * ((1.0 * x / 4.0) + (2.0 * y / 4.0));
				Complex expected = new Complex(1.0, 0.5).multiply(new Complex(Math.cos(angle), Math.sin(angle)));

				assertEquals(expected.getReal(), spatial[x][y].getReal(), EPS);
				assertEquals(expected.getImaginary(), spatial[x][y].getImaginary(), EPS);

				// ifft2Real should match the real part of ifft2
				assertEquals(expected.getReal(), real[x][y], EPS);
			}
		}
	}
}
