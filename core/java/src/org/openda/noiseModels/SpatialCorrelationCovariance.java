package org.openda.noiseModels;

import org.openda.utils.Matrix;

/**
 * Created by pelgrim on 08-May-17.
 */
public class SpatialCorrelationCovariance {
	private final Matrix covariance;
	private final Matrix sqrtCovariance;
	private final double determinantCov;

	public SpatialCorrelationCovariance(Matrix covariance, Matrix sqrtCovariance, double determinantCov) {
		this.covariance = covariance;
		this.sqrtCovariance = sqrtCovariance;
		this.determinantCov = determinantCov;
	}

	public Matrix getCovariance() {
		return covariance;
	}

	public Matrix getSqrtCovariance() {
		return sqrtCovariance;
	}

	public double getDeterminantCov() {
		return determinantCov;
	}
}
