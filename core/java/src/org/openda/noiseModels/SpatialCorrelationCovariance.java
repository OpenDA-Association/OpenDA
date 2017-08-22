/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
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
