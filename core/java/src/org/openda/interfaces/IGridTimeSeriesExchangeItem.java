/* OpenDA v2.4.3 
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

package org.openda.interfaces;

/**
 * Interface for an exchangeItem that represents a time series with a grid for each timeStep.
 * The geometry of the grid may be different for each time.
 */
public interface IGridTimeSeriesExchangeItem extends IExchangeItem {

	public IGeometryInfo getGeometryInfoForSingleTimeIndex(int timeIndex);

	public double[] getValuesAsDoublesForSingleTimeIndex(int timeIndex);

	public void setValuesAsDoublesForSingleTimeIndex(int timeIndex, double[] values);

	public void axpyOnValuesForSingleTimeIndex(int timeIndex, double alpha, double[] axpyValues);

	public void multiplyValuesForSingleTimeIndex(int timeIndex, double[] multiplicationFactors);
}
