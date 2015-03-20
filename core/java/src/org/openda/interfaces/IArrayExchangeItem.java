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


package org.openda.interfaces;

/**
 * General ExchangeItem for storing array based data.
 *
 * Example 1: 2D cartesian grid
 *
 *   time      = [55880.0, 55880.1, ..., 55881.0]   (Nov 15 2011)
 *     timeIndex = [0]
 *   latitude  = [-180, -179, ..., 178, 179, 180]   (WGS84 degrees east)
 *     latitudeIndex = [1]
 *   longitude = [-90, -89, ... ,89 ,90]
 *     longitudeIndex = [2]
 *   height = null
 *     heightIndex = []
 *
 *   values is a 3D array with indices[time, latitude, longitude]
 *   which corresponds to values[time][y][x] in c or java and
 *   to values[x,y,time] in fortran.
 *
 *  Example 1b: 2D structured grid
 *
 *   time      = [55880.0, 55880.1, ..., 55881.0]   (Nov 15 2011)
 *     timeIndex = [0]
 *   latitude[y][x]  = 2D array with latitudes   (WGS84 degrees east)
 *     latitudeIndex = [1,2] horizontal grid indexing along grid
 *   longitude[y][x] = 2D array with longitudes
 *     longitudeIndex = [1,2]
 *   height = null
 *     heightIndex = []
 *   values[time][y][x] 3D-array
 *
 *  Example 2: several time-series with common times
 *
 *   time      = [55880.0, 55880.1, ..., 55881.0]   (Nov 15 2011)
 *     timeIndex = [0]
 *   latitude  = [xSeries1, xSeries2, ...] 1D array with latitudes   (WGS84 degrees east)
 *     latitudeIndex = [1]
 *   longitude = [ySeries1, ySeries2, ...] 1D array with longitudes
 *     longitudeIndex = [1]
 *   values[time][position] 2D array values
 *     
 *  Example 3: 3D structured grid
 *
 *   time      = [55880.0, 55880.1, ..., 55881.0]   (Nov 15 2011)
 *     timeIndex = [0]
 *   latitude  = 2D array with latitudes   (WGS84 degrees east)
 *     latitudeIndex = [1,2]
 *   longitude = 2D array with longitudes
 *     longitudeIndex = [1,2]
 *   height[z] = vertical coordinate for fixed z-layers
 *   or height[z][y][x] for spatially varying layer-heights
 *   or height[t][z][y][x] if the vertical changes in time as well
 *     heightIndex = [3] for z-layers and [1,2,3] if layer-height changes with horizontal position 
 *     (eg for sigma-layers) and [0,1,2,3] for layers that change in time and space
 *   values[t][z][y][x] = 4D array 
 *    
 *   Many other gridded data-types can be cast in this framework. Additional information is needed
 *   if multiple grids are needed to cover the dataset or when the grid is unstructured.
 *   
 *   NOTE: The timeInfo and geometry should be of type IArrayTimeInfo and IArrayGeometryInfo for this type. 
 */

public interface IArrayExchangeItem extends IExchangeItem{

    public IArray getArray();

    public void setArray(IArray array);

 }
