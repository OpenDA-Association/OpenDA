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

package org.openda.exchange.dataobjects;

import java.util.ArrayList;
import java.util.List;

import ucar.nc2.Dimension;

/**
 * Class to store the properties of the grid dimensions and variables
 * in a netcdf file that correspond to a given IGeometryInfo instance in OpenDA.
 */
public class GridVariableProperties {
    private List<Dimension> dimensions = new ArrayList<Dimension>();
    private String zVariableName = null;
    private String y1DVariableName = null;
    private String x1DVariableName = null;
    private String row1DVariableName = null;
    private String column1DVariableName = null;
    private String trueLatitude2DVariableName = null;
    private String trueLongitude2DVariableName = null;
    private String gridMappingVariableName = null;

    public GridVariableProperties() {
    }

    public List<Dimension> getDimensions() {
        return this.dimensions;
    }

    public void setDimensions(List<Dimension> dimensions) {
        if (dimensions == null) {
            throw new IllegalArgumentException("dimensions is null.");
        }

        this.dimensions = dimensions;
    }

    public String getZVariableName() {
        return this.zVariableName;
    }

    public void setZVariableName(String zVariableName) {
        this.zVariableName = zVariableName;
    }

    public String getY1DVariableName() {
        return this.y1DVariableName;
    }

    public void setY1DVariableName(String y1DVariableName) {
        this.y1DVariableName = y1DVariableName;
    }

    public String getX1DVariableName() {
        return this.x1DVariableName;
    }

    public void setX1DVariableName(String x1DVariableName) {
        this.x1DVariableName = x1DVariableName;
    }

    public String getRow1DVariableName() {
        return this.row1DVariableName;
    }

    public void setRow1DVariableName(String row1DVariableName) {
        this.row1DVariableName = row1DVariableName;
    }

    public String getColumn1DVariableName() {
        return this.column1DVariableName;
    }

    public void setColumn1DVariableName(String column1DVariableName) {
        this.column1DVariableName = column1DVariableName;
    }

    public String getTrueLatitude2DVariableName() {
        return this.trueLatitude2DVariableName;
    }

    public void setTrueLatitude2DVariableName(String trueLatitude2DVariableName) {
        this.trueLatitude2DVariableName = trueLatitude2DVariableName;
    }

    public String getTrueLongitude2DVariableName() {
        return this.trueLongitude2DVariableName;
    }

    public void setTrueLongitude2DVariableName(String trueLongitude2DVariableName) {
        this.trueLongitude2DVariableName = trueLongitude2DVariableName;
    }

    public String getGridMappingVariableName() {
        return this.gridMappingVariableName;
    }

    public void setGridMappingVariableName(String gridMappingVariableName) {
        this.gridMappingVariableName = gridMappingVariableName;
    }
}
