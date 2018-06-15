/* MOD_V2.0
 * Copyright (c) 2016 OpenDA Association
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

package org.openda.model_delft3d;

/**
 * Simple class containing all information on a 2D Field
 */
public class D3dValuesOnGrid2D {

    private double[] values;
    private final double missingValue = -999.0;
    private D3dGrid2D grid2D;

    public D3dValuesOnGrid2D(D3dGrid2D grid2D) {
        this.grid2D = grid2D;
        this.values = null;
    }

    public D3dValuesOnGrid2D(D3dGrid2D grid2D, double[] values) {
        this(grid2D);
        this.values = values;
    }

    public double[] getValues() {
        return values;
    }

    public void setValues(double[] values) {
        if (!(values.length == this.values.length)) {
            throw new IllegalArgumentException("D3dField2D.setValues: #values (" + values.length +
                    ") != this.values (" + this.values.length + ")");
        }
        this.values = values;
    }

    public D3dGrid2D getGrid() {
        return grid2D;
    }
}
