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

import java.util.ArrayList;

/**
 * Simple class containing all information on a 2D Field
 */
public class D3dField2D {

    protected double[] values;
    private int mmax;
    private int nmax;
	protected final double missingValue = -999.0;

    public D3dField2D(int mmax, int nmax) {
        this.mmax = mmax;
        this.nmax = nmax;
        this.values = null;
    }

    public D3dField2D(int mmax, int nmax, double[] values) {
        this(mmax,nmax);
        this.values = values;
    }

    public void setValues(ArrayList<MnPoint> mnPoints, double[] values) {
        if (!(values.length == mnPoints.size())) {
            throw new IllegalArgumentException("D3dField2D.setValues: #values (" + values.length +
                    ") != #M,N-points (" + mnPoints.size() + ")");
        }
        for (int i = 0; i < mnPoints.size(); i++) {
            setValue(mnPoints.get(i), values[i]);
        }
    }

    public void setValue(MnPoint mnPoint, double value) {
        setValue(mnPoint.getM(), mnPoint.getN(), value);
    }

    public void setValue(int m, int n, double value) {
        if (m<1 || n<1 || m>mmax || n>nmax) {
            throw new IllegalArgumentException("D3dField2D.setValue(" + m + "," + n + ",...): " +
                    "expected 0<m<=" + mmax + "0<n<=" + nmax);
        }
        int index = (n - 1) * mmax + m - 1;
        if (Double.compare(values[index], missingValue) != 0) {
            values[index] = value;
        }
    }

    public double[] getValues() {
        return values;
    }

    public int getMmax() {
        return mmax;
    }

    public int getNmax() {
        return nmax;
    }

    public double[] getValues(ArrayList<MnPoint> mnPoints) {
        double[] values = new double[mnPoints.size()];
        for (int i = 0; i < mnPoints.size(); i++) {
            values[i] = getValue(mnPoints.get(i));
        }
        return values;
    }

    public double getValue(MnPoint mnPoint) {
        return getValue(mnPoint.getM(), mnPoint.getN());
    }

    private double getValue(int m, int n) {
        return values[(n-1)*mmax + m-1];
    }

    public void setValues(double[] values) {
        if (!(values.length == this.values.length)) {
            throw new IllegalArgumentException("D3dField2D.setValues: #values (" + values.length +
                    ") != this.values (" + this.values.length + ")");
        }
        this.values = values;
    }
}
