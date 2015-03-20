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


package org.openda.utils;

import org.openda.interfaces.IDimensionIndex;

import java.io.Serializable;

/**
 * Dimension indices specification for n-dimensional arrayslice selection
 */
public class DimensionIndex implements IDimensionIndex, Serializable {

    private int start;
    private int end;
    private int size;

    public DimensionIndex(String indexString, int dimensionBase) {
        if (indexString.indexOf(":") >= 0) {
            if (indexString.startsWith(":")) {
                start = Integer.MIN_VALUE; // use lower model index
            } else {
                start = Integer.parseInt(indexString.substring(0, indexString.indexOf(':')));
                start -= dimensionBase;
            }
            if (indexString.endsWith(":")) {
                end = Integer.MAX_VALUE; // use upper model index
            } else {
                end = Integer.parseInt(indexString.substring(indexString.indexOf(':') + 1)) + 1;
                end -= dimensionBase;
            }
        } else {
            start = Integer.parseInt(indexString);
            start -= dimensionBase;
            end = start + 1;
        }
        size = end - start;
    }

    public DimensionIndex(int start, int end) {
        this.start = start;
        this.end = end;
        this.size = end - start;
    }

    public DimensionIndex(int size) {
        this.start = 0;
        this.end = this.size = size;
    }

    public int getStart() {
        return start;
    }

    public int getEnd() {
        return end;
    }

    public int getSize() {
        return size;
    }

    public static int[] getStartIndices(IDimensionIndex[] selectionIndices) {
        int[] startIndexes = new int[selectionIndices.length];
        for (int i = 0; i < selectionIndices.length; i++) {
            if (selectionIndices[i] != null) {
                IDimensionIndex mappingIndex = selectionIndices[i];
                startIndexes[i] = mappingIndex.getStart();
            }
        }
        return startIndexes;
    }

    public static int[] getEndIndices(IDimensionIndex[] selectionIndices) {
        int[] endIndexes = new int[selectionIndices.length];
        for (int i = 0; i < selectionIndices.length; i++) {
            if (selectionIndices[i] != null) {
                IDimensionIndex mappingIndex = selectionIndices[i];
                endIndexes[i] = mappingIndex.getEnd();
            }
        }
        return endIndexes;
    }

    public static void checkDimensions(int[] sizes, int[] indices, String exchangeItemID) {
        for (int dim = 0; dim < indices.length; dim++) {
            int index = indices[dim];
            if (index == Integer.MIN_VALUE) index = 0;
            if (index == Integer.MAX_VALUE) index = sizes[dim];
            if (index > sizes[dim]) {
                throw new IllegalArgumentException(
                        "(start or end) Index/Indices out of bound for \"" + exchangeItemID + "\" (dimension " + (dim + 1));
            }
        }
    }

    public static double[] accessSubArray(String exchangeItemID, double[] orgValues, double[] newValues, IDimensionIndex[] selectionIndices, int[] dimSizes) {
        int[] startIndices = DimensionIndex.getStartIndices(selectionIndices);
        int[] endIndices = DimensionIndex.getEndIndices(selectionIndices);
        DimensionIndex.checkDimensions(dimSizes, startIndices, exchangeItemID);
        DimensionIndex.checkDimensions(dimSizes, endIndices, exchangeItemID);
        if (newValues == null) {
            return DimensionIndex.getSubArray(orgValues, dimSizes, startIndices, endIndices);
        } else {
            DimensionIndex.setSubArray(orgValues, newValues, dimSizes, startIndices, endIndices);
            return orgValues;
        }
    }

    public static double[] getSubArray(double[] sourceValues, int[] sourceDimensions, int[] startIndices, int[] endIndices) {

        double[] subValues;
        final int Idir = 0;
        final int Jdir = 1;
        final int Kdir = 2;
        if (sourceDimensions.length == 1) {

            int startIndexI = startIndices[Idir];
            int endIndexI = endIndices[Idir];
            if (startIndexI == Integer.MIN_VALUE) startIndexI = 0;
            if (endIndexI == Integer.MAX_VALUE) endIndexI = sourceDimensions[Idir];

            subValues = new double[endIndexI - startIndexI];

            System.arraycopy(sourceValues, startIndexI, subValues, 0, endIndexI - startIndexI);

        } else if (sourceDimensions.length == 2) {

            int startIndexI = startIndices[Idir];
            int endIndexI = endIndices[Idir];
            if (startIndexI == Integer.MIN_VALUE) startIndexI = 0;
            if (endIndexI == Integer.MAX_VALUE) endIndexI = sourceDimensions[Idir];

            int startIndexJ = startIndices[Jdir];
            int endIndexJ = endIndices[Jdir];
            if (startIndexJ == Integer.MIN_VALUE) startIndexJ = 0;
            if (endIndexJ == Integer.MAX_VALUE) endIndexJ = sourceDimensions[Jdir];

            subValues = new double[(endIndexI - startIndexI) * (endIndexJ - startIndexJ)];

            int subValuesIndex = 0;
            for (int j = startIndexJ; j < endIndexJ; j++) {
                for (int i = startIndexI; i < endIndexI; i++) {
                    int sourceIndex = i * sourceDimensions[Jdir] + j;
                    if (sourceIndex >= sourceValues.length) {
                        throw new RuntimeException("Source index [" + sourceIndex + "] out of bound (max:" + sourceValues.length + ")");
                    }
                    if (subValuesIndex >= subValues.length) {
                        throw new RuntimeException("subValues index [" + subValuesIndex + "] out of bound (max:" + subValues.length + ")");
                    }
                    subValues[subValuesIndex++] = sourceValues[sourceIndex];
                }
            }

        } else if (sourceDimensions.length == 3) {

            int startIndexI = startIndices[Idir];
            int endIndexI = endIndices[Idir];
            if (startIndexI == Integer.MIN_VALUE) startIndexI = 0;
            if (endIndexI == Integer.MAX_VALUE) endIndexI = sourceDimensions[Idir];

            int startIndexJ = startIndices[Jdir];
            int endIndexJ = endIndices[Jdir];
            if (startIndexJ == Integer.MIN_VALUE) startIndexJ = 0;
            if (endIndexJ == Integer.MAX_VALUE) endIndexJ = sourceDimensions[Jdir];

            int startIndexK = startIndices[Kdir];
            int endIndexK = endIndices[Kdir];
            if (startIndexK == Integer.MIN_VALUE) startIndexK = 0;
            if (endIndexK == Integer.MAX_VALUE) endIndexK = sourceDimensions[Kdir];

            subValues = new double[(endIndexI - startIndexI) * (endIndexJ - startIndexJ) * (endIndexK - startIndexK)];

            int subValuesIndex = 0;
            for (int k = startIndexK; k < endIndexK; k++) {
                for (int j = startIndexJ; j < endIndexJ; j++) {
                    for (int i = startIndexI; i < endIndexI; i++) {
                        int sourceIndex = (i * sourceDimensions[Jdir] + j) * sourceDimensions[Kdir] + k;
                        if (sourceIndex >= sourceValues.length) {
                            throw new RuntimeException("Source index [" + sourceIndex + "] out of bound (max:" + sourceValues.length + ")");
                        }
                        if (subValuesIndex >= subValues.length) {
                            throw new RuntimeException("subValues index [" + subValuesIndex + "] out of bound (max:" + subValues.length + ")");
                        }
                        subValues[subValuesIndex++] = sourceValues[sourceIndex];
                    }
                }
            }

        } else {
            throw new UnsupportedOperationException("org.openda.blackbox.config.DimensionIndex.getSubArray(): more then 3 dimensions.");
        }

        return subValues;
    }

    public static void setSubArray(double[] orgValues, double[] newValues, int[] orgValuesDimensions, int[] startIndices, int[] endIndices) {

        final int Idir = 0;
        final int Jdir = 1;
        final int Kdir = 2;
        if (orgValuesDimensions.length == 1) {

            int startIndexI = startIndices[Idir];
            int endIndexI = endIndices[Idir];
            if (startIndexI == Integer.MIN_VALUE) startIndexI = 0;
            if (endIndexI == Integer.MAX_VALUE) endIndexI = orgValuesDimensions[Idir];

            System.arraycopy(newValues, 0, orgValues, startIndexI, endIndexI - startIndexI);

        } else if (orgValuesDimensions.length == 2) {

            int startIndexI = startIndices[Idir];
            int endIndexI = endIndices[Idir];
            if (startIndexI == Integer.MIN_VALUE) startIndexI = 0;
            if (endIndexI == Integer.MAX_VALUE) endIndexI = orgValuesDimensions[Idir];

            int startIndexJ = startIndices[Jdir];
            int endIndexJ = endIndices[Jdir];
            if (startIndexJ == Integer.MIN_VALUE) startIndexJ = 0;
            if (endIndexJ == Integer.MAX_VALUE) endIndexJ = orgValuesDimensions[Jdir];

            int subValuesIndex = 0;
            for (int j = startIndexJ; j < endIndexJ; j++) {
                for (int i = startIndexI; i < endIndexI; i++) {
                    int sourceIndex = i * orgValuesDimensions[Jdir] + j;
                    orgValues[sourceIndex] = newValues[subValuesIndex++];
                }
            }
        } else {

            int startIndexI = startIndices[Idir];
            int endIndexI = endIndices[Idir];
            if (startIndexI == Integer.MIN_VALUE) startIndexI = 0;
            if (endIndexI == Integer.MAX_VALUE) endIndexI = orgValuesDimensions[Idir];

            int startIndexJ = startIndices[Jdir];
            int endIndexJ = endIndices[Jdir];
            if (startIndexJ == Integer.MIN_VALUE) startIndexJ = 0;
            if (endIndexJ == Integer.MAX_VALUE) endIndexJ = orgValuesDimensions[Jdir];

            int startIndexK = startIndices[Kdir];
            int endIndexK = endIndices[Kdir];
            if (startIndexK == Integer.MIN_VALUE) startIndexK = 0;
            if (endIndexK == Integer.MAX_VALUE) endIndexK = orgValuesDimensions[Kdir];

            for (int k = startIndexK; k < endIndexK; k++) {
                for (int j = startIndexJ; j < endIndexJ; j++) {
                    for (int i = startIndexI; i < endIndexI; i++) {
                    }
                }
            }
            throw new UnsupportedOperationException("org.openda.blackbox.config.DimensionIndex.getSubArray(): more then 3 dimensionst.");
        }
    }

    public static int getSelectionSize(IDimensionIndex[] indices, int[] orgValuesDimensions) {
        int size = 1;
        for (int dim = 0; dim < indices.length; dim++) {
            int startIndex = indices[dim].getStart();
            int endIndex = indices[dim].getEnd();
            if (startIndex == Integer.MIN_VALUE) startIndex = 0;
            if (endIndex == Integer.MAX_VALUE) endIndex = orgValuesDimensions[dim];
            size *= endIndex - startIndex;
        }
        return size;
    }
}
