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

import org.openda.interfaces.IArray;
import org.openda.interfaces.IVector;
import org.openda.interfaces.ProposedIVector;

import java.io.*;
import java.util.List;

/**
 * Vector
 */
public class ProposedVector implements ProposedIVector, Externalizable{

    /* use Array for storage
    */
	private IArray array=null;
    public int maxFullExpandLength = 20;


    // simple constructor for derived simple-tree-vector
    public ProposedVector() {
    	this.array = new Array(0);
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param n Length of the vector
     */
    public ProposedVector(int n) {
        this.array = new Array(n);
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param values content of the vector as array of doubles
     */
    public ProposedVector(double[] values) {
        int n = values.length;
        this.array = new Array(n);
        this.array.setValuesAsDoubles(values);
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param values content of the vector as array of ints
     */
    public ProposedVector(int[] values) {
        int n = values.length;
        this.array = new Array(n);
        double[] valuesAsDoubles = new double[n];
        for (int i = 0; i < n; i++) {
        	valuesAsDoubles[i] = (double) values[i];
        }
        this.array.setValuesAsDoubles(valuesAsDoubles);
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param v content of the new vector, provide as source vector
     */
    public ProposedVector(ProposedIVector v) {
    	this.array=new Array(v.length());
        this.array.setValuesAsDoubles(v.getValuesAsDoubles(false));
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param v content of the new vector, provide as source vector
     */
    public ProposedVector(IArray array, boolean copyValues) {
    	this.array = new Array(array.getDimensions());
        this.array.setValuesAsDoubles(array.getValuesAsDoubles(copyValues));
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param valuestring content of the vector as array string, eg. "[1.0,2.0]"
     */
    public ProposedVector(String valuestring) {
        int ifirst = valuestring.indexOf("[") + 1;
        int ilast = valuestring.indexOf("]");
        String buffer = valuestring.substring(ifirst, ilast);
        String[] values = buffer.split(",");
        int n = values.length;
        this.array = new Array(n);
        for (int i = 0; i < this.length(); i++) {
            try {
				this.array.setValueAsDouble(i, Double.parseDouble(values[i]));
			} catch (NumberFormatException e) {
				throw new RuntimeException("Error parsing vector at "+values[i]+" in "+valuestring);
				//e.printStackTrace();
			}
        }
    }

    public ProposedVector(List<ProposedIVector> vectorList) {
        int totalSize = 0;
        for (ProposedIVector vector : vectorList) {
            totalSize += vector.length();
        }
        this.array = new Array(totalSize);
        int startInArray = 0;
        for (ProposedIVector vector : vectorList) {
            double[] values = vector.getValuesAsDoubles();
            this.array.setValuesAsDoubles(startInArray, startInArray+values.length-1, values);
            startInArray += vector.length();
        }
    }

    /**
     * Write Vector to string
     * <p/>
     */
    public String toString() {
    	double[] content=this.array.getValuesAsDoubles(false);
        String temp = "[";
        if (this.length() < maxFullExpandLength) {
            for (int i = 0; i < this.length(); i++) {
                temp = temp + content[i];
                if (i < this.length() - 1) {
                    temp += ",";
                }
            }
        } else {
            for (int i = 0; i < maxFullExpandLength / 2; i++) {
                temp = temp + content[i];
                temp += ",";
            }
            temp += "...,";
            for (int i = this.length() - maxFullExpandLength / 2; i < this.length(); i++) {
                temp = temp + content[i];
                if (i < this.length() - 1) {
                    temp += ",";
                }
            }
        }
        temp = temp + "]";
        return temp;
    }

    public String printString(String indent) {
    	double[] content=this.array.getValuesAsDoubles(false);
        String temp = indent + "[";
        final int maxFullExpandLength = 20;

        if (this.length() == 1) {
            temp = indent + "= " + PrintNumber.printNumber(content[0]);
            return temp;
        }
        int n=this.length();
        if (n < maxFullExpandLength) {
            for (int i = 0; i < n; i++) {
                temp = temp + PrintNumber.printNumber(content[i]);
                if (i < n - 1) {
                    temp += ",";
                }
            }
        } else {
            for (int i = 0; i < maxFullExpandLength / 2; i++) {
                temp = temp + PrintNumber.printNumber(content[i]);
                temp += ",";
            }
            temp += "...,";
            for (int i = n - maxFullExpandLength / 2; i < n; i++) {
                temp = temp + PrintNumber.printNumber(content[i]);
                if (i < n - 1) {
                    temp += ",";
                }
            }
        }
        temp = temp + "]";
        return temp;
    }

    /**
     * {@inheritDoc}
     */
    public void setConstant(double value) {
        this.array.setConstant(value);
    }

    /**
     * {@inheritDoc}
     */
    public void scale(double alpha) {
        double[] content = this.array.getValuesAsDoubles(false);
        int n=this.length();
        for(int i=0;i<n;i++){
        	content[i]*=alpha;
        }
        this.array.setValuesAsDoubles(content);
    }

    /**
     * {@inheritDoc}
     */
    public void setValues(double[] values) {
        this.array.setValuesAsDoubles(values);
    }

    /**
     * {@inheritDoc}
     */
    public double[] getValues() {
        return this.array.getValuesAsDoubles();
    }

    /**
     * {@inheritDoc}
     */
    public void axpy(double alpha, ProposedIVector x) {
        if (this.length() != x.length()) {
            throw new RuntimeException("Vector.axpy: x.length(" + x.length() + ") != internal length (" + this.length() + ")");
        }
        this.array.axpyOnValues(alpha, x.getValuesAsDoubles());
    }

    /**
     * {@inheritDoc}
     */
    public double dotProduct(ProposedIVector otherVector) {
        if (this.length() != otherVector.length()) {
            throw new RuntimeException("Vector.dotProduct: otherVector.length(" + otherVector.length() + ") != internal length (" + this.length() + ")");
        }
        double sum = 0;
        double[] content = this.array.getValuesAsDoubles(false);
        double[] values = otherVector.getValuesAsDoubles(false);
        for (int i = 0; i < this.length(); i++) {
            sum += content[i] * values[i];
        }
        return sum;
    }

    /**
     * {@inheritDoc}
     */
    public double norm2() {
        double out = 0;
        double[] content=this.array.getValuesAsDoubles(false);
        for (int i = 0; i < this.length(); i++) {
            out += content[i] * content[i];
        }
        return Math.sqrt(out);
    }

    /**
     * {@inheritDoc}
     */
    public void pointwiseDivide(ProposedIVector otherVector) {
        if (this.length() != otherVector.length()) {
            throw new RuntimeException("Vector.pointwiseDivide: otherVector.length(" + otherVector.length() + ") != internal length (" + this.length() + ")");
        }
        double[] content=this.array.getValuesAsDoubles(false); //changing in place here.
        double[] values = otherVector.getValuesAsDoubles(false);
        for (int i = 0; i < this.length(); i++) {
            content[i] = content[i] / values[i];
        }
    }

    /**
     * {@inheritDoc}
     */
    public void pointwiseMultiply(ProposedIVector otherVector) {
        if (this.length() != otherVector.length()) {
            throw new RuntimeException("Vector.pointwiseMultiply: otherVector.length(" + otherVector.length() + ") != internal length (" + this.length() + ")");
        }
        double[] content=this.array.getValuesAsDoubles(false); //changing in place here.
        double[] values = otherVector.getValuesAsDoubles(false);
        for (int i = 0; i < this.length(); i++) {
            content[i] = content[i] * values[i];
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public ProposedVector clone() {
        ProposedVector out = new ProposedVector(this.array,true);
        return out;
    }

    /**
     * Free a Vector Instance.
     */
    public void free() {
    } //nothing to do, but required to match interface


    /**
     * Create a vector as a range of numbers
     *
     * @param xmin  first number
     * @param xmax  last number is less or equal this number
     * @param xstep increment between numbers
     * @return Vector containing the numbers defined by min,max,step.
     */
    public static ProposedIVector range(double xmin, double xmax, double xstep) {
        int n = (int) Math.floor((xmax - xmin) / xstep) + 1;
        ProposedIVector result = new ProposedVector(n);
        double[] content=result.getValuesAsDoubles(false); //change in place!
        double x = xmin;
        for (int i = 0; i < n; i++) {
            content[i]=x;
            x += xstep;
        }
        return result;
    }

    /**
     * Compute pointwise sqrt of a vector x(i) <= sqrt(x(i))
     */
    public void sqrt() {
        double[] content=this.array.getValuesAsDoubles(false); //change in place!
        for (int i = 0; i < this.length(); i++) {
            content[i] = Math.sqrt(content[i]);
        }
    }


    public void serialize(PrintStream outputStream) {
        outputStream.print("[");
        double[] content=this.array.getValuesAsDoubles(false);
        for (int i = 0; i < this.length(); i++) {
            outputStream.print(content[i]);
            if (i < this.length() - 1) {
                outputStream.print(",");
            }
        }
        outputStream.print("]");
    }

    /**
     * Concatenate two vectors
     *
     * @param v1 first part
     * @param v2 second part
     * @return [v1,v2] concatenated
     */
    public static ProposedVector concatenate(ProposedIVector v1, ProposedIVector v2) {
        double temp[] = new double[v1.length() + v2.length()];
        System.arraycopy(v1.getValuesAsDoubles(), 0, temp, 0, v1.length());
        System.arraycopy(v2.getValuesAsDoubles(), 0, temp, v1.length(), v2.length());
        return new ProposedVector(temp);
    }

    public void invert() {
        double[] content=this.array.getValuesAsDoubles(false); //change in place!
        for (int i = 0; i < this.length(); i++) {
            content[i] = 1.0 / content[i];
        }
    }

	public void writeExternal(ObjectOutput objectOutput) throws IOException {
        double[] content=this.array.getValuesAsDoubles(false);
		objectOutput.writeObject(content);
	}

	public void readExternal(ObjectInput objectInput) throws IOException, ClassNotFoundException {
		double[] content= (double[]) objectInput.readObject();
        this.array.setValuesAsDoubles(content);
	}

	public int getNumberOfDimensions() {
		return this.array.getNumberOfDimensions();
	}

	public int[] getDimensions() {
		return this.array.getDimensions();
	}

	public int length() {
		return this.array.length();
	}

	public double[] getValuesAsDoubles() {
		return this.array.getValuesAsDoubles();
	}

	public double[] getValuesAsDoubles(boolean copyValues) {
		return this.array.getValuesAsDoubles(copyValues);
	}

	public double[] getValuesAsDoubles(int firstIndex, int lastIndex) {
		return this.array.getValuesAsDoubles(firstIndex, lastIndex);
	}

	public double getValueAsDouble(int[] indices) {
		return this.array.getValueAsDouble(indices);
	}

	public void setValuesAsDoubles(double[] values) {
		this.array.setValuesAsDoubles(values);
	}

	public void setValuesAsDoubles(int firstIndex, int lastIndex,
			double[] values) {
		this.array.setValuesAsDoubles(firstIndex, lastIndex, values);
	}

	public void setValueAsDouble(int[] indices, double value) {
		this.array.setValueAsDouble(indices, value);
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		this.array.axpyOnValues(alpha, axpyValues);
	}

	public void multiplyValues(double[] multiplicationFactors) {
		this.array.multiplyValues(multiplicationFactors);
	}

	public void reshape(int[] dimensions) {
		this.array.reshape(dimensions);
	}

	public boolean allowsGrowingFirstDimension() {
		return this.array.allowsGrowingFirstDimension();
	}

	public IArray getSlice(int dimension, int index) {
		return this.array.getSlice(dimension, index);
	}

	public IArray getSlice(int dimension, int minIndex, int maxIndex) {
		return this.array.getSlice(dimension, minIndex, maxIndex);
	}

	public double[] getSliceAsDoubles(int dimension, int minIndex, int maxIndex) {
		return this.array.getSliceAsDoubles(dimension, minIndex, maxIndex);
	}

	public void setSlice(IArray slice, int dimension, int index) {
		this.array.setSlice(slice, dimension, index);
	}

	public void setSlice(double[] slice, int dimension, int index) {
		this.array.setSlice(slice,dimension,index);
	}

	public void setSlice(IArray slice, int dimension, int minIndex, int maxIndex) {
		this.array.setSlice(slice, dimension, minIndex, maxIndex);
	}

	public void setSlice(double[] slice, int dimension, int minIndex,
			int maxIndex) {
		this.array.setSlice(slice, dimension, minIndex, maxIndex);
	}

	public int valueIndex(int[] indices) {
		return this.array.valueIndex(indices);
	}

	public double getValueAsDouble(int index) {
		return this.array.getValueAsDouble(index);
	}

	public void setValueAsDouble(int index, double value) {
		this.array.setValueAsDouble(index, value);
	}

	public IArray getArray() {
		return this.array;
	}


}
