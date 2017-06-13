/* OpenDA v2.4 
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
package org.openda.utils;
import org.openda.interfaces.IVector;
import java.io.*;
import java.util.List;


/**
 * VectorFloat
 */
public class VectorFloat implements IVector, Externalizable {

    /* use java doubles for storage
    */
    private float content[];
    private int length = 0;
    //public int maxFullExpandLength = 20;


    // simple constructor for derived simple-tree-vector
    public VectorFloat() {
    }

    /**
     * Create a new VectorFloat
     * <p/>
     *
     * @param n Length of the vector
     */
    public VectorFloat(int n) {
        this.content = new float[n];
        this.length = n;
    }

    /**
     * Create a new VectorFloat
     * <p/>
     *
     * @param values content of the vector as array of doubles
     */
    public VectorFloat(double[] values) {
        int n = values.length;
        this.content = new float[n];
        this.length = n;
		for (int i=0; i<n;i++){
			this.content[i]= (float) values[i];
		}
    }

    /**
     * Create a new VectorFloat
     * <p/>
     *
     * @param values content of the vector as array of ints
     */
    public VectorFloat(int[] values) {
        int n = values.length;
        this.content = new float[n];
        this.length = n;

        for (int i = 0; i < n; i++) {
            this.content[i] = (float) values[i];
        }
    }

    /**
     * Create a new VectorFloat
     * <p/>
     *
     * @param v content of the new vector, provide as source vector
     */
    public VectorFloat(IVector v) {
        this.content = doubleToFloat(v.getValues());
        this.length = v.getSize();
    }

    /**
     * Create a new VectorFloat
     * <p/>
     *
     * @param valuestring content of the vector as array string, eg. "[1.0,2.0]"
     */
    public VectorFloat(String valuestring) {
        int ifirst = valuestring.indexOf("[") + 1;
        int ilast = valuestring.indexOf("]");
        String buffer = valuestring.substring(ifirst, ilast);
        String[] values = buffer.split(",");
        int n = values.length;
        this.content = new float[n];
        this.length = n;
        for (int i = 0; i < this.length; i++) {
            try {
				this.content[i] = Float.parseFloat(values[i]);
			} catch (NumberFormatException e) {
				throw new RuntimeException("Error parsing vector at "+values[i]+" in "+valuestring);
				//e.printStackTrace();
			}
        }
    }

    public VectorFloat(List<IVector> vectorList) {
        int totalSize = 0;
        for (IVector vector : vectorList) {
            totalSize += vector.getSize();
        }
        this.length = totalSize;
        this.content = new float[totalSize];
        int startInArray = 0;
        for (IVector vector : vectorList) {
            double[] values = vector.getValues();
			for (int i=0; i<values.length; i++){
				this.content[startInArray+i]=(float) values[i];
			}
            startInArray += vector.getSize();
        }
    }

    /**
     * Write Vector to string
     * <p/>
     */
    public String toString(int maxFullExpandLength) {
        String temp = "[";
        if (this.length < maxFullExpandLength) {
            for (int i = 0; i < this.length; i++) {
                temp = temp + this.content[i];
                if (i < this.length - 1) {
                    temp += ",";
                }
            }
        } else {
            for (int i = 0; i < maxFullExpandLength / 2; i++) {
                temp = temp + this.content[i];
                temp += ",";
            }
            temp += "...,";
            for (int i = this.length - maxFullExpandLength / 2; i < this.length; i++) {
                temp = temp + this.content[i];
                if (i < this.length - 1) {
                    temp += ",";
                }
            }
        }
        temp = temp + "]";
        return temp;
    }

    public String printString(String indent) {
        String temp = indent + "[";
        final int maxFullExpandLength = 20;

        if (this.length == 1) {
            temp = indent + "= " + PrintNumber.printNumber(this.content[0]);
            return temp;
        }
        if (this.length < maxFullExpandLength) {
            for (int i = 0; i < this.length; i++) {
                temp = temp + PrintNumber.printNumber(this.content[i]);
                if (i < this.length - 1) {
                    temp += ",";
                }
            }
        } else {
            for (int i = 0; i < maxFullExpandLength / 2; i++) {
                temp = temp + PrintNumber.printNumber(this.content[i]);
                temp += ",";
            }
            temp += "...,";
            for (int i = this.length - maxFullExpandLength / 2; i < this.length; i++) {
                temp = temp + PrintNumber.printNumber(this.content[i]);
                if (i < this.length - 1) {
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
        for (int i = 0; i < this.length; i++) {
            this.content[i] = (float) value;
        }
    }

    /**
     * {@inheritDoc}
     */
    public void scale(double alpha) {
        for (int i = 0; i < this.length; i++) {
            this.content[i] = (float) alpha * this.content[i];
        }
    }

    /**
     * {@inheritDoc}
     */
    public void setValues(double[] values) {
        if (this.length != values.length) {
            throw new RuntimeException("VectorFloat.setValues: values.length(" + values.length + ") != internal length (" + this.length + ")");
        }
		doubleToFloat(values, this.content);
    }

    /**
     * {@inheritDoc}
     */
    public double[] getValues() {
		return floatToDouble(this.content);
    }

    /**
     * {@inheritDoc}
     */
    public void setValue(int index, double value) {
        if (index >= length) {
            throw new RuntimeException("index (" + index + ") > length (" + length + ")");
        }
        this.content[index] = (float) value;
    }

    /**
     * {@inheritDoc}
     */
    public double getValue(int index) {
        if (index >= length) {
            throw new RuntimeException("index (" + index + ") > length (" + length + ")");
        }
        return this.content[index];
    }

    /**
     * {@inheritDoc}
     */
    public int getSize() {
        return this.length;
    }

    /**
     * {@inheritDoc}
     */
    public void axpy(double alpha, IVector x) {
        if (this.length != x.getSize()) {
            throw new RuntimeException("VectorFloat.axpy: x.getSize(" + x.getSize() + ") != internal length (" + this.length + ")");
        }
		if (x instanceof VectorFloat){
			VectorFloat v=(VectorFloat) x;
			for (int i = 0; i < this.length; i++) {
            	this.content[i] = this.content[i] + (float) alpha * v.content[i];
			}
		}
		else {
        	for (int i = 0; i < this.length; i++) {
            	this.content[i] = this.content[i] + (float) (alpha * x.getValue(i));
			}
		}
    }

    /**
     * {@inheritDoc}
     */
    public double dotProduct(IVector otherVector) {
        if (this.length != otherVector.getSize()) {
            throw new RuntimeException("VectorFloat.dotProduct: otherVector.getSize(" + otherVector.getSize() + ") != internal length (" + this.length + ")");
        }
        double sum = 0;
		if (otherVector instanceof VectorFloat){
			VectorFloat v = (VectorFloat) otherVector;
			for (int i = 0; i < this.length; i++) {
				sum += this.content[i] * v.content[i];
			}
		}
		else {
        	for (int i = 0; i < this.length; i++) {
            	sum += this.content[i] * otherVector.getValue(i);
        	}
		}
        return sum;
    }

    /**
     * {@inheritDoc}
     */
    public double norm2() {
        double out = 0;
        for (int i = 0; i < this.length; i++) {
            out += this.content[i] * this.content[i];
        }
        return Math.sqrt(out);
    }

    /**
     * {@inheritDoc}
     */
    public void pointwiseDivide(IVector otherVector) {
        if (this.length != otherVector.getSize()) {
            throw new RuntimeException("VectorFloat.pointwiseDivide: otherVector.getSize(" + otherVector.getSize() + ") != internal length (" + this.length + ")");
        }
		if (otherVector instanceof VectorFloat){
			VectorFloat v = (VectorFloat) otherVector;
			for (int i = 0; i < this.length; i++) {
				this.content[i] = this.content[i] / v.content[i];
			}
		}
		else {
        	for (int i = 0; i < this.length; i++) {
            	this.content[i] = this.content[i] / (float) otherVector.getValue(i);
        	}
		}
    }

    /**
     * {@inheritDoc}
     */
    public void pointwiseMultiply(IVector otherVector) {
        if (this.length != otherVector.getSize()) {
            throw new RuntimeException("VectorFloat.pointwiseMultiply: otherVector.getSize(" + otherVector.getSize() + ") != internal length (" + this.length + ")");
        }
		if (otherVector instanceof VectorFloat){
		   	VectorFloat otherV=(VectorFloat) otherVector;
			for (int i = 0; i < this.length; i++) {
            	this.content[i] = this.content[i] * otherV.content[i];
        	}
		}
		else {
        	for (int i = 0; i < this.length; i++) {
            	this.content[i] = this.content[i] * (float) otherVector.getValue(i);
        	}
		}
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    
    public VectorFloat clone() {
        VectorFloat out = new VectorFloat(this.length);
        System.arraycopy(this.content, 0, out.content, 0, this.length);
        return out;
    }

    /**
     * Free a VectorFloat Instance.
     */
    public void free() {
    } //nothing to do, but required to match interface


    /**
     * Create a vectorFloat as a range of numbers
     *
     * @param xmin  first number
     * @param xmax  last number is less or equal this number
     * @param xstep increment between numbers
     * @return Vector containing the numbers defined by min,max,step.
     */
    public static IVector range(double xmin, double xmax, double xstep) {
        int n = (int) Math.floor((xmax - xmin) / xstep) + 1;
        IVector result = new VectorFloat(n);
        double x = xmin;
        for (int i = 0; i < n; i++) {
            result.setValue(i, x);
            x += xstep;
        }
        return result;
    }

    /**
     * Compute pointwise sqrt of a vector x(i) <= sqrt(x(i))
     */
    public void sqrt() {
        for (int i = 0; i < this.length; i++) {
            this.content[i] = (float) Math.sqrt(this.content[i]);
        }
    }


    public void serialize(PrintStream outputStream) {
        outputStream.print("[");
        for (int i = 0; i < this.length; i++) {
            outputStream.print(this.content[i]);
            if (i < this.length - 1) {
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
    public static VectorFloat concatenate(IVector v1, IVector v2) {
        double temp[] = new double[v1.getSize() + v2.getSize()];
        System.arraycopy(v1.getValues(), 0, temp, 0, v1.getSize());
        System.arraycopy(v2.getValues(), 0, temp, v1.getSize(), v2.getSize());
        return new VectorFloat(temp);
    }

    public void invert() {
        for (int i = 0; i < this.length; i++) {
            this.content[i] = (float) (1.0 / this.content[i]);
        }
    }

	private float[] doubleToFloat(double [] dVals){
		float[] retVal=new float[dVals.length];
		for (int i=0; i<dVals.length; i++){
			retVal[i]= (float) dVals[i];
		}
		return retVal;
	}

	private void doubleToFloat(double [] dVals, float  [] fVals){
		for (int i=0; i<dVals.length; i++){
			fVals[i]= (float) dVals[i];
		}
	}

	private double[] floatToDouble(float [] fVals){
			double[] retVal=new double[fVals.length];
			for (int i=0; i<fVals.length; i++){
				retVal[i]= (double) fVals[i];
			}
		return retVal;
	}


	public void writeExternal(ObjectOutput objectOutput) throws IOException {
		objectOutput.writeInt(length);
		objectOutput.writeObject(content);
	}

	public void readExternal(ObjectInput objectInput) throws IOException, ClassNotFoundException {
		length = objectInput.readInt();
		content= (float[]) objectInput.readObject();
	}



}
