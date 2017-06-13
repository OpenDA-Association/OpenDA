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

import org.openda.costa.CtaVector;
import org.openda.interfaces.IVector;
import org.openda.utils.performance.OdaGlobSettings;

import java.io.*;
import java.util.List;
import java.util.Set;

/**
 * Vector
 */
public class Vector implements IVector, Externalizable {

    IVector dVec=null;
	VectorFloat fVec=null;
	public int maxFullExpandLength = 20;
	private boolean doublePrecision=true;
    private boolean nativeVector=false;


	private void setAndCheckSettigs(){
		doublePrecision=!OdaGlobSettings.getVectorPrecisionFloat();
		nativeVector=OdaGlobSettings.getVectorIsNative();
		if (doublePrecision==false && nativeVector==true){
			throw new RuntimeException("You have specified to use single precision vectors in combination with native vector implementation. This combination is not yet supported. Either choose to use non-native vectors or select double precision or add support to OpenDA doing some programming ;-)");
		}
	}


    // simple constructor for derived simple-tree-vector
    public Vector() {
		setAndCheckSettigs();
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param n Length of the vector
     */
    public Vector(int n) {
		setAndCheckSettigs();

		if (doublePrecision){
			if (nativeVector){
				dVec = (IVector) new CtaVector(n);
			}
			else {
				dVec = new VectorDouble(n);
			}
		}
		else {
			fVec=new VectorFloat(n);
		}
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param values content of the vector as array of doubles
     */
    public Vector(double[] values) {
		setAndCheckSettigs();
		if (doublePrecision){
			if (nativeVector){
				dVec = (IVector) new CtaVector(values.length);
				dVec.setValues(values);
			}
			else {
				dVec=new VectorDouble(values);
			}
		}
		else {
			fVec=new VectorFloat(values);
		}
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param values content of the vector as array of ints
     */
    public Vector(int[] values) {
		setAndCheckSettigs();
		if (doublePrecision){
			dVec=new VectorDouble(values);
		}
		else {
			fVec=new VectorFloat(values);
		}
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param v content of the new vector, provide as source vector
     */
    public Vector(IVector v) {
		setAndCheckSettigs();
		if (doublePrecision){
			dVec=new VectorDouble(castVector(v));
		}
		else {
			fVec=new VectorFloat(castVector(v));
		}
    }

    /**
     * Create a new Vector
     * <p/>
     *
     * @param valuestring content of the vector as array string, eg. "[1.0,2.0]"
     */
    public Vector(String valuestring) {
		setAndCheckSettigs();
		if (doublePrecision){
			dVec=new VectorDouble(valuestring);
		}
		else {
			fVec=new VectorFloat(valuestring);
		}
    }

    public Vector(List<IVector> vectorList) {
		setAndCheckSettigs();
		if (doublePrecision){
			dVec=new VectorDouble(vectorList);
		}
		else {
			fVec=new VectorFloat(vectorList);
		}
    }

    /**
     * Write Vector to string
     */
    public String toString() {
    	return toString(this.maxFullExpandLength);
    }

    public String toString(int maxFullExpandLength) {
		if (doublePrecision){
			if (dVec instanceof VectorDouble) {
				return ((VectorDouble) dVec).toString(maxFullExpandLength);
			}
			else{
				return dVec.toString();
			}
		}
		else {
			return fVec.toString(maxFullExpandLength);
		}
    }

    public String printString(String indent) {
		if (doublePrecision){
			return dVec.printString(indent);
		}
		else {
			return fVec.printString(indent);
		}
    }

    /**
     * {@inheritDoc}
     */
    public void setConstant(double value) {
		if (doublePrecision){
			dVec.setConstant(value);
		}
		else {
			fVec.setConstant(value);
		}
    }

    /**
     * {@inheritDoc}
     */
    public void scale(double alpha) {
		if (doublePrecision){
		   dVec.scale(alpha);
		}
		else {
		   fVec.scale(alpha);
		}
    }

    /**
     * {@inheritDoc}
     */
    public void setValues(double[] values) {
		if (doublePrecision){
			dVec.setValues(values);
		}
		else {
			fVec.setValues(values);
		}
    }

    /**
     * {@inheritDoc}
     */
    public double[] getValues() {
		if (doublePrecision){
			return dVec.getValues();
		}
		else {
			return fVec.getValues();
		}
    }

    /**
     * {@inheritDoc}
     */
    public void setValue(int index, double value) {
		if (doublePrecision){
			dVec.setValue(index,value);
		}
		else {
			fVec.setValue(index,value);
		}
    }

    /**
     * {@inheritDoc}
     */
    public double getValue(int index) {
		if (doublePrecision){
			return dVec.getValue(index);
		}
		else {
			return fVec.getValue(index);
		}
    }

    /**
     * {@inheritDoc}
     */
    public int getSize() {
		if (doublePrecision){
			return dVec.getSize();
		}
		else {
			return fVec.getSize();
		}
    }

    /**
     * {@inheritDoc}
     */
    public void axpy(double alpha, IVector x) {
		if (doublePrecision){
			dVec.axpy(alpha,castVector(x));
		}
		else {
			fVec.axpy(alpha,castVector(x));
		}
    }

    /**
     * {@inheritDoc}
     */
    public double dotProduct(IVector otherVector) {
		if (doublePrecision){
			return dVec.dotProduct(castVector(otherVector));
		}
		else {
			return fVec.dotProduct(castVector(otherVector));
		}
    }

    /**
     * {@inheritDoc}
     */
    public double norm2() {
		if (doublePrecision){
			return dVec.norm2();
		}
		else {
			return fVec.norm2();
		}
    }

    /**
     * {@inheritDoc}
     */
    public void pointwiseDivide(IVector otherVector) {
		if (doublePrecision){
			dVec.pointwiseDivide(castVector(otherVector));
		}
		else {
			fVec.pointwiseDivide(castVector(otherVector));
		}
    }

    /**
     * {@inheritDoc}
     */
    public void pointwiseMultiply(IVector otherVector) {
		if (doublePrecision){
			dVec.pointwiseMultiply(castVector(otherVector));
		}
		else {
			fVec.pointwiseMultiply(castVector(otherVector));
		}
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    
    public Vector clone() {
		Vector cloned=new Vector();
		if (doublePrecision){
			  cloned.dVec=this.dVec.clone();
		}
		else {
			cloned.fVec=this.fVec.clone();
		}
		return cloned;
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
    public static IVector range(double xmin, double xmax, double xstep) {
        Vector newVec=new Vector();
		if (newVec.doublePrecision){
		   newVec.dVec= (VectorDouble) VectorDouble.range(xmin,xmax,xstep);
		}
		else {
			newVec.fVec= (VectorFloat) VectorFloat.range(xmin,xmax,xstep);
		}
		return newVec;
    }

    /**
     * Compute pointwise sqrt of a vector x(i) <= sqrt(x(i))
     */
    public void sqrt() {
		if (doublePrecision){
			dVec.sqrt();
		}
		else {
			fVec.sqrt();
		}
    }

    /**
     * Concatenate two vectors
     *
     * @param v1 first part
     * @param v2 second part
     * @return [v1,v2] concatenated
     */
    public static Vector concatenate(IVector v1, IVector v2) {
        Vector newVec=new Vector();
		if (newVec.doublePrecision){
		  	newVec.dVec=VectorDouble.concatenate(v1,v2);
		}
		else {
			newVec.fVec=VectorFloat.concatenate(v1,v2);
		}
		return newVec;
    }

	/**
	 * Compute 1./x(i) for all elements i in Vector
	 */
    public void invert() {
		if (doublePrecision){
			if (dVec instanceof VectorDouble) {
				((VectorDouble) dVec).invert();
			}
			else {
				throw new RuntimeException("Hmm, method is using invert with is not officially part of the vector interface. Please do not use a special default vector inplementation");
			}
		}
		else {
			fVec.invert();
		}
    }

	public void serialize(PrintStream outputStream) {
		if(doublePrecision){
			if (dVec instanceof VectorDouble) {
				((VectorDouble) dVec).serialize(outputStream);
			}
			else {
				outputStream.print(this.toString());
			}
		}
		else {
			fVec.serialize(outputStream);
		}
 }
	/**
	 * write values to output object
	 * @param objectOutput output object
	 */
	public void writeExternal(ObjectOutput objectOutput) throws IOException {
		objectOutput.writeBoolean(doublePrecision);
		if(doublePrecision){
			objectOutput.writeObject(dVec);
		}
		else {
			objectOutput.writeObject(fVec);
		}
	}
	/**
	 * get values from input object
	 * @param objectInput input object
	 */
	public void readExternal(ObjectInput objectInput) throws IOException, ClassNotFoundException {
		doublePrecision = objectInput.readBoolean();
		if(doublePrecision){
			dVec=(VectorDouble) objectInput.readObject();
		}
		else {
			fVec=(VectorFloat) objectInput.readObject();
		}
	}

	// Return handle to one of the sub-vector classes (used for argument in Vector-Vector operations
	// This will often results in operations between similar vector classes improving performance significantly
	private IVector castVector(IVector vecIn){
		if (vecIn instanceof Vector){
			Vector vec=(Vector) vecIn;
			if (vec.dVec!=null){
				return vec.dVec;
			}
			else {
				return vec.fVec;
			}
		}
		else {
			return vecIn;
		}
	}

	/**
     * Removes one entry from a Vector
     * @param index index to remove from Vector
     * @return resulting Vector; original Vector for incorrect index value
     */
    public Vector remove_entry(int index){
		int n = this.getSize();
		if (index < 0 || index > n-1) return this;
		Vector temp = new Vector(n-1);
        for ( int i=0 ; i<index ; i++) {
		    temp.setValue(i,this.getValue(i));
		}
		for (int i=index+1 ; i<n ; i++){
			temp.setValue(i-1,this.getValue(i));
		}
    	return temp;
    }

	/**
	 * Return part of the Vector
	 * @param i1 start index for selection
	 * @param i2 end index for selection
	 * @return resulting Vector; null if i2 &lt i1.
	 */
	public Vector get_selection(int i1, int i2){
		if (i1<0) i1 = 0;
		if (i2 < i1) return null;
		int n = this.getSize();
		if (i2>n-1) i2=n-1;

		Vector temp = new Vector((i2-i1)+1);
		for ( int i=i1 ; i<=i2 ; i++) {
			temp.setValue(i-i1,this.getValue(i));
		}
		return temp;
	}

}
